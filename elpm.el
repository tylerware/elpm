;;; elpm.el --- An elisp cli package manager -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Tyler Ware
;;
;; Author: Tyler Ware <https://github.com/tylerware>
;; Maintainer: Tyler Ware <https://github.com/tylerware>
;; Created: December 22, 2020
;; Modified: December 22, 2020
;; Version: 0.1.0
;; Keywords:
;; Homepage: https://github.com/tylerware/elpm
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  An elisp cli package manager.
;;
;;  This package manager leverages straight.el.
;;
;;; Code:
(require 'seq)
(require 'info)

(defconst elpm-version 0)

(defvar elpm-directory user-emacs-directory
  "The directory to install packages into.")

(defun elpm-use-packages (recipes &optional directory wait)
  "Load / install RECIPES in the DIRECTORY.

If DIRECTORY is not defined then `elpm-directory' is used.

Note that this also loads all previously installed packages in DIRECTORY."
  (let ((p
         (funcall (if wait #'elpm--sync #'elpm--async)
                  `(progn
                     (require 'elpm)
                     (require 'info)
                     (let ((elpm-directory ,(or directory elpm-directory))
                           (original-load-path load-path)
                           (original-Info-directory-list Info-directory-list))

                       ;; Bootstrap straight
                       (elpm--straight)
                       ;; Make sure we have all the current packages
                       (elpm-use-packages--intern (plist-get
                                                   (elpm-get-packages-plist)
                                                   :recipes))
                       ;; Add new recipes
                       (elpm-use-packages--intern ',recipes)
                       ;; Take the diff of the load path to return to the parent process
                       (message
                        "##elpm-details:begin##
%s
##elpm-details:end##"
                        (prin1-to-string
                         (list :load-path (seq-difference load-path original-load-path)
                               :info-directory-list (seq-difference Info-directory-list original-Info-directory-list))))))
                  #'(lambda (process &optional _ignore)
                      (when (memq (process-status process) '(exit signal))
                        (elpm--process-output-buffer (process-buffer process)))))))

    (if (bufferp p)
        (elpm--process-output-buffer p)
      (set-process-filter p (lambda (p output)
                              (setq output (replace-regexp-in-string "\n$" "" output))
                              (when (string-match-p "^##elpm-details:begin##" output)
                                (setq elpm--consuming-elpm-details t))
                              (if elpm--consuming-elpm-details 
                                  (with-current-buffer (process-buffer p)
                                    (when (string-match-p "##elpm-details:end##" output)
                                      (setq elpm--consuming-elpm-details nil))
                                    (goto-char (point-max))
                                    (insert output))
                                (message output)))))))


(defun elpm--process-output-buffer (buffer)
  "Process the output BUFFER.

This loads values into `load-path'and into `Info-directory-list'.
"
  (let* ((details (with-current-buffer buffer 
                    (save-excursion
                      (save-match-data
                        (goto-char (point-min))
                        (let ((start
                               (when (re-search-forward "^##elpm-details:begin##$" nil t)
                                 (forward-line)
                                 (beginning-of-line)
                                 (point)))
                              (end
                               (when (re-search-forward "^##elpm-details:end##$" nil t)
                                 (forward-line -1)
                                 (end-of-line)
                                 (point))))

                          (when (and start end)
                            (car (read-from-string (buffer-substring start end)))))))))
         (load-path-additions (plist-get details :load-path))
         (info-directory-list-additions (plist-get details :info-directory-list)))

    (when (listp load-path-additions)
      (dolist (path load-path-additions)
        (when (stringp path)
          (add-to-list 'load-path path))))

    (when (listp info-directory-list-additions)
      (dolist (dir info-directory-list-additions)
        (when (stringp dir)
          (add-to-list 'Info-directory-list dir))))))


(defvar elpm--consuming-elpm-details nil
  "elpm running in the subprocess send feedback that is buffered. This variable
indicates that we are reading data from the subprocess (specifically lisp data)")


(defun elpm-use-package (recipe &optional directory)
  "Load / install a RECIPE in the DIRECTORY.

See `elpm-use-packages' for more."
  (elpm-use-packages (list recipe) directory t))

(defun elpm-use-all (&optional directory)
  "Load all packages previously installed in the DIRECTORY.

See `elpm-use-packages' for more."
  (elpm-use-packages () directory))

(defun elpm-get-packages-file-name (&optional directory)
  "Get the packages file for the DIRECTORY."
  (expand-file-name "elpm-packages.el" (or directory
                                           elpm-directory)))

(defun elpm-get-packages-plist (&optional directory)
  "Get the packages plist from the packages file in the DIRECTORY."
  (let* ((file-name (elpm-get-packages-file-name directory))
         (file-contents (when (file-exists-p file-name)
                          (with-temp-buffer
                            (insert-file-contents file-name)
                            (buffer-string))))
         (packages-data (and file-contents
                             (not (equal file-contents ""))
                             (car (read-from-string file-contents)))))
    (when (listp packages-data)
      packages-data)))

(defun elpm-save-packages-plist (packages-plist &optional directory)
  "Save the PACKAGES-PLIST to a packages file in the DIRECTORY."
  (let ((file-name (elpm-get-packages-file-name directory))
        (contents (pp-to-string packages-plist)))
    (with-temp-buffer
      (insert contents)
      (write-region (point-min) (point-max) file-name))))

(defun elpm--get-existing-recipe (recipe recipes)
  (let ((package-name (if (listp recipe) (car recipe)
                        recipe)))
    (catch 'similar-recipe
      (dolist (existing-recipe recipes)
        (let ((existing-package-name (if (listp existing-recipe)
                                         (car existing-recipe)
                                       existing-recipe)))
          (when (eq package-name existing-package-name)
            (throw 'similar-recipe existing-recipe)))))))

(defun elpm--new-packages-plist ()
  "Generate a new packages plist."
  `(:version ,elpm-version
             :recipes ()))

(defun elpm--async (sexp sentinel-fn)
  "Execute SEXP asyncronously in a new instance of Emacs.

This allows for a sandbox environment. The SENTINEL-FN allows
reporting of the results.

Returns the process."
  (let* ((buf (generate-new-buffer " *elpm-async*"))
         (p (start-process
             "emacs"
             buf
             (file-truename
              (expand-file-name invocation-name
                                invocation-directory))
             "-Q"
             "--batch"
             "--eval"
             (prin1-to-string sexp))))
    (set-process-sentinel p sentinel-fn)
    p))

(defun elpm--sync (sexp _)
  "Execute SEXP syncronously in a new instance of Emacs.

This allows for a sandbox environment."
  (message "Running...")
  (let ((buf (generate-new-buffer " *elpm-sync*")))
    (pop-to-buffer buf)
    (call-process
     ;; emacs binary
     (file-truename
      (expand-file-name invocation-name
                        invocation-directory))
     nil
     buf
     t 
     "-Q"
     "--batch"
     "--eval"
     (prin1-to-string sexp))
    buf))

(defun elpm--straight ()
  "Bootstrap straight.el."
  (let ((user-emacs-directory elpm-directory))
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))))

(defun elpm-update-recipes (&optional directory)
  (interactive)
  (let ((p (elpm--sync
            `(progn
               (require 'elpm)
               (let ((elpm-directory ,(or directory elpm-directory))
                     (original-load-path load-path))

                 ;; Bootstrap straight
                 (elpm--straight)
                 (let ((user-emacs-directory elpm-directory))
                   )))
            #'(lambda (a b) nil))))

    (set-process-filter p (lambda (_p output)
                            (message (replace-regexp-in-string "\n$" "" output))))))

(defmacro elpm--make-simple-command (command-name &rest sexp)
  `(defun ,(intern (concat "elpm-" (symbol-name command-name)))
       (&optional directory)
     (interactive)
     (let* ((sexp ',sexp)
            (p (elpm--async
                `(progn
                   (require 'elpm)
                   (let ((elpm-directory ,(or directory elpm-directory))
                         (original-load-path load-path))

                     ;; Bootstrap straight
                     (elpm--straight)
                     (let ((user-emacs-directory elpm-directory))
                       ,@sexp)))
                #'(lambda (a b) nil))))

       (set-process-filter p (lambda (_p output)
                               (message (replace-regexp-in-string "\n$" "" output))))

       p)))


(elpm--make-simple-command update-recipes 
                           (straight-pull-recipe-repositories))

(elpm--make-simple-command update-packages 
                           (straight-pull-all))


(defun elpm-use-packages--intern (recipes)
  "Add RECIPES to the `elpm-directory'."
  (let* ((user-emacs-directory elpm-directory)
         (straight-base-dir elpm-directory)
         (packages-plist (or (elpm-get-packages-plist)
                             (elpm--new-packages-plist)))
         (existing-recipes (plist-get packages-plist :recipes)))
    (dolist (recipe recipes)
      (straight-use-package recipe)
      (let ((existing-recipe (elpm--get-existing-recipe recipe existing-recipes)))
        (unless (equal existing-recipe recipe)
          (when existing-recipe
            (setq existing-recipes (delete existing-recipe existing-recipes)))
          (plist-put packages-plist
                     :recipes (append (plist-get packages-plist :recipes)
                                      (list recipe))))))
    (elpm-save-packages-plist packages-plist)))

(provide 'elpm)
;;; elpm.el ends here
