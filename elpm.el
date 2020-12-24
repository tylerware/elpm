;;; elpm.el --- An elisp cli package manager -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Tyler Ware
;;
;; Author: Tyler Ware <https://github.com/tylerware>
;; Maintainer: Tyler Ware <https://github.com/tylerware>
;; Created: December 22, 2020
;; Modified: December 22, 2020
;; Version: 0.0.1
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
(defconst elpm-version 0)

(defvar elpm-directory user-emacs-directory
  "The directory to install packages into.")

(defvar elpm--bootstrapped nil
  "Stat variable to ensure we've only bootstraped once.")

(defun elpm-bootstrap (&optional directory)
  (unless elpm--bootstrapped
    (let* ((elpm-directory (or directory
                               elpm-directory))
           (user-emacs-directory elpm-directory)
           (straight-base-dir elpm-directory)
           (straight-repository-branch "master"))
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
        (load bootstrap-file nil 'nomessage)))
    (setq elpm--bootstrapped t))
  (elpm-use-packages (plist-get
                      (elpm-get-packages-plist)
                      :recipes)))

(defun elpm-use-packages (recipes &optional directory)
  (let* ((straight-base-dir elpm-directory)
         (packages-plist (or (elpm-get-packages-plist directory)
                             (elpm--new-packages-plist)))
         (existing-recipes (plist-get packages-plist :recipes)))
    (dolist (recipe recipes)
      (straight-use-package recipe)
      (let ((existing-recipe (elpm-get-existing-recipe recipe existing-recipes)))
        (unless (equal existing-recipe recipe)
          (when existing-recipe
            (setq existing-recipes (delete existing-recipe existing-recipes)))
          (setq packages-plist
                (plist-put packages-plist
                           :recipes (append existing-recipes (list recipe)))))))
    (elpm-save-packages-plist packages-plist directory)))

(defun elpm-use-package (recipe &optional directory)
  (elpm-use-packages (list recipe) directory))

(defun elpm-get-packages-file-name (&optional directory)
  (expand-file-name "elpm-packages.el" (or directory
                                  elpm-directory)))

(defun elpm-get-packages-plist (&optional directory)
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
  (let ((file-name (elpm-get-packages-file-name directory))
        (contents (pp-to-string packages-plist)))
    (with-temp-buffer
      (insert contents)
      (write-region (point-min) (point-max) file-name))))

(defun elpm--new-packages-plist ()
  `(:version ,elpm-version
    :recipes ()))

(defun elpm-get-existing-recipe (recipe recipes)
  (let ((package-name (if (listp recipe) (car recipe)
                        recipe)))
    (catch 'similar-recipe
      (dolist (existing-recipe recipes)
        (let ((existing-package-name (if (listp existing-recipe)
                                         (car existing-recipe)
                                       existing-recipe)))
          (when (eq package-name existing-package-name)
            (throw 'similar-recipe existing-recipe)))))))

(provide 'elpm)
;;; elpm.el ends here
