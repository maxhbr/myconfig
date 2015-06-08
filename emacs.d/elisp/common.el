; (ensure-package-installed 'package)
(require 'package)

(setq package-enable-at-startup nil)

(defun ensure-package-installed (&rest packages)
    "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
    (mapcar
     (lambda (package)
       (if (package-installed-p package)
           nil
         (if (y-or-n-p (format "Package %s is missing. Install it? " package))
             (package-install package)
           package)))
     packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
        (package-refresh-contents))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
