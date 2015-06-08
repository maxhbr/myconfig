; (ensure-package-installed 'package)
(require 'package)

(setq package-enable-at-startup nil)

;; (defun ensure-package-installed (&rest packages)
;;     "Assure every package is installed, ask for installation if it’s not.

;; Return a list of installed packages or nil for every skipped package."
;;     (mapcar
;;      (lambda (package)
;;        (if (package-installed-p package)
;;            nil
;;          (if (y-or-n-p (format "Package %s is missing. Install it? " package))
;;              (package-install package)
;;            package)))
;;      packages))

(defun require-package (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Would be better:
;; (defun require-packages (&rest packages)
(defun require-packages (packages)
  "Ensure that PACKAGEs are installed."
  (mapcar 'require-package packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
        (package-refresh-contents))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(if (fboundp 'with-eval-after-load)
    (defmacro after (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(with-eval-after-load ,feature ,@body))
  (defmacro after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defmacro delayed-init (&rest body)
  "Runs BODY after idle for a predetermined amount of time."
  (run-with-idle-timer
   1.5
   nil
   `(lambda () ,@body)))
