;; To encrypt:
;;     mml-secure-message-encrypt-pgp
;;     mml-secure-message-sign-pgp
;; To decrypt:
;;    epa-mail-decrypt
;;
;; mu4e
(use-package mu4e
  :ensure t
  :config
  (defun mu4e-message-maildir-matches (msg rx)
    (when rx
      (if (listp rx)
          ;; If rx is a list, try each one for a match
          (or (mu4e-message-maildir-matches msg (car rx))
              (mu4e-message-maildir-matches msg (cdr rx)))
        ;; Not a list, check rx
        (string-match rx (mu4e-message-field msg :maildir)))))
  (setq
   mu4e-use-maildirs-extension t
   mu4e-enable-notifications t
   mu4e-enable-mode-line t)

  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  ;; PGP-Sign all e-mails
  ;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)
  (setq mu4e-maildir "~/Maildir/"
        mu4e-get-mail-command "mbsync -a"
        mu4e-attachment-dir  "~/Downloads"
        ;; This enabled the thread like viewing of email similar to gmail's UI.
        mu4e-headers-include-related t
        mu4e-view-show-images t

        mu4e-view-show-addresses t
        message-kill-buffer-on-exit t

        mu4e-update-interval 300

        mu4e-compose-format-flowed nil

        mml2015-use 'epg
        mml2015-encrypt-to-self t
        mu4e-decryption-policy 'ask ;; t

        mml2015-sign-with-sender t ;; also encrypt for self (https://emacs.stackexchange.com/questions/2227/how-can-i-make-encrypted-messages-readable-in-my-sent-folder)
        mu4e-context-policy 'pick-first

        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"
        message-sendmail-envelope-from 'header)

  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions
               '("retag message" . mu4e-action-retag-message) t)

   mu4e-context-policy 'pick-first ;; mu4e-context-policy 'ask
   mu4e-compose-context-policy 'ask-if-none
   mu4e-bookmarks '(("\\\\Inbox" "Inbox" ?i)
                    ("flag:unread or flag:flagged" "Unread messages" ?u)
                    ("flag:unread" "Only unread messages" ?U)
                    ("flag:flagged" "Starred messages" ?s)
                    ("date:today..now or flag:flagged" "Today's messages" ?t)
                    ("date:7d..now or flag:flagged" "Last 7 days" ?w)
                    ("date:2d..1d" "Yesterday" ?y)
                    ("mime:image/*" "Messages with images" ?p)
                    ("mime:application/pdf" "Messages with PDFs" ?P)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Marks
  ;; See: https://www.djcbsoftware.nl/code/mu/mu4e/Adding-a-new-kind-of-mark.html
  (add-to-list 'mu4e-marks
               '(tag
                 :char       "g"
                 :prompt     "gtag"
                 :ask-target (lambda () (read-string "What tag do you want to add?"))
                 :action     (lambda (docid msg target)
                               (mu4e-action-retag-message msg (concat "+" target)))))
  (add-to-list 'mu4e-marks
               '(archive
                 :char       "A"
                 :prompt     "Archive"
                 :show-target (lambda (target) "archive")
                 :action      (lambda (docid msg target)
                                ;; must come before proc-move since retag runs
                                ;; 'sed' on the file
                                (mu4e-action-retag-message msg "-\\Inbox")
                                (mu4e~proc-move docid nil "+S-u-N"))))
  (mu4e~headers-defun-mark-for tag)
  (mu4e~headers-defun-mark-for archive)
  (define-key mu4e-headers-mode-map (kbd "g") 'mu4e-headers-mark-for-tag)
  (define-key mu4e-headers-mode-map (kbd "A") 'mu4e-headers-mark-for-archive)

  ; ;; https://github.com/jeremy-compostella/org-msg
  ; (after! org-msg
  ;   (setq mail-user-agent 'mu4e-user-agent)
  ;   (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
  ;         org-msg-startup "hidestars indent inlineimages"
  ;         org-msg-greeting-fmt "\nHi %s,\n\n"
  ;         org-msg-greeting-name-limit 3
  ;         org-msg-default-alternatives '(text) ;; html
  ;         )
  ;   (org-msg-mode)

  ;   ;; https://kitchingroup.cheme.cmu.edu/blog/2016/10/29/Sending-html-emails-from-org-mode-with-org-mime/
  ;   (defun mu4e-compose-org-mail ()
  ;     (interactive)
  ;     (mu4e-compose-new)
  ;     (org-mu4e-compose-org-mode))
  ;   (defun htmlize-and-send ()
  ;     "When in an org-mu4e-compose-org-mode message, htmlize and send it."
  ;     (interactive)
  ;     (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
  ;       (org-mime-htmlize)
  ;       (message-send-and-exit)))

  ;   (add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)
  ;   )
  )
