;;; privatebin.el --- Interface to privatebin CLI -*- lexical-binding: t -*-

;; Author: Bryan Frimin
;; Keywords: TODO
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (transient "0.3.0"))

;;; Code:

(require 'url-parse)
(require 'transient)

(defgroup privatebin nil
  "Interface to privatebin CLI."
  :group 'external)

(defcustom privatebin-executable "privatebin"
  "Path to the privatebin executable."
  :type 'string
  :group 'privatebin)

(defcustom privatebin-default-expire "1day"
  "Default expiration time for pastes."
  :type 'string
  :group 'privatebin)

(defcustom privatebin-default-formatter "plaintext"
  "Default formatter for pastes."
  :type '(choice (const :tag "Plain Text" "plaintext")
                 (const :tag "Markdown" "markdown")
                 (const :tag "Syntax Highlighting" "syntaxhighlighting"))
  :group 'privatebin)

;;;###autoload (autoload 'privatebin-dispatch "privatebin" nil t)
(transient-define-prefix privatebin-dispatch ()
  "Dispatch a privatebin command."
  ["Actions"
   ("c" "Create paste" privatebin-create-dispatch)
   ("s" "Show paste" privatebin-show-dispatch)])

(transient-define-prefix privatebin-create-dispatch ()
  "Create a new paste with specified options."
  ["Paste Options"
   ("-f" "Formatter" "--formatter="
    :choices ("plaintext" "markdown" "syntaxhighlighting"))
   ("-e" "Expire time" "--expire="
    :choices ("5min" "10min" "1hour" "1day" "1week" "1month" "1year" "never"))
   ("-b" "Burn after reading" "--burn-after-reading")
   ("-d" "Open discussion" "--open-discussion")
   ("-p" "Password protect" "--password=")]
  ["Actions"
   ("c" "Create from region or buffer" privatebin-create-paste)])

(defun privatebin--get-text ()
  "Get text from active region or whole buffer."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun privatebin-create-paste ()
  "Create a new paste using the selected region or current buffer."
  (interactive)
  (let* ((args (transient-args 'privatebin-create-dispatch))
         (password-arg (--first (string-prefix-p "--password=" it) args))
         (password (when password-arg
                    (read-passwd "Password: " t)))
         (filtered-args (remove password-arg args))
         (buffer (generate-new-buffer "*privatebin-temp*"))
         (text (privatebin--get-text))
         (command (concat privatebin-executable " create "
                         (string-join filtered-args " ")
                         (when password
                           (format " --password=%s" password)))))
    (with-current-buffer buffer
      (insert text)
      (shell-command-on-region (point-min) (point-max) command
                              "*privatebin-output*"))
    (kill-buffer buffer)
    (with-current-buffer "*privatebin-output*"
      (message "Paste URL: %s" (buffer-string))
      (kill-new (buffer-string)))
    (kill-buffer "*privatebin-output*")))

(transient-define-prefix privatebin-show-dispatch ()
  "Show a paste with specified options."
  ["Show Options"
   ("-c" "Confirm burn" "--confirm-burn")
   ("-i" "Allow insecure" "--insecure")
   ("-p" "Password protect" "--password=")]
  ["Actions"
   ("s" "Show paste" privatebin-show-paste)])

(defvar privatebin--current-url nil
  "Store the URL for the current show command.")

(defun privatebin-show-paste-with-url (url)
  "Set the URL for showing a paste."
  (interactive "sPrivatebin URL: ")
  (setq privatebin--current-url url)
  (transient-setup 'privatebin-show-dispatch))

(defun privatebin-show-paste ()
  "Show paste with selected options."
  (interactive)
  (let* ((args (transient-args 'privatebin-show-dispatch))
         (password-arg (--first (string-prefix-p "--password=" it) args))
         (password (when password-arg
                    (read-passwd "Password: " t)))
         (filtered-args (remove password-arg args))
         (buffer (generate-new-buffer "*privatebin-paste*"))
         (command (concat privatebin-executable " show "
                         (string-join filtered-args " ")
                         (when password
                           (format " --password=%s" password))
                         " "
                         (shell-quote-argument privatebin--current-url))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (call-process-shell-command command nil t)
        (goto-char (point-min))
        (view-mode)
        (pop-to-buffer (current-buffer))))))

(defalias 'privatebin-show 'privatebin-show-paste-with-url)

;;;###autoload
(defalias 'privatebin 'privatebin-dispatch
  "Dispatch privatebin commands.")

;;;###autoload
(global-set-key (kbd "C-c P") 'privatebin)

(provide 'privatebin)
;;; privatebin.el ends here
