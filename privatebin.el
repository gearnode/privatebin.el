;;; privatebin.el --- Interface to privatebin CLI -*- lexical-binding: t -*-

;; Author: Bryan Frimin <bryan@frimin.fr>
;; Maintainer: Bryan Frimin <bryan@frimin.fr>
;; URL: https://github.com/gearnode/privatebin.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (transient "0.3.0"))
;; Keywords: convenience, tools

;;; Commentary:
;; This package provides a global interface to the privatebin CLI tool.
;; It allows creating and viewing pastes directly from any buffer in Emacs.

;;; Code:

(require 'url-parse)
(require 'transient)
(require 'json)

(defgroup privatebin nil
  "Interface to privatebin CLI."
  :group 'external)

(defcustom privatebin-executable "privatebin"
  "Path to the privatebin executable."
  :type 'string
  :group 'privatebin)

(defvar privatebin--current-url nil
  "Store the URL for the current show command.")

(defun privatebin--read-config (config-file)
  "Read privatebin config from CONFIG-FILE."
  (when (and config-file (file-exists-p (expand-file-name config-file)))
    (with-temp-buffer
      (insert-file-contents (expand-file-name config-file))
      (let ((json-object-type 'hash-table)
            (json-array-type 'list))
        (json-read-from-string (buffer-string))))))

(defun privatebin--get-bin-names (&optional config-file)
  "Get list of bin names from CONFIG-FILE or default config."
  (let* ((config (privatebin--read-config
                 (or config-file
                     "~/.config/privatebin/config.json")))
         (bins (when config (gethash "bin" config))))
    (when bins
      (mapcar (lambda (bin) (gethash "name" bin)) bins))))

(transient-define-prefix privatebin-dispatch ()
  "Dispatch a privatebin command."
  :value '()
  [:class transient-columns
   ["Global Options"
    ("-b" "Bin instance" "--bin="
     :reader (lambda (_prompt _init _hist)
               (completing-read "Bin instance: "
                              (privatebin--get-bin-names)
                              nil nil)))
    ("-c" "Config file" "--config="
     :reader (lambda (_prompt _init _hist)
               (read-file-name "Config file: " nil nil t)))]
   ["Actions"
    ("c" "Create paste" privatebin-create-dispatch)
    ("s" "Show paste" privatebin-show-dispatch)]])

(transient-define-prefix privatebin-create-dispatch ()
  "Create a new paste with specified options."
  :value '()
  ["Paste Options"
   ("-f" "Formatter" "--formatter="
    :choices ("plaintext" "markdown" "syntaxhighlighting"))
   ("-e" "Expire time" "--expire="
    :choices ("5min" "10min" "1hour" "1day" "1week" "1month" "1year" "never"))
   ("-b" "Burn after reading" "--burn-after-reading")
   ("-d" "Open discussion" "--open-discussion")
   ("-p" "Password protect" "--password="
    :reader privatebin--read-password)
   ("-g" "Gzip" "--gzip")
   ("-a" "Attachment" "--attachment")]
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
  (let* ((args (append (transient-args 'privatebin-dispatch)
                      (transient-args 'privatebin-create-dispatch)))
         (buffer (generate-new-buffer "*privatebin-temp*"))
         (text (privatebin--get-text))
         (command (concat privatebin-executable
                         " create "
                         (string-join args " "))))
    (with-current-buffer buffer
      (insert text)
      (shell-command-on-region (point-min) (point-max) command
                              "*privatebin-output*"))
    (kill-buffer buffer)
    (with-current-buffer "*privatebin-output*"
      (message "Paste URL: %s" (buffer-string))
      (kill-new (buffer-string)))
    (kill-buffer "*privatebin-output*")))

(defun privatebin--read-password (prompt &optional _initial-input _history)
  "Read password securely using PROMPT.
Ignore INITIAL-INPUT and HISTORY arguments as they're not used for passwords."
  (let ((pass (read-passwd prompt)))
    (when (and pass (not (string-empty-p pass)))
      pass)))

(transient-define-prefix privatebin-show-dispatch ()
  "Show a paste with specified options."
  :value '()
  ["Show Options"
   ("-c" "Confirm burn" "--confirm-burn")
   ("-i" "Allow insecure" "--insecure")
   ("-p" "Password protected" "--password="
    :reader privatebin--read-password)]
  ["Actions"
   ("s" "Show paste" privatebin-show-paste)])

(defun privatebin-show-paste-with-url (url)
  "Set the URL for showing a paste."
  (interactive "sPrivatebin URL: ")
  (setq privatebin--current-url url)
  (transient-setup 'privatebin-show-dispatch))

(defun privatebin-show-paste ()
  "Show paste with selected options."
  (interactive)
  (unless privatebin--current-url
    (setq privatebin--current-url
          (read-string "Privatebin URL: ")))
  (when privatebin--current-url
    (let* ((args (append (transient-args 'privatebin-dispatch)
                        (transient-args 'privatebin-show-dispatch)))
           (buffer (generate-new-buffer "*privatebin-paste*"))
           (command (concat privatebin-executable
                          " show "
                          (mapconcat #'shell-quote-argument args " ")
                          " "
                          (shell-quote-argument privatebin--current-url))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; (message "Debug: executing command: %s" command)
          (call-process-shell-command command nil t)
          (goto-char (point-min))
          (view-mode)
          (pop-to-buffer (current-buffer))))
      (setq privatebin--current-url nil))))

;;;###autoload
(defalias 'privatebin 'privatebin-dispatch
  "Dispatch privatebin commands.")

;;;###autoload
(global-set-key (kbd "C-c P") 'privatebin)

(provide 'privatebin)
;;; privatebin.el ends here
