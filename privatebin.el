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

(defgroup privatebin nil
  "Interface to privatebin CLI."
  :group 'external)

(defcustom privatebin-executable "privatebin"
  "Path to the privatebin executable."
  :type 'string
  :group 'privatebin)

(defvar privatebin-bin nil
  "Current privatebin instance name.")

(defcustom privatebin-default-bin nil
  "Default privatebin instance name.
This can be overridden using transient options."
  :type '(choice (const :tag "None" nil)
                (string :tag "Instance name"))
  :group 'privatebin
  :set (lambda (sym val)
         (set sym val)
         (setq privatebin-bin val)))

(defvar privatebin-config nil
  "Current config file path.")

(defcustom privatebin-default-config nil
  "Default config file path.
This can be overridden using transient options."
  :type '(choice (const :tag "None" nil)
                (string :tag "Config path"))
  :group 'privatebin
  :set (lambda (sym val)
         (set sym val)
         (setq privatebin-config val)))

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
(require 'json)

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
                     privatebin-config
                     "~/.config/privatebin/config.json")))
         (bins (when config (gethash "bin" config))))
    (when bins
      (mapcar (lambda (bin) (gethash "name" bin)) bins))))

(cl-defmethod transient-init-value ((obj privatebin--config-variable))
  "Initialize transient variable OBJ from its corresponding global variable."
  (let* ((global-key (oref obj global-key))
         (current-var (cl-case global-key
                       (privatebin-default-bin 'privatebin-bin)
                       (privatebin-default-config 'privatebin-config)))
         (global-val (symbol-value current-var)))
    (when global-val
      (oset obj value global-val))))

(cl-defmethod transient-infix-read ((obj privatebin--config-variable))
  "Read value for transient variable OBJ."
  (if (slot-value obj 'reader)
      (funcall (slot-value obj 'reader)
               (slot-value obj 'prompt)
               nil
               (slot-value obj 'history))
    (read-string (slot-value obj 'prompt) nil nil)))

(cl-defmethod transient-format-value ((obj privatebin--config-variable))
  "Format value for transient variable OBJ."
  (let ((value (slot-value obj 'value)))
    (if value
        (propertize value 'face 'transient-value)
      (propertize "none" 'face 'transient-inactive-value))))

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
   ("-p" "Password protect" "--password=")
   ("-g" "Gzip" "--gzip")
   ("-a" "Attachment" "--attachment")]
  ["Actions"
   ("c" "Create from region or buffer" privatebin-create-paste)])

(defun privatebin--get-global-options ()
  "Get command line options from global transient values."
  (let ((args (transient-args 'privatebin-dispatch)))
    (string-join args " ")))

(defun privatebin--get-text ()
  "Get text from active region or whole buffer."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun privatebin-create-paste ()
  "Create a new paste using the selected region or current buffer."
  (interactive)
  (let* ((global-opts (privatebin--get-global-options))
         (args (transient-args 'privatebin-create-dispatch))
         (password-arg (--first (string-prefix-p "--password=" it) args))
         (password (when password-arg
                    (read-passwd "Password: " t)))
         (filtered-args (remove password-arg args))
         (buffer (generate-new-buffer "*privatebin-temp*"))
         (text (privatebin--get-text))
         (command (concat privatebin-executable " "
                         global-opts
                         " create "
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
  :value '()
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
  (let* ((global-opts (privatebin--get-global-options))
         (args (transient-args 'privatebin-show-dispatch))
         (password-arg (--first (string-prefix-p "--password=" it) args))
         (password (when password-arg
                    (read-passwd "Password: " t)))
         (filtered-args (remove password-arg args))
         (buffer (generate-new-buffer "*privatebin-paste*"))
         (command (concat privatebin-executable " "
                         global-opts
                         " show "
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
