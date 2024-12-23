# privatebin.el

An Emacs interface to [privatebin CLI](https://github.com/gearnode/privatebin).

## Overview

This package provides a convenient interface to the privatebin CLI tool directly from Emacs. It allows you to:
- Create pastes from buffer content or regions
- View pastes with password support
- Configure multiple privatebin instances
- Set various paste options like expiration, formatting, etc.

## Prerequisites

- Emacs 25.1 or later
- [privatebin CLI](https://github.com/gearnode/privatebin) installed and in your PATH
- transient package

## Installation

You can install this package in several ways:

### Using use-package

```elisp
(use-package privatebin
  ;; Install from source
  :load-path "/path/to/privatebin.el"
  ;; Or when it's on MELPA (coming soon):
  ;; :ensure t
  
  ;; Optional - default key binding
  :bind ("C-c P" . privatebin)
  
  ;; Optional - customize variables
  :custom
  (privatebin-executable "privatebin")
  
  ;; Optional - configure after load
  :config
  ;; Any additional configuration
  )
```

### Manual Installation

```elisp
;; Clone the repository
git clone https://github.com/yourusername/privatebin.el.git

;; Add to your init.el
(add-to-list 'load-path "/path/to/privatebin.el")
(require 'privatebin)
```

### Via MELPA (Coming soon)

```elisp
M-x package-install RET privatebin RET
```

The package can be configured in several ways:

### Using Customize

You can use `M-x customize-group RET privatebin RET` to configure all available options.

### Using setq in your init.el

```elisp
;; Path to the privatebin CLI executable
(setq privatebin-executable "privatebin")  ; This is the default
```

### Using Configuration File

You can use the CLI's configuration file format. By default, it looks for `~/.config/privatebin/config.json`:
```json
{
    "bin": [
        {
            "name": "personal",
            "host": "https://privatebin.example.com"
        },
        {
            "name": "work",
            "host": "https://work-privatebin.example.com"
        }
    ]
}
```

## Usage

The package provides a global key binding `C-c P` that opens the main command menu.

### Creating Pastes

1. Select text (optional - if no selection, uses whole buffer)
2. `C-c P c` to open create menu
3. Set options if needed:
   - `-f` Select formatter (plaintext/markdown/syntaxhighlighting)
   - `-e` Set expiration time
   - `-b` Enable burn after reading
   - `-d` Enable discussion
   - `-p` Set password
   - `-g` Enable gzip compression
   - `-a` Create as attachment
4. `c` to create paste
5. URL is copied to kill ring and shown in minibuffer

### Viewing Pastes

1. `C-c P s` to open show menu
2. Enter paste URL
3. Set options if needed:
   - `-c` Confirm burn after reading
   - `-i` Allow insecure instances
   - `-p` Enter password for protected pastes
4. `s` to show paste

### Using Multiple Instances

1. In the main menu (`C-c P`):
   - `-b` Select privatebin instance
   - `-c` Select config file
2. Proceed with create or show commands

## Key Bindings

- `C-c P` - Main command menu
  - `c` - Create paste submenu
  - `s` - Show paste submenu

## Support

Encountered a bug or have questions? Feel free to open a GitHub issue
or contact me directly via [email](mailto:bryan@frimin.fr).

## License

This project is released under the ISC license. See the
[LICENSE.txt](LICENSE.txt) file for details. It's designed with both
openness and freedom of use in mind, but with no warranty as per the
ISC standard disclaimer.

