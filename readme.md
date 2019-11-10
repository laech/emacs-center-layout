# center-layout

Provides a global minor mode `center-layout-mode` for centering
windows by applying left and right margins.

## Installation

TODO

## Usage

Invoke this within Emacs or place it in your init file:

```lisp
(center-layout-mode t)
```

Alternatively you can also enable it within custom: <kbd>M-x</kbd>
`customize-variable` <kbd>return</kbd> `center-layout-mode`
<kbd>return</kbd>.

## Customization

Customizations can be done via: <kbd>M-x</kbd> `customize-group`
<kbd>return</kbd> `center-layout` <kbd>return</kbd>.

See below for customization options.

### `center-layout-columns`

Default `80`. Width of centered content.

### `center-layout-apply-right-margin`

Default `nil`. If `t` right margin will be applied also. If `nil` no
right margin will be applied, allowing longer lines to flow to the
right exceeding `center-layout-columns`.

### Non-Global Customization

To set non-global customization, use `setq-local` in your init file,
for example, to set mode specific customization for `emacs-lisp-mode`:

```lisp
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq-local center-layout-apply-right-margin t)
   (setq-local center-layout-columns 100)))
```
