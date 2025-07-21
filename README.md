# diff-lsp.el
Emacs package for using with diff-lsp with magit &amp; code-review


## Functionality

`diff-lsp.el` is for configuring [diff-lsp](https://www.github.com/C-Hipple/diff-lsp) to run in various diff modes, allowing your lsp servers to parse diffs (using the full source code, not just the contents of the diff)

The end vision is to have it active on magit-status and [code-review](https://www.github.com/C-Hipple/code-review) buffers to make our lives easier as developers.

## Usage

Add a call to the setup function to your init or call it manually when you want to use diff-lsp automatically.

```elisp
(diff-lsp-setup-advice)
```

To remove it, run
```elisp
(diff-lsp-remove-advice)
```


## Configuration

You can tweak your tempfile name if you so desire via adjusting.

```elisp
(setq diff-lsp-tempfile-name "~/.diff-lsp-tempfile")
```

You can also adjust which major modes are active using.  Default value is code-review-mode only.

```elisp
(setq diff-lsp-major-modes '(code-review-mode magit-status-mode))
```




## Installation

Not currently available on MELPA or other repositories.  You can use your favorite package manager to install directly from github.

### Spacemacs Installation

Simply add this to your `dotspacemacs-additional-packages` list

```elisp
  (diff-lsp :location (recipe
                     :fetcher github
                     :repo "C-Hipple/diff-lsp.el"
                     :files ("*.el")
                     )
          )
```
