# diff-lsp.el
Emacs package for using with diff-lsp with magit &amp; code-review

## WARNING:
(hopefully) Obviously this package is still very much in easy development.  Not everything is even close to working.

## Functionality

`diff-lsp.el` is for configuring [diff-lsp](https://www.github.com/C-Hipple/diff-lsp) to run in various diff modes, allowing your lsp servers to parse diffs (using the full source code, not just the contents of the diff)

The end vision is to have it active on magit-status and [code-review](https://www.github.com/C-Hipple/code-review) buffers to make our lives easier as developers.

## Installation

Not currently available on MELPA or other repositories.  You can use your favorite package manager to install directly from github.

### Spacemacs Installation

Simply add this to your `dotspacemacs-additional-packages` list

```elisp
  (diff-lsp :location (recipe
                     :fetcher github
                     :repo "C-Hipple/diff-lsp"
                     :files ("*.el")
                     )
          )
```
