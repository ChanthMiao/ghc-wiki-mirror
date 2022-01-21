## Markdown Preview mode

This emacs package makes use of websockets to deliver rendered markdown to a web browser.

https://daringfireball.net/projects/markdown/

Extract Markdown_1.0.1.zip and move `Markdown.pl` to a location on your `$PATH`.

Add MELPA Stable to your `package-archives` in your init file.

```elisp
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
```

In emacs type the following:

<kbd>M-x list-packages RET</kbd>  
<kbd>M-x package-install RET markdown-preview-mode RET</kbd>  
<kbd>M-x markdown-preview-mode RET</kbd>  
<kbd>M-x customize-variable RET markdown-command RET</kbd>  

Change the Markdown Command to Markdown.pl and click the "Apply and Save" button.

Your `custom-set-variables` in your init file should now contain these entries.

```elisp
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "Markdown.pl")
 '(package-selected-packages '(markdown-preview-mode)))
```
Debian users would likely install the _markdown_ package.

list of files

```
/usr/bin/markdown
/usr/share/doc/markdown/changelog.Debian.gz
/usr/share/doc/markdown/copyright
/usr/share/man/man1/markdown.1.gz
/usr/share/man/man3/Markdown.3.gz
/usr/share/perl5/Text/Markdown.pm
```

<a href="https://stable.melpa.org/#/markdown-preview-mode"><img alt="MELPA Stable" src="https://stable.melpa.org/packages/markdown-preview-mode-badge.svg"/></a>