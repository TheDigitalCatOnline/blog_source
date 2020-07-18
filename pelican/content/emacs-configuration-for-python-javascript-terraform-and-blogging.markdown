Title: Emacs Configuration for Python/JavaScript, Terraform and blogging
Date: 2020-07-18 15:30:00 +0100
Category: Programming
Tags: blogging, editor, Emacs, JavaScript, Markdown, Python, Terraform, tools
Authors: Leonardo Giordani
Slug: emacs-configuration-for-python-javascript-terraform-and-blogging
Image: emacs-configuration-for-python-javascript-terraform-and-blogging
Summary: A step-by-step analysis of the Emacs configuration that I use to write Python/JavaScript/Terraform code and  posts for the blog

(image from https://commons.wikimedia.org/wiki/File:Gnu-listen-half.jpg)

I have been an Emacs user for a long time. There is no specific reason why I started using Emacs: it was available in the RedHat 6.0 distribution that I found in a magazine in 1999, and with which I started my journey in the open source world. It was mentioned in some Linux guide I read at the time, so it became my editor.

I'm not into flame wars, in particular about editors. If I don't like a software/operating system/language/whatever, I just don't use it, and at the same time I'm not scared to test alternatives, even though I'm not actively looking to replace tools that work. Admittedly, at the time I didn't properly configure my Emacs for years, in particular because the C language support was very good out of the box, and that was what I needed, so when I started programming in Python not everything was optimal.

One day a new colleague showed me [Sublime Text](https://www.sublimetext.com/) and PyCharm. I don't like IDEs that much, they are too integrated, so the PyCharm wasn't very attractive, but Sublime Text is a very good editor, it's fast and has a lot of features (multiple cursors blew my mind when I first discovered them) and so it became my editor of choice for some years. In time, however I started growing increasingly dissatisfied with it, and with some alternatives that I tested like Atom. The main reason is that modern editor rely too much on the mouse: many people are happy with this, in particular because they use trackpads, but I honestly can't get use to them, and I simply don't want to take my hands off the keyboards while I code because I want to change tab, reorganise the screen, open a file, and so on.

So I went back to Emacs, and started from scratch to configure it in order to match my needs. In this post I want to describe what I did, so that newcomers might be helped to setup their own editor. Emacs is incredibly customisable, and this is one of its main strengths, but a proper setup takes time. Please don't consider configuring your editor a waste of time. If you are a programmer, the editor is your main tool, and you have to take care of it: like any other editor, Emacs has pros and cons, and a proper setup minimises the impact of the shortcomings on your daily work.

## Requirements

As always, the choice of tools and their configuration depends on the requirements, so these are the ones I have at the moment.

* I need to create **keyboard shortcuts** for everything I do often in the editor, I should be able to work without the mouse
* **Multiple cursors** are a very useful feature and I am used to have them, so there should be an implementation similar to the one I found in Sublime Text
* **Python**/**JavaScript**/**Terraform** syntax highlighting and formatting/linting
* As I have some projects written with React, **JSX** syntax highlighting is also needed
* **Markdown** syntax highlighting with spell-checking for my blogger activity

## Preamble

The Emacs configuration file is `~/.emacs`, and when you install the editor you might get a default minimal version of it. Whenever part of the configuration is changed from the menus, Emacs writes the changes in the .emacs, using the `custom-set-variables` function. That file also contains the list of packages that I have installed, so I moved the `custom-set-variables` invocation to a separate file, `.emacs-custom`. This way Emacs doesn't have to change the main file whenever I install or remove a package and when I customise the face (colours and fonts).

For packages, I'm using the standard [MELPA configuration](https://melpa.org/#/getting-started)

``` lisp
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file "~/.emacs-custom")
(load custom-file)

;; Load package system and add MELPA repository.
(require 'package)
(add-to-list
 'package-archives
 ;; '("melpa" . "https://stable.melpa.org/packages/") ; many packages won't show if using stable
 '("melpa" . "https://melpa.milkbox.net/packages/")
 t)
```

## Global settings

In Emacs you can define new functions and assign them to key combinations, and this is usually a major part of the setup. As I often need to work on the configuration file, at least initially, the first function I defined is one that allows me to quickly open the `.emacs` file. In the comments I'm following [Emacs key notation](https://www.emacswiki.org/emacs/EmacsKeyNotation)

``` lisp
;; Open .emacs with C-x c
(defun dotemacs () (interactive) (switch-to-buffer (find-file-noselect "~/.emacs")))
(global-set-key (kbd "C-x c") 'dotemacs)
```

I like the [visual line mode](https://www.emacswiki.org/emacs/VisualLineMode) to wraps long lines

``` lisp
;; Visual line mode everywhere, please
(global-visual-line-mode t)
```

I also want to always see [line numbers](http://ergoemacs.org/emacs/emacs_line_number_mode.html)

``` lisp
;; Display line numbers
(global-display-line-numbers-mode)
```

I use `C-End` a lot to reach the end of the file, but often I press `C-<next>` (`Ctrl-PgDown`) by mistake. That is by default associated with `scroll-left` which is disabled, but Emacs opens a buffer to warn me. I don't want that, so I just unset the key combination

``` lisp
;; I keep pressing C-next by mistake...
(global-unset-key (kbd "C-<next>"))
```

I might not be a purist here. I used `M-w` and `C-y` for many years, but I also use other software during my day, and now the `Ctrl-c/x/v` combinations are universally implemented. It's really hard to remap my brain every time I go back to Emacs, so I prefer to remap Emacs. Welcome [Cua Mode](https://www.emacswiki.org/emacs/CuaMode), which also maps `undo` to `C-z`

``` lisp
;; Set C-c, C-x, C-v just to be in sync with the rest of the world
(cua-mode t)
```

I like Emacs to highlight the [matching parenthesis](https://www.emacswiki.org/emacs/ShowParenMode) whenever I'm on an opening or closing one. Please note that the word "parenthesis" here refers to what are more generally called [brackets](https://en.wikipedia.org/wiki/Bracket) in English, which includes parentheses or round brackets `()`, curly brackets or curly braces `{}`, and square brackets `[]`

``` lisp
;; Show the matching parenthesis
(show-paren-mode 1)
```

Speaking of brackets, I like to have a way to automatically match the opened ones. This is good in Lisp because there are more parentheses than words, but also in other languages like Python when dealing with complex data structures. Long story short, I mapped `C-]` to the auto-closing feature called `syntactic-close` (installed as a package)

``` lisp
;; Syntactic close
;; https://github.com/emacs-berlin/syntactic-close
(global-set-key (kbd "C-]") 'syntactic-close)
```

Minibuffer is a very important part of Emacs, not only when you open files but also when running commands. I like the Ivy completion, so I installed it with MELPA and activate it. I want to have a quick access to the commands that I run previously (history), but since `<down>` and `<up>` are already used to navigate the suggestions I mapped the history functions to their Shift version. 

``` lisp
;; Ivy completion
;; https://github.com/abo-abo/swiper
(ivy-mode 1)
(define-key ivy-minibuffer-map (kbd "S-<up>") #'ivy-previous-history-element)
(define-key ivy-minibuffer-map (kbd "S-<down>") #'ivy-next-history-element)
```

The standard behaviour of `C-Del` in Emacs is far too greedy for me. I'm used to have a combination that deletes all spaces if I'm on a space, and the word if I'm on a word, so I found this

``` lisp
;; https://stackoverflow.com/questions/17958397/emacs-delete-whitespaces-or-a-word
(defun kill-whitespace-or-word ()
  (interactive)
  (if (looking-at "[ \t\n]")
      (let ((p (point)))
        (re-search-forward "[^ \t\n]" nil :no-error)
        (backward-char)
        (kill-region p (point)))
    (kill-word 1)))
(global-set-key (kbd "C-<delete>") 'kill-whitespace-or-word)
```

Last, as I have a rotating wallpaper with screenshots from my favourite films I like to see them behind the editor and the terminal, so I enable a little bit of transparency. I like this to be callable as an interactive function as I might want to remove the transparency, for example when sharing the screen, as that way the text might be easier to read.

``` lisp
;; Set transparency of emacs
;; https://www.emacswiki.org/emacs/TransparentEmacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
(set-frame-parameter (selected-frame) 'alpha '90)
```

## Keyboard mapping

I'm an Italian native speaker, so I often write texts in that language for my personal blog or for other reasons. In Italian, we use the accented letters à, è, é, ì, ò, ù a lot ("is" is just "è", "why" and "because" are "perché", almost all future tenses end with an accented letter, and so on), so I like to have a proper mapping. Emacs has a very powerful mode for Latin alphabet [diacritics](https://en.wikipedia.org/wiki/Diacritic), which is `latin-postfix`, where you can write "è" typing `e` followed by backticks, so that was my first choice. While this is very powerful, and allows me to input almost everything I need also for other languages like German, on the long run I found it very difficult to use efficiently.

First of all, I'm used to the Italian keymap, I know it by heart as I know the English keyboard mapping, so I tend to press keys that are supposed to insert the accented letters directly. Moreover, other software like the browser work with a system keyboard mapping (that Emacs ignores), so again, I need to rewire my brain every time I go back to the editor. Last, `latin-postfix` is far too generic and has mappings for letters like ą or ę which are created typing "a," which is a combination that we have every time we type a comma in Italian given that almost all words end in a vowel. This forces me to type "a,," whenever I need "a,", which is far too different from the normal system I'm used to.

The input method `italian-keyboard` unfortunately doesn't map very well on a standard modern English keyboard, so I decided to just write my own mapping using the [quail](http://web.mit.edu/Emacs/source/emacs/lisp/international/quail.el) system, which is unsurprisingly not very well documented. I'm sorry to say it, but multilingual input has been and still is one of the great forgotten and messy topics in computer science, in particular in the open source world. Let's not even mention Chinese and other languages not based on an alphabet.

``` lisp
; Define a proper mapping for the Italian keyboard over the English one
(quail-define-package
 "italian-english-keyboard" "Latin-1" "IT@" t
 "Italian (Italiano) input method for modern English keyboards"
 nil t t t t nil nil nil nil nil t)

;; \|  1!  2"  3£  4$  5%  6&  7/  8(  9)  0=  '?  ì^
;;      qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  èé  +*
;;       aA  sS  dD  fF  gG  hH  jJ  kK  lL  òç  à°  ù§
;;    <>  zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

;; èè -> È Originally CapsLock+è
;; àà -> # Originally AltGr+à
;; òò -> @ Originally AltGr+ò
;; ìì -> ~ Originally AltGr+ì

(quail-define-rules
 ("`" ?\\) ("¬" ?|)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("-" ?') ("_" ??)
 ("=" ?ì) ("+" ?^) ("==" ?~)

 ("[" ?è) ("{" ?é) ("[[" ?È)
 ("]" ?+) ("}" ?*)

 (";" ?ò) (":" ?ç) (";;" ?@)
 ("'" ?à) ("@" ?\°) ("''" ?#)
 ("#" ?ù) ("~" ?§)
 
 ("\\" ?<) ("|" ?>)
 ("<" ?\;)
 (">" ?:)
 ("/" ?-) ("?" ?_)
)
```

The standard Italian keyboard allows to input `È` activating CapsLock, pressing the key for `è` and deactivating CapsLock. This because `Shift-è` inputs `é`. I always found that system incredibly awkward, not to mention that it is impossible (as far as I understand) to create such a combination using quail, so I mapped it to `èè`, since that combination doesn't exist in Italian.

Another issue is that the Italian keyboard makes use of the right Alt key (AltGr) to input third level characters like `#`, `@`, and `~`. Again, I don't know if and how I can express these in quail so I went for a slight variation of that. Instead of typing AltGr+à for `#` I will type `àà` and so on. This is not a perfect mapping, but considering that I don't use those character that much when writing notes or blog posts I can accept it.

## Lines and regions

This section is about shortcuts to interact with lines and regions. First of all something that comments the regions I highlighted or just the line I'm on. This is a good example of something that Emacs doesn't provide out of the box, maybe surprisingly, but that is easily implemented. OK, "easily" might be an overstatement, but this function is a good starting point if you want to learn how [Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/elisp/) works. If you are not interested you can just copy and paste it and call it a day

``` lisp
;; Define C-/ to comment and uncomment regions and lines
(defun comment-or-uncomment-line-or-region ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))
(global-set-key (kbd "C-/") 'comment-or-uncomment-line-or-region)
```

I want to be able to move up and down lines or regions using `Ctrl-Shift-up/down`, that is to [drag stuff](https://github.com/rejeep/drag-stuff.el)

``` lisp
;; Drag-stuff - Drag lines and regions
(drag-stuff-global-mode 1)
;; Use C-S-up/down
(setq drag-stuff-modifier '(control shift))
(define-key drag-stuff-mode-map (drag-stuff--kbd 'up) 'drag-stuff-up)
(define-key drag-stuff-mode-map (drag-stuff--kbd 'down) 'drag-stuff-down)
```

I also want to be able to duplicate the current line or region with `Ctrl-Shift-d`. This function is a mixture of several solutions I found on Internet, and I'm not 100% satisfied with it, as when I duplicate a region it also duplicates the last line of the region itself, even though the line is not highlighted. Again, it might be surprising that Emacs doesn't provide a solution out-of-the-box, but this way I can implement the behaviour I prefer. The fact that I'm not able to implement the exact behaviour (yet!) is part of the game, I think.

``` lisp
;; Duplicate line with C-S-d
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(global-set-key (kbd "C-S-d") 'duplicate-current-line-or-region)
```

Multiple cursors! Emacs has an implementation of [multiple cursors](https://github.com/magnars/multiple-cursors.el), that is slightly different from the Sublime Text one, but that can be customised to my needs. Most notably, multiple cursors don't work perfectly with some parts of the editor like `TAB` for indentation, which is not automatically applied to all of them. So, this part of the setup is not perfect, but it's definitely usable and effective.

`Ctrl-d` creates a second cursor on the next repetition of the current word, `Ctrl-c Ctrl-d` marks all the repetitions in the text, `Ctrl-Alt-up/down` add a new cursor on the previous/following line, and last `Ctrl-Alt-d` marks all the lines in a region

``` lisp
;; Multiple cursors
(global-set-key (kbd "C-d") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-c C-d") 'mc/mark-all-words-like-this)
(global-set-key (kbd "C-M-<up>") 'mc/mark-previous-lines)
(global-set-key (kbd "C-M-<down>") 'mc/mark-next-lines)
(global-set-key (kbd "C-M-d") 'mc/edit-lines)
```

How many times do I need to highlight the whole buffer? More than I imagined, so I defined the classic `Ctrl-a` shortcut

``` lisp
;; C-a marks the whole buffer
(global-set-key (kbd "C-a") 'mark-whole-buffer)
```

## Windows and buffers

As everyone else, when I code I need to keep multiple files open, and often to have them side-by-side. Emacs provides out of the box the window split shortcuts `Ctrl-2` (horizontal split) and `Ctrl-3` (vertical split), but I need a shortcut to quickly move between them. Enter [Windmove](https://www.emacswiki.org/emacs/WindMove), which by default uses `Shift-<arrow>` combinations, which I instead want to keep for highlighting regions, so I remapped it to `Alt-<arrow>`

``` lisp
;; Windmove - move between windows use ALT
(windmove-default-keybindings 'meta)
;; Unset Shift-arrow keybindings
(global-unset-key (vector (list 'shift 'left)))
(global-unset-key (vector (list 'shift 'right)))
(global-unset-key (vector (list 'shift 'up)))
(global-unset-key (vector (list 'shift 'down)))
```

Next I need to move between buffers (open files not currently visible) Emacs used buffers also for its own internal logs, and all those buffers have a name surrounded by `*`, so I filter them out when I move through open files

``` lisp
;; Skip system buffers when cycling
(set-frame-parameter (selected-frame) 'buffer-predicate
		     (lambda (buf) (not (string-match-p "^*" (buffer-name buf)))))
```

To move between buffers I map `previous-buffer` and `next-buffer` to `Ctrl-Super-left/right` (Super here is my left Win key, which is between Ctrl and Alt)

``` lisp
;; Map previous and next buffer to C-s-
(global-set-key (kbd "C-s-<left>") 'previous-buffer)
(global-set-key (kbd "C-s-<right>") 'next-buffer)
```

Oh, I love the [golden ratio mode](https://github.com/roman/golden-ratio.el) that splits window giving more space to the current one 

``` lisp
;; Golden ratio mode
(require 'golden-ratio)
(golden-ratio-mode 1)
```

## Spell checking

When I will learn how to properly spell `inconceivably` and `counter-intuitive` I might get rid of this. In the meanwhile a good spell check is a blessing from heaven. I haven't used function keys that much so far, and since I'm used to the standard Sublime Text mapping `F6` I reused that.

``` lisp
;; Spell check
(global-set-key (kbd "<f6>") 'flyspell-mode)

;; Spell check with hunspell
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))
```

By the way, I misspell simpler words all the time, no need to get into inconceivably counter-intuitive words. And I'm sure there will be typos in this text, no matter how many times I check and read it =)

## Python development

At last, let's get into development! Emacs has a very good package for Python, called [Elpy](https://elpy.readthedocs.io/en/latest/).

``` lisp
;; Enable Elpy for Python development
;; https://elpy.readthedocs.io/en/latest/
(setq elpy-rpc-python-command "python3")
(elpy-enable)
```

There are some adjustment needed to prevent the mode to override the Windmove and other binding

``` lisp
;; Prevent Elpy from overriding Windmove shortcuts
(eval-after-load "elpy"
  '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))

;; Prevent Elpy from overriding standard cursor movements
(eval-after-load "elpy"
  '(cl-dolist (key '("C-<left>" "C-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))
```

Since I use [black](https://github.com/psf/black) to format Python code I run it on save. Plus I have a shortcut to fix the syntax in the buffer at any time. Since the syntax document is PEP8 `C-8` is a nice combination.

``` lisp
;; Run black on save
(add-hook 'elpy-mode-hook (lambda ()
  (add-hook 'before-save-hook 'elpy-black-fix-code nil t)))

;; Set C-8 to format Python code
(global-set-key (kbd "C-8") 'elpy-black-fix-code)
```

## JavaScript development

Let's go full-stack and add some JavaScript and JSX to the recipe. The [rjsx-mode](https://github.com/felipeochoa/rjsx-mode) is perfect for React development and similar things

``` lisp
;; JSX
;;https://github.com/felipeochoa/rjsx-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;; Don't override my beloved multiple cursors
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map (kbd "C-d") nil))
```

As I will install Node.js modules locally in `node_modules` I want code formatters and other tools to run the binaries stored there

``` lisp
;; Use binaries in node_modules
;; https://github.com/codesuki/add-node-modules-path
(add-hook 'js2-mode-hook 'add-node-modules-path)
```

And also JS need to be beautified, at least when I have to read and understand it. I use [prettier](https://prettier.io/) and [prettier-emacs](https://github.com/prettier/prettier-emacs)

``` lisp
;; Pretty JS code
;; https://github.com/prettier/prettier-emacs
(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)
```

## Other languages

Other languages I use are less demanding in term of customisation (generally because they are simpler or less general-purpose). The [Terraform mode](https://github.com/emacsorphanage/terraform-mode) automatically fixes the syntax of the file on save

``` lisp
;; Terraform
;; https://github.com/emacsorphanage/terraform-mode
(require 'terraform-mode)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
```

## Files and projects

My setup is still a work in progress here, I'm testing [Projectile](https://docs.projectile.mx/projectile/usage.html), [Prescient](https://github.com/raxod502/prescient.el), and [Treemacs](https://github.com/Alexander-Miller/treemacs). So far, only Projectile made it to the configuration file, but I'm not using it that much yet. I think I might use a good sidebar with Git integration through colours and icons, but I still have to find something I feel comfortable with.

``` lisp
;; Projectile
;; https://docs.projectile.mx/projectile/usage.html
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(projectile-mode +1)
(setq projectile-completion-system 'ivy)
```

## The custom file

As I mentioned when I discussed the preamble the file `.emacs-custom` contains the customisation of variables and faces, and this is what I have

``` lisp
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(default-input-method (quote italian-english-keyboard))
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (syntactic-close ivy projectile rjsx-mode js2-mode golden-ratio
	 add-node-modules-path prettier-js tide elpy jinja2-mode drag-stuff
	 markdown-mode fcitx command-log-mode yaml-mode terraform-mode multiple-cursors))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((t (:foreground "orange red" :background "yellow"))))
 '(js2-external-variable ((t (:foreground "orange red" :background "yellow")))))
```

As you can see I have a default theme `tango-dark`. The default alternate input method activated by `C-\` is the one I defined previously, `italian-english-keyboard`. JavaScript uses 2 spaces for indentation, and last I changed some colours to make errors in Python and JavaScript stand out very clearly.

## Final words

Any configuration is a work in progress, as it changes with the personal preferences and the requirements, but this has been my configuration for a while now, so it seems pretty stable. I hope this might help whoever wants to try emacs or who is already using it but struggles to properly configure it. Happy coding!

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.


