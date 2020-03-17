Title: 
Date: 2020-03-17 12:00:00 +0100
Category: Programming, Projects, Retro
Tags: algorithms, amiga, AMQP, architectures, assembly, AWS, C, Clojure, compilers, concurrent programming, cryptography, decorators, Django, Erlang, Flask, functional programming, generators, Git, HTTP, infrastructure, M68000, metaclasses, metaprogramming, Notebook, OOP, operating systems, pelican, Pika, Postage, Python, Python2, Python3, RabbitMQ, refactoring, retroprogramming, RSA, Scala, SSH, SSL, TDD, testing, versioning, video, WWW
Authors: Leonardo Giordani
Slug: emacs-configuration
Summary: 


MELPA repository

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "https://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "https://melpa.milkbox.net/packages/")
   t))

One of the main complains I have with ST and in general with the modern computing world is that programming tools rely too much on the mouse. I don't want to take my hands from the keyboard while I code or write, to change tab, to open a file, to see another window.

Emacs provides standard word/paragraphs movements out of the box. C-left/right to move between words, Home and End for the beginning and the end of the line, C-Home and C-End for the beginning and the end of the file. C-up/down moves between paragraphs or code blocks.

;; Loading window movement keybindings
(windmove-default-keybindings 'meta)

This gives me Meta-arrow power

;; Display line numbers
(global-display-line-numbers-mode)

Cut/copy/paste are different from the usual C-x/c/v but I learned to use them, so cut is kill-region C-w, copy kill-ring-save M-w, paste is yank C-y. And I'm fine with them.

ESC-ESC-ESC is my favourite key combination ever. I'm not using C-g that much.

Undo is C-/, easy under my pinkies.

Split horizontally C-x 2, vertically C-x 3, keep this only C-x 1, close the current one C-x 0.

Autocomplete M-/

Multiple cursors

Duplicate lines

Drag stuff

Spell check
