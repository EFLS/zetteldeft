#+title: Zetteldeft
#+author: EFLS
#+HTML_HEAD: <link rel='stylesheet' href='./static/style.css' type='text/css'/>

[[http://melpa.org/#/zetteldeft][file:http://melpa.org/packages/zetteldeft-badge.svg]]

The =zetteldeft.org= file in this repository contains documented code for a set of functions for =emacs=, which aims to extend the =deft= package and turn it into a (very very) basic Zettelkasten note-taking system.

The best way to get to know Zetteldeft, is from within Zetteldeft.
Get started by cloning the =zd-tutorial= repository, available at https://github.com/EFLS/zd-tutorial.
Bootstrap =use-package= installation instructions included.

If you'd rather start with an introduction of key concepts and basic functions, check out an introduction here: [[https://efls.github.io/zetteldeft/][efls.github.io/zetteldeft]].

Documentation, literate code and further technical details can be found in the =.org= file of this repository, or can be read in a more convenient format over at [[https://efls.github.io/zetteldeft/zetteldeft.html][efls.github.io/zetteldeft/zetteldeft.html]].

=zetteldeft= is available on [[https://melpa.org/#/zetteldeft][MELPA]].
If you use =use-package=, installing is as easy as

#+begin_src emacs-lisp
(use-package zetteldeft
  :after deft
  :config
    (zetteldeft-set-classic-keybindings))
#+end_src

Please note that this was originally written for personal use and that I'm far from an =emacs lisp= expert.
That said, contributions and feedback are more than welcome.
