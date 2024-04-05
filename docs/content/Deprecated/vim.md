+++
title = "Vim"
author = ["Michael Reitmeir"]
draft = false
weight = 1
+++

It's honestly a stretch to say my vim config is "deprecated". I still use it quite a lot, and I love vim! I just moved to Emacs as my general big text manipulator, whereas vim is only used for quick edits of system files or similar things. The _"Vim vs. Emacs"_ debate is quite silly; the real answer for me is both. I would probably never have used Emacs without [evil mode](https://github.com/emacs-evil/evil).

Still, you probably don't wanna use my vim config. I haven't updated it in forever, and all the cool kids use [Neovim](https://neovim.io/) with fancy Lua configs nowadays. I would do that too, but there's no real point with the tiny things I use vim for.

The plugin manager used by my vim is [Vundle](https://github.com/VundleVim/Vundle.vim). Install it first, otherwise almost none of the following is gonna work properly. Also check out the installation instructions for [YouCompleteMe](https://github.com/ycm-core/YouCompleteMe), it requires a bit more setup outside of vim.

```vimrc
{{% include file="symlink/vimrc" %}}
```
