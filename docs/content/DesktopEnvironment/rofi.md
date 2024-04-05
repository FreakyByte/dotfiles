+++
title = "Rofi"
author = ["Michael Reitmeir"]
draft = false
weight = 3
+++

[Rofi](https://github.com/davatorium/rofi) is my application launcher, replacing the classic dmenu. When rofi is run, a menu opens where I can type whatever program I want to use.

If I remember correctly, this config is mostly taken form [elenapan](https://github.com/elenapan/dotfiles/blob/master/config/rofi/config.rasi). There's a couple details I'd probably change if I get around to it, but it just works, so more than good enough.

The last line loads the current colorscheme generated by [pywal](https://github.com/dylanaraps/pywal).

```json
{{% include file="symlink/rofi.rasi" %}}
```