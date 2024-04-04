+++
title = "Kitty"
author = ["Michael Reitmeir"]
draft = false
weight = 2
+++

I use [kitty](https://sw.kovidgoyal.net/kitty/) as my terminal emulator, cause who doesn't love cute kitties?


## Config {#config}

My config is honestly not very interesting, just some appearance settings and some keybindings. The only mildly interesting thing is how to make it use the color scheme from [pywal](https://github.com/dylanaraps/pywal). More on that below.

```python
{{% include file="symlink/kitty.conf" %}}
```


## Pywal template {#pywal-template}

For me the default pywal template for kitty somehow didn't theme the colors of tabs. No idea if that's still the case nowadays, but I just put the [template from here](https://github.com/dylanaraps/pywal/blob/master/pywal/templates/colors-kitty.conf) in my `~/.config/wal/templates` directory. No reason to change a running system.

```python
{{% include file="symlink/colors-kitty.conf" %}}
```
