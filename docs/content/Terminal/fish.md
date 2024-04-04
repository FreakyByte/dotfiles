+++
title = "Fish"
author = ["Michael Reitmeir"]
draft = false
weight = 1
+++

My [fish](https://fishshell.com/) config is actually really short. That's the nice thing about fish: The defaults are just very nice.

Only thing to note is that [eza](https://github.com/eza-community/eza) instead of the classic `ls` command because its colors are nicer. So either install `eza` or remove this line.

```fish
{{% include file="symlink/config.fish" %}}
```


## Prompt {#prompt}

I use the [tide](https://github.com/IlanCosman/tide) prompt to make the shell look nicer and display useful infos like some git status information. Follow the instructions on its Github page to install it too (using the [fisher](https://github.com/jorgebucaran/fisher) plugin manager). It has its own very nice and simple configuration wizard, so you really don't need to see my config.


## Bash scripts break {#bash-scripts-break}

Only "downside" about fish is that it doesn't follow the POSIX standard. This essentially means that standard `sh` scripts might not work with fish since fish uses different syntax. But not to worry! Whenever you need a more standard shell like `bash`, just use that for a bit. So run `bash`, execute the stuff you gotta execute, and leave `bash` again for `fish` with `CTRL+d`. If your script has a [shebang](https://en.wikipedia.org/wiki/Shebang_(Unix)), there's nothing to do in the first place.
