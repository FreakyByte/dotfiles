+++
title = "Dunst"
author = ["Michael Reitmeir"]
draft = false
weight = 4
+++

[Dunst](https://github.com/dunst-project/dunst) is my notification daemon. This means it is responsible for actually displaying the notifications that other apps want to send. You can send your own notifications using `notify-send`.

The configuration below is mostly for visuals. You can find all configuration options [here](https://dunst-project.org/documentation/).

```toml
{{% include file="symlink/dunstrc" %}}
```
