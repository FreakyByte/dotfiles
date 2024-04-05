+++
title = "Graphics Driver"
author = ["Michael Reitmeir"]
draft = false
weight = 1
+++

My PC runs on a _AMD R9 280_ GPU (yes I know it's old). Other words to describe that are _Southern Islands_ and _GCN 1_ ([source](https://en.wikipedia.org/wiki/List_of_AMD_graphics_processing_units#Features_overview)). So I want to switch from the older `radeon` driver to the better `amdgpu` driver (which has [Vulkan](https://wiki.archlinux.org/title/Vulkan) support). The [Arch wiki](https://wiki.archlinux.org/title/AMDGPU) principally explains how to do this, but on EndeavourOS with systemd boot, I had to do it slightly differently. [This guide](https://discovery.endeavouros.com/installation/systemd-boot/2022/12/) mentions what file to edit, but it doesn't really mention the syntax, which is a bit different than the one mentioned in the Arch wiki.

Turns out I had to add the following to the end of the one-line file `/etc/kernel/cmdline/`, then run `sudo reinstall-kernels` and reboot.

<a id="code-snippet---etc-kernel-cmdline"></a>
```nil
amdgpu.si_support=1 radeon.si_support=0
```
