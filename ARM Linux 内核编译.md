---
title: ARM Linux 内核编译
tags: ARM, Linux
grammar_cjkRuby: true
---


## 下载 Linux 最新版内核源码

[官方链接](https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-4.17.tar.xz)

**解压 Linux 内核源码**

```shell
tar xaf linux-4.17.tar.xz 
```

## 实验环境

* ArchLinux amd64

### qemu

**qemu-system-arm**

```shell
yaourt -S qemu-arch-extra
```

### Arm toolchain

### gcc with gnu-eabi

```shell
yaourt -S arm-linux-gnueabi-gcc
```

> 提示 `未知的公共密匙 13FCEF89DD9E3C4F`
> 解决方法: gpg 导入公钥 ==gpg --recv-key 13FCEF89DD9E3C4F==

## 参考

http://devarea.com/building-embedded-linux-system-with-qemu/#.WxoUmSRfhhE