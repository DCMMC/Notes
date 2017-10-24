---
title: MASM汇编学习笔记
tags: MASM,汇编,学习笔记
grammar_cjkRuby: true
---

# 内存寻址

## 实模式

为了解决16位的CPU使用20位的内存地址的问题.(8086/8088)

PA(物理地址, Physical Address) = 段地址(基址, 16bits) x 2^4 (10H) + 偏移地址(16bits)

用逻辑地址表示为 段基址:段内偏移地址 e.g.3000H:2000H

## 保护模式

