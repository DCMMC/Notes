---
title: MASM汇编学习笔记
tags: MASM,汇编,学习笔记
grammar_cjkRuby: true
---

# 内存寻址

## 实模式(实地址)

为了解决16位的CPU使用20位的内存地址的问题.(8086/8088)

PA(物理地址, Physical Address) = 段地址(基址, 16bits) x 2^4 (10H) + 偏移地址(16bits) (最大偏移量为 `FFFFH` 也就是64KB)

用逻辑地址表示为 [段基址]:[段内偏移地址] e.g.3000H:2000H

段寄存器(用来保存段地址的寄存器): CS(代码), SS(堆栈), DS(数据), ES(附加), FS和GS(这两个是80386+新增的两个附加段寄存器)

每个段的最大长度为64KB(也就是段内偏移量的最大大小), 而且各段还可以相互重叠

## 保护模式(虚拟地址)

因为实模式的寻址空间(1M)已经不能满足80286+的内存寻址范围了, 还有就是对多任务处理的支持.

### 80286 保护模式

logic address: [16bits selector 选择子]:[16bits offset 偏移地址]

与实模式的区别在于段寄存器中存储的不是段开头的实际的物理地址而是一个选择子.

**选择子**

bit [15....3] 为指向 descriptor table entry(描述符表的条目的序号, 所以描述符表中最多有 2^13 ~= 8k个条目) [2] 说明是位于GDT(全局, golbal descriptor table)还是LDT(局部, local descriptor table) [1 0] 描述请求的优先级别从00B到11B依次递减.

然后CPU通过选择子中的条目的index以及第三位的说明, 并对权限进行检查(还有其他几个检查, 这里略去), 找到 DTE(Descriptor Table Entry)

**DTE**

首先DT(Descriptor Table)和DTE是由OS(Operate System)创建的.

80286的DTE总有64bits, 其中有24bits的段起始物理地址(因为80286就是24bits的), 和16bits的段内偏移地址(所以最大偏移量只有64KB, 这一点饱受诟病), 另外24bits在80286中并未使用到.

通过 24bits段起始地址 + 16bits段内偏移地址就可以得到选择子对应的真正的物理地址了.

## 80386+ 的保护模式



