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

不过为了保持对8086/8088的兼容, 80286+都支持实模式(在实模式下只能访问内存最开始的1M的空间), 并且开机都是默认为实模式, 需要手动开启(一般现代OS的第一个步骤就是开启保护模式).

### 80286 保护模式

logic address: [16bits selector 选择子]:[16bits offset 偏移地址]

与实模式的区别在于段寄存器中存储的不是段开头的实际的物理地址而是一个选择子.

**选择子**

bit [15....3] 为指向 descriptor table entry(描述符表的条目的序号, 所以描述符表中最多有 2^13 ~= 8k个条目) [2] 说明是位于GDT(全局, golbal descriptor table)还是LDT(局部, local descriptor table) [1 0] 描述请求的优先级别从00B到11B依次递减.

然后CPU通过选择子中的条目的index以及第三位的说明, 并对权限进行检查(还有其他几个检查, 这里略去), 找到 DTE(Descriptor Table Entry)

**DTE**

首先DT(Descriptor Table)和DTE是由OS(Operate System)创建的.

80286的DTE总有64bits, 其中有24bits的段起始物理地址(因为80286就是24bits的), 和16bits的段长(segment limit, 所以最大的段长只有64KB, 这一点饱受诟病), 另外24bits在80286中并未使用到.

通过 24bits段起始地址 + 16bits段内偏移地址(logic address中给出的offset)就可以得到选择子对应的真正的物理地址了.

## 80386+ 的IA32保护模式

80286的保护模式因为诸多原因并没有流行起来, 80386对80286时期的保护模式的诸多问题进行的改进和完善, 现在主流操作系统的X86版本都是使用的80386的保护模式.

80386支持两种保护模式内存选址:

### 非分页模式选址(分段模式, 也就是80286保护模式的选址方式)

非分页模式选址的选址方式流程和80286是一样的, 只不过因为80386是32bits CPU, 所以selector和offset都是32bits的.

i.e. logic address: [32bits selector 选择子]:[32bits offset 偏移地址]

> P.S. selector虽然是32位的, 不过只有低16位有用(行为和80286的selector一致)

同样地, DTE中段起始地址为32bit(非分页模式的物理地址, 分页模式下的虚拟地址, 统称为 **线性地址(linear address)**), 段长数据宽度为20bits, 而且还可以设置段长粒度(1Byte或者4KiB, 也就是对应的最大段长为1MB或者4GiB)

### 分页内存寻址

是否采用分页由 **PG** 指定.

如果 **PG** 是1, 则为分页内存寻址模式.

前面的步骤和非分页模式一模一样, 只不过非分页模式得到的linear address在分页模式只是virtual address, 还需要进一步通过 **paging unit** 进行转换成真正的物理地址.

**paging unit** 又有两个阶段组成:

1. 从 **CR3** 寄存器中获得 **Page Directory** 的 base address, 也就是其中第一个条目(entry)的物理内存地址, 然后根据 **Linear Address** 的高10位(linear address的高10位指定PDE(Page Directory Entry)的index, 所以PD总共最多有2^10 = 1024个条目, 每个条目长4bytes)找到对应的 **PDE** , **PDE** 中储存了segment base address, 还有段长和粒度的有关信息.
2. 如果 **PDE** 中的粒度为 4KB(也就是4K分页模式, 一个Page最大有4GB大小), 则该PDE中的 segment base address 就是该 linear address所在的 **Page** 的 base address, linear address中剩下的 低22bits就是相对于这个 Page的base address的offset, base address + offset 就是这个 linear address对应的 physical address;
3. 如果 **PDE** 中的粒度为1Byte(一个Page最大有1MB大小), 则 PDE 中的 segment base address 指向 对应的 PT(Page Table) 的 physical base address, 然后 linear address的 12到21位 (也就是linear address的高10位后面的10位) 为 对应的 PTE(Page Table Entry)的index(所以PT还是最多有2^10 = 1024个条目), PTE中保存的正是linear address对应的物理地址(这个physical address可以是在内存中也可以是在swap中, 由PTE的第0位指定).
4. 最后算上logic address中的偏移地址就可以了. 

### 参考

[umbc课件](https://www.csee.umbc.edu/~cpatel2/links/310/slides/chap17_lect16_paging_segmentation.pdf)

[Linux手册](http://www.tldp.org/LDP/khg/HyperNews/get/memory/80386mm.html)

## 数据寻址方式

指令: MOV DST, SRC

其中, MOV为操作码的助记符, DST和SRC为操作数, MOV为双操作数的操作码.

### 1. 立即寻址(Immediate addressing)

