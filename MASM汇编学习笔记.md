---
title: MASM汇编学习笔记
tags: MASM,汇编,学习笔记
grammar_cjkRuby: true
---

# 笔记中有关符号的规约

80286- 表示80286及以下

80286+ 表示80286及以上

# 寄存器

## 八大通用寄存器

AX(accumulator) 累加器, 用于算术运算

BX(base) 基址变址

CX(count) 计数, 在循环(loop), 串处理, 移位等指令作为 **隐含** 的计数器(往往可以自动增加或者减少)

DX(data) 数据, 一般在做双字长运算时把DX和AX结合起来存放一个双字长数, DX用来放高位字.

SP(stack pointer) 堆栈指针

BP(base pointer) 基址指针

DI(destination index) 目的变址

SI(source index) 源变址

> 80386+ 中分别为 EAX, EBX, ECX, EDX, ESP, EBP, EDI, ESI

>  (只有)AX, BX, CX, DX都可以分为 \*L 和 \*H 来表示高位字节和低字节

## 专用寄存器

IP(instruction pointer) 表示相当于CS的 **偏移地址** , 与CS一起连用确定下一条指令的物理地址.

SP

FLAGS

## 段寄存器

用于主存储器寻址

CS 代码

DS 数据

SS 堆栈

ES 附加

FS 80386+ 附加

GS 80386+ 附加

# 内存寻址

主存储器的存储单元大小: **1byte**

一个地址按照使用情况, 可以表示多种大小的单元.

e.g. 主存储器中 0005H ~ 00008H 中的值分别为 78H, 56H, 34H, 12H,

则 (0005H) 在双字单元, 字单元, 单元中的表示的值分别为 12345678H, 5678H, 78H.

低位字节存入低地址, 高位字节存入高地址.

8086和80286中访问存储器都是以字为单位进行的, 80386+都是以双字为单元处理的. 所以在80286/8086中访问技术地址的字单元的时候, 取一个字要访问两次主存储器.


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

## 80386+ 的虚拟86模式

在保护模式下运行为实模式设计的代码

## 数据寻址方式(操作数的寻址方式)

指令: MOV DST, SRC

其中, MOV为操作码的助记符, DST和SRC为操作数, MOV为双操作数的指令.

### 1. 立即寻址(Immediate addressing)

**立即数** : 8bits或者16bits(80286及以下, 80386+则为32bits)

立即数只能用于SRC, 毕竟这相当于常量嘛, 而且SRC必须和DST字长一致.

如果立即数第一个数字为十六进制的A~F时, 前面必须加上0, 否则 AH就分不清到底是寄存器还是一个立即数.

### 2. 寄存器寻址(Register addressing)

操作数在寄存器中, 指令指定寄存器号.

> 注意: CS不能用MOV改变, i.e. CS不能作为MOV的DST, e.g. MOV CS, AX

> 为了缩短指令平均长度, 规定: 对于双操作数指令, 除了SRC可以为立即数方式之外, 一定要收一个寄存器寻址方式


以上两种寻址方式都是操作数都在寄存器中, 所以速度比较快, 而以下几种寻址方式都需要访问存储器来来去的操作数.

20bits物理地址计算(实模式/8086/8088)

PA(Physical Address) = Segment Address(段地址) x 10H + Effective Address(段内偏移地址, EA, 又叫有效地址)

而EA = BASE(基址) + (INDEX(变址, 相对于BASE的偏移量) x SCALE(比例因子, **80386+** , 表示1, 2, 4, 8字节宽度)) + DISP(位移量, 可以是0, 8, 16(80286-, 80386+则是32)bits)

其中默认情况下选择的寄存器有:

EA的成分 | 16bits | 32bits
-------|------|-------
BASE| BX, BP | 任何32位通用寄存器
INDEX | SI, DI | 除ESP之外所有32位通用寄存器

访问类型 | 所用段及段寄存器 | 缺省选择规则
----|-----|-------
指令 |CS | 用于取指令
堆栈 | SS | 所有的堆栈的进栈和出栈, 任何用于BP ESP EBP作为基址寄存器的访问
局部数据 | DS | 除相对于堆栈以及串处理指令的目的串意外的所有数据访问
目的串 | ES | 串处理指令的目的串

当然, 还可以通过段跨越前缀来指定使用其他非默认选择的寄存器(在后面的直接寻址方式里面有例子)

但是以下三种情况下不允许使用 `段跨越前缀`

* 串处理指令的DST必须用ES
* PUSH 指令的DST和POP的SRC必须用SS
* 指令必须存放在CS中

### 3. 直接寻址方式

SRC是一个只有DISP的EA

e.g. MOV AX, [2000H]
 
默认的段地址用的是 `DS` 里面的, i.e. [2000H] 表示为 DS中的值 x 10H + 2000H (这里的2000H就是EA), 假设DS = 3000H, 则 [2000H]表示主存储器物理地址 3 2000H中的内容

可以使用段跨越前缀来使用其他非默认寄存器作为段地址:

e.g. MOV AX, ES:[2000H] 这里就是使用的附加段寄存器中的值来作为操作数.

书写程序的时候, 为了方便, 可以用一个有效的标识符代表地址作为 **符号地址**(变量) , e.g. 假定 VALUE = 2000H, 则 MOV AX, VALUE(等价于MOV AX, [VALUE])就等价于 MOV AX, [2000H]

> 符号地址默认是作为DISP当作EA对待, 也就是直接取EA中的内容, 不过可以在符号地址前面加上 **OFFSET** 属性操作符, 把符号地址代表的值作为立即数而不是DISP, e.g. MOV AX, OFFSET VALUE 这里相当于 MOV AX, 2000H

相当于取得主存储器中的变量的值

### 4. 存储器间接寻址方式(Register indirect addressing)

EA中只含有基址存储器内容(BASE)或变址存储器内容(INDEX)的一种成分, 可以使用段跨越前缀来去的其他段中的数据. 所以80286-不允许AX, CX, DX, SP存放EA. 

e.g. MOV AX, [BX] 等价于 MOV AX, DS:[BX]

如果 DS = 2000H, BX = 1000H, (2 1000H) = 0A088H (双字单元)

则 指令结束后 (AX) = 0A088H

可用于表格处理, 改变寄存器内容就可以取出表格的下一项.

### 5. 存储器相对寻址方式(Register relative addressing 或称直接变址寻址方式)

EA为基址寄存器或变址寄存器的内容和指令中指定的位移量之和. i.e. EA = DISP[INDEX] 或者 EA = DISP[BASE] 等价于 EA = [INDEX + DISP] 或 EA = [BASE + DISP]

e.g.MOV AX, 4[SI] (等价于MOV AX, [SI + 4] , 4是偏移量(DISP), SI是INDEX, 偏移量可以不放在中括号里面, 这里用的默认段寄存器DS)

可用于表格处理, 表格的首地址可设置为DISP, 利用修改基址或变址寄存器的内容来取得表格中的值.

### 6. 基址变址寻址方式(based indexed addressing)

EA是一个基址寄存器和一个变址寄存器的内容之和. i.e. EA = [BASE][INDEX] 等价于 EA = [BASE + INDEX]

e.g. MOV AX, [BX][DI] (等价于MOV AX, [BX + DI])

同样适用于数组或表格处理, 首地址可存放在基址寄存器中, 用变址寄存器来访问数组中的各个元素, 因为BASE和INDEX都可以修改, 所以比上面那种方式更加灵活.

### 7. 相对变址寻址方式(relative based indexed addressing)

EA是一个基址寄存器与一个变址寄存器的内容和指令中指定的位移量之和. i.e. EA = DISP[BASE][INDEX] (等价于 DISP[BASE + INDEX] 或 [BASE + INDEX + DISP])

通常用于对二维数组的寻址. DISP指向二维数组之首, BASE和INDEX分别指向一维和二维; 也可用于堆栈处理, BASE指向栈顶, DISP表示从栈顶到数组的首址, INDEX用于访问这个数组的某个元素.

### 8. 比例变址寻址方式

EA = DISP[INDEX * SCALE] (equal to [INDEX * SCALE + DISP])

对于元素大小为2, 4, 8的数组, 可以直接在INDEX给出数组元素下标, 然后指定SCALE为元素字节大小

### 9. 基址比例变址寻址方式(baesd scaled indexed addressing)

EA = [BASE][INDEX * SCALE]

### 10. 相对基址比例变址寻址方式(relative based scaled indexed addressing)

EA = DISP[BASE][INDEX * SCALE]

## 与转移地址有关的寻址方式

这种寻址方式用来确定转移指令及CALL指令的转向地址.

### 1. 段内直接寻址(intrasegment direct addressing)

操作数类似于数据寻址的直接寻址, 也就是有效地址EA只有一个DISP, 而且这个有效地址是 **相对于** IP寄存器中的内容, 也就是 **EA = 转向有效地址 - 当前IP中的内容** .

适用于条件转移及无条件转移, 但是用于条件转移指令的时候, 位移量只允许8bits(80386+可以是8bits或者32bits).

无条件转移指令的位移量为8bits的时候为短跳转, 位移量是16bits(80286-, 80386+为32bits)的时候为近跳转, 格式如下:

16bits(32bits)近跳转: JMP NEAR PTR [EA] (操作数前面加NEAR PTR操作符)

8bits短跳转: JMP SHORT [EA] (操作数前面加SHORT操作符)

> 这里面的EA是一个表示地址的有效数值, 也可以把[EA]用一个符号地址来代替

### 2. 段内间接寻址(intrasegment indirect addressing)

EA类似于数据寻址那样, 只不过这个有效地址用来 **取代** IP寄存器中的内容.

可以用 **WORD PTR** 来指出( **属性操作符, 强制类型说明** )其后的寻址方式所取得的转向地址是一个**字**的有效地址.

e.g. JMP WORD PTR [BP + 4]

> 这种寻址方式以及下面两种段间寻址方式都不能用于条件转移指令

### 3. 段间直接寻址(intersegment direct addressing)

在指令中直接提供了转向段地址和偏移地址, 所以会用指令中指定的偏移地址取代IP寄存器中的内容, 用指令中指定的段地址取代CS寄存器中的内容.

需要在操作数之前用 **FAR PTR** 操作符来说明是远跳转.

e.g. JMP FAR PTR 标号

其中标号所在的段地址取代CS中的内容, 标号所在的偏移地址取代IP中的内容.

### 4. 段间间接寻址

用存储器中的 **两个相继字** 的内容来取代IP和CS寄存器中的原始内容, 存储单元的地址由指令指定**除立即数方式和寄存器方式之外** 的任何一种数据寻址方式.

并且 **高字** 中的内容用来取代 **CS寄存器** 的内容, **低字** 中的内容用来取代 **IP寄存器** 中的内容.

并且要在操作数前面用 **DWORD PTR** 来说明转向地址需取双字为段讲转移指令.

e.g. JMP DWORD PTR [BX]

# 8086 PC工作过程简述

* 8086CPU加电启动或者复位后, CS和IP会被设置为 **CS = FFFFH, IP = 0000H**, i.e., 8086PC刚启动的时候, CPU从内存 **FFFF0H**单元中读取指令执行第一条指令.

* CPU读取并执行指令: CS:IP指向的就是要被CPU执行的指令的内存单元, 然后读取指令到指令缓冲器, 

# 80x86指令集

80x86的指令系统可以划分为六组: 数据传送指令, 串处理指令, 算术指令, 逻辑指令, 控制转移指令, 处理机控制指令.

## 数据传送指令

数据传送指令负责把数据, 地址或立即数送到寄存器或存储单元中.

又可以分为以下五种:

### 通用数据传送指令

MOV (move) 传送

MOV DST, SRC

指令操作: (DST) <- (SRC)

> 立即数只允许送到通用寄存器或者存储器中, 不允许送到段寄存器中

> 双操作数不能同时为存储器或者段寄存器

> DST不能是CS段寄存器或者立即数

> MOV指令不影响标志位


MOVSX (move with sign-extend) 带符号扩展传送 (80386+, 本课不做要求)

MOVZX(move with zero-extend) 带零扩展传送

> MOVSX和MOVZX都要求SRC小于DST的长度, 毕竟是要扩展嘛

PUSH(push onto the stack) 进栈 

PUSH SRC

操作: 16位指令: (SP) <- (SP) - 2 然后 ((SP) + 1, (SP))  （也就是一个字单位的（SP）鬼知道这书上为什么是这样表示的。。。）<- (SRC), 32位指令: (ESP) <- (ESP) - 4 然后 ((ESP) + 3, (ESP) + 2, (ESP) + 1, (ESP)) <- (SRC)

因为是后进先出, 所以SP先退2bytes(32CPU中就是4bytes), 然后在把数据放进去.

POP(pop form the stack) 出栈

POP DST

操作: 16bits: (DST) <- ((SP) + 1, (SP)) then (SP) <- (SP) + 2, 32bits: (dst) <- ((ESP) + 3, (ESP) + 2, (ESP) + 1, (ESP)) then (ESP) <- (ESP) + 4

> PUSH的操作数可以是 reg(通用寄存器) segreg(段寄存器) mem(存储器) data(立即数, **80286+** )

> POP的操作数除了立即数之外都可以

> CPU并不会检查栈顶是否越界, 所以一定要根据自己要用的最大栈空间安排栈的大小, 避免栈顶越界

> PUSH和POP中的 SP是相对于SS的, 反正都是SP都是偏移量, SS记录是栈顶的地址

PUSHA/PUSHAD(push all registers) 所有寄存器进栈

PUSHA: 16bits寄存器依次进栈, 进栈次序为 AX, CX, DX, BX, SP, BP, SI, DI, then (SP) <- (SP) - 16d(十进制)

PUSHAD: 32bits寄存器依次进栈, 进栈持续为EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI, then (SP) <- (SP) - 32

POPA/POPAD(pop all registers) 所有寄存器出栈

类似于PUSHA和PUSHD, 顺序就是进栈顺序的逆序

XCHG(exchange) 交换

XCHG OPR1, OPR2

操作: OPR1 <-> OPR2

两个OPR表示操作数, 并且两个操作数中必须有一个寄存器, 不允许使用段寄存器, 可使用立即数之外的其他任何寻址方式.

### 地址传送指令

LEA REG, EA (load effective address,  有效地址送寄存器)

操作: (REG) <- EA

> EA为存储器寻址中的有效地址

> DST 不能是寄存器

> 对于DST和EA大小不一样的情况, 会做截取(取低位)或零扩展(这种情都是80386+上的)

LDS (load DS with pointer) 指针送寄存器和DS寄存器

LDS REG, SRC

操作: (REG) <- (SRC) then (DS) <- (SRC + 2)

LES (load ES with pointer) 

操作数和LDS一致

操作: (REG) <- (SRC) then (ES) <- (SRC + 2)

> LDS和LES的SRC必须是存储器寻址

> LEA LES LDS 不影响标志位

类似的还有LFS(load FS with pointer), LGS(load GS with pointer), LSS(load SS with pointer)

### 标志寄存器传送指令

LAHF(load AH with flags) 标志送AH

operate: (AH) <- (FLAGS的 **低字节** )

SAHF(store AH into FLAGS) AH送标记寄存器

 (FLAGS的 **低字节** ) -> (AH)
 
PUSHF/PUSHFD(push the flags or eflags) 标志进栈

PUSHF: (SP) <- (SP) - 2 then ((SP) + 1, (SP) <- (FLAGS)

PUSHFD 看书(p57)

POPF/POPFD(pop the flags or eflags) 标志出栈

看书p57

### 类型转换指令

CBW(convert byte to word) 字节转换为字

**AL的内容符号扩展到AH** , 形成AX中的字. 如果(AL)最高位是0, 则(AH) = 0H, 如果(AL)最高位是1, 则(AH) = 0FFH

CWD/CWDE(convert word to double word) 字转换为双字

**AX的内容符号扩展到DX**

如果(AX)最高位是0, 则(DX) = 0H, 如果(AX)最高位是1, 则(DX) = 0FFFFH

还有 CDQ (convert double word to quad word), BSWAP(byte swap) (这两个本课不做要求)

### 累加器专用传送指令

IN(input) 输入

OUT 输出

XLAT(translate) 换码

## 算术指令

加法 ADD DST, SRC

operate: (DST) <- (SRC) + (DST)

**带进位** 加法(add with carry) ADC DST, SRC

operate: (DST) <- (SRC) + (DST) + CF, 其中CF为进位标志(最高有效位有进位的话, 就置为1).

加1(increment) INC OPR

op: (OPR) <- (OPR) + 1

> 以上三个命令都可作字或者字节运算(80386+为双字), 而且 **除了INC不影响CF标志外, 它们都影响条件标志位(CF进位, ZF结果为0, SF符号, OF溢出)**





# 汇编代码格式
定义一个段:

用 **segment...ends**, segment和ends分别 **通知编译器** 一个程序段的开始和结束, segment和ends是成对的 **伪指令** . 

并且一个程序段必须用名称来标识, i.e. 段名 segment 段名 ends

e.g.

``` x86asm
codesg segment
	mov ax, 01234H
	
	mov ax 4c00H
	int 21H
codesg ends
```

其中 codesg 只是一个名称(标识).

用 **assume** 某一段寄存器中的某一个用 **segment...ends** 定义的段相关联

e.g. assume cs:codesg (codesg为上面的例子中的那个符号地址)

程序返回:

在程序(段)的结尾加上以下两句指令用于程序返回:

``` x86asm
mov ax 4c00H
int 21H
```

通知编译器整个汇编程序的结束: **end** 伪指令, 一般放在源文件的最后一行.

总的完整的示例:

``` x86asm
; 说明cs与段codesg相关联, 也就是cs就是段中的第一个指令的物理内存地址
assume cs:codesg

; 定义段
codesg segment
	mov ax, 01234H
	
	; 程序返回
	mov ax 4c00H
	int 21H
codesg ends

; 源文件结束
end
```

## 程序编译链接

### 编译(build)

```
masm [filename]
```

其中filename可以省略asm后缀, e.g. MASM EXAMPLE.ASM或者 MASM EXAMPLE都行.

将会进入一个提示导出文件名的交互界面, 如果都只想使用默认文件名的话, 可以使用 `masm [filename];` , 也就是添加一个 `;` 在后面, 相当于后面的提示都是默认输入回车.

### 链接(link)

```
link [filename]
```

其中filename可以省 `obj` 后缀, e.g. MASM EXAMPLE.OBJ或者 MASM EXAMPLE都行.

将会进入一个提示导出文件名的交互界面, 如果都只想使用默认文件名的话, 可以使用 `link [filename];` , 也就是添加一个 `;` 在后面, 相当于后面的提示都是默认输入回车.

这样就会导出 一个exe可执行文件(DOS下的).

### Debug

* 用 `r` 可以查看所以寄存器的值, 用 `r [register name]` 可以修改对应的寄存器的值
* 用 `u[起始偏移地址] [结束偏移地址]` 可以查看 `CS:IP` 一直到 `CS:[IP+20H]` 的指令. 默认只对20个字节进行反汇编, 而且记住起始偏移地址和u之间没有空格.
* 用 `t` 执行 `CS:IP` 的指令, 并且会 `INC IP`
* 用 `p` 遇到了中断返回的时候就会退出程序, 跟 `t` 的功能有点像.
* 用 `d` 查看内存中的内容, 可以用 `d段地址:偏移地址`, 还可以在偏移地址后面加上一个最后一个要打印的元素的偏移地址.
* 用 `e 起始地址 数据 数据 数据 ……` 改变内存中的值, 起始地址是 `段地址：偏移地址` 形式.
* 用 `a 地址` 以汇编指令形式向内存中指令, 不过不能用地址变量.
* 用 `q`退出debug程序
* 用 `g =offset0 offset1` 来指定程序从 `CS:offset0` 运行到 `CS:offset1`.

可以通过 `debug [execuable file with suffix]` 来调试程序。

# MASM 数据定义

## 符号变量(符号地址)

```
[符号变量名] [助记符] [操作数1, 操作数2, .....]
```

助记符有 **DB** , **DW** , **DD** 分别表示操作数为字节单位, 字单位, 双字单位

e.g. **ARRAY DW 123H, 256** (没有H后缀的表示十进制)

## **?** 占位符

表示预留位置, 不初始化里面的值.

e.g. ARRAY DB ?,?,?

## 复制数据

```
[数值] DUP([数据1, 数据2, ....])
```

表示把 括号中的数据组重复 **\[数值\]** 次数.

> 还可以把 DUP 嵌套, e.g. 2 DUP (0, 1, 2 DUP(2)) 相当于 0 1 2 2 0 1 2 2
