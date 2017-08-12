---
title: 算法 Algorithm 
tags: Java,算法,笔记
grammar_abbr: true
grammar_table: true
grammar_defList: true
grammar_emoji: true
grammar_footnote: true
grammar_ins: true
grammar_mark: true
grammar_sub: true
grammar_sup: true
grammar_checkbox: true
grammar_mathjax: true
grammar_flow: true
grammar_sequence: true
grammar_plot: true
grammar_code: true
grammar_highlight: true
grammar_html: true
grammar_linkify: true
grammar_typographer: true
grammar_video: true
grammar_audio: true
grammar_attachment: true
grammar_mermaid: true
grammar_classy: true
grammar_cjkEmphasis: true
grammar_cjkRuby: true
grammar_center: true
grammar_align: true
grammar_tableExtra: true
---

# (p)1 Fundamentals

## 重定向和管道

% java Average < data.txt

相当于:
data.txt -> standard input -> Average

% java RadomSeq 100 1 2 > data.txt

相当于:
RandomSeq -> standard output -> data.txt

% java RandomSeq 100 1 2 | java Average

相当于:
RandomSeq -> standard output -> standard input -> Average

## 递归(Recursion)

**递归三准则:**

* 递归有一个基准情形 (base case): 一个包含==return==的条件语句作为递归的第一条语句.
* 递归调用越来越处理 (adress) 更小的子问题 (subproblems) , 使递归调用慢慢的向基准情形 (base case) 靠拢.
* 递归调用间不能处理到重叠 (overlap) 的子问题.

违背这三条准则会导致不正确的结果或者极低的效率.

## 1. 欧几里德算法 (Euclid's Algorithm)

证明:
首先证明引理 gcd(a, b) = gcd(b, a mod b)
 假设 a = bq + r, 且a, b的最大公约数是d
1) d|a, d|b, 所以d|(a - bq), 即d|r, 所以d也是r的约数, 所以d是b, r的最大公约数.
2) 假设d是b和r的最大公约数, 即d|b, d|r, 所以d|(bq + r), 即d|a, 所以d是a, b的最大公约数.
证毕

由上面的引理不难递归出 gcd(a, b) = gcd(b, r1) = gcd(r1, r2) = ... = gcd(rn-1, rn) = gcd(rn, 0);
且b > r1 > r2 > ... > rn > 0 = rn+1, 以及rn-1能够被rn整除, 所以gcd(a, b) = gcd(rn-1, rn) = rn;

实现:
``` java
/**
	* Ex 1.1.25 
	* 欧几里德辗转相除求最大公约数
	* @param p nonzero int num.
	* @param q nonzero int num.
	* @return gcd of p and q.
	* @throws IllegalArgumentException p, q非0且|p| >= |q|.
	*/
int gcd(int p, int q) throws IllegalArgumentException {
		//取绝对值
		p = Math.abs(p);
		q = Math.abs(q);

		if (p < q || p == 0 || q == 0)
			throw new IllegalArgumentException("要求: p, q非0且|p| >= |q|\n但是, 参数p = " + p + ", q = " + q);

		if(p % q == 0)
			return q;
		else 
			return gcd(q, p % q);
	}
```

## ADTs(Abstract Data Types)

ADT是一种能对使用者(clients)隐藏数据表示的数据类型, 并且该类型跟很多的方法实现(APIs)相关联(就跟基本数据类型(`primitive data type`)和操作符(`operators`)相关联一样).

在OOP(Object-Oriented Programming)语言中常常把ADT封装(encapsulation)在一个类(class)中.
用APIs的实现来描述算法和数据结构.

把算法封装在ADT中, 能够在不影响client代码的同时为了提高性能更换算法.

ADT用法示例: [Github-Exercise 1.2](https://github.com/DCMMC/Java/blob/master/Algorithms/tk/dcmmc/fundamentals/Exercises/DataAbstraction.java)

## 背包(Bag)

Bag是一种只能添加元素不能删除元素, 可以元素迭代(比如使用foreach)的ADT.

## Queue (FIFO)

先进先出.

比如foreach的时候, 先进去的元素会被foreach先遍历到的.

## 2. Stack (LIFO)

后进先出.

一个简单应用: Dijkstra双栈算术表达式求值算法:

为了方便起见, 假设算术表达式只由数字, +, -, \*, /, sqrt, 括号和空格注册, 且没有省略任何括号(也就是暂时不考虑操作符的优先级), 数字和字符均以空白符相隔.
i.e., ( 1 + ( ( 2 + 3 ) \* ( 4 \* 5 ) ) )

然后对字符串从左往右解析并处理这些实体:
* 将操作数压入操作数栈
* 将运算符压入运算符栈(不包括括号)
* 忽略左括号
* 遇到右括号时, 弹出一个运算符, 弹出所需要数量的操作数, 并将运算符和操作数的运算结果压入操作数栈

## 实现方法

上述三种ADTs(Bags Stacks Queues) 都可以使用一下三种方式实现:
* fixed-capacity array 固定大小数组
* resizing-capacity array 可变大小的数组
* Linked List 链表

推荐使用SLL(Single Linked List)或者DLL(DoubleLinkedList)方式实现.

## 算法分析

### 算法分析常用函数

`!$\lfloor x \rfloor$` 向下取整, 不大于x的最大整数
`!$\lceil x \rceil $` 向上取整, 不小于x的最小整数
`!$\lg N$` 以2为底的对数(`!$\log _2{N}$`)
`!$H_N$`, i.e., `!$1+\frac {1} {2}+\frac {1} {3} +...+\frac {1} {N}$` 调和级数

### 算法分析常用近似函数

* `!$H_N$` ~ `!$\ln N$` (证明见高数无穷级数章节)
* `!$\sum N$` ~ `!$\frac {N^2} {2}$`
* `!$\lg N!$` ~ `!$N\lg N$`
* `!$(1-\frac {1} {x})^x$` ~ `!$\frac {1} {e}$` (见高数等价无穷小)
* `!$\dbinom {N} {k} = \frac {N!} {k! (N-k)!}$`(二项式系数, i.e., N取k的组合) ~ `!$\frac {N^k} {k!}$`

> **定义**:
> g(N) ~ f(N) 即 `!$\sum_ {N \rightarrow \infty} \frac {g(N)} {f(N)} = 1$`

计算loop的增长数量级:
e.g.
ThreeSum中的三重循环中
`!$\sum_ {i=1} ^N \sum_ {j=i+1} ^N \sum_ {k=j+1} ^N 1$` ~ `!$\int_{x=1} ^N \int_{y=x} ^N \int_{z=y} ^N \mathrm {d}z \mathrm {d}y \mathrm {d}x$` ~ `!$\frac 1 6 N^3$`


### 增长数量级(时间复杂度)
* 常数级别 1
* 对数级别 `!$\log N$` e.g. 二分查找
* 线性级别 N 
* 线性对数级别 `!$N \log N$` e.g. 归并查找(分治)
* 平方级别 `!$N^2$` 
* 立方级别 `!$N^3$`
* 指数级别 `!$2^N$` e.g. 穷举查找, 检查所以子集

### 倍率定理(近似模型)

如果T(N) ~ `!$aN^b\lg N$`(幂次法则的数学模型), 那么T(2N)/T(N) ~ `!$2^b$`
**Proof:** 
T(2N)/T(N) = `!$a(2N)^b\lg 2N$`/`!$aN^b\lg N$`
 = `!$2^b(1+ \frac {\lg 2} {\lg N})$`
 ~ `!$2^b$`
当N足够大的时候, 忽略就能很小了.

**推广:** T(kN)/T(N) ~ `!$2^k$`(k就是每次测试数据规模与上一次测试的数据规模的比值)

> **大O记法:**
> 如果`!$\exists c$`和`!$N_0$`, 使得对于所有`!$N > N_0$`都有`!$|f(N)| < cg(N)$`, 则称`!$f(N)$`为`!$O(g(N))$`

`!$O(g(N))$`也就是`!$g(N)$`为运行时间的**上限**.

> **大Omega记法**
>  如果`!$\exists c$`和`!$N_0$`, 使得对于所有`!$N > N_0$`都有`!$|f(N)| > cg(N)$`, 则称`!$f(N)$`为`!$\Omega (g(N))$`

`!$\Omega (g(N))$`, 那么也就是`!$g(N)$`为运行时间的**下限**.

> **大Theta记法**
> 如果`!$f(N)$`即是`!$O(g(N))$`又是`!$\Omega (g(N))$`, 那么称`!$f(N)$`为`!$\Theta (g(N))$`

大Theta记法用来描述算法的**最优性能**.


### 均摊分析

记录所有操作的总成本再除以总操作数.
这种情况可以允许少量开销很大的操作, 只要使平均开销小于预估上限就行.
典型的例子就是那个resizeing-Capacity array实现的Stack, 在触发resize指令的时候开销很大, 不过总的平均每操作的开销可以达到常数级别(即使在最坏的情况(worst-case)下).


### 内存开销

JDK在intel64 bits平台上的实现(不带引用/指针压缩的)是最小单位为8bytes(64bits), 也就是一个word.
例如一个一下代码创建的实例对象, 需要24bytes:
~~~ java
class ObjWithOneBoolean {
	boolean bool;
	String refObj;
}
~~~
* Object需要12~16bytes(64bits JDK实现, 32bits系统的JDK实现为8bytes)的head(或称overhead),  head中包含了这个object的Class对象的引用, 垃圾回收的信息, ID and status flags such as whether the object is currently reachable, currently synchronization-locked etc.(不过基本类型数组的head需要加上4bytes用来储存length)
* 基本类型存储区域: 一个boolean需要1byte
* 引用类型存储区域: 每个引用(pointer)大小的8bytes(64bits JDK实现, 32bits的实现就是4bytes的指针).
* 因为64bits系统最小内存单位都是8bytes, 为了使整个对象的大小为8(32bits系统的JDK实现就需要4的倍数)的倍数bytes, 最后还需要3bytes的padding用来对齐.

如果有成员内部类, 因为成员内部类需要一个指向外部类的开销:
e.g.
~~~ java
class ObjWithInnerClass {
	byte b; //1byte
	int i;//4bytes
	boolean bool;//1byte
	double d;//8bytes
	Objet objref;
	class Node { //8bytes, pointer to OuterClass
		int item;//4bits
		Node next;//8bytes
	}
}
~~~

内存结构:
* 12bytes的overhead
* d: 8bytes
* i: 4bytes
* bool: 1byte
* b: 1byte
* objref: 8bytes
* 用于Node指向外部的引用(extra head)
* item: 4bytes
* next: 8bytes
* padding: 6bytes

> hotSpot为了减少padding的占用, 会适当的调节这些变量和引用在内存中的相对位置, 所以他们的内存结构中的顺序并不会和代码声明的顺序相同.

> 可以通过[Java Object Layout](http://openjdk.java.net/projects/code-tools/jol/)查看内存占用详细情况

> 还可以直接使用java.lang.Instrument.getObjectSize()查看对象占用内存大小

[参考](http://pcpig.iteye.com/blog/1206902)
[Java对象内存占用分析](https://segmentfault.com/a/1190000006933272)

对于二维数组int\[5][6], 第一维度是一个存储有5个引用变量的对象, 然后为一个引用变量又指向一个存储了6个int的对象.

当一个方法被调用的时候, 系统会从栈内存中为方法分配所需的内存(用来保存局部变量), 当方法返回的时候, 内存会重新被返回栈内存.
当使用new创建对象时, 系统会从堆内存中为该对象分配所需的内存.

### 比较算法的一般步骤

* 实现并且调试这些算法
* 分析他们的基本性质(时间空间复杂度)
* 制定一个比较他们性能的猜想(假说)(可以基于某些定理)
* 进行实验来验证上述猜想

## 排序

### 3. 选择排序(Selection Sort)

时间复杂度不太取决于目标数组, 反正效率都不高

一次一次的找到每一轮的最小值并放到这一轮的第一个位置

~ 1/2N^2(固定这么多次) compare, ~ N(最差情况下) exchange

数据交换这一方面的开销小是优点, 总的效率低的缺点

### 4. 插入排序(Insertion Sort)

时间复杂度比较取决于目标数组, 目标数组越接近于完全正序, 时间复杂度就越低

交换的次数就是逆序数对的对数, 比较的次数就是交换的次数+(N-1)

平均情况下(这里取的是每一个item都是进行到一半就找到了位置), 比较 ~ 1/4N^2, 交换 ~ 1/4N^2

在最坏情况(完全倒序), 比较 ~ 1/2N^2, 交换 ~ 1/2N^2

最好情况(完全正序), 比较 ~ N-1, 交换 0

### 5. 希尔排序(Shell Sort)

在最糟糕的情况下时间复杂度为O(N^1.5), 一个小小的改进就能从InsertionSort的O(N^2)降低到O(N^1.5)(而且是最坏情况)

平均下的时间复杂度不确定, 取决于increment sequence的选取.

InsertionSort在每一轮插入的时候需要跟相邻的元素一个一个交换, 这样很消耗资源, 希尔排序作为插入排序的扩展,

每次比较并交换的间隔不再是1(h=1这一特殊情况下h-sorting就是InsertionSort), 而是大于1的不同的步长.

**定义(Def.): h-sorting**

> 如果数组中的元素任意间隔h都是有序的, 那就称为h-sorting array, e.g., `1 2 3 4 5 6 7`, 取h为2, 则子序列1 3 5 7和子序列2 4 6分别都是有序的(即有h个互相独立有序的子序列交错组合在一起), 则该数组是h-sorted array
> 
> h-sorting即以h为间隔对子序列进行排序, e.g., 对序列` 1 5 8 2 3 4 6 7 6 8 9 `(共11个元素), 以h = 3进行h-sorting,
> 首先这个序列拆分为一下3个子序列: `1 2 6 7`和`5 3 7 9`和`8 6 4`, 然后依次对这三个子序列进行InsertionSort, 得到`1 2 6 7, 3 5 7 9`和`4 6 8`这三个子序列, 合并之后为: `1 3 4 2 5 6 6 7 8 7 9`, 这个序列就被称为原序列以h=3的h-sorted array
> 
> 一个更好的理解就是把序列放在一个h列的表中, 然后对其形成的二维表的每一列进行InsertionSort:
> 
> 1 5 8 2 3 4 6 7 6 8 9以h=3:
> 
> 5 8 
> 2 3 4
> 6 7 6
> 8 9
> 
> 然后依次对第一第二第三第四列进行InsertionSort:
> 
> 1 3 4
> 2 5 6
> 6 7 8
> 7 9


ShellSort大体的原理是以一系列值(increment sequence)作为h(又叫步长), 由大的h到小的h来对序列进行h-sorting, 只要该h序列最后的h是1,
就一定能得出排序好的序列, 比如 5 3 1就是一个h序列, 先以h=5对原序列进行h-sorting, 然后再以h=3进行h-sorting, 最后以h=1进行h-sorting.
不过从１开始然后不断乘以２这样的序列的效率很低，因为１后面都是偶数，彼此ｈ-sorting排序都是错开的的.

而公认的最好步长序列是由Sedgewick(本书作者)提出的`(1, 5, 19, 41, 109,...)`，该序列的项来自`!$9 \times 4^i - 9 \times 2^i + 1$` 和`!$2^{i + 2} \times (2^{i + 2} - 3) + 1$` 这两个算式合并起来的结果.
用这样步长序列的希尔排序比插入排序要快，甚至在小数组中比快速排序和堆排序还快，但是在涉及大量数据时希尔排序还是比快速排序慢。

另一个在大数组中表现优异的步长序列是（斐波那契数列除去0和1将剩余的数以黄金分区比的两倍的幂进行运算得到的数列）

不过一般使用的序列是 ==Knuth== 提出的由递推公式 `!$h = h \times 3 + 1$` 确定的数列(转成通项公式即为`!$\frac {1} {2} (3^k - 1)$`, 这个序列在元素数量比较大的时候,
相比于 ==SelectionSort== 和 ==InsertionSort== , 性能按照数组大小以2的次幂递增.
使用 ==Knuth== 提出序列的比较次数大概为N的若干倍再乘以这个序列的长度(差不多约为`!$N^{1.5}$`, 由大量N很大的实验可以估算出)

虽然使用最优的序列的时候, 在对小数组排序性能有时候可以超过 ==heapsort== 和 ==quicksort== , 不过在大量数据的时候还是慢于后两个, 不过相比于
后两者复杂一些的实现, ==ShellSort== 只需少量代码而且对资源的消耗也比较小, 所以适合用在嵌入式系统这些比较重视资源的场景中.

### 6. 归并排序(MergeSort)

核心是归并: 把两个已排序的子序列归并成一个已经排序的序列.

Top-down mergesort:

采用递归和分治思想把一个序列不断的二分, 直到子序列只有一个数值, 这样一个数的子序列肯定是排序好的, 然后直接开始不断归并.

Bottom-up mergesort:

采用自底向上的方法非递归的归并排序数组

先把整个数组分为最小的情况(也就是每个子数组长度为1), 先这样进行归并, 然后按照数组长度为2进行归并, 子数组长度每次都是上一轮

归并的子数组的长度的两倍. 直到能够归并整个数组.

每一轮最多需要N次比较, 并且需要logN轮, 所以总的时间复杂度为NlogN.

Bottom-up mergesort可用于原地排序LinkedList.

#### **分治思想**

把一个复杂的问题不断分成很多小的子问题, 首先解决这些子问题, 然后用这些子问题的结果去结果整个问题. 分治思想常常涉及到递归. 

**对于一个大小为N的数组, 采用自顶向下(top-down)的mergesort的方法进行排序, 比较次数在 `!$\left[ \frac {1} {2} N \lg N, \ N \lg N\right]$`**

**Proof.**

定义函数 `!$C(N)$` 表示排序一个长度为N的数组的比较次数, 显然: `!$C(0) = C(1) = 0$`, 
而且对于 `!$N > 0$`, 在递归方法 mergeSort() 中, 有此上界:

```mathjax!
$$C(N) \leq C\left( \lfloor \frac {N} {2} \rfloor \right) + C\left( \lceil \frac {N} {2} \rceil \right) + N $$
```

最后一个N表示merge花费的最多比较次数.

并且同时有此下界:

```mathjax!
$$C(N) \ge C\left( \lfloor \frac {N} {2} \rfloor \right) + C\left( \lceil \frac {N} {2} \rceil \right) + \lfloor \frac {N} {2} \rfloor $$
```
 `!$\lfloor \frac {N} {2} \rfloor$` 表示merge所花费的最少比较次数, 正好就是两个子序列直接合在一起(前后两部分反着合起来运算)就是完全有序的了, merge还是需要花费一半的比较次数来比较前半部分, 到了i > mid或者j > hi的时候, 就不需要比较了.
 
 为了方便计算, 这里假设 `!$N = 2^n , \ \therefore \lfloor \frac {N} {2} \rfloor = \lceil \frac {N} {2} \rceil = 2^{n - 1}$`
 
 于是上界:
 `!$ C(N) = C(2^n) = 2C(2^{n - 1}) + 2^n$`
 
 左右同除以 `!$2^n$`, 得到:
 
 
 `!$ \frac  {C(2^n)} {2^n} = \frac {C(2^{n - 1})} {2^{n - 1}} + 1$`, 这是一个等差数列, 
 
 易得: `!$\frac {C(2^n)} {2^n} = \frac {C(2^0)} {2^0} + n, \Rightarrow C(N) = C(2^n)  = n2^n = N \log N $`
 
 另外一个证明方法为:
 
 mergeSort采用递归和分治思想, 把整个序列分为了 在二叉树的kth level, 共有`!$2^k$`个merge调用, 而且每个merge调用都需要最多比较`!$2^{n - k}$`次, 所以在n-level需要`!$2^k \cdot 2^{n - k} = 2^n$`次比较, 所以对于有n个level的二叉树状mergeSort中, 共需要`!$n2^n$`次比较, 又对于N个结点的二叉树, 其深度为`!$\log_2 N$`, 所以总共的最多比较次数为`!$N \log N$`.
 
 **Top-down 和 Bottom-up mergesort 最多需要 6NlogN次数组访问**
 
 **Proof.**
 
 每一次merge最多访问数组6N次: 2N次用于数组访问, 2N次用于移动回去, 还有最多2N用于比较.
 
 **所以MergeSort的平均时间复杂度为O(N logN), 空间复杂度为O(N)**
 
 #### **优化**
 
 * 对于较小的子序列, 使用InsertionSort会比默认的merge更加的高效(能够提高10%-15%),  see Exercise 2.2.23.
 * 在merge()中添加对a[mid] <= a[mid + 1]的情况的校验, 如果a[mid] <= a[mid + 1], 那么就不进行归并, 这样能够在处理完全有序的序列时达到线性时间复杂度, see Exercise 2.2.8
 * 消除暂存数组的复制操作: 每一次merge都要对子序列进行复制, 这样会造成复制数据的时间开销(空间开销不变), 设计两次sort()调用, 一次从数组中取出那些数据, 然后将归并好的结果放入暂存数组中, 另外一次就从暂存数组中取出数据然后将归并好的结果放入原数组, 这样两个数组同时工作在递归中, 减少复制的开销. 这需要一定的递归技巧. see Ex 2.2.11

> P.S. 不是说一定要每次都实现这些所有的优化, 而是我们应当注意: 不要对一个算法的初始性能下绝对的结论, 很多时候还有很多优化的空间.

研究一个新问题的时候, 最好的方法是先用最简单的方法实现, 然后在这个方法成为瓶颈的时候再去重新实现一个新的算法. 实现那些仅仅带来参数因子的优化可能并不值得, 并且在每次优化之后最好一定要进行科学的实验(就像书上的练习一样).

#### **Computational Complexity of MergeSort**

**所有的基于比较的排序算法的在最坏情况下的比较次数的下界(low bounds)为 log(N!)~N logN(see P185 Stirling's approximation: log(N!) = log1 + log2 + ... + logN ~ NlogN)**

**Proof.**

构造一个适用于所有compare-based sorting algorithms的**二叉决策树(decision tree)**, 在compare-based sorting algorithms中, 树中的每一个 **内部结点(internal node)** 表示一次比较, 每一片 **叶子(leaf)** 表示完整排序后的序列.

显然, 对于长度为N的序列, 叶子的最少个数为N!, 即有N!中不同的排列可能. 否则如果少于N!, 说明有一些排列的可能会被遗漏, 不过可以多于N!, 因为有可能出现重复的叶子.

从root到某一个leaf之间的路径上结点的个数即为这种情况下比较的次数, 最长的那一条路径叫做树的**高度**, 它代表最坏情况下的比较次数.

又显然, 一棵高度为h的二叉树, 最多有`!$2^h$`片叶子(当且仅当为**完全二叉平衡树**的时候).

综上所述, 高度为h的二叉决策树的叶子的数量在 `!$\left[ N!, \ 2^h\right]$`区间内.

所以比较次数至少为 `!$\log (N!)$` ~ `!$N \log N$`次.

> P.S. 如果算法会对某些特殊顺序的序列进行优化或者算法能够了解到序列的值的分布或者序列的初始顺序或者有重复key之类的情况, 上述下界将不再适用.

除了在最坏情况下的比较次数的下界已经是确定的了, 还有很多因素需要关注: 空间占用, 一般情况下的时间复杂度, 不基于比较的排序算法, 数组访问次数等等.

### 7. Knuth(Fisher-Yates) shuffle算法

原地(in-place)随机打乱一个数组, 并且是等概率的随机排列数组, 时间O(n), 空间O(1).

算法实现见: tk.dcmmc.sorting.Algorithms.ArrayShuffle.java.

**Proof.**

要使第k(`!$1 \leq k \leq N$`)个元素被交换在第i(`!$1 \leq i \leq N$`)个元素的位置上, 即如下两种情况:

* 如果k < i, 前i - 1轮交换都不可能把k交换到i的位置, 所以不用管, 然后第i轮交换一定要保证k交换到了i的位置, 也就是`!$\frac {1} {i - 1}$`的概率, 然后还要保证i + 1 ... n轮都没有把k从i的位置上被交换到其他位置. 
* 如果i <= k, 前k - 1轮交换都不需要管, 第k轮交换一定要确保i被交换到了k的位置上, 即`!$\frac {1} {k - 1}$`, 然后还要确保k + 1...n轮交换都没有把i从k的位置上被交换到别的位置.

以第二种情况为例计算其概率:

```mathjax!
$$P_{i \to k} = \frac {1} {k - 1} \cdot \frac {k - 1} {k} \cdot \frac {k} {k + 1} \cdot \cdot \cdot \frac {n - 1} {n} = \frac {1} {n}$$
```

证毕.

