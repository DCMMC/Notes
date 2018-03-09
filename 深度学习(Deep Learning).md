---
title: 深度学习(Deep Learning) 
tags: 深度学习, AI, DL, ML,笔记
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
# Notations
在本书中, `!$log x$` 对数均指代 **自然对数 `!$\log_e x$` 即 `!$\ln x$`**

# Intro

在现在人工智能(Artificial Intelligence, *abbr.* AI)领域主要需要解决的是那些对人类来说很 **直观的(Intuitively)** 但是难以形式化(formally, *i.e.*, mathematic rules)的描述的问题. 本书讨论的一种方案, 可以让计算机通过较为简单的知识来从经验中学习产生更加复杂的 **层次体系(相互关联的层次概念, hierarchy concepts)** , 我们称这种方法为 **深度学习(AI deep learning)** . 

一些早期的人工智能项目希望通过将整个世界通过人为的硬编码(hard-code)成形式化的语言, 但这个想法的困难实现表明, AI 系统需要能够直接用原始数据中提取模式(extract pattern)的能力来获取(acquire)自己的知识而不是人为的硬编码进去, 这个能力被称为 **机器学习(Machine Learning)**. 

**逻辑回归(Logistic  Regression)** 作为一种简单的机器学习算法可以用来判断是否需要剖腹产, 另一种被称为 **朴素贝叶斯(naive Bayes)** 的机器学习算法可以用来区分是否是垃圾邮件. 但是, 这两种简单的机器学习算法很大程度上依赖于给定数据的表示(representation), 例如在预测是否需要剖腹产时, 医生必须告诉系统几条与之密切相关的患者的信息(e.g. 是否存在子宫疤痕), 表示患者的每条信息被称为 **特征(features)** , 逻辑回归学习这些特征如何与各种结果相关联, 而不能直接想医生一样从核磁共振成像得出结果.  

不管实在 CS 领域还是日常生活领域, 对数据表示的依赖是很正常的现象, 例如要画一条直线将对一下数据分为两类, 在笛卡尔坐标系下显然是不可能的, 但是在极坐标系下就是很简单的.

![Figure 1.1][1]

但是, 对于很多任务来说, 很难知道什么特征需要被提取出来, 例如我们需要判断图片中是否是一辆车, 我们知道车有轮子, 但是轮子可能会有金属部分被太阳反射的光, 车落在轮子上的阴影, 轮子上的挡泥板... 等等, 一个解决方案就是使用机器学习, 不仅把数据的表示映射到输出, 还要使用机器学习发掘出数据的表示本身, 这种方案被称为 **表示学习(representation learning)** . 学习到的表示往往比手动设计的特征集更加好, 而且只需要很少的人工干预.

表示学习的一个典型例子就是 **自编码器(autoencoder)**, 自编码器由一个编码器(encoder, 将输入数据转换成不同的表示, )和一个解码器(decoder, 将新表示转换回原来的格式)组成. 自编码器被训练成通过编码器和解码器能够保留尽可能多的信息, 而且新表示就很多很好的属性(properties), 不同的自编码器实现不同的属性. 

但我们设计用来学习特征(features)的算法或特征时, 我们的目标一般是分离出能够解释被观测数据的 **变化因素(factors of variation)**, 这些因素通常是不能直接观察到的量, 他们可以被看作是数据的概念或抽象(abstract). 在分析图像中的车时, 因素可能是车的颜色, 位置, 太阳的方位角等等. 大部分应用需要理清(disentangle)变化因素, 丢弃我们不关心的变化因素. 显然, 从原始数据中提取出如此高级别, 抽象的特征是困难的, 很多类似于人的口音之类的变化因素, 只能使用复杂的接近人类的理解来辨识, 这几乎跟解决原问题一样复杂, 至少乍一看, 表示学习对我们没帮助.

**深度学习** 通过使用更加简单的表示来表达复杂表示, 解决了表示学习中最核心的问题. 例如对一个人物图像的识别可以通过组合类似于转角, 轮廓(他们转而由边线定义)的简单概念.

![Figure 1.2][2]

上图中, 输出像素在可见层, 然后是一系列从图像中提取的越来越多抽象特征的 **隐藏层(hidden layers, 因为他们的值不是输入直接给出的)**, 模型必须判断那些概念(concepts) 对于解释可见数据之间的关系是有用的.

深度学习的一个典型模型就是 **前馈神经网络(feedforward netword)** 或称 **多层感知机(MultiLayer Perceptron, *abbr.*, MLP)**. MLP 仅仅是通过一个数学函数将一组输入值映射到输出值, 这个函数又是有很多更加简单的函数复合而成, 我们可以认为每一个不同函数的应用都提供了输入的一个新的表示. 

另外一种对深度学习的理解是深度(depth)允许计算机学习一个多步骤(multi-step)的程序, 每一层表示可以被认为是计算机在并行执行了一系列指令之后的内存状态, 网络越深, 累积执行的指令也就越多, 并且后面的指令可以使用前面的指令的结果, 所以在激活的某层里面并不是所有信息都是编码了解释输入的变化因素. 这些表示还存储了有助于帮助模型更好地组织其处理过程的状态信息, 虽然这些状态信息与具体的输入内容无关.

主要有两种用来度量模型深度的方法. 第一种是基于评估架构(Architecture)必须要执行的顺序指令数目. 我们可以认为是对模型给定输入之后, 描述模型计算出各种输出的流程图(flowchart)中最长路径的流程图的路径(因为流程图的长度取决于我们允许哪些函数能够在流程图中的每一步中被使用), 就像两个完全相同的程序使用不同的语言编写会有不同的长度.  

![Figure 1.3][3]

比如同样的逻辑回归模型, 如果使用加法, 乘法和 logistic sigmoid 符号作为模型元素, 那么这个模型的深度为 3, 如果把逻辑回归视为元素本身, 那么该模型的深度为 1.

另外一种度量模型深度的方法是在深度概率模型中是使用的方法, 将描述概念之间的关系的图的深度作为模型深度而不是计算图的深度. 在这种情况下计算每个概念(concept)的表示(representation)的流程图的深度(也就是第一种方法计算出来的)可能比概念之间关系的图中该概念的深度更加深, 例如一个 AI 系统观察一个有一直眼睛在阴影中的人脸的图的时候, 它最初可能只看到一只眼睛, 但当系统检测出脸部之后, 系统可以推断出第二只眼睛也是可能存在的. 在这种情况下, 概念图只有两层(眼睛和脸部), 而如果计算每一个概念的计算模型都需要 `!$n$` 次, 那么计算图的深度为 `!$2n$`.

因为并不总是清楚是计算图的深度还是概率模型图的深度哪一个更加有意义, 所以架构的深度没有单一的正确值. 相比传统的机器学习, 深度学习研究的模型涉及更多的(计算机)学到的函数或学到的概念的组合.

![Figure 1.4][4]

上图表示了 AI 领域不同 AI 训练方法之间的关系.

![Figure 1.5][5]

上图表示了AI 各个训练方法之间的高层次原理, 阴影的方框表示能够从数据中自主学习的部件, 表示学习中输入到各种特征(features)之间的箭头表示了各种表示(representation)的转换.

本书分为三个部分, 第一部分介绍一些基本的数学方法和机器学期概念, 第二部分介绍最成熟的深度学习算法, 这些技术基本上得到了解决, 第三部分讨论一些广泛的被认为是深度学习未来研究的重点的具有展望性的想法.

![Figure 1.6][6]

上图描述了本书的高层结构, 其中箭头表示先后依赖关系.

## DL 的历史趋势

> 略

# (p) Part 1 Applied Math and Machine Learning Basis

## (p) 1.1 Linear Algebra 线性代数

> 如果已经熟悉线性代数, 可以跳过本章, 推荐 *The Matrix Cookbook(Petersen and Pedersen, 2006)* 来回顾一些重要公式.
> 本章只涉及深度学习需要重要的线性代数知识, 建议参考阅读 *Shilov(1977)* 来全面学习线性代数.

> 线性代数是连续数学, 非离散数学

### (p) 1.1.1 Scalars(标量), Vectors(向量), Matrices(矩阵) 和 Tensors(张量)

*  标量一般用小写字母, 斜体表示, 并且一般会指明数的类型, e.g., `!$s \in \mathbb{R}$`

* 通常向量用小写粗体, 其中的元素写成小写斜体, 如果向量有 `!$n$` 个元素, 并且所有元素都属于 `!$\mathbb{R}$` , 那么该向量属于实数集 `!$\mathbb{R}$` 的 n 次 **笛卡尔乘积(Cartesian product)** 构成的集合, 记作(denote) `!$\mathbb{R}^{n}$`

* 如果要明确表示向量中的元素, 将元素排列成一个方括号包围的纵列

```mathjax!
\begin{bmatrix} x_1 \\ x_2 \\ \vdots \\ x_n \end{bmatrix}
```

> `!$x_i$` 是小写斜体, Markdown 里面的内置 `!$\LaTeX $` 公式我不知道怎么打小写斜体.

* 要取向量 `!$x$`的部分元素作为一个集合, 例如下标 1, 3, 6 的元素, 可以定义 `!$S = \{ 1, 3, 6\}$`, 则这个集合表示为 `!$x_S$`. 用符号`!$-$` 表示集合的补集的索引, 例如 `!$x_{-1}$` 表示向量 `!$x$` 中除 `!$x_1$` 之外的所有元素, 同理 `!$x_{-S}$`.

* 矩阵通常用大写粗斜体表示, m行n列并且所有元素都是实数域的矩阵 `!$A$` 可记作 `!$A \in \mathbb{R}^{m \times n}$`, 对于矩阵元素下标, 可以使用 `!$:$` 通配符, 例如 `!$A_{i,:}$` 表示矩阵第 `!$i$` 行的元素. 里面的元素用大写非粗体.

> `!$f(A)_{i,j}$` 表示函数 `!$f$` 应用在 **A** 上输出的矩阵的索引元素.

* 张量(Tensors) 就是超过两维的数组, 用大写正粗体字母表示.

矩阵的 **转置(transpose)**, 相当于绕着 **主对角线(main diagonal)** 的镜像, 表示为

```mathjax!
$$\left(A^{\top}\right)_{i,j} = A_{j, i}$$
```
向量就相当于只有一列的矩阵的转置, 有时候就用这种形式表示向量, e.g. `!$[1,2,3]^{\top}$`.	标量的转置就等于其本身.

矩阵的加减就是把矩阵中的每一个元素进行对应的运算产生一个新的矩阵.

在深度学习中, 我们还会使用一种惯用记号, 允许矩阵和向量相加, `!$C = A + b$` 表示 `!$C_{i,j} = A_{i, j} + b_j$`, 换言之, 矩阵的每一行和向量的转置相加, 这种隐式的复制到很多位置的方式, 被称为 **广播(broadcasting)** .

### (p) 1.1.2 矩阵的乘法

```mathjax!
$$C = AB$$

$$C_{i, j} = \sum_{k} A_{i, k}B_{k, j}$$
```

> 所以矩阵的标准乘积不是相同位置上元素的乘积(区别于矩阵的加减), 不过矩阵相同位置上的乘积叫做 **元素对应乘积(element-wise product)** 或 **Hadamard 乘积**, 记作 `!$A \bigodot B$`

两个相同维数(dimensionality)的向量的 **点乘(dot product)** 可以看做矩阵乘积 `!$x \cdot y = x^{\top}y$`.

矩阵的乘法服从分配律(distributive)和结合律(associative):

```mathjax!
$$A(B + C) = AB + AC$$
$$A(BC) = (AB)C$$
```

> 注意, 矩阵乘法运算 `!$AB$` 和 `!$BA$` 是不同的, 不符合交换律, 不过向量的点乘符合交换律, `!$x \cdot y = y \cdot x = x^{\top} y = y^{\top} x$`.

```mathjax!
$$(AB)^{\top} = B^\top A^\top$$
```

矩阵方程式

```mathjax!
$$Ax = b$$
$$A \in \mathbb{R}^{m \times n}, b \in \mathbb{R}^m, x \in \mathbb{R}^n \text{ is a vector of unknown variables.}$$
$$\Leftrightarrow$$
$$A_{1, :} x = b_1$$
$$A_{2, :} x = b_2$$
$$\vdots$$
$$A_{m, :} x = b_m$$
```

### (p) 1.1.3 单位矩阵(Identity Matrices)和逆矩阵(Inverse Matrices)

**单位矩阵定义**

```mathjax!
$$I_n \in \mathbb{R}^{n \times n}, \forall x \in \mathbb{R}^n, I_n x = x$$
```

所以单位矩阵就是主对角线的元素都是1, 其他都是0 的矩阵, 例如

```mathjax!
$$I_3 = \begin{bmatrix} 1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 1 \end{bmatrix}$$
```

矩阵 `!$A$` 的逆矩阵记作 `!$A^{-1}$`, 并且有 `!$A^{-1} A = I_n$`

所以对于矩阵方程 `!$Ax = b$` 的解是 `!$x = A^{-1} b$`, 所以这取决于能否找到 `!$A^{-1}$`. 不过用逆矩阵求解矩阵方程主要是一个理论工具, 实际上在软件中使用的不多, 因为在数字计算机中逆矩阵只能表现出有限的精度, 有效使用向量 `!$b$` 的算法能够获得更精确的 `!$x$` .

### (p) 1.1.4 线性相关(Linear Dependence)和生成子空间(Span)

矩阵方程组的解只有 无解, 一个解, 无穷多解 三种.

将矩阵 `!$A \in \mathbb{R}^{m \times n}$` 按列拆分为 `!$n$` 个列向量(被称为生成空间的 **原点(origin)** ), 则 `!$A x = \sum_i x_i A_{:,i}$`. 其中 `!$x_i$` 是标量. `!$ \sum_i x_i A_{:,i}$$` 就称为 **线性组合(linear combination)** . 一组向量的 **生成子空间(Span)** 就是通过不同的系数(`!$x$` 向量), 所能线性组合出来的所有矩阵(相对于生成空间中的其他所能到达的点)的集合. 这个生成子空间被称为 `!$A$` 的列空间(column space) 或 `!$A$` 的值域(range).

确定方程组是否有解就是确定 `!$b$` 是否在 `!$A$` 的生成子空间中.

为了使 `!$\forall b \in \mathbb{R}^m$`,  `!$Ax = b$` 都有解, 则 `!$A$` 的列(生成)空间构成整个 `!$\mathbb{R}^m$`. 所以 `!$n \ge m$` 是对每一点有解的必要条件(但不是充分条件). 例如 一个 `!$3 \times 2$` 的矩阵的列向量可以看作是一个二维向量, 而 `!$b$` 却是一个三维向量, 所以 `!$A$` 的列向量仅仅是 `!$\mathbb{R}^3$` 上的一个平面.

`!$n \ge m$` 仅仅是方程对每一点都有解的必要条件, 因为有可能会出现某些列重复的现象(所以要求所有列线性无关), 例如对于 一个 `!$2 \times 2$` 的矩阵, 但是这两列向量是等价的(或者是线性相关的), 则它的列空间是一条直线, 而不能包含整个 `!$\mathbb{R}$^2`. 如下图:

![两个列向量在同一直线上][7]

这种冗余的列向量被称为	**线性相关(linear dependence)**, 如果一组列向量中的任一向量都不能用其他向量的线性组合表示, 则称这组列向量 **线性无关(linear independent)**. 所以如果矩阵的列向量要包含整个 `!$\mathbb{R}^m$`, 则矩阵必须包含至少一组 `!$m$` 个线性无关的列向量(注意是至少一组 `!$m$` 个线性无关的向量而不是至少 `!$m$` 个线性无关的向量). 这是对方程 `!$Ax = b$` 中对于任意 `!$b$` 都有解的 **充要条件** .

又因为矩阵必须可逆, 所以矩阵最多只能有 `!$m$` 个列向量(因为矩阵多于 `!$m$` 列但是有 `!$m$` 个线性无关的列向量就会导致对于同一个 `!$b$` , 有无穷多解).

**综上, 矩阵必须是一个方阵(square), 即 `!$n = m$`, 并且所有列向量都线性无关.** 

> 一个列向量线性相关的方阵被称为 **奇异的(singular)** .

如果一个矩阵不是方阵或者是奇异的, 方程组仍然可能有解, 只不过不能使用逆矩阵的方法求解.

前面我们讨论的都是逆矩阵左乘, 同样我们可以定义逆矩阵右乘:

```mathjax!
$$AA^{-1} = I$$
```

对于方阵, 它的左逆(left-inverse)和右逆(right-inverse)是相同的.

### (p) 1.1.5 范数(Norms)

在机器学习中, 我们经常使用范数来度量向量的大小, `!$L^P$` 范数定义如下:

```mathjax!
$$\lVert x \rVert _p  = \left( \sum_i |x_i|^p \right) ^{\frac{1}{p}}$$
```
其中 `!$p \in \mathbb{R}, p \ge 1$`.

范数(包括 `!$L^P$` 范数) 是将向量映射到非负值的函数. 直观的来说, 一个向量 `!$x$` 的范数衡量从原点(origin)到点 `!$x$` 的距离. 更严格地说, 范数是满足以下条件的任意函数:

* `!$f(x) = 0 \Rightarrow x = 0$`
* `!$f(x + y) \le f(x) + f(y)$` (三角不等式, the triangle ineuqality)
* `!$\forall \alpha \in \mathbb{R}, f(\alpha x) = |\alpha| f(x)$`

`!$L^2$` 范数又称 **欧几里德范数(Euclidean Norm)**, 表示从原点到点 `!$x$` 的欧几里德距离, 因为使用频繁, 经常把 `!$\lVert x \rVert _2$`简写成 `!$\lVert x \rVert$` . 

> 有时候为了方便, 也会使用平方 `!$L^2$` 范数(也就是去根号的), 不过在靠近原点附近时, 平方 `!$L^2$` 范数因为增长缓慢不太好用来判断是否非 0, 所以这种情况下还会使用 `!$L^1$` 范数, i.e., `!$\lVert x \rVert _1 = \sum_i |x_i|$`

有时候我们会统计向量中非零个数, 有些作者错误得把它叫做 `!$L^0$` 范数. `!$L^1$` 范数经常用来替代表示非零元素数目的函数.

另一个经常在机器学习中使用的就是 **`!$L^{\infty}$` 范数**, 又被称为 **最大范数** , 用来表示向量中最大幅值的绝对值, `!$\lVert x \rVert _\infty = \max_i |x_i|$`

有时我们可能希望衡量矩阵的大小, 在深度学习中, 最常见的方法是使用 **Frobenius norm** :

```mathjax!
$$\lVert A \rVert _F = \sqrt{\sum_{i, j} A_{i, j}^2}$$
```
类似于向量的 `!$L^2$` 范数. 

向量的点乘可以写成范数的形式: `!$x \cdot y = x^\top y = \lVert x \rVert _2 \lVert y \rVert _2 \cos \theta$`.

### (p) 1.1.6 特殊类型的矩阵和向量

**对角矩阵(diagonal matrix)** `!$diag(v)$` 表示主对角线中元素由向量 `!$v$` 给定的对角方阵. 对角矩阵的运算很高效, 例如 `!$diag(v) x = v \bigodot x$`, `!$diag(v)^{-1} = diag([\frac{1}{v_1}, \frac{1}{v_2}, \cdots , \frac{1}{v_n}]^\top)$`.

> 不是所有的对角矩阵都是方阵, 非方阵的对角矩阵没有逆矩阵.

**对称矩阵(symmetric matrix)**

**单位向量(unit vector)** 是具有 **单位范数(unit norm)** 的向量: `!$\lVert x \rVert _2 = 1$`.

如果 `!$x ^\top y = 0$`, 那么向量 `!$x$` 和 `!$y$` **正交(orthogonal)** , 在 `!$\mathbb{R}^n$` 中, 最多有 `!$n$` 个非零向量互相正交. 如果这些向量不仅正交而且他们都是单位向量, 就叫做 **标准正交(orthonoomal)** .

**正交矩阵(orthogonal matrix)** 是行向量和列向量都相互标准正交的矩阵. `!$A^\top A = A A^\top = I$`, 也就是说 `!$A^{-1} = A^{\top}$` .

### (p) 1.1.7 特征分解(Eigendecomposition)

我们可以通过分解(decompose)矩阵来获得一些用数组表示矩阵不能直观的看出来的函数属性(funtional properties). 

特征分解是使用最广的一种矩阵分解方法, 将矩阵分解为一组 **特征向量(eigenvectors)** 和 **特征值(eigenvalues)** . 

**方阵 `!$A$`** 的特征向量是一个非零向量使得 `!$Av = \lambda v$`. 其中 `!$\lambda$` 是一个标量, 也就是特征向量 `!$v$` 对应的特征值.  

> 还可以使用左特征值向量(left eigenvector), `!$v^\top A = \lambda v^\top$`, 但是我们一般使用右特征向量(right eigenvector). 

如果 `!$v$` 是`!$A$` 的特征向量, 则对于 `!$sv, s \in \mathbb{R}, s \ne 0$`(s 是标量) 都是`!$A$` 的特征向量, 并且他们对应相同的特征值. 

如果 `!$A$` 有 `!$n$` 个线性无关的特征向量 `!$\left\{ v^{(1)}, v^{(2)}, \cdots, v^{(n)} \right\}$` , 和对应的特征值 `!$\left\{ \lambda_1, \lambda_2, \cdots, \lambda_n \right\}$`, 用特征向量按列组成矩阵 `!$V = [v^{(1)}, v^{(2)}, \cdots, v^{(n)}]$`, 用对应的特征值组成向量 `!$\lambda = [\lambda_1, \lambda_2, \cdots, \lambda_n ]$`. 则 `!$A$` 的 **特征分解** 可以记作

```mathjax!
$$A = V diag(\lambda) V^{-1}$$
```

> 不是所有的矩阵都能被分解为特征向量和特征值, 有时候分解是存在的不过是被分解成 **复数(complex)** 而不是实数. 在本书中通常只需要分解一些简单的矩阵.

所有的 **实对称矩阵(real symmetric matrix)** 都可以被分解成 **实特征向量(real-valued eigenvectors)** 和 **实特征值(real-valued eigenvalues)** .

`!$A = Q \Lambda Q^{\top}$`

其中 `!$Q$` 是由 `!$A$` 的特征向量组成的 **对阵矩阵** , `!$\Lambda$` 是对角矩阵, 并且 `!$\Lambda_{i, i}$` 是 特征向量 `!$Q_{:, i}$` 对应的特征值. 我们可以将 `!$A$` 看作是沿方向 `!$v^{(i)}$` 延展`!$\lambda_i$` 倍的空间.

![Figure 1.3][8]

> 虽然任意一个实对阵矩阵都可以特征分解, 但是特征分解可能不唯一, 如果多个特征向量共享相同的特征值, 则它们的生成子空间中任意正交的矩阵都可以作为特征向量.

按照惯例, `!$\Lambda$` 中的元素按照降序排列, 特征分解唯一当且仅当所有的特征值都唯一.

矩阵的特征分解提供了很多有用信息:

* 方阵是奇异的当且仅当它的特征值都是0.
* 实对称矩阵的特征分解可以用来优化二次方程 `!$f(x) = x^\top A x, \text{ for } \lVert x \rVert _2 = 1$`, 如果 `!$x$` 是 `!$A$` 的一个特征向量, 那么函数的值就是对应的特征值. `!$f$` 的最大和最小值分别为矩阵的特征值的最大值和最小值.
* 所有特征值都大于 0 的矩阵称为 **正定(positive definite)**, 所有特征值都是非负(也就是有可能为0)的矩阵称为 **半正定(positive semidefinite)**, 所有特征值都是负数的矩阵为 **负定(negative definite)**, 所有特征值都非正(也就是可能为0)的矩阵称为 **半负定(negative semidefinite)**.  对于半正定矩阵, `!$\forall x, x^\top A x \ge 0$`, 正定矩阵还额外保证 `!$x^\top A x = 0 \Rightarrow x = 0$`.

### (p) 1.1.8 奇异值分解(Singular Value Decomposition, *abbr.*, SVD)

除了上节提到的特征值分解, 奇异值分解提供了另外一种分解矩阵的方法, 将矩阵分解为 **奇异向量(singular vectors)** 和 **奇异值(singular vaules)**.

我们能通过奇异值分解获得与特征分解相同类型的信息, 不过奇异值分级应用的更加广泛, 每个实数矩阵(real matrix) 都有奇异值分解, 但是不一定有特征值分解, 例如如果一个矩阵不是方阵, 我们就只能使用奇异值分解而不能使用特征值分解.

使用奇异值可以将矩阵分解为:

```mathjax!
$$A = U D V^\top$$
```
如果 `!$A$` 是一个 `!$m \times n$` 矩阵, 则 `!$U$` 是 `!$m \times m$` 矩阵, `!$D$` 是 `!$m \times n$` 矩阵, `!$V$` 是 `!$n \times n$` 矩阵, 其中 `!$U$` 和 `!$V$` 都是正交矩阵(酉矩阵(unitary matrix)的特例), `!$D$` 是一个对角矩阵, 但不一定是方阵.

对角矩阵 `!$D$` 的对角线上的值被称为 `!$A$` 的 **奇异值(singular values)** , `!$U$` 和 `!$V$` 的列向量分别称为 **左奇异向量(left-singular vectors)** 和 **右奇异向量(right-singular vectors)**.

可以用 `!$A$` 相关的特征向量去解释奇异值分解:

* `!$A$` 的左奇异向量就是 `!$AA^\top$` 的特征向量
* `!$A$` 的右奇异向量就是 `!$A^\top A$` 的特征向量
* `!$A$` 的非零奇异值就是 `!$AA^\top$` 的特征值的平方根, 也就是 `!$A^\top A$` 的特征值的平方根.

**SVD 的最大的一个优点就是将矩阵求逆应用到非方阵上.**

### (p) 1.1.9 Moore-Penrose 伪逆 (The Moore-Penrose Pseudoinverse, aka 广义逆矩阵)

对于非方阵, 其逆矩阵没有定义.

我们希望对任意的矩阵 `!$A$` 求解方程 `!$Ax = y$`, 如果 `!$A$` 的行数大于列数, 则可能无解, 如果列数大于行数, 则可能有多解.

Moore-Penrose 伪逆是我们在这种问题上有了一定的进展:

```mathjax!
$$A^+ = \lim_{\alpha \searrow 0} (A^\top A + \alpha I)^{-1} A^\top$$
```
计算伪逆的实际算法没有使用上面这个定义, 而是使用下面的公式:

```mathjax!
$$A^+ = V D^+ U^\top$$
```
其中, `!$U, D, V$` 是 `!$A$` 的奇异值分解,  伪逆 `!$D^+$` 是对 `!$D$` 对角线上非零元素取倒数然后再转置而成的.

* 当 `!$A$` 的列数大于行数时, 使用伪逆求解线性方程是众多可能的解中的一个, 具体来说, `!$x = A^+ y$` 是方程所有可行解中欧几里德范数 `!$\lVert x \rVert _2$` 最小的一个
* 当 `!$A$` 的列数大于行数时, 可能没有解. 在这种情况下, 使用伪逆求得欧几里德范数 `!$\lVert Ax - y \rVert_2$` 最小(即 `!$Ax$` 到 `!$y$` 的距离最短的)的解.

### (p) 1.1.10 迹(Trace)运算

矩阵的迹:

```mathjax!
$$Tr(A) = \sum_i A_{i, i}$$
```
有些矩阵运算如果不使用求和公式, 会很难描述, 而通过矩阵乘法和迹运算能够很清楚的表示. 例如, 迹运算提供了另一种描述 Frobenius 范数的方式:

```mathjax!
$$\lVert A \rVert _F = \sqrt{Tr(AA^\top)}$$
```

在使用迹运算表示表达式时, 我们可以使用很多有用的等式巧妙得处理表达式. 例如, 迹运算在转置下是不变的: `!$Tr(A) = Tr(A^\top)$`

多个矩阵相乘形成得到的方阵的迹, 就算其中的矩阵位置交换, 也是同样成立的: `!$Tr(ABC) = Tr(CAB) = Tr(BCA)$`

而且显然有 `!$Tr(A - B) = Tr(A) - Tr(B)$`, 因为矩阵的加减运算就是对应的每个元素的算术运算.

更一般的可以表述成: `!$Tr\left( \prod_{i = 1}^n F^{(i)} \right) = Tr\left( F^{(n)} \prod_{i = 1}^{n - 1} F^{(i)} \right)$`

就算 **循环交换** 位置后相乘形成的方阵的形状变化了, 迹还是不变: `!$Tr(AB) = Tr(BA), \text{ for } A \in \mathbb{R}^{m \times n}, B \in \mathbb{R}^{n \times m}$`.

还有一个有用的事实就是对于标量 `!$a = Tr(a)$`.

### (p) 1.1.11 行列式(The Determinant)

方阵 `!$A$` 的行列式通常表示为 `!$\text{det}(A)$`, 是一个将方阵映射到实数的函数, 也就是说行列数是一个值. 并且方阵的行列式就是其特征值的乘积. 行列式的绝对值可以用来衡量矩阵参与矩阵乘法之后空间扩了或缩小了多少. 如果行列式为 0 , 则说明空间沿着至少一维完全收缩了, 使其丢失了所有体积(volume); 如果行列式为 1, 则说明这个转换保持体积不变(volume-preserving)

### (p) 1.1.12 实例: 主成份分析(Principal Components Analysis, *abbr.*, CPA)

假设在 `!$\mathbb{R}^n$` 空间中有 `!$m$` 个点 `!$\{ x^{(1)}, \cdots, x^{(m)}\}$` , 我们需要对它们进行有损压缩(用更少的内存空间但是(尽可能少得)损失精度得保存这些点). 

一种方式是降维(lower-dimensional), 对于每个 `!$x^{(i)} \in \mathbb{R}$`, 都存在一个对应的 **编码向量(corresponding code vector)** `!$c^{(i)} \in \mathbb{R}^l, l < n$` . 即有一个编码函数和一个解码函数使得 `!$f(x) = c, x \approx g(f(x))$` .

PCA 由我们选择的解码函数定义, 为了使解码简单. 我们使用矩阵乘法来将编码向量映射回原向量, `!$g(c) = Dc, D \in  \mathbb{R}^{n \times l}$`. 为了编码程序简单, PCA 限制 `!$D$` 的所有列都是互相正交的(不过 `!$D$` 仍然不是正交矩阵除非 `!$n = l$`). 但是仍然会有多种解, 因为如果我们放大 `!$D_{:, i}$`, 则对应的 `!$c_i$` 则会缩小, 为了使这个问题只有一个唯一解, 我们限制 `!$D$` 的每一列向量都有单位范数. 

为了明确如何根据输入点 `!$x$` 生成最优编码点(optimal code point) `!$c^*$`, 我们通过找到 `!$x$` 和 `!$g(c^*)$` 的最小(欧几里德)距离(为了简化计算, 我们使用平方 `!$L^2$` 范数):

```mathjax!
$$
\begin{equation}
\begin{split}
c^*& = \arg\min_c \lVert x - g(c) \rVert _2^2 \\
& =  \arg\min_c (x - g(c))^\top (x - g(c)) (\text{ by the definition of } L^2 \text{ norm.}) \\
& = \arg\min_c [ x^\top x - x^\top g(c)  - g(c)^\top x + g(c)^\top g(c) ] \text{ (distributive  property)} \\
& =  \arg\min_c [ x^\top x - 2 x^\top g(c) + g(c)^\top g(c) ] \text{ (`cause } x^\top g(c) \text{ is scalar, and its transpose is itself.)} \\
& =  \arg\min_c [ - 2 x^\top g(c) + g(c)^\top g(c) ] \text{ (omit } x^\top x \text{ because it does not depend on } c \text{ )} \\
& = \arg\min_c [ - 2 x^\top D c + c^\top D^\top Dc ] \text{ (substitude } g(c) = D c \text{ )} \\
& = \arg \min_c [ -2 x^\top D c + c^\top I_l c ] \text{ ( } D \text{ is orthogonal matrix )} \\
& = \arg \min_c [ -2 x^\top D c + c^\top c ] \\
\end{split}
\nonumber
\end{equation}
$$
```
通过 **向量微积分(vector calculus, see Section 1.3.3)** 求解这个最优化问题: 

```mathjax!
$$\nabla_c ( -2x^\top D c + c^\top c) = 0$$
$$ -2 D^\top x + 2c = 0$$
$$ c = D^\top x$$
```

所以编码函数就是: `!$f(x) = D^\top x$` (这是一个很高效的算法, 因为我们只需要进行一个矩阵向量运算) . 而 PCA 解码函数就是 `!$r(x) = g(f(x)) = DD^\top x$`.

同样的, 我们通过 **Frobenius 范数** (因为我们要用这一个 `!$D$` 来编码所有的点(向量))来衡量所有维数和所有点上的误差矩阵的大小: 

```mathjax!
$$D^* = \arg \min_D \sqrt{ \sum_{i,j} \left( x_j^{i} - r(x^{(i)})_j \right)}, D^\top D = I_l$$
```

> `!$x^{(i)}$` 是向量, `!$x_j$` 是向量中的一个元素

> `!$ D^\top D = I_l$` 是因为 `!$D$` 的列向量之间互相标准正交.

首先我们考虑 `!$l = 1$` 的情况, 把 `!$D$` 置换为 `!$d$`, 可以将上式简化为平方 `!$L^2$` 范数:

```mathjax!
$$d^* = \arg \min_d  \sum_i  \lVert x^{(i)} - dd^\top x^{(i)} \rVert_2^2 \text{ , } \lVert d \rVert _2 = 1$$
```
又因为 `!$d^\top x^{(i)}$` 是一个标量并且标量的转置就是他本身( **这种调整位置的技巧是很有用的** )

```mathjax!
$$d^* = \arg \min_d \sum_i \lVert x^{(i)} - x^{(i) \top} d d \rVert_2^2  \text{ , } \lVert d \rVert _2 = 1$$
```

对于所有的 `!$x^{(i)}$`, 我们可以按列把它们堆叠在一起作为矩阵进行运算, 这样能紧凑符号. 令 `!$X \in \mathbb{R}^{m \times n} \text{, as for } X_{i, :} = x^{(i) \top}$` , 我们可能将上式表述为** 平方Frobenius 范数**形式:

```mathjax!
$$d^* = \arg \min_d \lVert X - Xdd^\top \rVert _F^2 \text{, } d^{\top} d = 1$$
```

> 其中 `!$Xdd^\top$` 中的 `!$d^\top$` 是因为原来的 `!$x^{(i)}$` 都作为了 `!$X$` 的列向量, 原来后面的 `!$d$` 是将 `!$x^{(i)}d$` 转化成向量(i.e., 相当于`!$\mathbb{R}^{n \times 1}$`),  而这里的 `!$d^\top$` 是将其转化为 `!$\mathbb{R}^{1 \times n}$` 作为矩阵的列向量.

```mathjax!
$$
\begin{equation}
\begin{split}
\arg \min_d  \lVert X - Xdd^\top \rVert _F^2 &= \arg \min_d Tr\left(\left( X - Xdd^\top \right)^\top \left( X - X dd^\top \right) \right)\\
& = \arg \min_d [Tr(X^\top X ) - Tr(X^\top X d d^\top) - Tr(dd^\top X^\top X) + Tr(dd^\top X^\top X d d^\top)] \\
& = \arg \min_d [  - Tr(X^\top X d d^\top) - Tr(dd^\top X^\top X) + Tr(dd^\top X^\top X d d^\top) ] \text{ , remove terms not involving } d\\
& = \arg \min_d [ -2 Tr(X^\top X d d^\top ) +  Tr(X^\top X d d^\top dd^\top ) ] \text{, cycling the order of martices inside trace has same trace }\\
& = \arg \min_d [ -2 Tr(X^\top X d d^\top) + Tr(X^\top X d d^\top)] \text{, } d^\top d = 1\\
& = \arg \min -Tr(X^\top X d d^\top) \\
& = \arg \max  Tr( d^\top X^\top X d) 
\end{split}
\nonumber
\end{equation}
$$
```

**所以, `!$d$` 就是 `!$X^\top X$` 对应的最大的特征值对应的特征向量, 当 `!$l >1$` 时, `!$D$` 由 `!$X^\top X$` 前 `!$l$` 大的特征值对应的特征向量构成(可以用数学归纳法证明, 可作为练习).**

### (p) 1.1.13 矩阵微分

#### TODO

## (p) 1.2 概率论和信息论(Probability and Infomation Theory)

> 概率论在人工智能上的应用主要是教我们如何设计算法用来计算或者估算概率论导出的表达式, 还有就是用概率论知识还理论化分析 AI 系统的行为.

> 概率论能够使我们作出 **不确定声明(uncertainty statement)** 以及在不确定性存在情况下的推理, 而信息论使我们能够量化在概率分布中的不确定总量. 

> 推荐阅读 **Jaynes(2003)**

### (p) 1.2.1

不确定性的三大来源:

* 被建模系统的 **内在随机性(inherent stochasticity)**
* 不完整的观察(incomplete observability)
* 不完整的建模

很多情况下, 使用简单但是不确定的规则比复杂而确定(deterministic)的规则更加实用.

早期的概率论是用来分析事件的频率, 并且这类事件都是可以(趋近于无限)重复发生的. 而如果是一个医生给一个病人看病并且告诉病人他有 40% 的概率患流感, 因为病人不能被复制, 所以这不是一个可重复事件. 在这种情况, 我们使用概率来表示 **信念度(degree of belief)**, 其中 1 表示病人肯定患有流感, 0 表示病人肯定没有患流感, 前者直接与事件发生的比率(rate)相关, 被称为 **频率派概率(frequentist probability)**, 而后者涉及到 **确定度水平(qualitative levels of certainty)**, 被称为 **贝叶斯概率(Bayesian probability)**. 如果要列出一些关于不确定性(uncertainty)的常识推理(common sense reasons)中我们希望要有的性质(property), 要满足这些性质的唯一方法就是将贝叶斯概率的行为和概率派概率完全等同. 

概率论提供了一套形式化的规则, 用来在给定一些命题(proposition)的似然(likelihood)后, 计算其他命题为真的似然.

### (p) 1.2.2 随机变量(Random Variables)

**随机变量** 就是能够随机的取不同值的变量. 随机变量可以使 **离散的(discrete)** 或者是 **连续的(continuous)**, 离散随机变量有有限或可数的无限个状态(states, 不一定是数值), 而连续随机变量与实数值相关.

> 随机变量和可能的取值都用小写无格式字母表示

### (p) 1.2.3 概率分布(Probability Distributions)

**概率分布** 描述变量的值怎么样从它们的各种状态中选取(具体的方法取决于是离散随机变量还是连续随机变量).

离散随机变量的概率分布(probability distribution)被称为 **概率质量函数(probability mass function, *abbr.*, PML, 有些国内教材翻译为概率分布律)**, 一般用大写字母 *P* 表示, 一般用变量的标识来区分不同的概率质量函数而不是通过函数名称来区分, 例如 *P*(x) 和 *P*(y) 一般来说就表示不同的概率质量函数. 还可以简记作 x ~ *P*(x) , 对于 x 的某个状态值 `!$x_1$`, 我们可以用 *P*(`!$x_1$`) 或 *P*(x = `!$x_1$`) 表示这个状态的概率值. 对于有多个随机变量的概率分布 , 叫做 **联合概率分布(joint probability distribution)**, 例如 *P*(x = *x*, y = *y*) 或 *P*(*x*, *y*) (或记作 `!$P(x \cap y)$`或 `!$P(xy)$`).

关于 x 的PMF *P*(x) 必须满足以下几个条件:

* *P* 的定义域必须包含 x 的所有可能的状态
* `!$\forall x_i \in x,\ 0 \le P(x_i) \le 1$`
* `!$\sum_{x_i \in x} P(x_i) = 1$` 我们把这一条性质称为 **归一化(nornalized)** 的

连续随机变量的概率分布被称为 **概率密度函数(probability density function, *abbr.*, PDF)**, 如果函数是 *p* 关于 x 的PDF, 则必须满足以下三个条件:

* *p*的定义域是 x 所有状态的集合.
* `!$\forall x \in \mathtt{x}, p(x) \ge 0$`, 注意这里不要求 `!$p(x) \le 0$`
* `!$\int{p(x)}\mathrm{d}x = 1$`

PDF 没有直接给定某个状态的概率, 而是给出了落在面积为 `!$\sigma x$` 的无限小(infinitesimal)区域的概率是 `!$p(x) \sigma x$`. 我们可以通过对 PDF 求积分(integrate)来获得点集的概率质量. 对于单变量(univariate) PDF , x 在区间 \[a, b] 的概率为 `!$\int_{[a, b]} p(x) \mathrm{d} x$`.

例如概率在实数区间 \[*a*, *b*] ( *a* < *b*) 上均匀分布(uniform distribution), 考虑函数 *u*(*x*; *a*, *b*), ";" 表示以什么作为函数(parametrized by),  *x* 是函数的自变量(argument), 而 *a* 和 *b* 是函数的参数(parameters), 并且对于所有 `!$x \notin [a, b], u(x; a, b) = 0$`, 而对于所有 `!$x \in [a, b], u(x; a, b) = \frac{1}{b - a}$`, 可记作 x ~ *U*(a, b). 

### (p) 1.2.4 边缘概率(Marginal Probability)

有时候我们想求的一组变量中的一个子集的(联合)概率分布, 例如对于离散型概率分布 *P*(x, y), 我们应用求和法则(sum rule) `!$\forall x_i \in x, P(x = x_i) = \sum_{y_j} P(x = x_i, y = y_j)$`, 而对于连续型概率分布 `!$p(x) = \int p(x, y) \mathrm{d} y$`.

### (p) 1.2.5 条件概率(Conditional Probability)

条件概率是某个时间在给定其他事件发生时出现的概率:

```mathjax!
$$P(y = y_i | x = x_j) = \frac{P(y = y_i, x = x_j)} {P(x = x_j)}$$
```
> 注意区分条件概率和在从事某个动作之后会发生什么(这被称为 **干预查询(intervention query, 属于因果模型(cause modeling)的范畴, 本书不介绍)** )

### (p) 1.2.6 条件概率的链式法则(chain rule, *aka.*, 乘性法则(product rule))

任何多随机变量联合概率分布都可以变成只有一个随机变量的条件概率分布:

```mathjax!
$$P(x^{(1)}, \cdots, x^{n}) = P(x^{(1)}) \prod_{i = 2}^n P(x^{(i)} | x^{(1)}, \cdots, x^{(i - 1)})$$
```
### (p) 1.2.7 独立性(independence) 和条件独立性(conditional independence)

如果 `!$\forall x \in \mathtt{x}, y \in \mathtt{y}, p(\mathtt{x} = x, \mathtt{y} = y) = p(\mathtt{x} = x) \times p(\mathtt {y} = y)$`, 则称随机变量 x 和随机变量 y 是 **相互独立的(independent)**, **简记作 x `!$\perp$` y** .

如果 `!$\forall x \in \mathtt{x}, y \in \mathtt{y}, z \in \mathtt{z}, p(\mathtt{x} = x, \mathtt{y} = y | \mathtt{z} = z) = p(\mathtt{x} = x | \mathtt{z} = z) \times p(\mathtt{y} = y | \mathtt{z} = z)$`, 则称 x 和 y 是 **条件独立的** , **简记作 x `!$\perp$` y | z** .

### (p) 1.2.8 期望(Expectation), 方差(Variance) 和 协方差(Covariance)

函数 `!$f(x)$` 对于概率分布 `!$P(x)$` 的 **期望(expectation)** 或 **期望值(expected value)** 是指, 当 x 由 *P* 产生, *f* 作用于 *x* 时, `!$f(x)$` 的平均值. 

对于离散型随机变量:

```mathjax!	
$$\mathbb{E}_{x ~ P} [f(x)] = \sum_x P(x) f(x)$$
```

对于连续型随机变量:

```mathjax!
$$\mathbb{E}_{x ~ p} [f(x)] = \int_x p(x) f(x)$$
```
 
> 我们可以将   `!$mathbb{E}_{x ~ P}$` 简写为 `!$\mathbb{E}_x[f(x)]$` 或 `!$\mathbb{E}[f(x)]$`, 并且我们默认方括号是对 **所有随机变量** 的平均, 无歧义的时候可以省略方括号.

期望是线性的: `!$\mathbb{E}_x [ \alpha f(x) + \beta g(x)] = \alpha \mathbb{E}_x[f(x)] + \beta \mathbb{E}_x [f(x)]$`.

**方差(variance)** 衡量关于 x 的函数的值和我们对于 x 按照它的概率分布进行采样之间有多大的差距:

```mathjax!
$$\mathtt{Var} (f(x)) = \mathbb{E} [(f(x) - \mathbb{E}(f(x)))^2]$$
```
经过简单的变换(记得把内层嵌套的数学期望当做常数转移到外层数学期望外面), 反差还可以表示为

```mathjax!
$$
\mathtt{Var} (f(x)) = \mathbb{E} [ f(x)^2 ] - \mathbb{E}^2 [ f(x) ]
$$
```

方差的平方根就是 **标准差(standard deviation)** .

**协方差(covariance)** 从某种意义上给出了两个变量线性相关性的强度和这些变量的尺度(scale):

```mathjax!
$$\mathtt{Cov}(f(x), g(y)) = \mathbb{E}[(f(x) - \mathbb{E}[f(x)]) (g(y) - \mathbb{E}[g(y)])]$$
```

协方差的绝对值很大意味着变量值变化很大并且和距离他们各自的均值(期望值)很远. 如果协方差为正, 那么这两个变量同时都倾向于取到相对较大的值. 如果协方差为正, 那么其中有一个变量倾向于取得一个相对较大的值而另外一个变量倾向于取得一个相对较小的值. 其他的衡量方法例如 **相关性(correlation)** 将每一个变量的贡献归一化(normize) , 为了只衡量变量的相关性而不受各个变量尺度大小的影响.

如果两个变量的协方差不为 0 那么两个变量肯定是相关的, 如果两个变量的协方差为 0 , 则它们一定没有线性关系, 但是它们仍然可能有相关性. 例如从区间 \[-1, 1] 上的均匀分布中采样出一个实数 x , 然后对随机变量 s 进行采样, s以 0.5 的概率为 1, 否则为 -1 , 我们可以令 y = sx 来生成一个随机变量, 显然 y 和 x 相关, 但是 Cov(x, y) = 0 .

随机向量 `!$x \in \mathbb{R}^n$` 的 **协方差矩阵(covariance matrix)** 是一个 `!$n \times n$` 矩阵, 并且 `!$\mathtt{Cov} (\mathtt{x})_{i, j} = \mathtt{Cov}(x_{i}, x_{j})$`, 协方差矩阵的对角线元素就是方差: `!$\mathtt{Cov}(x_i, x_i) = \mathtt{Var}(x_i)$` .

### (p) 1.2.9 常见概率分布

**Bernoulli 分布**

Bernoulli 分布是单个二值随机变量(binary random variable, 也就是只有两个状态的随机变量, 在这里, 就是 0 和 1 )的分布, 由一个参数 `!$\phi \in [0, 1]$` 控制, 并且 `!$\phi$` 表示随机变量值为 1 的概率.

```mathjax!
$$ P(\mathtt{x} = 1) = \phi$$
$$P(\mathtt{x} = 0) = 1 - \phi$$
$$P(\mathtt{x} = x) = \phi^x (1 - \phi)^{1- x}$$
$$\mathbb{E}_\mathtt{x}(\mathtt{x}) = \phi$$
$$\mathtt{Var}_\mathtt{x} (\mathtt{x}) = \phi (1 - \phi)$$
```

**Multinoulli 分布**

> Multinoulli 分布是 **多项式分布(multinormial distribution)** 的一个特殊情况, 很多书上直接使用 **多项式分布** 来指代 **Multinoulli 分布** .

又称 **范畴分布(categorical distribution)**, 是 有 k (有限的) 个不同状态的单值离散变量(single discrete variable)的分布.  由参数 向量 `!$p \in [0, 1]^{k - 1}$` 控制, `!$p_i$` 表示第 i 个状态的概率, 而最后一个(也就是 第 k 个)状态的概率是 `!$1 - \boldsymbol{1}^\top p$`, 并且限制 `!$\boldsymbol{1}^\top p \le 1$` .

> Multiboulli 分布经常用来表示对象分类的分布, 所以我们一般不会假定第一个状态的数值为 1 之类的(也就是状态不一定有数值), 所以我们也就一般不会去求它们的期望或者方差.

Bernoulli 分布和 Mulinoulli 分布都足以用来描述他们领域内的任意分布, 这是因为它们是对那些能够很容易的枚举(enumerate)所有状态的离散型随机变量, 如果是处理连续型随机变量, 因为它们的状态都是不可数的(无限的), 所以像这种只有很少数量的参数的分布函数在处理连续型随机变量的时候必须对随机变量的分布加以严格的限制.

**高斯分布(Gaussian distribution)**

实数上最常用的分布就是 **正态分布(normal distribution)**, 或称 **高斯分布(Gaussian distribution)** .

```mathjax!
$$\mathcal{N} (x; \mu, \sigma^2) = \sqrt{\frac{1}{2 \pi \sigma^2}} \exp{\left( - \frac{1} {2 \sigma^2} (x - \mu)^2 \right)}$$
```
> 参数 `!$\mu \in \mathbb{R}$` 控制中间峰值的坐标, 并且是分布的均值 `!$\mathbb{E}[\mathtt{x}] = \mu$`, `$\sigma \in (0, \infty)$`, 标准差为 `!$\sigma$`, 方差为 `!$\sigma^2$` .

因为我们对正态分布函数求值的时候, 需要对 `!$\sigma$` 平方然后求倒数, 所以我们可以简化为用 `!$\beta^{-1}$` 代替 `!$\sigma^2$`, `!$\beta$` 控制精度(precision)或者说分布的方差的倒数:

```mathjax!
$$\mathcal{N}(x; \mu, \beta^{-1}) = \sqrt{\frac{\beta}{2\pi}} \exp \left( -\frac{1} {2} \beta (x - \mu)^2 \right)$$
```
正态分布可以推广到 `!$\mathbb{R}^n$` 空间, 被称为 **多维正态分布(multivariate normal distribution)** :

```mathjax!
$$\mathcal{N} (\boldsymbol{x}; \boldsymbol{\mu}, \boldsymbol{\Sigma}) = \sqrt{\frac{1}{(2 \pi)^n \det (\boldsymbol\Sigma)}} \exp \left( - \frac{1} {2} (\boldsymbol{x} - \boldsymbol{\mu})^\top \boldsymbol{\Sigma}^{-1}(\boldsymbol{x} - \boldsymbol{\mu}) \right)$$
```
> 参数是 `!$\boldsymbol{\Sigma}$` 一个 **正定对称矩阵(positive definite symmetric matrix)**, 是分布的协方差矩阵; 参数 `!$\boldsymbol{\mu}$` 依然是分布的均值, 不过现在是一个向量.

同样的, 对于单一变量情况, 我们可以使用 **精度矩阵(precision matrix) `!$\boldsymbol{\beta}$`** 代替 `!$\boldsymbol{\Sigma}$`:

```mathjax!
$$\mathcal{N} (\boldsymbol{x}; \boldsymbol{\mu}, \boldsymbol{\beta}^{-1}) = \sqrt{\frac{\det \boldsymbol{\beta} } {(2 \pi)^n}} \exp \left( - \frac{1} {2} (\boldsymbol{x} - \boldsymbol{\mu})^\top \boldsymbol{\beta} (\boldsymbol{x} - \boldsymbol{\mu}) \right)$$
```
我们经常把协方差矩阵固定为一个对角矩阵(diagonal matrix). 一个更简单的版本就是 **各向同性(isotropic)高斯分布**, 它的协方差是一个标量(scalar)乘以一个单位阵(identity matrix).

**指数(Exponential)分布和 Laplace 分布**

在深度学习中, 我们经常需要一个在 `!$x = 0$` 出取得边界点(sharp point)的分布 -- 指数分布: `!$p(x; \lambda) = \lambda \boldsymbol{1}_{x \ge 0} \exp (-\lambda x)$`

指数分布使用函数 `!$\boldsymbol{1}_{x \ge 0}$` 来指示所有 `!$x < 0$` 的情况的概率为 0. 

一个相关的能够允许我们自定义概率质量的边界点在任意位置的分布为 **Laplace distribution**: `!$\mathtt{Laplace}(x; \mu, \gamma) = \frac{1} {2 \gamma} \exp (-\frac{|x-\mu|}{\gamma})$`

**Dirac 分布和经验(Empirical)分布**

有时候我们想把所有概率分布中的质量向某一个点靠拢, 我们可以使用 **Dirac delta 函数**: `!$p(x) = \delta (x - \mu)$` , 其中 `!$\delta$` 函数当参数不为0时值都为 0, 但是它的积分是 1, `!$\delta$` 函数不像普通函数一样,每一个 `!$x$` 都有一个对应的数组值输出, 它是一个 **广义函数(generalized function, 按照积分性质定义的对象)**, 我们可以将 Dirac delta 函数理解为一系列在 `!$\mu$` 之外的值越来越小的函数的极限点.

所以 Dirc 分布就是一个在 `!$\mu$` 处有无限窄又无限高的峰值的分布函数.

Dirac 分布经常作为 **经验分布(empirical distribution)** 的一个组成部分: `!$\hat{p}(\boldsymbol{x}) = \frac {1} {m} \sum_{i=1}^{m} \delta (\boldsymbol{x} - \boldsymbol{x}^{(i)})$` 将概率质量 `!$\frac{1} {m}$` 放在 m 个点 `!$\boldsymbol{x}^{(1)}, \cdots, \boldsymbol{x}^{(m)}$` 上. 

> Dirac delta 函数只是在经验分布作用在连续型随机变量的时候需要用到, 对于离散型随机变量, 经验分布可以被定义成一个 Multinoulli 分布, 并且每个输出值的概率就是该值在训练集合(training set)中的 **经验频率(empirical frequency)** .

当我们在训练集上训练模型时, 我们可以认为从这个训练集上得到的经验分布指明了我们采样来源的分布, 还有就是它是训练数据的似然最大的那个概率密度函数.

**混合分布(Mixtures of Distribution)**

混合分布由一些组件(component)分布构成, 每次试验, 样本是由哪个组件产生的取决于从一个 Multinoulli 分布中采样的结果: `!$P(x) = \sum_i P(c = i) P(x | c = i)$`, 其中 `!$P(c)$` 是对各组件的一个 Multinoulli 分布.

前面的实数上的经验分布就是一个混合分布. 混合模型的思想将会在后面的学习中用到, 例如这里的 `!$c$` 就相当于一个 **潜变量(latent variable, See Chapter 16)** .

一个强大且常见的就是 **高斯混合模型**, 它的组件 `!$p(\mathtt{x} | c = i)$` 是高斯分布, 每一个组件都有他们单独的均值参数 `!$\boldsymbol{\mu}^{(i)}$` 和单独的协方差参数 `!$\boldsymbol{\Sigma}^{(i)}$`, 为了方便, 可以添加约束, 组件之间可以共享同一个协方差 `!$\boldsymbol{\Sigma}^{(i)} =  \boldsymbol{\Sigma} \forall i$`, 而且跟单独的高斯分布一样, 可以限制每一个组件的协方差矩阵是对角的(diagonal)或者是各向同性的(isotropic). 

除了协方差和均值之外, 高斯混合每个组件`!$i$` 的参数还指定了 **先验概率(prior probability)** `!$\alpha_i = P(c = i)$` , "先验(prior)" 一词表明了在观察到 `!$\mathtt{x}$` 之前传递给模型关于 `!$c$` 的信念(belief), 作为对比 `!$P(c | \boldsymbol{x})$` 是 **后验概率(posterior probability)** 因为它是在观察到 `!$\mathtt{x}$` 之后才计算. 高斯混合模型是概率密度的 **万能近似器(universal approximator)**, 任何平滑的概率密度都可以用具有足够多组件的高斯混合模型以任意精度去逼近(还是有非零的误差).

### (p) 1.2.10 常用函数的一些有用性质(property)

**logistic sigmod 函数**

```mathjax!
$$\sigma (x) = \frac{1} {1 + \exp (-x)}$$
```

logistic sigmod 函数经常用来生成 Bernoulli 分布中的参数 `!$\phi$`, 因为它的值域是 `!$(0, 1)$` .

![Figure 3.3][9]

> 当自变量的绝对值特别大的时候, 函数值趋于 **饱和(saturate)**, 也就是对输入值的变化变得不敏感.

**softplus 函数**

```mathjax!
$$\zeta (x) = \log(1 + \exp (x))$$
```

softplus 函数经常用来生成正态分布的 `!$\beta$` 或 `!$\sigma$`, 因为它的值域是 `!$(0, \infty)$`.  softplus 名字的由来是它差不多就是一个 **softened** 版本的 `!$x^+ = \max (0, x)$` (**正部函数(positive part function))** .

![Figure 3.4][10]

以下为一些有用的最好记住的函数性质:

```mathjax!
$$
\begin{equation}
\sigma (x) = \frac{\exp (x)} {\exp (x) + \exp (0)}
\end{equation}
$$
$$
\begin{equation}
\frac {\mathrm{d}} {\mathrm{d}x} \sigma (x) = \sigma(x) (1 - \sigma(x))
\end{equation}
$$
$$
\begin{equation}
1 - \sigma(x) = \sigma(-x)
\end{equation}
$$
$$
\begin{equation}
\log \sigma(x) = - \zeta(-x)
\end{equation}
$$
$$
\begin{equation}
\frac{\mathrm{d}}{\mathrm{d}x} \zeta(x) = \sigma(x)
\end{equation}
$$
$$
\begin{equation}
\forall x \in (0,1), \sigma^{-1}(x) = \log (\frac{x}{1-x})
\end{equation}
$$
$$
\begin{equation}
\forall x > 0, \zeta^{-1}(x) = \log (\exp (x) - 1)
\end{equation}
$$
$$
\begin{equation}
\zeta(x) = \int_{-\infty}^{x} \sigma(y)\mathrm(d)y
\end{equation}
$$
$$
\begin{equation}
\zeta(x) - \zeta(-x) = x
\end{equation}
$$
```

> 函数 `!$\sigma^{-1}(x)$` 在统计学中被称为 **分对数(logit)**, 但是在机器学习中很少用到.

> 最后一条性质为函数名提供了正当理由, 因为就像正部函数(`!$x^+$`) 和 负部函数 `!$x^-$` 之间的关系 `!$x^+ - x^- = x$`

### (p) 1.2.11 贝叶斯规则(Bayes' Rule)

```mathjax!
$$P(x | y) = \frac{P(x, y)}{P(y)} = \frac{P(x)P(y | x)}{P(y)}$$
```

> 可以用  `!$P(y) = \sum_{x_i} P(y | x_i) P(x_i)$` 来求的 `!$P(y)$`, 所以我们并不需要事先知道 `!$P(y)$` 的信息.

> 贝叶斯规则可以由条件概率的定义直接推导出来

### (p) 1.2.12 连续型变量的技术细节

> 对连续型变量及其 PDF 需要用数学的一个分支 -- **测度论(measure theory)** 来扩展概率论. 测度论超出了本书的范围, 不过这里可以简要的说明一下测度论要解决的问题.

我们知道连续型向量值(随机变量) `!$\mathtt{x}$` 落在某个集合 `!$\mathbb{S}$` 中的概率是 `!$p(x)$` 对集合 `!$\mathbb{S}$` 积分(integral) 得到的. 但是某些 `!$\mathbb{S}$` 的选择可能会引起悖论(paradox). 例如, 构造两个集合 `!$\mathbb{S}_1$` 和 `!$\mathbb{S}_2$` 使得 `!$p(x \in \mathbb{S}_1) + p(x \in \mathbb{S}_2) > 1 \text{ , and } \mathbb{S}_1 \cap \mathbb{S}_2 = \emptyset$` 是有可能的. 这样的集合通常是使用了大量的实数的无限精度(infinite precision)来构造的, 例如构造 **分形集合(fractal-shaped sets)** 或通过由由理数(rational numbers)构成的集合的变换(transform)来定义的集合(Banach-Tarski 定理(theorem)给出了这类集合的一个有趣的例子). 测度论的一个重要贡献就是提供了一些集合的特征使得我们在计算概率的时候不会遇到悖论. 在本书中, 我们只会对一些简单的集合进行积分, 所以测度论相关知识不会被考虑.

测度论对描述那些在 `!$\mathbb{R}^n$` 上大部分点适用而在少数几个边界情况(corner case)下不适用的定理很有用. 测度论为描述那些微小(negligibly small)的点集(set of points)提供了严格(rigorous)的方式, 这样的集合被称为 **零测度(measure zero)**. 我们可以认为零测度集在我们的度量空间中不占用空间, 例如在 `!$\mathbb{R}^2$` 空间中, 一条线有零测度, 被填充的多边形就是有正测度(positive measure). 有限个数的零测度集的并仍然是零测度的(所以所有的有理数构成的集合测度为零).

另一个在测度论中的术语是 **几乎处处(almost everywhere)**, 一个几乎处处成立的性质(property)在除了一个零测度集之外的空间上成立, 因为这些例外占有微小(negligibly amount)的空间, 所以它们在很多应用上都可以被忽略. 许多在所有离散值上成立的概率论结果对于连续型值来说几乎处处成立.

连续型变量的另外一个技术细节涉及到处理那些互相之间有确定(deterministic)函数关系的连续型随机变量. 假定有两个连续型随机变量 `!$\mathtt{x}$` 和 `!$\mathtt{y}$`, 并且有 `!$y = g(x)$`, `!$g$` 是一个可逆的, 连续可微(continous, differentiable)的函数(transformation). 但是 `!$p_y(y) = p_x(g^{-1}(y))$` 是错误的. 例如, 标量随机变量 x 和 y, 并且有 `!$y = \frac{x} {2}, x \sim U(0,1) $` . 假设 `!$p_y(y) = p_x(2y)$`, 也就是 `!$p_y(y)$` 在除了 `!$[0, \frac{1}{2}]$` 之外都是 0, 也就是 `!$\int p_y(y) \mathrm{d}y = \frac{1} {2}$`, 这跟概率分布的定义相矛盾. 这是因为没有考虑由 `!$g$` 造成的空间变形(distortion), 回忆落在一个无穷小面积 `!$\delta x$` 上的概率为 `!$p(x) \delta x$`, 如果 `!$g$` 能扩展(expand)或收缩(contract)空间, 在 `!$x$` 空间内的包围这 `!$x$` 的无穷小体积(volume)在 `!$y$` 空间中可能会有不同的体积. 为了纠正这一问题, 我们需要保持性质 `!$|p_y(g(x))\mathrm{d}y| = |p_x(x)\mathrm{d}x|$`, 所以有:

```mathjax!
$$p_y(y) = p_x(g^{-1}(y)) \left |\frac{\partial x} {\partial y} \right|$$
$$p_x(x) = p_y(g(x)) \left| \frac {\partial g(x)} {\partial x} \right|$$
```

对于更高维度, 微分运算(derivative)扩展为 **Jacobian 矩阵(`!$J_{i,j} = \frac {\partial x_i} {\partial y_i}$`)** 的行列数(determinant), 所以对于实数值向量 `!$x$` 和 `!$y$`,  有

```mathjax!
$$p_x(x) = p_y(g(x)) \left| \det \left( \frac{\partial g(x)}{\partial x} \right) \right|$$
```

### (p) 1.2.13 信息论 (Information Theory)

> 推荐阅读: Cover and Thomas(2006) 或 MacKay(2003)

信息论是应用数学(Applied Math)的一个分支, 主要是用来量化(quantify) **信号(signal)** 中包含的信息. 信息论最开始是用于在包含造成噪声(noisy)的信道(channel)中发送信息, 例如信息论告诉我们如何设计最优编码(optimal code), 计算从使用多种编码方案的特定概率分布取样的消息的期望长度. 在机器学习中, 我们可以把信息论应用在连续型变量上. 在本书中, 我们只涉及到少数几个信息论的概念, 用于描述(characterize)概率分布和量化概率分布之间的相似度(similiarity). 

信息论的基本想法是一个不太可能发生的事件的发生能够比一个很有可能发生的事件的发生要提供更多的信息. 例如说消息"今天早上太阳升起"消息少得没必要发送, 而"今天早上有日食"信息量就很丰富.

* 很可能发生的事件的信息量(information content)比较少, 在极端情况下, 必须发生的事件没有任何信息量.
* 较不可能发生的事件有更多的信息量. 
* 独立事件具有增量(additive)的信息, 例如抛硬币两次正面朝上的信息量应该是一次正面朝上的信息量的两倍.

为了满足上述三个性质, 我们定义一个事件 `!$\mathtt{x} = x$` 的 **自信息(self-information)** 为 `!$I(x) = - \log P(x)$` .

因为 `!$I(x)$` 使用的是自然对数, 所以定义 `!$I(x)$` 的单位是 **奈特(nats)**, 一奈特是以 `!$\frac{1} {e}$` 的概率观察到某事件的信息量. 其他使用以 2 为底的对数的单位是 **比特(bit)** 或 **香农(shannous)** , 通过比特度量的信息只是通过奈特度量的信息的常数倍(rescaling). 

当 x 是连续的, 我们使用类似的差不多的定义, 不过一些源自离散形式的性质就丢失了. 例如单元密度(unit density)的事件信息量依然为 0, 但是不能保证它一定发生.

自信息只处理单个的输出, 我们可以用 **香农墒(shannon entropy)** 来量化整个概率分布的总的不确定性:

```mathjax!
$$H(x) = \mathbb{E}_{x \sim P} [I(x)] = -\mathbb{E}_{x \sim P} [\log P(x)]$$
```

> 也可记作 `!$H(P)$`

一个分布的香农墒就是遵循该分布的事件的期望信息总量. 它给出了编码一个分布上的符号(symbol)所需要的平均单元数(units, 例如基于以 2 为底的对数时就是比特数)的下界(lower bound). 接近确定性的分布具有较低的墒, 接近均匀分布的概率分布具有较高的墒. 当 x 是连续的, 香农墒被称为 **微分墒(differential entropy)** . 下图给出了二值随机变量分布的香农墒. 


![Figure 3.5][11]

对于同一随机变量的两个相互独立的概率分布 `!$P(x)$` 和 `!$Q(x)$`, 我们可以使用 **KL散度(Kullback-Leibler divergence)**:

```mathjax!
$$D_{KL}(P \Vert Q) = \mathbb{E}_{x \sim P} \left[ \log \frac{P(x)}{Q(x)} \right] = \mathbb{E}_{x \sim P}[\log P(x) - \log Q(x)]$$
```
在离散型随机变量的情况下, KL 散度衡量的是, 当我们使用一种被设计成能够使得概率分布`!$Q$` 产生的消息的长度最小的编码, 发送包含由概率分布 `!$P$` 产生的符号的消息时, 所需要的额外信息量(如果是使用以2为底的对数, 信息量就用比特衡量).

KL 散度最重要的特征是它是非负的(nonnegative). KL 散度为 0 当且仅当离散型随机变量的 `!$P$` 和 `!$Q$` 是一模一样的分布或者连续型随机变量的 `!$P$` 和 `!$Q$` 的分布"几乎处处"相等. KL 散度将度量两个分布之间的距离概念化, 但是又不是真的距离因为 KL 散度不是对称的(symmetric): `!$D_{KL}(P \Vert Q) \ne D_{KL}(Q \Vert P)$`. 这种非对称性(asymmetric)意味着选择  `!$D_{KL}(P \Vert Q)$` 还是 `!$D_{KL}(Q \Vert P)$` 会产生不一样的结果. 

![Figure 3.6][12]

> 为了说明这两种选择的效果, 我们令 `!$p$` 为 两个高斯分布的混合, 令 `!$q$` 为单个高斯分布. 左边的是近似分布 q 在真实分布 p 反之高概率的所有地方都放置高概率, 右边的是近似分布 q 在 真实分布 p 放置低概率的所有地方放置高概率.

一个跟 KL 散度密切相关的是 **交叉墒(cross-entropy)**, `!$H(P, Q) = H(P) + D_{KL}(P \Vert Q) = - \mathbb{E}_{x \sim P}\log Q(x)$` . 针对 `!$Q$` 的最小化交叉墒等价与最小化 KL 散度, 因为 `!$Q$` 并不参与被省略的那一项.

> 当我们处理这些量的时候, 经常会遇到 `!$0 \log 0$`, 按照惯例, `!$\lim _{x \to 0} x \log x = 0$`

### (p) 1.2.14 结构化概率模型(Structured Probabilistic Models)

机器学习算法通常设计到大量的随机变量的联和概率分布, 但是这些随机变量中只有少量变量之间直接互相作用, 使用单个函数描述整个联合概率分布往往在计算和统计上面都很低效.  我们可以把概率分布分解成多个因子(factorization)相乘, 例如有三个随机变量 a, b, c, 并且 a 影响 b, b 影响 c , 但是 a 和 c 相互独立, 所以 `!$p(a, b, c) = p(a) p(b | a) p(c | b)$` . 这样能极大得降低表示联合分布的成本. 

当我们用图论中的图的概念来表示这种概率分布的分解, 我们把它称为 **结构化概率模型(structured probability model)** 或 **图模型(graphical mpdel)** . 有两种主要的结构化概率模型:  **有向(directed)** 和 **无向的(undirected)** . 这两种图模型都用以顶点表示随机变量, 边表示概率分布能够表示成这两个随机变量之间的直接作用的图 `!$\mathcal{G}$` .

**有向模型** 中对于分布中的每一个随机变量 `!$x_i$` 都有一个影响因子(factor), 这个组成 `!$x_i$` 条件概率分布的影响因子被称为 `!$x_i$`的父结点, 记作 `!$Pa_{\mathcal{G}}(x_i)$` : `!$p(\mathtt{x} = \prod_{i} p(x_i | Pa_{\mathcal{G}}(x_i)))$`.

![Figure 3.7][13]
 
**无向模型** 中, `!$\mathcal{G}$` 中任意结点都相互连接的集合称为 **团(clique, 也就是图论里面的连通分量) `!$\mathcal{C}$`**, 无向模型中的每个团 `!$\mathcal{C}^{(i)}$` 都有一个与之相关联的因子(factor) `!$\phi^{(i)}(\mathcal{C}^{(i)})$`, 这些因子仅仅是函数, 不是概率分布, 每个影响因子的输出(output, 也就是值)必须是非负的, 但是没有限制说这些因子的和或者积分(integrate)是 `!$1$`.  随机变量的联合概率和所有这些因子的乘积 **成比例(proportional)**, 也就是说因子的值越大, 可能性也就越大. 因为不保证因子的和或者积分为 1, 所以我们需要一个额外的归一化常数(normalized constant) `!$\mathcal{Z}$` 是 `!$\phi$` 函数乘积的所有状态的求和或积分. 所以 `!$p(\mathtt{x}) = \frac{1} {\mathcal{Z}} \prod_{i} \phi^{(i)} \left( \mathcal{C}^{(i)} \right)$` 

![Figure 3.8][14]

> 有向模型和无向模型只是描述(descript)概率分布的两种不同方式. 任何概率分布都可以用这两种方式描述. 

> 在研究部分(Part III)之前,  结构化概率模型仅仅是作为一个描述每种机器学习算法选择的直接概率化关系的语言.

## (p) 1.3 数值计算(Numericla Computation)

机器学习中数值计算主要是通过迭代来更新近似解, 而不是通过解析过程推导公式, 并且对数字计算机来说很难用有限的内存精确表示实数, 所以计算含有实数的函数是困难的, 常见的操作包括优化(找到使函数达到最大或最小值的参数值)和解线性方程组.  

### (p) 1.3.1 上溢(Overflow)和下溢(Underflow)

处理含有实数的数学问题时最基本的困难就是需要在有限位模式(bit pattern)的数字计算机中表示无限多位的实数, 所以基本上对于所有的实数, 我们都会遇到近似误差(approximation error), 特别的, 大多数情况就是舍入误差(rounding error), 这往往会导致本来理论上可行的没有设计为最小化累积舍入误差的算法在实践上出问题.  

一种严重的舍入误差就是 **下溢(Underflow)**, 下溢发生在接近 0 的数字被舍入为 0 . 很多函数在参数为很小的数和参数为 0 时表现的很不一样, 例如我们应该避免除以 0 (有些环境会抛出异常或者使用 NaN(not-a-number)占位值(placehold value))或对 0 取对数. 

另外一种就是 **上溢(Overflow)** , 上溢发生在很大的数字被近似为 `!$\infty$` 或 `!$-\infty$` . 进一步的操作就会使它变成 NaN .

一个例子就是 `softmax` 函数, `softmax` 函数经常用于预测 Multinoulli 分布相关的概率 : `!$\text{softmax}(x)_i = \frac{\exp (x_i)} {\sum_{j=1}{n}\exp (x_j)}$` .  如果 `!$\forall x_i = c$`, 则 `!$\text{softmax}(x) = \frac{1}{n}$`, 但是如果 `!$c$` 是一个足够大的负数, 就会导致它的对数下溢, 这样就会使得 `softmax` 函数的分母为 0, 或者 `!$c$` 是足够大的正数, 就会导致上溢, 这样的行为是未定义的. 可以通过令 `!$\text{softmax})(z) \text{, while } z = x - \max_i x_i$` , 这样对于分子, 不可能会超过 1, 对于分母, 至少有一个 1, 并且其他都不会大于 1. 不过这仍然可能会导致分子下溢为 0, 如果对此时的 `softmax` 函数值进行取对数, 将会导致 `!$\log 0$` . 所以我们必须实现一个单独的函数, 并以 **数值稳定(numerically stable)** 的方式计算 `!$\log \text{softmax}$`.

本书中的大部分算法都没有显式的考虑数值计算的细节, 低级库的作者应该在实现深度学习算法的时候把数值问题考虑在心中. **Theano** 就是这样的一个软件包, 自动检测并且稳定深度学习中许多常见的数值不稳定的表达式. 

### (p) 1.3.2 病态条件(Poor Conditioning)

条件(Conditioning) 表征当函数的输入的细小变化会导致函数值的改变的快慢程度. 在科学计算中, 细小的输入变化可能因为舍入误差造成输出值的很大改变. 

考虑函数 `!$f(x) = \boldsymbol{A}^{-1} x$` , 当  `!$\boldsymbol{A} \in \mathbb{R}^{n \times n}$`  有特征值分解, 则它的 **条件数(condition number)** 是 `!$\max_{i,j}\left| \frac{\lambda_i} {\lambda_j}\right|$`. 当这个数很大的时候, 矩阵求逆对输入的误差特别敏感, 这种敏感性是矩阵的固有属性, 而不是在矩阵求逆过程中因为舍入误差造成的. 即使我们在乘以完全正确的矩阵, 病态条件的矩阵也会放大预先存在的误差. 在实践中, 该错误会与求逆过程本身的数值误差进一步复合(compound).

### (p) 1.3.3 基于梯度的优化方法(Gradient-based optimization)

大部分深度学习算法都涉及某种形式的优化. 优化是指改变 `!$x$` 来使 `!$f(x)$` 的值最大或最小化. 最大化 `!$f(x)$` 可以由最小化 `!$-f(x)$`来完成. 我们要最大化或最小化的函数称为 **目标函数(objective function)** 或 **准则(criterion)**, 当我们最小化它时, 我们也把它称为 **代价函数(cost function)**, **损失函数(loss function)** 或 **误差函数(error function)** (本书中将会交替使用这些术语, 不过在有些机器学习文献中会赋予这些名称特殊的意义). 我们经常用上标(superscript) `!$*$` 号来表示最大化或最小化函数的值. 例如 `!$x^* = \arg \min f(x)$` .

对于足够小的 `!$\epsilon \text{, } f(x - \epsilon \mathtt{sgn}(f'(x)) < f(x)$`. 所以我们可以通过把 `!$x$` 向导数(derivative) 相反的方向移动一小步来使函数值减小. 这种技术称为 **梯度下降(gradient descent)**. 导数为 0 的点被称为 **临界点(critical point)** 或 **驻点(stationary point)** . 

![Figure 4.2][15]

> 最右边那种情况称为 **鞍点(saddle point)** .

**全局最小点(global minimum)** 可以有一个或多个. 在深度学习的背景下, 我们要优化的函数可能会含有许多不是最优的局部最小点, 或者是鞍点, 这样会使得优化变得很困难(尤其是多维情况下), 因此我们寻找使 `!$f$` 非常小的点, 但不一定是全局最小的点. 

![Figure 4.3][16]

在多维情况下(为了使最小最大这些术语有意义, 一般规定函数的值域必须是一维空间标量), 使用 **偏导数(partial derivative, `!$\frac {\partial}{\partial x_i} f(\boldsymbol{x})$`)** 和 **梯度(gradient, `!$\nabla_{\boldsymbol{x}}f(\boldsymbol{x})$`)**, **方向导数(directional derivative, 在方向 `!$\boldsymbol{u}$`, 上的方向导数为 `!$\nabla_{\boldsymbol{u}}f(\boldsymbol{x}) = \nabla f(\boldsymbol{x}) \cdot \frac{\boldsymbol{u}}{|\boldsymbol{u}|}$` )** , 临界点就是梯度的所有元素都为 0 的点. 

为了使 `!$f$` 最小化, 我们需要找到降低最快的方向: `!$\min_{\boldsymbol{u}, \boldsymbol{u}^{\top}\boldsymbol{u} = 1} \boldsymbol{u}^\top \nabla _{\boldsymbol{x}} f(\boldsymbol{x}) = \min_{\boldsymbol{u}, \boldsymbol{u}^{\top}\boldsymbol{u} = 1} \lVert \boldsymbol{u} \rVert_{2} \lVert \nabla_{\boldsymbol{x}} f(\boldsymbol{x}) \rVert_2 \cos \theta = \min_{\boldsymbol{u}, \boldsymbol{u}^\top \boldsymbol{u} = 1} \cos \theta$` (`!$\theta$` 是 `!$\boldsymbol{u}$` 和梯度的夹角, 最后面是因为省略了与 `!$\boldsymbol{u}$` 无关的量). 也就是说 **`!$\boldsymbol{u}^*$`为梯度相反的方向** . 这种方法称为 **最速下降法(method of steepest descent)** 或 **梯度下降(gradient descent)** .

为了最速下降, 新的点选为 `!$x' = x - \epsilon \nabla_x f(x)$`, `!$\epsilon$` 就是 **学习率(learning rate)**, 一个用来表示步长的(很小的)正标量, 而且有很多种取法: 我们可以找使梯度消失的 `!$\epsilon$`, 我们还可以用 **线搜索(linear search, 使用不同的 `!$\epsilon$` 来找到最小的 `!$f(x - \epsilon \nabla_x f(x))$` )**, 我们也可以直接求梯度为 0 的临界点而不用迭代求得 .

> 虽然梯度下降是限制于优化连续空间, 不过我们通过步长不断的移动, 可以看作是离散空间. 递增带有离散参数的目标函数被称为 **爬山(hill climbing)算法** . 

**梯度之上: Jacobian 矩阵和 Hessian 矩阵**

对于定义域和值域都是多维向量的情况, 包含所有偏导数的矩阵为 **Jacobian 矩阵**: `!$\boldsymbol{f}: \mathbb{R}^m \rightarrow \mathbb{R}^n, \boldsymbol{J} \in \mathbb{R}^{n \times m}, J_{i, j} = \frac{\partial}{\partial x_j} f(\boldsymbol{x})_i$` . 

我们可以认为 **二阶导(second derivative)** 作为 **曲率(curvature)** 的衡量. 

![Figure 4.4][17]

对于二阶导数, 我们可以使用 **Hessian 矩阵**: 

```mathjax!
$\boldsymbol{H}(f)(\boldsymbol{x})_{i,j} = \frac{\partial^2}{\partial x_i \partial x_j} f(\boldsymbol{x})$
```

对于二阶偏导处处连续的地方, 微分算子是可交换的, 所以 `!$\frac{\partial^2}{\partial x_i \partial x_j} f(\boldsymbol{x}) = \frac{\partial^2}{\partial x_j \partial x_i} f(\boldsymbol{x})$`
, 也就是说 Hessian 矩阵是对阵阵, `!$H_{i,j} = H_{j,i}$`, 在深度学习的背景下, 我们遇到的大部分函数都在几乎处处有 对称 Hessian 矩阵. 因为 Hessian 矩阵是实对称阵, 所以我们可以把它分解成一组实特征值(eigenvalue)和一组特征向量(eigenvector)的正交基(orthogonal basis). 对于 `!$d^\top Hd$`, `!$d$` 为 `!$H$` 的特征向量, 并且`!$H$` 对应的特征值就是特征向量 `!$d$` 方向上的单位向量对应的方向导数. 对于其他方向 `!$d$` , 二阶方向导数就是所有特征值的加权(权重都在 0 ~ 1, 并且与 `!$d$` 夹角越小, 权重越大)平均.  

我们在点 `!$\boldsymbol{x}^{(0)}$` 处作近似二阶泰勒级数(second-order Taylor series): `!$f(\boldsymbol{x}) \approx f(\boldsymbol{x}^{(0)}) + (\boldsymbol{x} - \boldsymbol{x}^{(0)})^\top \boldsymbol{g} + \frac{1}{2} (\boldsymbol{x} - \boldsymbol{x}^{(0)})^\top \boldsymbol{H} (\boldsymbol{x} - \boldsymbol{x}^{(0)})$`, 其中 `!$g$` 是梯度, `!$H$` 是 `!$x^P{(0)}$` 处的 Hessian 矩阵. 如果我们使用学习率 `!$\epsilon$`, 也就是令 `!$\boldsymbol{x} = \boldsymbol{x}^{(0)} - \epsilon \boldsymbol{g}$`, 则  `!$f\left(\boldsymbol{x}^{(0)} - \epsilon \boldsymbol{g}\right) \approx f\left(\boldsymbol{x}^{(0)}\right) - \epsilon  \boldsymbol{g}^\top \boldsymbol{g} + \frac{1}{2} \epsilon^2 \boldsymbol{g}^\top \boldsymbol{H} \boldsymbol{g}$`, 通过该式很容易得出, `!$\boldsymbol{g}^\top \boldsymbol{H} \boldsymbol{g} \le 0$` 时, `!$\epsilon$` 可以取任意正实数(但是实践中还是不能太大, 因为毕竟是近似泰勒级数(二阶)), 都能使函数值是下降的, 而  `!$\boldsymbol{g}^\top \boldsymbol{H} \boldsymbol{g} > 0$` 并且大到一定程度之后, 函数值将会上升(uphill),  并且很容易得出(二次函数) `!$\epsilon^* = \frac {\boldsymbol{g}^\top \boldsymbol{g}} {\boldsymbol{g}^\top \boldsymbol{H} \boldsymbol{g}}$` . 

> 最坏情况下, 也就是 `!$\boldsymbol{g}$` 与 `!$\boldsymbol{H}$` 最大特征值 `!$\lambda_\max$` 对应的特征向量对齐(align, 也就是共线)的时候, 最优步长 `!$\epsilon^* = \frac{1}{\lambda_\max}$` . 

> 对于能用二次函数(quadratic function)很好拟合的函数, Hessian 的特征值决定了学习率的量级. 

可以用二阶导判断临界点是(局部)极大值点还是极小值点, 这被称为 **二阶导测试(second derivative test)**. 一维情况是高中学过的, 对于多维情况, 也就是 `!$\nabla_\boldsymbol{x} f(\boldsymbol{x}) = 0$` 的时候, 如果 Hessian 矩阵在临界点是正定(positive definite), 该临界点就是极小值点(因为方向二阶导数在任意方向都是正数), 如果是负定的, 该临界点就是极大值点, 如果 Hessian 矩阵既有正特征值又有负特征值, 则这个临界点就是鞍点(saddle point, 相比于单变量情况, 多维情况给鞍点的判断带来了点积极证据, 也就是说该临界点在某个横断面(cross section)上是局部最小而在另外一个横断面上是局部最大点), 而如果是所有非零特征值是同号的并且有零特征值, 那么该临界点我们依然我无法判断是鞍点还是平坦区域(flat region)的一部分.

在多维情况下, 每一个点都有很多个(方向)二阶导数. Hessian 矩阵的条件数(condition number)很差(poor)的时候, 说明在某些方向导数变化很快而其他地方却特别慢. 梯度下降法并不知道这种改变, 所以就不知道它应该在导数长期为负的方向优先探索. 这样使得去找步长 `!$\epsilon$` 变得困难, 因为步长要足够小避免直接冲过(overshoot)极小值点, 但是步长很小又意味着在其他曲率较小的方向会进展得很不明显. 

我们可以使用Hessian 矩阵提供的信息来指导搜寻. 其中最简单的方法就是 **牛顿法(Newton's method)** , 牛顿法基于二阶泰勒展开 `!$f(\boldsymbol{x}) \approx f(\boldsymbol{x}^{(0)}) + (\boldsymbol{x} - \boldsymbol{x}^{(0)})^\top \nabla_\boldsymbol{x}f(\boldsymbol{x}^{(0)}) + \frac{1}{2} (\boldsymbol{x} - \boldsymbol{x}^{(0)})^\top \boldsymbol{H}(f) (\boldsymbol{x} - \boldsymbol{x}^{(0)})$`, 求导后得到临界点 `!$\boldsymbol{x}^* = \boldsymbol{x}^{(0)} - \boldsymbol{H}(f)(\boldsymbol{x}^{(0)})^{-1}\nabla_{\boldsymbol{x}}f(\boldsymbol{x}^{(0)})$`, 如果还函数是 **正定二次函数(positive definite quadratic function)**, 那么直接通过上次计算一次就可以得出 `!$x^*$`, 如果函数局部可以近似为正定二次函数, 可以通过多次迭代更新计算上式(毕竟这牛顿法还得基于一个定点 `!$\boldsymbol{x}^{(0)}$` , 这个顶点需要迭代更新, 并且这样迭代的更新近似函数和跳到近似函数的最小点可以比梯度下降更快得到达临界点). 牛顿法在靠近极小值点(也就是盖点的 Hessian 为负定矩阵)才是有用的性质, 如果是靠近鞍点, 那就是一个危险的性质(而梯度下降不会有这样的危险, 除非梯度就是指向鞍点的). 

仅使用梯度的方法(例如梯度下降)被称为 **一阶优化算法(first-order optimazation algorithms)**, 像牛顿法这样利用 Hessian 矩阵的方法就被称为 **二阶优化算法(second-order optimazation algorithms)**. 

书上使用的这些优化方法可以应用在很大范围的函数, 但是几乎都没有保证(适用于所有), 因为书中设计到的函数族相当复杂. 在深度学习的背景下, 可以通过限制函数为 **Lipschitz 连续(continous)** 或有 其导数是Lipschitz连续的来提供一些保证. Lipschitz 连续函数指的是变化速度 **以Lipschitz 常数(constant) `!$\mathcal{L}$` 为界** :

```mathjax!
$$\forall \boldsymbol{x}, \forall \boldsymbol{y}, |f(\boldsymbol{x}) - f(\boldsymbol{y})| \le \mathcal{L} \lVert \boldsymbol{x} - \boldsymbol{y} \rVert_2$$
```
这个性质为量化 "微小的输入改变将会带来微小的输出的改变" 提供保证(guarantee), 并且这是一个很弱(weak) 的约束(constraint), 深度学习中很多优化问题都可以通过略微的修改变得 Lipschitz 连续 . 最成功的特定优化领域或许就是 **凸优化(convex optimazation)**, 凸优化可以通过附加更强约束来提供更多的保证, 凸优化只适用于凸函数(Hessian 矩阵在处处都是半正定的), 因为这些函数没有鞍点并且所有极小值点都是全局最小值点(global minima), 但是深度学习遇到的大部分问题很难表示为凸优化的形式, 所以凸优化只是作为一些深度学习算法的子程序(subroutine), 不过凸优化中的分析思路对深度学习算法的收敛性(convergence)非常有用. 一般来说, 深度学习背景下凸优化的重要性大大降低, 更多的信息参考 *Boyd and Vandenberghe(2004)* 或 *Rockafellar(1997)* .

### (p) 1.3.4 约束优化(Constrained Optimization)

有时候我们只想从 `!$\boldsymbol{x}$` 的某些集合 `!$\mathbb{S}$` (也就是说定义域的子集) 中寻找最大和最小值, 这被称为 **约束优化(constrained optimization)** , 集合 `!$\mathbb{S}$` 中的点被称为 **可行(feasible)点** . 例如我们想找某种意义上小的点, 我们可以加以范数约束如 `!$\lVert x \rVert_2 \le 1$` .

一个简单的方案就是修改梯度下降使得将约束考虑在内. 如果我们使用很小的常数步长 `!$\epsilon$` , 我们可以先梯度下降, 然后将结果映射(project)回 `!$\mathbb{S}$` . 如果我们使用线搜索, 我们只搜索生成的 `!$\boldsymbol{x}$` 为可行点的 `!$\epsilon$` . 如果可以的话, 在梯度下降或线搜索之前, 把梯度映射到可行域(feasible region)会更高效.

一个更加复杂的方案就是转化成一个解和原问题一模一样的无约束的优化问题, 例如求 `!$\text{minize } f(\boldsymbol{x}) , \boldsymbol{x} \in \mathbb{R}^2, \lVert \boldsymbol{x} \rVert_2  \equiv 1$`, 可以转化为 `!$g(\theta) = f([\cos \theta, \sin \theta]^\top)$`, 这样对于 `!$\theta$` 来说没有约束. 这种方案需要对每种情况都单独设计一个转换. 

**KKT 方法(The Karush-Kuhn-Tucker approach, Lagrange 乘子法的推广形式)** 提供了约束优化的一般解决方案. 

新函数: **广义(generalized) Lagrangian** 或 **广义 Lagrange 函数** . 首先定义 `!$\mathbb{S}$` 为 `!$m$` 个函数 `!$g^{(i)}$`  组成的等式( **等式约束(equality constraints)** )和 `!$n$` 个函数 `!$h^{(i)}$` 组成的不等式( **不等式约束(inequality constraints)** )构成的集合 `!$\mathbb{S} = \left\{\boldsymbol{x} | \forall i, g^{(i)}(\boldsymbol{x}) = 0, \forall j, h^{(j)} \le 0\right\} $`, 并且对于每个约束, 有变量 `!$\lambda_i, \alpha_i$` (这些被称为 KKT 乘子(multiplier)), 所以广义 Lagrangian 定义为: `!$L(\boldsymbol{x}, \boldsymbol{\lambda}, \boldsymbol{\alpha}) = f(\boldsymbol{x}) + \sum_i \lambda_i g^{(i)}(\boldsymbol{x}) + \sum_j \alpha_j h^{(j)}(\boldsymbol{x})$`. 我们现在就可以将约束优化转化为无约束优化: `!$\min_\boldsymbol{x} \max _\boldsymbol{\lambda} \max_{\boldsymbol{\alpha}, \boldsymbol{\alpha} \ge 0} L(\boldsymbol{x, \lambda, \alpha}) \Leftrightarrow \min_{\boldsymbol{x} \in \mathbb{S}} f(\boldsymbol{x})$`. 这是因为满足约束条件的时候 `!$\max _\boldsymbol{\lambda} \max_{\boldsymbol{\alpha}, \boldsymbol{\alpha} \ge 0} L(\boldsymbol{x, \lambda, \alpha}) = f(\boldsymbol{x})$`, 当不满足约束条件的时候, `!$\max _\boldsymbol{\lambda} \max_{\boldsymbol{\alpha}, \boldsymbol{\alpha} \ge 0} L(\boldsymbol{x, \lambda, \alpha}) = \infty$`.

为了在约束条件下求最大值, 可以应用 `!$\min_\boldsymbol{x} \max _\boldsymbol{\lambda} \max_{\boldsymbol{\alpha}, \boldsymbol{\alpha} \ge 0} -L(\boldsymbol{x, \lambda, \alpha})$` ( Lagrangian 相反值的最小值) 或者 `!$\max\boldsymbol{x} \max _\boldsymbol{\lambda} \max_{\boldsymbol{\alpha}, \boldsymbol{\alpha} \ge 0} L(\boldsymbol{x, \lambda, \alpha})$`.

**////////////////////////////// 这些性质都不知道怎么来的 //////////////////////////////**

等式约束对应变量 `!$\lambda_i$` 的符号不重要, 可以随意选择. 对于不等式约束, 如果 `!$h^{(i)}(\boldsymbol{x}^*) = 0$`, 则称该约束是 **活跃的(active)**, 如果不等式约束不是活跃的, 则去除这一个约束的优化问题的解(solution)跟原问题的解至少有一个相同的局部解(local solution), 一个不活跃的约束可能会排除其他的一些解 . 例如, 含有一整块平坦的相等代价区域都是全局最优点(globally optimal points)的凸问题(convex problem)中这块区域可能会会因为这些约束(constraints)被消除掉, 或者在非凸问题中, 收敛时不活跃的约束可能会排除掉更好的局部驻点(local stationary points). 不过无论不活跃的约束是否被包含, 收敛时找到的点都是驻点(stationary point). 这是因为不活跃的 `!$h^{(i)}$`有负值, 然后 `!$\min_{\boldsymbol{x}} \max_{\boldsymbol{\lambda}} \max_{\boldsymbol{\alpha}, \boldsymbol{\alpha} \ge 0} L(\boldsymbol{x}, \boldsymbol{\lambda}, \boldsymbol{\alpha})$` 会有`!$\alpha_i = 0$`, 因此我们可以观察到该解中, 有 `!$\boldsymbol{\alpha} \bigodot \boldsymbol{h}(\boldsymbol{x}) = \boldsymbol{0}$`, 换句话说, 对于所有 `!$i$`, 我们知道在该解中至少有一个约束 `!$\alpha_i \ge 0$` 或 `!$h^{(i)}(\boldsymbol{x}) \le 0$`是活跃的. 直观的说, 我们可以说这个解是由不等式强加的边界，我们必须通过对应的 KKT 乘子影响 `!$\boldsymbol{x}$` 的解，或者不等式对解没有影响，我们则归零 KKT 乘子。

有一些描述约束优化问题的最优点的性质, 被称为 Karush-Kuhn-Tucker (KKT) 条件(conditions) (Karush, 1939; Kuhn and Tucker, 1951). 这些是确定一个点是最优点的必要条件(necessary conditions)而不一定是充分条件(sufficient conditions): 

* 广义 Lagrangian 的梯度为 0
* 所有关于 `!$\boldsymbol{x}$` 和 KKT 乘子的约束都满足
* 不等式约束显示的‘‘互补松弛性’’(complementary slackness):  `!$\boldsymbol{\alpha} \bigodot \boldsymbol{h}(\boldsymbol{x}) = \boldsymbol{0}$`

### (p) 1.3.5 例子: 线性最小二乘(Squares)

如果我们要找到 `!$f(\boldsymbol{x}) = \frac{1}{2} \lVert \boldsymbol{Ax} - \boldsymbol{b} \rVert_2^2$` 的最小值. 虽然线性代数能够高效的解决该问题, 但是我们可以使用基于梯度的优化方法来解决这个问题. 

求梯度, `!$\nabla_\boldsymbol{x}f(\boldsymbol{x}) = \boldsymbol{A}^\top (\boldsymbol{Ax} - \boldsymbol{b}) = \boldsymbol{A}^\top \boldsymbol{Ax} - \boldsymbol{A}^\top \boldsymbol{x}$` .

![Figure 4.5][18]

如图, 可以使用梯度下降算法, 又因为这是一个二次函数, 所以我们还可以使用牛顿法来一步达到最优点.

现在我们添加约束 `!$\boldsymbol{x}^\top \boldsymbol{x} \le 1$`, 我们定义 Lagrangian : `!$L(\boldsymbol{x}, \alpha) = f(\boldsymbol{x}) + \alpha(\boldsymbol{x}^\top \boldsymbol{x} - 1)$`
于是我们可以通过 `!$\min_{\boldsymbol{x}} \max_{\alpha, \alpha \ge 0} L(\boldsymbol{x}, \alpha)$` 来找到最小值点. 

求得 Lagrangian 的导数并找到零点, `!$\boldsymbol{x} = (\boldsymbol{A}^\top \boldsymbol{A} + 2\alpha \boldsymbol{I})^{-1} \boldsymbol{A}^\top \boldsymbol{b}$`(矩阵求逆需要用到 Moore-Penrose 伪逆), 其中 `!\alpha$` 的量纲的选取必须使其满足约束, 观察 `!$\frac{\partial}{\partial \alpha} L(\boldsymbol{x}, \alpha) = \boldsymbol{x}^\top \boldsymbol{x} - 1 \le 0$` 恒成立, 所以我们只需要一直增大 `!$\alpha$`, 并且`!$\boldsymbol{x}$`满足其约束条件就行. 

## (p) 1.4 机器学习基础(Machine Learning Basics)

本章介绍本书其余部分涉及到的机器学习的最重要原理. 

> 对于新手, 推荐阅读 **Murphy (2012)** or **Bishop(2006)**

大部分机器学习算法需要设置 **超参数(hyperparameters, 必须在算法外设定)**, 机器学习是一种应用统计学, 用计算机来统计地估计复杂函数而不太关注这些函数提供的置信空间(confidence intervals), 并且有两个核心方法来统计: 频率派估计(frequentist estimator)和贝叶斯推断(Bayesian inference). 大部分机器学习算法可以划分为两类: **监督学习(supervised learning)** 和 **无监督学习(unsupervised learning)** . 大部分深度学习算法都是基于一个叫做 **随机梯度下降(stochastic gradient descent)** 的优化算法. 我们将介绍如何如何将各种算法组件(优化算法, 代价航函数(cost function), 模型, 数据集(dataset))来构建机器学习算法.

### (p) 1.4.1 学习算法

 Mitchell (1997) 提供了一个简洁的学习算法的定义: **对于某任务 T 和性能度量 P , 一个计算机程序能够从经验 E 中学习是指, 通过经验 E 改进后, 它在任务 T 上由性能度量 P 衡量的性能有所提升.** E, T, P 的种类相当广泛, 所以我们不会形式化得定义它们, 我们给出一些直观的描述和例子:
 
 **任务 T**
 
 学习过程本身不是任务, 学习是实现任务的手段. 例如, 要让机器人行走, 行走就是任务. 机器学习任务定义为机器学习系统如何处理 **样本(example, 样本就是一些特征(features)的集合, 通常表示为向量 `!$x \in \mathbb{R}^n$` 表示 `!$n$` 个特征 )** . 能用机器学习解决的任务主要有以下几类:
 
 * **分类(Classfication)** : 将输入分类为 `!$k$` 类(有可能还带有概率), `!$f: \mathbb{R}^n \rightarrow \{ 1, \ldots, k\}, y = f(\boldsymbol{x})$`, 返回一个数值码(numeric code) `!$y$` . 例如物体识别(object recognition), 人脸识别.
 * **Classiﬁcation with missing inputs**: 如果输入中有些内容被缺失, 我们的学习算法就需要一组函数来处理有不同缺失情况的子集. 我们可以通过学习所有相关变量的概率分布, 然后通过边缘化( marginalizing)缺失变量来解决分类任务.`!$n$` 个输入变量需要 `!$2^n$` 个不同的分类函数来应对每种可能的缺失情况, 但是计算机程序只需要学习一个描述联合概率分布的函数.
 * **回归(Regression)**: 对给定输入输出预测数值. `!$f: \mathbb{R}^n \rightarrow \mathbb{R}$`. 例如预测证劵未来价格, 预测投保人的索赔金额.
 * **转录(Transcription)**: 将非结构化的数据转化成离散文本形式, 例如光学字符识别和语音识别.
 * **机器翻译(machine translation)**
 * **结构化输出(structured output)** : 输出为各个元素有关联的向量. 结构化输出与许多机器学习任务(例如上面提到的机器翻译和转录)相关联, 例如将自然语言按照语法解析为为语句树. 还有一个例子就是像素级分割.
 * **异常检测(Anomaly detection)** : 从一堆事件或物体中筛选出异常或非典型的. 例如信用卡欺诈识别.
 * **合成和采样(Sythesis and sampling)** : 用机器学习来生成和训练样本类似的新样本. 例如游戏中大型物体或风景的纹理, 以及语音合成.
 * **缺失值填补(Imputation of missing values)** : 算法为输入中确实的条目预测值. 
 * **去噪(denoising)** : 算法的输入是 **干净样本(clean example)** `!$\boldsymbol{x} \in \mathbb{R}^n$` 通过未知损坏过程后得到的 **损坏样本(corruption example)** `!$\tilde{\boldsymbol{x}} \in \mathbb{R}^n$` , 算法通过损坏样本预测干净样本, 或者更一般的预测条件概率分布 `!$p(\boldsymbol{x} | \boldsymbol{\tilde{\boldsymbol{x}}})$` .
* **密度估计(density estimation)** 或 **概率质量函数估计(probability mass function estimation)** : 算法学习函数 `!$p_{\text{model}}(\boldsymbol{x}): \mathbb{R}^n \rightarrow \mathbb{R}$`, 其中 `!$p$` 可以解释为样本采样空间的概率密度函数(如果是连续型)或概率质量函数(如果是离散型). 上面提到的任务大多只需要至少能隐式地捕获(capture)概率分布的结构, 而密度估计必须显式地捕获该分布. 例如, 我们可以将密度估计的结果用于缺失值填补任务. (但实际情况是密度估计不能解决所有这类问题).

**性能度量(Performance Measure) P**

对于分类, 缺失输入的分类和转录, *P* 往往是衡量模型的 **准确率(accuracy, 模型产生正确输出的比率, 或者用错误率(error rate)来度量)** . 我们通常把错误率称为 **0-1损失的期望** , 在一定特定的样本上, 如果结果是对的, 那么 0 -1 损失为0, 否则是1. 但是 0 - 1 损失模型或者准确率错误率对于密度估计这类任务而言, 是没有意义的, 我们可以对模型的每个样本都输出一个连续数值的得分, 最常见的方法就是输出模型在一些样本上概率对数的平均值.  我们使用 **测试集(test set)** 来度量性能. 有时候, 很难确定到底要度量什么, 比如在度量转录任务时, 我们是应该度量整个序列的准确率还是更加细粒度(fine-grained)地对序列中正确的那部分元素以正面评价, 在度量回归任务时, 我们是应该很多的惩罚(penalize)那些频繁的中等错误还是少数几个很大的错误?	这些选取取决于应用. 又有些时候, 我们知道应该度量什么, 但是度量他们又不现实, 例如密度估计, 很多最好的概率模型只能隐式的表现其概率分布, 在这种情况, 我们必须设计一个对应于设计对象的替代标准, 或者设计一个理想标准的近似. 

**经验(experience) E**

按照在学习过程中算法被允许有什么类型的经验, 可以把机器学习算法分为 **无监督(unsupervised)** 和 **监督(supervised)学习**. 

本书中大部分学习算法可以为认为是在整个 **数据集(dataset, 很多样本组成的集合)** 上获取经验. 

最古老的用来给统计学家和机器学习研究人员学习的数据集是 Iris(鸢尾花卉) 数据集, 每个样本包括该植物不同部分的测量结果(萼片长度、萼片宽度之类的), 这个数据集中包含三个品种, 并且标明了每个样本的品种.

* 无监督学习从数据集中学习这个数据集上有用的结构性质. 在深度学习的背景下, 学习生成数据集的整个概率分布(例如密度统计着这种显式的, 或者合成, 去噪这些隐式的). 还有些其他类型的, 例如 **聚类(clustering)** , 将数据集中的所有样本按照相似的放在一起来分成若干个集群.
* 监督学习也是从包含很多特征的数据集中学习, 不过每个样本都有一个与之关联的 **标签(label)** 或 **目标(target)** . 例如, 监督学习通过研究 Iris 数据集, 学习如果通过测量信息来将样本划分为三个品种.

粗略得说, 无监督学习从一些随机向量 `!$\boldsymbol{\mathtt{x}}$` 中观察并隐式或显式的学习概率分布 `!$p(\boldsymbol{\mathtt{x}})$` , 或这个概率分布的有趣的性质, 而监督学习从一些随机向量`!$\boldsymbol{\mathtt{x}}$` 和与之关联的值或向量 `!$\boldsymbol{\mathtt{y}}$`观察学习来预测 `!$\boldsymbol{\mathtt{y}}$` (也就是估计 `!$p(\boldsymbol{\mathtt{y}} | \boldsymbol{\mathtt{x}})$` ). 监督学习和无监督学习并非形式化定义的术语, 所以他们之间的界限往往也比较模糊, 我们知道条件概率的链式法则 `!$p(\boldsymbol{\mathtt{x}} \in \mathbb{R}^n) = \prod_{i = 1}^n p(x_i | x_1, \ldots, x_{i - 1})$`, 所以我们可以把模型 `!$p(\boldsymbol{\mathtt{x}})$`的无监督学习转化为 `!$n$` 个监督学习. 又由贝叶斯法则 `!$p(y | \boldsymbol{\mathtt{x}}) = \frac{p(\boldsymbol{\mathtt{x}}, y')} {\sum_{y'}p(\boldsymbol{\mathtt{x}}, y')}$` , 可以把监督学习转化为无监督学习. 

一般的, 回归, 分类, 结构化输出问题被称为监督学习, 密度估计被称为无监督学习. 还有其他一些变种学习范例, 例如 **半监督学习(semi-supervised)** , 一些样本包含监督目标而另外一些不包含. 而在 **多实例学习(multi-instance learning)** 中, 一个包含若干样本的集合(称为包(bag))只是被标记为包含还是没有包含某一类的样本, 而这个集合中的样本本身没有做任何标记. 有些学习算法并不是只从固定的数据集中学习, **强化学习(reinfocement learning)** 会和环境交互, 也就是说学习系统和它的训练过程之间还有反馈回路(feedback loop), 利用深度学习方法的强化学习称为 **深度强化学习(deep learning approach to reinfocement learning)** , 强化学习不在本书范围内. 

一种常见的表示数据集的方式是使用 **设计矩阵(design matrix)** , 一个设计矩阵中每一行代表一个样本, 而每一列则代表不同的特征. 但是, 设计矩阵中的每一个向量都必须是相同的大小, 这在某些情况下是不可能的, 例如如果有一堆不同高度宽度的照片的集合, 肯定不能用相同大小的向量来表示所有的照片, 我们之后会介绍怎么处理这样的 **异构数据(heterogeneous data)** , 并且在这种情况下我们使用集合表示 `!${\boldsymbol{x}^{(1)}, \ldots, \boldsymbol{x}^{(m)}}$`, 其中任意两个向量 `!$\boldsymbol{x}^{(i)}$` 和 `!$\boldsymbol{x}^{(j)}$` 不一定是相同的大小.  对于监督学习中每个样本的标签或目标, 我们可以使用一个数值码(numeric code)来表示(例如对于照片分类器, 0表示照片里面有人, 1表示有车, ...), 并且将这些数值码存在一个向量中, 例如 `!$\boldsymbol{y}$`, 并且 `!$y_i$` 表示第 `!$i$` 个样本的标签, 不过有时候标签不止一个数值, 例如声音识别. 

**例子: 线性回归(linear regression)**

`!$\hat{y} = \boldsymbol{w}^\top \boldsymbol{x}$`, 其中 `!$\boldsymbol{x} \in \mathbb{R}^n$` 为输入, `!$\boldsymbol{w} \in \mathbb{R}^n$` 称为 **参数(parameters)**, 我们可以把它理解为一个决定每一个特征 `!$x_i$` 影响预测值的能力的权重(weight)的集合, `!$y \in \mathbb{R}$` 为模型预测的值. 

所以在这里, T 就是用 `!$\hat{y} = \boldsymbol{w}^\top \boldsymbol{x}$` 来从输入 `!$\boldsymbol{x}$` 预测 `!$y$` . 对于性能测试 P, 我们可以用一个含有 `!$m$` 个输入样本的设计矩阵 `!$\boldsymbol{X}^{(\text{test})}$` 作为测试集(该测试集不能用来训练), 对于这些测试样本, 我们需要提前准确对应的正确的 `!$y$` 的向量`!$\boldsymbol{y}^{(\text{test})}$` . 一种度量性能的方式就是计算测试集的 **均方误差(mean squared error)**: `!$\text{MSE}_{\text{test}} = \frac{1}{m} \sum_i(\hat{\boldsymbol{y}}^{(\text{test}))} - \boldsymbol{y}^{(\text{test})})_i^2 = \frac{1}{m} \lVert \hat{\boldsymbol{y}}^{(\text{test})} - \boldsymbol{y}^{(\text{test})}\rVert_2^2$`(越小越好, 也就是平方欧几里德距离除以 `!$m$` ).

为了构建一个机器学习算法, 我们需要算法能够通过观察训练集 `!$(\boldsymbol{X}^{(\text{train})}, \boldsymbol{y}^{(\text{train})})$` 获得经验来改善权重集 `!$\boldsymbol{w}$` 使得减小 `!$\text{MSE}_{\text{train}}$`. 我们可以简单的找到 `!$\text{MSE}_{\text{train}}$` 关于 `!$\boldsymbol{w}$` 梯度为 0 的地方就是它的极值点, 

```mathjax!
$$\nabla_{\boldsymbol{w}} \text{MSE}_{\text{train}} = 0$$
$$\Rightarrow \nabla_{\boldsymbol{w}} \frac{1}{m} \lVert \hat{\boldsymbol{y}}^{(\text{train})} - \boldsymbol{y}^{(\text{train})} \rVert_2^2 = 0$$
$$\Rightarrow \nabla_{\boldsymbol{w}} \frac{1}{m} \lVert \boldsymbol{X}^{(\text{train})} \boldsymbol{w} - \boldsymbol{y}^{(\text{train})} \rVert_2^2 = 0$$
$$\Rightarrow \nabla_{\boldsymbol{w}} (\boldsymbol{X}^{(\text{train})} \boldsymbol{w} - \boldsymbol{y}^{(\text{train})} )^\top (\boldsymbol{X}^{(\text{train})} \boldsymbol{w} - \boldsymbol{y}^{(\text{train})} ) = 0$$
$$\Rightarrow \boldsymbol{w} = \left( \boldsymbol{X}^{(\text{train})\top} \boldsymbol{X}^{(\text{train})}\right)^{-1} \boldsymbol{X}^{(\text{train})\top} \boldsymbol{y}^{(\text{train})} $$
```

通过上式给出的解的系统方程被称为 **正规方程(normal equations)**, 线性回归通过添加一个额外的 **偏差(偏置)参数(bais parameter)** `!$b$` 可以用来解决更加复杂一点的模型: `!$\hat{y} = \boldsymbol{w}^\top \boldsymbol{x} + b$`(这样的一个映射叫做 **仿射函数(affine function)**, 本书后面提到仿射函数时会频繁地使用线性这个术语). 

### (p) 1.4.2 容量(Capacity), 过拟合(Overfitting) 和欠拟合(Underfitting)

机器学习算法最核心的挑战就是要在新的, 以前从未见过的数据集中表现良好, 这种能力被称为 **泛化(generalization)** . 在训练的时候, 我们希望降低 **训练误差(training error)**(也就是一个优化问题), 同样我们还希望 **泛化误差(generalization error)** 或 **测试误差(test error)** 也能很低, 泛化误差被定义为新输入的误差期望(期望的计算基于不同的可能的输入, 并且这些输入来自于系统在现实中遇到的分布).   一般的, 我们用测试集上的性能度量来估计泛化误差.

例如在线性回归上, 我们实际上需要关注的是测试误差: `!$\frac{1}{m} \lVert \boldsymbol{X}^{(\text{test})} \boldsymbol{w} - \boldsymbol{y}^{(\text{test})}\rVert_2^2$`. 但是当我们只能看到训练集的时候怎么影响测试集上的性能? **统计学习理论(statistical learning theory)** 给出了一些答案. 我们必须知道训练集和测试集中的元素是怎么收集的, 也就是我们可以事先做出一些假设, 这样我们才可能去解决上述问题. 

训练集和测试集从数据集中按照概率分布生成这一过程称为 **数据生成过程(data-generating process)**, 我们作出以下假设(称为 **独立同分布假设(i.i.d. assumption))**: 训练集和测试集中的样本都是相互独立的, 训练集和测试集具有 **相同的分布**, 并且该分布称为 **数据生成分布(data-generating distribution)**, 记作 `!$p_{\text{data}}$` . 对于一个事先固定的 `!$\boldsymbol{w}$`, 理论上训练误差和测试误差的期望是一样的, 而很多情况下, 我们是先通过选择对于训练集来说误差足够小的 `!$\boldsymbol{w}$`, 再在测试集上测试, 所以在测试集上的测试误差肯定会大于等于训练误差, 所以性能良好的机器学习算法必须是使训练误差足够小, 并且还要使训练误差和测试误差的差距足够小. 

欠拟合就是训练误差太大, 过拟合就是训练误差和测试误差的差距太大. 一种控制模型过拟合和欠拟合行为的方法就是改变其 **容量(capacity)**, 通俗的说, 模型的容量就是模型所能表示各种不同函数的能力. 容量太小会使拟合训练集变得困难, 容量太大又会使模型记住太多训练集 **独有** 的一些属性. 一种改变模型的办法就是选择不同的 **假设空间(hypothesis space, 学习算法能够被允许产生的结果(函数)的集合(函数族))**, 例如对于线性规划, 算法的假设空间就是关于输入的所有线性函数, 我们可以通过把可能的结果设置为 **多项式(polynomials, 也就是添加 `!$x^i$` 作为新的线性规划的特征)** 而不是线性函数, 这样就能增加模型的容量. 例如 `!$\hat{y} = b + \sum_{i = 1}^{3} w_i x^i$` 就是一个三次方程, 如果把 `!$x^i$` 看作整体作为一个个特征(或则叫参数), 那么这样的一个方程仍然是线性函数.

![Figure 5.2][19]

> 上图从左至右分别是线性函数, 二次函数, 9 阶多项式拟合的结果(其中 9 阶多项式的可以完全拟合的结果有很多个, 这里只是其中的一种). 并且测试集是符合二次函数的点集. 可以看出, 左边欠拟合, 中间正好, 右边过拟合. 右边的 9 阶多项式使用 Moore-Penrose 伪逆去解那个正规方程(normal equation)得出的结果. 

容量不仅仅由模型的选择来决定, 模型规定了在调整参数训练目标时, 学习算法可以从哪些函数族中选择函数, 这被称为模型的 **表示容量(representational capacity)**, 但是往往找到最优解是困难的, 一般是找尽量减少训练误差的次优解, 加上这些限制条件(例如不要求找到最完美的优化解)之后, **有效容量(effective capacity)** 往往小于模型族的表示容量. 

提高机器学习模型泛化的现代思想可以依据 **简约思想(principle of parsimony)**: 在很多个都能解释观察现象的假设中, 我们应该选择最简单的那一个. (这个思想后面被统计学习理论的创始人形式化并准确化). 统计学习理论提供多种量化模型容量的方法, 其中最有名的就是 **Vapnik-Chervonenkis(VC) 维(demension)**, VC 维度量二元分类器(binary classify)的容量( **具体定义略** ). 

统计学习理论最重要的结论表明了训练误差和泛化误差之间的差距的上界随着模型容量的增长而增长并且随着训练样本的增多而减小. 但是这个解决很难用在深度学习中(而在机器学习中使用的很好), 因为这个界限经常很松(loose), 而且确定深度学习的模型容量相当困难(因为模型的有效容量受限于优化算法的能力, 并且对于一般的非凸(nonconvex)优化问题缺乏理论基础).

![Figure 5.3][20]

> 训练误差随着模型容量的上升而渐进与最小可能误差值(如果有的话, 一般很多实际应用中就会设定误差度量的最小值). 泛化误差以最优容量(optimal capacity)为最低点呈现 U-形.

为了处理任意高的容量的极端情况, 我们介绍 **非参数(nonparametric)**, 非参数模型没有参数模型的限制(在观察任意数据之前, 参数的个数就是有限且固定的). 有时候非参数模型仅仅是理论抽象的(也就是实际上做不出来), 不过我们可以设计一个函数复杂度与训练集大小正相关的函数作为非参数模型.

一个简单的非参数的例子就是 **最邻近回归(nearest neighbor regression)**, 最邻近回归直接简单的存储训练集中的 `!$\boldsymbol{X}$` 和 `!$\boldsymbol{y}$` , 然后在测试使用的时候, 就是简单的从训练集中找到一个与输入 `!$\boldsymbol{x}$` 距离(这个距离不仅仅是 `!$L^2$` 欧几里得距离, 还可以是例如 learned distance metrics)最近的一个点对应的 `!$y$` 返回. 显然, 在所有训练集上训练误差都是 `!$0$` 或 最小可能误差(如果对于训练集中相同输入不同输出的情况取平均的话).

我们还可以在一个参数会按照需求改变的算法中包裹一个参数学习算法来创建一个非参数学习算法. 

就算是理想模型(完全能够知道真实分布)都还是会遇到一些误差因为分布中的一些噪声. 例如在监督学习中, 有可能从 `!$\boldsymbol{x}$` 到 `!$y$` 的映射就是随机的或者是 `!$y$` 是一个还跟其他自变量关联的确定函数. 这种通过真实分布 `!$p(\boldsymbol{x}, y)$` 预测出现的误差被称为 **贝叶斯误差(Bayes error)** . 

**没有免费的午餐定理(No Free Lunch Theorem)**

考虑所有可能的数据生成分布并平均下来, 所有的分类算法在事先没有观察过的数据输入上都有相同的错误率. (因为就算是在某种分布下最优的模型, 在其他分布下就会得到更糟的结果, 差不多抵掉了).  也就是说没有适用于所有任意分布的"大一统"的最优学习算法.

但是现实生活中, 一般我们都会对可能的概率分布做一下假设(限制), 所以机器学习的主要目的不是研究出一个适用于所有情况(分布) 的学习算法, 而是仅仅关注于真实世界某一个特定问题(所以要给学习算法限定一个 **偏好(perference)** ), 找出一个近视的表现良好的分布.

**正则化(regularization)**

模型的效果取决于假设空间中的函数数量(也就是模型的表示容量)以及这些函数的具体形式.

对于线性回归, 我们可以添加对权重 `!$\boldsymbol{\omega}$` 的 **偏好**, 这种方法叫做 **权重衰减(weight decay)**  :

```mathjax!
$$J(\boldsymbol{\omega}) = \text{MSE}_{\text{train}} + \lambda \boldsymbol{\omega}^\top \boldsymbol{\omega}$$
```

> 其中 `!$\lambda$`(提前设定好的) 就是对权重的偏好, `!$\lambda = 0$` 表示对权重没有偏好, 对于取 `!$J(\boldsymbol{\omega})$` 最小值, `!$\lambda$` 越大表示偏好范数越小的权重(也就是权重中包含更少的特征).

跟一般的说, 正则化一个学习函数的模型时, 我们可以给代价函数(也就是上例中的 `!$J(\boldsymbol{\omega})$` ) 加上一个被称为 **正则化项(regularizer)** 的惩罚(penalty), 上例中的正则化项就是 `!$\boldsymbol{\omega}^\top \boldsymbol{\omega}$`

![Figure 5.5][21]

> 上图是一个 9-维多项式回归的例子, `!$\lambda$` 就能用来控制过拟合和欠拟合(曲线的导数/梯度).

表达偏好要比直接删减假设空间中的函数更加一般地控制模型的容量, 例如删去一个成员函数相当于对这个函数表达无限大的一个偏好(使其权重边的无限小)

显式或隐式得对模型表达偏好的方法统称为 **正则化** .

> *Regularization is any modiﬁcation we make to a learning algorithm that is intended to reduce its generalization error but not itstraining error.*

就如没有免费的午餐定理所述, 同样一般来说也没有最优的正则化形式. 深度学习的哲学(尤其是本书中)就是能够通过一个非常通用的(general-purpose)正则化形式来解决大部分的任务.

### (p) 1.4.3 超参数(Hyperparameters)和验证集(Validation Sets)

超参数不是由学习算法本身学习出来的(有时候这些设定很难优化, 所以一般是预先设定好的, 不过也可以做一个内嵌学习算法用来学习生成给外部学习算法的超参数).

大部分机器学习算法有 **超参数**, 例如前面说的多项式回归中有一个控制多项式次数的 **容量超参数(capacity hyperparameter)**, 还有上面的 `!$\lambda$` 超参数用于控制权重衰减的长度.

在训练过程优化超参数, 可以采用将训练集划分出 `!$20\%$` (典型地说) 作为 **验证集** (一个训练算法观察不到的样本数据集) , 另外 `!$80\%$` 还是跟原来一样的训练集用来学习出参数, 这个验证集用于在训练过程或训练之后估计泛化误差(因为验证集比较小, 所以通常验证集误差比训练误差小), 可以来更新超参数. 

> 实际中, 有时候测试集有点陈旧了, 而且里面包含的所有情况多差不多被学术界给研究透了, 我们就可能会得到一个看起来特别乐观的结果估计, 这不能反映真实性能. 反正要注意用更新更全面的测试集.

**交叉验证(cross-validation)**

如果验证集过小, 将会有问题, 因为这会造成在平均测试集误差的统计不确定性.

当样本过小的时候, 我们可以在原始数据上随机采样和分离出不同数据集来重复训练和测试. 最常见的手段是 **k-折(k-fold) 交叉验证**, 把原始数据分割成 `!$k$` 个不重合(nonoverlapping)的子集, 求 `!$k$` 次训练并测试的测试误差的平均作为最终的测试误差, 在 `!$i$` 次测试用, 将第 `!$i$` 个子集作为测试集, 其他的全部作为训练集.

> 上述 k-fold 方法带来的一个问题是不存在平均误差的方差的无偏估计(unbiased estimator), 但是通常用近似来解决. 

![Algo 5.1][22]

> `!$\boldsymbol{z}^{(i)}$` 在监督学习中是 `!$(\boldsymbol{x}^{(i)}, y^{(i)})$`, 在无监督学习中是 `!$\boldsymbol{x}^{(i)}$`

### (p) 1.4.4 估计(Estimators), 偏差(Bias) 和 方差(Variance)

**点估计(point estimation)**

点估计用来尝试找到一些感兴趣的量的单个最优预测, 这个感兴趣的量可以是参数模型中的一个参数或参数向量, 甚至是整个函数(这时候还叫做 **函数估计()** ).

> 为了区分参数的估计和它们的真正的值, 我们将参数 `!$\theta$` 的点估计记住 `!$\hat{\theta}$`.
11
> 假设`!$\theta$` 是固定且未知的, 而由于数据采样是随机的, 所以点估计 `!$\hat{\theta}$` 的值也是随机的, 也就是一个随机变量.

例如, 对于独立同分布(i.i.d)数据点集 `!$\boldsymbol{x}^{(1)}, \boldsymbol{(m)}$`, 一个点估计或统计(statistic)可以是任意函数: `!$\boldsymbol{\hat{\theta}}_m = g(\boldsymbol{x}^{(1)}, \boldsymbol{x}^{(m)})$`. 并且没有要求点估计的值要和真实值接近(当然是越接近越好), 也没有要求函数的值域必须在真实值的合法范围, 这样能够让点估计的设计更加的灵活.

**偏差(Bias)**

一个估计的偏差定义为: 

```mathjax!
$$\text{bias}(\hat{\theta}_m) = \mathbb{E}(\hat{\theta}_m) - \theta$$
```

当 `!$\text{bias}(\hat{\theta}_m) = 0$`, 称为 **无偏(unbiased)**, 如果 `!$\lim_{m \rightarrow \infty} \text{bias}(\hat{\theta}_m) = 0$`, 就叫做 **渐进无偏(asymptotically unbiased)** . 

**例子, 伯努利分布(Bernoulli Distribution)**

均值为 `!$\theta$` 的伯努利分布的独立同分布样本 `!$x^{(1)}, \ldots, x^{(m)}$`.

```mathjax!
$$P(x^{(i)}; \theta) = \theta^{x^{(i)}} (1 - \theta)^{1 - x^{(i)}}$$
```
一种常见的估计就是

```mathjax!
$$\hat{\theta}_m = \frac{1}{m} \sum_{i=1}^{m} x^{(i)}$$
```

结合两式, 计算其偏差

```mathjax!
$$
\begin{equation}
\begin{split}
\text{bias}(\hat{\theta}_m) &= \mathbb{E} (\hat{\theta}_m) - \theta \\
&= \frac{1}{m} \sum_{i=1}^{m} \mathbb{E} \left[ x^{(i)} \right] - \theta \\
&= \frac{1}{m} \sum_{i=1}^{m} \sum_{x^{(i)} = 0}^{1} x^{(i)} \theta^{x^{(i)}} (1 - \theta)^{1 - x^{(i)}} - \theta \\
&= \frac{1}{m} m \theta - \theta \\
&= 0
\end{split}
\nonumber
\end{equation}
$$
```

所以可以看出, 该估计是无偏差的.

同样的类似于上面过程的还可以证明估计高斯分布 `!$\mathcal{N} (x^{(i)}; \mu, \sigma^2)$`的均值 `!$\mu$` 的估计的一般方法 `!$\hat{\mu}_m = \frac{1}{m} x^{(i)}$`(样本均值) 是无偏的.

同样我们可以用样本方差来估计真实方差, 但是它是有偏差的, 证明如下

```mathjax!
\begin{equation}
\begin{split}
\text{bias}(\hat{\sigma}_m^2) &= \mathbb{E} [\hat{\sigma}_m^2] - \sigma^2 \\
&= \frac{1}{m} \mathbb{E}\left[ \sum_{i=1}^{m} \left( x^{(i)} - 2x^{(i)} \hat{\mu}_m + \hat{\mu}^2_m \right) \right] - \sigma^2 \\
&= \frac{1}{m} \mathbb{E} \left[ \sum_{i=1}^m \left( x^{(i)} \right)^2 - 2 \sum_{i=1}^m x^{(i)} \hat{\mu}_m + \sum_{i=1}^m \hat{\mu}_m^2 \right] - \sigma^2 \\
&= \frac{1}{m} \left( \mathbb{E} \left[ \sum_{i=1}^m \left( x^{(i)} \right)^2 \right] - \mathbb{E} \left( 2m \hat{\mu}_m^2 + m \hat{\mu}^2_m \right) \right) - \sigma^2 \\
&= \frac{1}{m} \sum_{i=1}^m \mathbb{E} \left[ (x^{(i)})^2 \right] - \mathbb{E} [ \hat{\mu}^2_m ] - \sigma^2 \\
&= \mathbb{E}^2 [ x ] + \mathtt{Var} [ x ] - \mathbb{E}^2 [ \hat{\mu}_m ] - \mathtt{Var}[\hat{\mu}_m] - \sigma^2 \\
&= \sigma^2 - \mathtt{Var}[\hat{\mu}_m] - \sigma^2 \\
&= - \mathtt{Var} \left[ \frac{1}{m} \sum_{i=1}^m x^{(i)} \right] \\
&= - \frac{1}{m} \sum_{i=1}^m \mathtt{Var} [ x^{(i)}] \\
&= - \frac{1}{m} \sigma^2
\end{split}
\nonumber
\end{equation}
```

因为偏差不为 `!$0$`, 所以这个估计是有偏差的, 修正为无偏差估计只需要换一下这个归一公式的系数

```mathjax!
$$\tilde{\sigma}^2_m = \frac{1}{m-1} \sum_{i=1}^m \left( x^{(i)} - \hat{\mu}_m \right)^2$$
```

**方差(Variance)** 和 **标准差(standard error)**

类似于计算估计量的数学期望, 我们还会计算其方差 `!$\mathtt{Var} (\hat{\theta})$`, 以及标准差(方差的平方根) `!$\text{SE}(\hat{\theta})$`.

因为估计在统计上会随着训练样本的变化而发生变化, 所以方差衡量这些估计离真实值的变化程度的大小.

不过不管是用样本方差的平方根还是方差的无偏估计(也就是上面高斯分布的那个修正的方差估计)的平方根来估计标准差都是有偏的(都倾向于低估(underestimate)真实方差), 不过方差的无偏估计的平方根低估的少一点, 对于数量很大的样本, 还是有意义的.

例如这里用样本均值的方差的平方根估计其标准差, 由前面高斯分布的方差的有偏估计的推导, 很容易得出

```mathjax!
$$
\text{SE} (\hat{\mu}_m) = \sqrt{\mathtt{Var} \left[ \frac{1}{m} \sum_{i=1}^{m} x^{(i)} \right]} = \frac{\sigma}{\sqrt{m}}
$$
```

又由 **中心极限定理(central limit theorem)**, 这些服从正态分布的随机变量的均值也一样服从正态分布. 均值的标准差在机器学习实验中很有用, 例如均值为 `!$\hat{\mu}_m$`, 方差 `!$\text{SE}(\hat{\mu}_m)^2$` 的高斯分布的以均值 `!$\hat{\mu}_m$` 为中心 `!$95\%$`(也就是该高斯函数在该置信区间上的积分是整个定义域上积分的值的 `!$95\%$`) 的 **置信区间(confidence interval)** 为 `!$\left( \hat{\mu}_m - 1.96 \text{SE}(\hat{\mu}_m), \hat{\mu}_m + 1.96 \text{SE}(\hat{\mu}_m) \right)$`  
  
  算法 A 比算法  B 好通常是指算法 A 误差的 `!$95\%$` 置信区间的上界都比算法 B 的 `!$95\%$` 置信区间的下界还小.
  
  **例子: 伯努利分布**
  
  同样类似上面的例子, 我们可以求得均值的方差
  
  ```mathjax!
  \begin{equation}
  \begin{split}
  \mathtt{Var}(\hat{\theta}_m) &= \mathtt{Var} \left( \frac{1}{m} \sum_{i=1}^m x^{(i)} \right) \\
  &= \frac{1}{m^2} \sum_{i=1}^m \mathtt{Var}(x^{(i)}) \\
  &= \frac{1}{m^2} \sum_{i=1}^m \left( \mathbb{E}\left[ \left( x^{(i)} \right)^2 \right] - \mathbb{E}^2 [x^{(i)}] \right) \\
  &= \frac{1}{m^2} \sum_{i=1}^m \left( 1^2 \Pr (x^{(i)} = 1) + 0 - \left( 1 \Pr (x^{(i)} = 1) + 0 \right)^2 \right) \\
  &= \frac{1}{m} \theta (1 - \theta)
  \end{split}
  \nonumber
  \end{equation}
  ```
  
  [1]: ./images/1516613842738.jpg
  [2]: ./images/1516613842738.jpg
  [3]: ./images/1516621710096.jpg
  [4]: ./images/1516624370367.jpg
  [5]: ./images/1516624435832.jpg
  [6]: ./images/1516624998426.jpg
  [7]: ./images/1516685167266.jpg
  [8]: ./images/1516697795762.jpg
  [9]: ./images/1517034158757.jpg
  [10]: ./images/1517036009925.jpg
  [11]: ./images/1517112955868.jpg
  [12]: ./images/1517134589413.jpg
  [13]: ./images/1517209835378.jpg
  [14]: ./images/1517211421206.jpg
  [15]: ./images/1517301723997.jpg
  [16]: ./images/1517302544553.jpg
  [17]: ./images/1517399058008.jpg
  [18]: ./images/1517580400888.jpg
  [19]: ./images/1518084054593.jpg
  [20]: ./images/1520217653005.jpg
  [21]: ./images/1520248674777.jpg
  [22]: ./images/1520316296099.jpg