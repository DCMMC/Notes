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
如果 `!$A$` 是一个 `!$m \times n$` 矩阵, 则 `!$U$` 是 `!$m \times m$` 矩阵, `!$D$` 是 `!$m \times n$` 矩阵, `!$V$` 是 `!$n \times n$` 矩阵, 其中 `!$U$` 和 `!$V$` 都是正交矩阵, `!$D$` 是一个对角矩阵, 但不一定是方阵.

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

早期的概率论是用来分析事件的频率, 并且这类事件都是可以(趋近于无限)重复发生的. 而如果是一个医生给一个病人看病并且告诉病人他有 40% 的概率患流感, 因为病人不能被复制, 所以这不是一个可重复事件. 在这种情况, 我们使用概率来表示 **信念度(degree of belief)**, 其中 1 表示病人肯定患有流感, 0 表示病人肯定没有患流感, 前者直接与时间发生的比率(rate)相关, 被称为 **频率派概率(frequentist probability)**, 而后者
涉及到 **确定度水平(qualitative levels of certainty)**, 被称为 **贝叶斯概率(Bayesian probability)**. 如果要列出一些关于不确定性(uncertainty)的常识推理(common sense reasons)中我们希望要有的性质(property), 要满足这些性质的唯一方法就是将贝叶斯概率的行为和概率派概率完全等同. 

概率论提供了一套形式化的规则, 用来在给定一些命题(proposition)的似然(likelihood)后, 计算其他命题为真的似然.

### (p) 1.2.2 随机变量(Random Variables)

**随机变量** 就是能够随机的取不同值的变量. 随机变量可以使 **离散的(discrete)** 或者是 **连续的(continuous)**, 离散随机变量有有限或可数的无限个状态(states, 不一定是数值), 而连续随机变量与实数值相关.

> 随机变量和可能的取值都用小写无格式字母表示

### (p) 1.2.3 概率分布(Probability Distributions)

**概率分布** 描述变量的值怎么样从它们的各种状态中选取(具体的方法取决于是离散随机变量还是连续随机变量).

离散随机变量的概率分布(probability distribution)被称为 **概率质量函数(probability mass function, *abbr.*, PML, 有些国内教材翻译为概率分布律)**, 一般用大写字母 *P* 表示, 一般用变量的标识来区分不同的概率质量函数而不是通过函数名称来区分, 例如 *P*(x) 和 *P*(y) 一般来说就表示不同的概率质量函数. 还可以简记作 x ~ *P*(x) , 对于 x 的某个状态值 `!$x_1$`, 我们可以用 *P*(`!$x_1$`) 或 *P*(x = `!$x_1$`) 表示这个状态的概率值. 对于有多个随机变量的概率分布 , 叫做 **联合概率分布(joint probability distribution)**, 例如 *P*(x = *x*, y = *y*) 或 *P*(*x*, *y*).

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

一个相关的能够允许我们自定义概率质量的边界点在任意位置的分布为 **Laplace distribution**: `!$\mathtt{Laplcae}(x; \mu, \gamma) = \frac{1} {2 \gamma} \exp (-\frac{|x-\mu|}{\gamma})$`

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
$$P(x | y) = \frac{P(x)P(y | x)}{P(y)}$$
```

> 可以用  `!$P(y) = \sum_x P(y | x) P(x)$` 来求的 `!$P(y)$`, 所以我们并不需要事先知道 `!$P(y)$` 的信息.

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


 
  [1]: ./images/1516877903228.jpg
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