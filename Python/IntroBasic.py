#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# 上述两行注释放在源码顶部分别用于指定 *nix 环境能够直接把源程序当做脚本来调用, 以及限定该文件编码

###########################
# Python3 简介和基础      #
###########################

# 介绍:
# 主流 Python 解释器是 CPython(用 C 开发的), 同样也有 JVM 上的 Jython 和 JIT技术的 PyPy. 学习过程中一般使用
# IPython(基于 CPython, 不过交互性更好, 可以在交互界面查看更多的信息)
# 在 Ipython 下, 使用 %lsmagic 查看所有解释器有关的 magic 命令(有点像 Linux 命令), 例如
# %whos 查看当前变量空间, %reset -f 重置变量空间.
# 并且在函数后面加一个 '?' , 可以查看该函数的帮助, '??'查看函数源代码, '_'
# 表示上个 cell (上个执行的代码)的输出结果, '!' 开头调用系统命令
# %%writefile [file name] 写入到 文件
# $run [filename] 从文件运行

# 单行注释
# py 语法其实没有多行注释, 不过可以使用下面这种作为多行注释, 尤其是 function doc
"""
多行注释
"""

# array
from numpy import array

# 一个有趣的图形化的例子
# 需要 sudo pip install -i https://pypi.tuna.tsinghua.edu.cn/simple matplotlib
# 然后 sudo pacman -S tk, 安装 matplotlib
import matplotlib.pyplot as plot
plot.plot(range(20), range(20))
plot.show()

##########################################
# 这里主要讲一些内置类型和其中的一些方法 #
##########################################

# Python 是弱类型OOP语言, 解释器会自动寻找最适合的类型, 但其实仍然是有类型的, py 的数据类型:
# 可以用 type([var]) 来获取当前该变量的类型, 模块 types 中包含了更多的关于类型的信息
# type 用于类型名时返回 type, 用于实例对象的时候才返回具体类型.
# 函数名和类型名都可以作为变量名, 这是极其危险的行为, 尽量避免, 但是关键字作为变量名会报错
# int = 1

# 基本类型, 都是不可变类型(Immutable), 这样能够避免赋值给其他变量之后, 原对象改变造成所有关联的引
# 用造成改变, 还有一些非基本类型也是不可变类型, 目的都是一样的
# 整数(int), 可以用 0x 前缀, 表示十六进制, 同样可用 0b, 0X, 0B, 0前缀, 没有范围限制
integerVar = 0x50ffffffffffffffffffffffffffffffffffffffffffffffffffff
# 在py2中, 整数类型有范围限制, 后缀 L 可以强制作为长整数(没有 L 但是大小很大也会变成长整数, )
# int2 = 88888L
# 浮点数(float), 可采用科学计数法, 并且有一定精度范围
# 不过有一点需要注意, 默认浮点数都是转化为精度内最靠近的二进制浮点数, 难免会误差, 最典型的例子就是
# 0.1+0.2, 可以使用 round()来近似并得到正确十进制数值, 并且print也会自动修正这个误差
floatVar = 3.1415926535897932380011001100110011001100110011001e-3
print('float 有精度范围:', floatVar)
# int 和 float 可以用构造方法来从字符串来构造数值类型

# 字符串(str), 用 '' 或 "" 或 '''多行字符串'''包围都行, 其中字符可用转移字符, 使用r前缀表示不转义
字符串Var = r'''string\n
多行'''
# 多行字符串还可以使用 每一行字符串之间只有空白字符 以及用 \ 来作为换行的方法
字符串Var = 'hello '
'world' \
    ", 你好"

# 布尔值(bool), py 大小写敏感
boolVar = True
# 元组(tuple), 元组可以嵌套, 可以省略小括号, 但是如果只有1个或0个元素, ()不能省略, 并且只有1个元素
# 的时候, 为了避免歧义, 必须在括号里面加上一个逗号!
# 元组经常作为 C 风格 format 的参数, 在 dict 中作为 key. 使用构造器可以从 list 构造一个 tuple
tupleVar = 1, 'str', (1, 3, 4), ['demo', 2]
tupleVar = (1, )
# 复数(complex), 其中 J 或 j 表示虚部 `!$i$`
complexVar = 1 + 2J

# 非基本类型但是是内置类型, 对这些导出类型的赋值, 都是拷贝引用, 也就是改变对象会影响所有指向该对象
# 的变量, 也就是说他们是可变的(muatble)
# 列表(list), 中括号括起来, 中括号不能省略, 有序集, 可以相当于数组
listVar = [1, 1.2, 'hello']
# list 可以从 range 中生成, range表示范围的一种类型, 第一个参数默认缺省为0, 例如 range(5) 等价
# range(0,5), list(range(0,5)) 将会生成从 0 到 5(不包括5) 的整数序列

# 字典(dict), dict 的 key 必须是 Immutable 对象, 并且 key 不能重复, 重复的 key 只会更新老 key 的
# value
dictVar = {'dog': 5,'dog':2, 'pig': 8} # nopa F601
# 集合(set), 集合中只有 key, 没有 value, 并且 key 必须是 Immutable 对象, dict 和 set 应该都是用的
# 哈希表, 所以 key 也不能重复
setVar = {1, 2, '4'}
# set 和 dict 都会打乱加入时的顺序(进而会按照 key 有序的存放)
# 特殊值: None(类型为NoneType), 表示空值
noneVar = None

# 这些内置类型除了可以使用字面量直接赋值之外, 也可以使用 [typename]([value])来赋值
listVar = list([dictVar, boolVar])

# 除了上述直接有字面量提供的内置类型, py 还有一些其他内置类型, 如 range 存储整数范围,
# 还有 frozenset 类似于 set , 不过是像 tuple 的 set, 因为里面的元素一经创建就不能添加删除
# 注意: python3 的 range 类似于 python2 的 xrange, 也就是说 py3 的 range 不是 list, 只是
# 存储的 lower 和 upper 和 step, 只在用的时候才计算, 不会在创建的时候创建所有的元素

# 非内置类型
# Numpy 数组, 一种自定义类型(Object Oriented Classes)
arrayVar = array([1, 2, 3])

# py 中只有变量, 没有真正的常量, 约定完全大写的变量为常量
PI = 3.14159265359

# 对于有迭代器的类型, 还可以使用负数下标表示倒数第几个元素, 例如 a[-1] 表示倒数第 1 个元素
print(arrayVar[-1])

##########################
# Python 的赋值机制      #
##########################
# python 的所有变量都是对象的引用
# 可以用 id(var) 来查看变量的 id (类似于地址)
# 还可以用 is 关键字查看两个变量是否指向同一个对象
a = 500
b = a
print('a is b', a is b)
# 对于很小的 int, str 之类的, 还有像 Java 的String 一样有常量池, 也就是两个变量就算不是有复制关系
# 也有可能指向同一对象
x = 5
y = 5
print('id(x) == id(y)', id(x) == id(y))

######################
# 基本类型操作       #
######################
# 数值类型的 + - * % 和 C-family 和 Java 没啥区别, 不过整数之间的 / 返回的是浮点数, 而 //(地板除,
# floor divide)则会将结果截尾整, 还有这些操作加上 = 号的原地(in-place)计算, 例如 var += 1, 但
# 是如果 // 有一个 Operand 是浮点数, 那么结果是浮点数(小数部分为0).
# 浮点型也能使用 % 取余
# py 没有对数值的 ++ --操作

# 布尔操作: and, or, not
# 赋值语句类似于 Java, 对于基本类型, 是直接复制值, 而对于非基本类型, 是复制引用
# ** 运算是幂运算, 例如 a ** b 表示数学上的 `!$a^b$`

# 创建的比较运算有 <, >, <=, >=, ==, !=, 并且 py 还支持链式比较
var = 5
var = 1 <= var < 5

# str 有很多内置函数(方法): split, replace, upper, lower, join(str_sequence) (用该str对象存储的
# 的字符串来将序列 str_sequence(必须是 iterable 的变量) 中的一个一个元素连接起来, len计算字符个数,
# 还有去除两头的空格 strip(类似于 java 的trim), 去除左边的空格 lstrip 和 去除右边的空格 rstrip
# 这些方法.
# 将任意变量转化为 str: str(var) 和 repr(var) 如果其中参数是数值类型不会进行二进制转十进制误差修正
# 还可以使用hex, oct, bin这些方法, 将整数类型转化为按照不同进制字符串

# 编码:
# 字符(长度为1的字符串) 和 对应编码的相互转换: ord 和 chr 方法
# str 默认编码为 unicode, 不过存储在文件中我们经常需要转化为 bytes 数组, 用 'b' 前缀可以限制字符串
# 中每个字符都是一个 byte(也就是ASCII编码)
# 同样可是使用 str中的 encode('编码类型') 方法来编码成 bytes, 使用 decode('编码类型')来从 bytes中
# 解码
'小李'.encode('utf-8').decode('utf-8')

# 格式化字符串
# 可以使用 C 风格格式化, 不过是 '等格式化字符串' % 替换内容的元组 的形式, 占位符有 s,d,f, 不管什
# 么时候 %s 总有用, 例如
'%s 学 py  %.1f 1%%' % ('kevin', 3.6)
# 还可以使用 str 的format 方法, 用占位符 {0}, {1}, ...
'Hello, {0}, 成绩提升了 {1:.1f}%'.format('小明', 17.125)


# list 中的方法: pop(index = len - 1) 删除列表中指定index元素(默认删除最后一个)并返回, 要改变列表
# 中某一个元素, 直接通过索引获取然后赋值就可以了, 也可以用全局方法 del var[index or slice]
# list 还可以分片, var[lower:upper:step], step 默认为1(可以省略最后一个 :step), 也就是连续的元素,
# upper 为exclusive, lower 为inclusive, 当 step 为1时, 默认 lower为0, upper 为 n,
# 而step 为负时, 默认lower 为 -1, upper 为 -len(var)-1, 分片相当于该片段的引用, 对分片直接
# 修改相当于原对象, 还可以直接将该分片用其他相等长度的 list 取代, 删除代码片可用 del 方法
'hello'[-1::-2]
# list 还有 + 连接两个列表(类似于成员方法 append), 以及 * [integer number] 将列表重复多少次的操作
print(listVar * 3)
# list的成员方法 count 可以用来记数参数在 list 中的出现次数, 成员方法 index 返回 参数 在list 中的
# 序号, 不存在就抛出异常, 成员方法 insert(index), 成员方法 remove(var) 移除元素 var, 不存在就
# 抛异常, 还有成员方法 sort 和 reverse

# 对于 list, dict, set 这些, 都可以用 in 和 not in 来判断元素是否存在

# 对于 dict, 可以用索引[[key]] 来访问和修改 value, 还可以用 [key] in [dict] 来返回 dict 中是否存在
# 该 key 的 bool 类型, 同样可以使用 dict 内置方法 get(key, NoneValue = None) 来访问 key 对应的
# value, 如果不存在就返回 NoneValue 指定的值, 默认为 None, 内置方法 [value] pop(key) 可以删除该
# 值键对, 成员方法 values 和 keys 分别返回所有 value 和 key 的 list, items 则返回值键对组成的二元
# 组的 list. 虽然可以通过索引直接更改单个 value, 但是有时候我们需要更改多个,
# 这时候可以用成员方法 update(new_dict) 来更新多个和添加多个(new_dict中可以用原dict没有的key)

# set 可以用 add 成员方法来添加, 可以重复添加, 但是没有效果, 成员方法 remove(key) 删除指定 key,
# 还可以使用 & 和 | 将两个集合求并和交之后的新集合, 差 - , 对称差 ^ (交减去并)操作.
# 集合之间的包含关系: a.issuperset(b) 等价 a >= b 判断 b 是否是 a 的子集, 同样的
# a.issubset(b) 或 a <= b 判断 a 是否是 b 的子集, 用 a < b 判断 a 是否是 b 的真子集
# discard 成员方法类似于remove, 只不过在元素不存在的时候不会报错, 还有
# a.difference_update(b) 从 a 去除掉所有属于 b 的元素

# tuple 有一个特别有趣的操作: tuple 与 tuple 之间的复制
# tuple1  = tuple2 会将 tuple2 中对应元素赋值给 tuple1 中对应的变量(注意是变量, 不能赋值到字面量
# literal)
# 例如:
x, y = 1, 2

###########################################################################
# 查看一个变量当前所属类型中所有成员函数的方法: dir                       #
###########################################################################

# 输出函数 print(), 并且最后会输出一个回车
# 多参数之间的逗号输出时变为空格
print('100 + 50 =', 100 + 50)
# 上面这个是 py3 风格的 print, py2 的 print 形式跟 py 3 有点不一样,
# 没有括号, 并且参数跟函数名之间空格隔开:
# print a, b

# 输入函数 input([提示信息]), 读行(包括回车), 返回一个 str 类型(不包括回车)
sth = input("prompt: input sth: ")
print('input is', sth)


# 控制语句

# py 用缩进来处理代码块, 当一条语句以 ':' 结尾, 下面的语句按照缩进作为代码块
# 虽然 py 有点格式要求, 但是仍然是格式自由的语言
# 例如
if (integerVar > 0):
    print('integerVar >= 0')
    print('next line')
# 条件还可以像 C 一样, 非零数值、非空字符串、非空list等都可以相当于 True
elif integerVar:
    print('integerVar < 0')
else:
    print('integerVar == 0')

# 列表循环, for [temp var] in [listvar] 语句
for s in listVar:
    print(s)

# for in 必须是作用在可迭代的类型, 所有可迭代类型有一个公共基类: collections.Iterable
# (需要导入 collections包), 可以用 isinstance 判断是否是可迭代类型
# 对于 dict 类型的迭代, 可以使用 for keyVar,valueVar in dictVar 的形式
# 还有一点就是 for in 类似于 java/cpp 的 for each, 但是我们有时候同样需要带索引(下标)的 for
# , 这时候我们可以使用 py 内置函数 enumerate(Iterable), 返回 index:value 的 enumrate(dict) 类型

# 列表推导式(List comprehensive), 就是在[] 中使用 for in 语法
listVar = [str(char) + ',' for char in 'hello']
# 这些列表推导式其实就是 for 循环的一种变式, 所以甚至可以用多重 for 循环
# 列表推导式可以带条件,用 for in if, 并且也可以用来
# 推导创建 dict, set 这些
dictVar = {x: x**2 for x in [-2, 0, 1, 3] if x >= 0}
print('dictVar', dictVar)
# 可以直接把函数中的 iterable 参数直接用列表推导式代替
# 这样还可以减少一次中间产生临时列表变量的浪费
# 性能比较:
# x = range(100000)
# %timeit total = sum([i**2 for i in x])
# %timeit total = sum(i**2 for i in x)
# 然而我发现性能没有差距... 说明 py3 的优化很不错

# 生成器(generator)
# 和 List comprehensive 类似, 也就是在 () 中使用 for in 语法
# generator 也是一种 Iterable 类型, 但是 generator 有一个显著特点, 就是其元素是在迭代它的时候, 边
# 迭代边计算生成的(这被成为惰性计算(lazy evaluation)), 这无穷数列带来了可能. 访问 generator
# 对象当前 index 的下一个元素使用内置函数 next(generatorVar) 就可以了.
# generator 就是 Iterator(Iterator 有是 Iterable 的之类) 的子类

while s:
    print(s)
    s = 0

# break 和 continue 同 C-family/Java
