#!/usr/bin/env python3
# -*- coding: utf-8 -*-

############################################################
#                                                          #
# 函数和类                                                 #
# 参考: https://docs.python.org/3/library/functions.html   #
# 2018.2.11                                                #
############################################################

# 导入 math 包
import math
# 导入 reduce
from functools import reduce
# time
import datetime
# functools.wraps(func), 我们也可以用 from functools import wraps 来直接导入 wraps
# 到当前命名空间
import functools

# py 的函数模型类似于 cpp, 最后几个参数可以有缺省值
# 函数名一般遵循 cpp 的全小写风格
# 这里是定义一个全局函数

#################################
# 计算L^n 范数, 默认为 2        #
# @param array_x 向量           #
# @param n = 2 Ln范数           #
#################################
def norm(array_x, n=2): # NOQA 302
    # code block
    # 一般使用 isinstance 来检查参数
    if n < 0 or isinstance(n, int) and isinstance(n, float):
        return None
    result = 0.0
    for x_i in array_x:
        result += abs(x_i) ** n
    return result ** (1 / n)


print(norm([3, 4]))

# 对于函数参数缺省值, 一定要注意不能将可变类型作为缺省值! 因为函数被定义出来的时候, 默认参数的值就
# 被计算出来了, 因为默认参数也是一个变量, 并且指向缺省值这个对象, 如果改变了参数的内容, 下一次调用
# 该函数的时候, 该参数指向的对象已经是修改过的了, 例如:
def add_end_wrong(l=[]): # noqa E302
    l.append('end')
    return l


add_end_wrong()
print(add_end_wrong())


# 所以正确的使用默认列表的方法是:
def add_end(L=None):
    if (L is None):
        L = []
    L.append('end')
    return L


# 对于空代码块, 可以使用 pass 作为占位符, 例如
def empty_fun():
    pass


# 对于上述类似的空函数, 没有pass 会报错
# 同样的, if for while 等等需要代码块的都可以使用 pass 作为占位符


def move(x, y, step, angle=0):
    """
    py允许多返回值(其实不是真正的多返回值, 因为返回的是一个 tuple)
    """
    nx = x + step * math.cos(angle)
    ny = y - step * math.sin(angle)
    return nx, ny


# python 不支持方法重载, 理由: 1. 大多数情况下函数重载的目的就是对于不同类型参数实现相同功能,
# 然而 py 本身就是弱类型, 所以不需要. 2. 功能相同, 但是参数个数不同, py 有参数缺省值, 所以依然不需要
# 如下的代码就是非法的
# def move(x, step, angle=0):
#     """
#     函数重载
#     """
#     new_x = x + step * math.cos(angle)
#     return new_x
#

x, y = move(100, 100, 60, math.pi / 6)
print(type(move(100, 100, 60, math.pi / 6)))

# 注意: 函数执行完毕也没有return语句时，自动return None, 类似于过程


def calc(*numbers):
    # 可变参数, 只需要在变量前面加上一个 * 号
    sum = 0
    for n in numbers:
        sum = sum + n * n
    return sum


# 如果要把一个 list 或者 tuple 直接当做可变参数传入函数, 可以实参变量前面加一个 *
list0 = [1, 2, 3]
calc(*list0)


def add(x, **kwargs):
    """
    关键词参数, 形参名前面加 ** , 相当于字典
    """
    total = x
    for arg, value in kwargs.items():
        print("adding ", arg)
        total += value
    return total


# 关键词参数, 调用参数中的变长部分就是 key=value, ... 的形式
print(add(10, y=11, z=12, w=13))

# 使用函数迭代(多个)列表生成行新列表:
# map(func, *iterables), 其中后面列表的数量对应与func中参数的个数, 并且每个列表中的元素个数要相等
print(list(map(lambda x, y: x + y, [0, 1, 2], (1, 2, 3))))
# 像上面这个 map 函数一样能够接搜另外一个函数(在 py 中函数名本身也是一个变量, 可用于赋值给其他变量
# )作为参数的函数成为高阶函数. map 返回的是一个惰性序列(Iterable)
# 并且这里还使用了 lambda 表达式. 格式为 lambda [参数, 逗号隔开]: [返回值]
# 并且 py 的 lambda 表达式只能一行, 并且只能有一个返回的表达式, 不能有函数体, 感觉比 java 的要
# 受限一点
# lambda 函数又叫匿名函数

# functools 包里面还有类似与 map 的函数 reduce, 这个函数用于递归迭代, reduce把一个函数作用在
# 一个序列[x1, x2, x3, ...]上，这个函数必须接收两个参数，reduce把结果继续和序列的下一个元素做
# 累积计算，其效果就是：
# reduce(f, [x1, x2, x3, x4]) = f(f(f(x1, x2), x3), x4)
# 利用 recude 可以很方便的求 prod 连乘


print((lambda *arr: reduce(lambda x, y: x*y, arr))(*[1, 2, 3]))

# 函数式编程是一种抽象程度很高(往往效率也没这么高)的变成范式, 其哲学: 纯粹的函数式变成没有变量,
# 任意一个函数, 只要输入是确定的, 输出也就是确定的, 这种纯函数没有副作用, 对于一般的非函数式编程
# 语言, 函数内部的变量状态往往是不确定的, 同样的输入可能造成不同的输出, 所以这种函数是有副作用的
# 函数式编程允许函数作为参数, 也允许返回一个函数:
fn = (lambda x: lambda y: x+y)(1)
fn(2)

# py 同时提供 filter(function or None, iterable) 用于通过function返回的值当做判断条件来筛选list
# 并且 filter 返回的结果也是惰性计算(lazy evaluation), 也就说在在访问其中的那一个元素的时候, 才会
# 计算出那一个元素, 而不是一开始就全部计算好, 这给无限长序列带来了可能

# sorted 排序函数(高阶函数): sorted(iterable, key=None, reverse=False), 其中 key 就是指定
# key 的函数, 例如 str.lower 就是一个

# yield 关键字用于给函数创建 generator 对象, 例如我们要创建斐波那契数列, 我们可以直接在函数中
# print, 但是这样复用性不强, 进而我们可以用 list 保存结果, 但是这样的话, 每一个元素都要占用空间,
# 如果这个 list 很大的话, 很浪费空间, 所以这时候 yield 就派上了用场, yield 用在函数中, 函数相当
# 于返回一个 generator 对象, 在被 for in 调用的基本过程如下:
# 迭代第一个元素的时候, generator 对象运行生成它的函数, 直到遇到了 yield 语句, 将会将第一个生成
# 的元素返回给 for in, 然后在那一句暂停, 直到 for in 需要第二个元素的时候, 函数又继续运行, 直到
# 又遇到了 yield
# generator 类具体的结构后面讲 class 会提到
# 例如, 埃氏筛法求素数:


def odd_iter():
    """
    从3开始的奇数序列
    """
    n = 1
    # 别担心, 这其实在调用函数的时候不会死循环
    while True:
        n += 2
        # yeild 语句只能有一个变量, 其中不能有表达式
        yield n


def primes():
    """
    生成无限长素数序列
    """
    yield 2
    it = odd_iter()  # 初始序列
    while True:
        n = next(it)
        yield n
        it = filter(lambda x: x % n > 0, it)  # 将序列中所有当前 n 的倍数都剔除掉


# 打印 1 到 10 中的素数序列
for p in primes():
    if p <= 10:
        print(p)
    else:
        break


# 递归
# 汉诺塔问题: 主要思想就是 divide and conquer(分治算法)
# 要像把 a 中所有盘移动到 c, 可以像把除了最大之外的盘按照规定的要求移动到 b, 只把最大的移动到 c
# 这时候问题就变成如何把 a 中除了最大盘之外的盘全部移动到 b 去. 下一步就是相当于把 b 中所有盘
# 通过 a 这个缓存盘移动全部移动到 c, 以此类推


def hanoi(n, src='A', cache='B', dst='C'):
    """
    @param n 初始时 src 中的盘的个数
    """
    if n == 1:
        # 递归终止条件
        print(src, '-->', dst)
        return
    # 将问题缩小为 src 中除最大盘之外的 n-1 块盘移动到 cache 中去, 这样 src 中最大的盘就可以放到
    # dst 中去了, 并且这个子问题将 dst 作为缓存
    hanoi(n - 1, src, dst, cache)
    # 打印 src 中最大盘移动到了 dst
    print(src, '-->', dst)
    # 现在我们需要做的就是把 cache 中的 n-1 块盘移动到 dst 去, 并把 src 作为缓存
    hanoi(n - 1, cache, src, dst)


# 期待输出:
# A --> C
# A --> B
# C --> B
# A --> C
# B --> A
# B --> C
# A --> C
hanoi(3, 'A', 'B', 'C')

# 可以在函数中定义函数, 因为函数也是一个变量嘛


def lazy_sum(*args):
    # 并且每次调用该 lazy_sum , 都会返回一个新的 sum 函数变量
    def sum():
        ax = 0
        #####################################################################
        # Ref:
        # https://www.ibm.com/developerworks/cn/linux/l-cn-closure/index.html
        # 像这样一个引用了外部自由变量的函数可被成为 **闭包(Closure)**.
        # **闭包** 的具体定义(并没有一个形式化的定义, 这里只是众多解释中一个比较确切的定义)是:
        # 闭包是由函数和与其相关的引用环境组合而成的实体.
        # 因为函数在一定义的时候就已经被确定了下来, 但是闭包因为有外界与之关联的应用环境, 所以
        # 上述说法更加准确. 闭包可能会在执行时发生变化.
        #####################################################################
        for n in args:
            ax = ax + n
        return ax
    return sum

###############################################################################
# 在返回闭包的时候, 一定要注意闭包中不能引用循环变量,                         #
# 或者是后续会发生变化的变量, 类似于                                          #
# Java 中匿名类对外部引用变量的要求.                                          #
###############################################################################

# 例如


def count_wrong():
    fs = []
    for i in range(1, 4):
        def f():
            return i*i
        fs.append(f)
    return fs


(f1, f2, f3) = count_wrong()
print(f1(), f2(), f3())
# 结果却是所有三个函数变量都是返回 9 (也就是 3 * 3), 因为这些闭包只有在调用他们的时候, 才会被确定
# , 也就是在调用他们的时候, 他们引用外部环境的变量 i 已经是 3 了.
# 为了解决这一问题, 可以在闭包中先调用一遍内部函数, 这时候循环变量的值已经确定了下来


def count_correct():
    def f(j):
        def g():
            return j*j
        return g
    fs = []
    for i in range(1, 4):
        fs.append(f(i))  # f(i)立刻被执行，因此i的当前值被传入f()
    return fs


f1, f2, f3 = count_correct()
print(f1(), f2(), f3())


# 函数的实例其实也是一个对象, 函数对象有一个__name__属性，可以拿到函数的名字

# 装饰器(decorator)模式: 利用返回函数的特点, 可以对函数功能不修改函数的任何定义, 在运行期间额外
# 的添加更多的功能.
# 例如我们可以做一个日志记录器, 接受所有的函数, 并且记录函数的名称


def log(func):
    # log 函数就是一个装饰器
    def wrapper(*args, **kw):
        print('call %s():' % func.__name__)
        return func(*args, **kw)
    # 这样返回的 wrapper 函数依然还是有点没有达到装饰器的要求, 因为其 __name__ 是wrapper 而不是
    # func 原来的 __name__
    # 解决这样的问题只需要使用内置的一个装饰器(自己直接修改应该也行)
    return wrapper

# python 的 @ 用法, 可以把 @ 语法作用的函数传入 @ 的参数, 这种语法正是适用于装饰器


@log
def now():
    print(datetime.datetime.now())


now()
# 如果 @函数还需要接受额外的参数, 例如 @log(text) 这样的, 这样的方式是 log(text) 返回的才是一个
# 真正的装饰器, 也就是在上面的 log 外面再加一层, 用于接受并且处理参数, 再返回一个使用装饰器的函数
# 例如


def log_arg(text='call'):
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kws):
            print('%s %s():' % (text, func.__name__))
            return func(*args, **kws)
        return wrapper
    return decorator


@log_arg('call_new')
def now_new():
    print(datetime.datetime.now())


now_new()

# 偏函数
# 有时候我们需要将函数中某些参数按照另外一个非默认参数的值作为新的默认参数而构成的一个新函数
# 例如 int(str, base=10), 但是某个情况下我们需要连默认 base 为 2 的新函数, 这时候我们可以:


def int_2(x, base=2):
    return int(x, base)


# 而这样需要我们自己定义一个新函数, 不过利用 py 自带库的functools.partial函数就可以
# 直接返回我们需要的这样的函数, 这些默认参数值都是从左到右补全要处理的函数
fn_int_2 = functools.partial(int, base=2)
print(int_2('10000'), fn_int_2('10000'))
