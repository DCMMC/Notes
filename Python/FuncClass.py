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
# 导入fucntools 并使用别名 fc
import functools as fc
import sys

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
# 该函数的时候, 该参数指向的对象已经是修改过的了, 默认参数又叫关键参数(key argument), 因为类似于
# 字典, 在调用函数时多个有默认参数的参数可以不用按顺序指定, 可以直接用 [arg name]=value 来指定
# 例如:
def add_end_wrong(l=[], count=4): # noqa E302
    l.append('end')
    return l


add_end_wrong(count=1)
# print(*objects, sep=' ', end='\n', file=sys.stdout) 可以指定 end 为''表示不用换行
print(add_end_wrong(), end='')


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
# py 还有个内置方法 zip 有点像 map, 不过是把多个 iterable 对象(可以不等长, 反正取最小长度的那个)
# 你一个我一个合成一个 tuple 组成的 list
# zip(*zip(*iterable)) 相当于 unzip

# fc 包里面还有类似与 map 的函数 reduce, 这个函数用于递归迭代, reduce把一个函数作用在
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


# 函数的实例其实也是一个对象, 函数对象有一个__name__属性(attribute)，可以拿到函数的名字

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

# python 的 @ 修饰符号(decorator)用法, 可以把 @ 语法作用的函数传入 @ 的参数,
# 这种语法正是适用于装饰器


@log
def now():
    print(datetime.datetime.now())


now()
# 如果装饰器函数还需要接受额外的参数, 例如 @log(text) 这样的, 这样的方式是 log(text) 返回的
# 才是一个真正的装饰器, 也就是在上面的 log 外面再加一层, 用于接受并且处理参数,
# 再返回一个使用装饰器的函数, 这种设计模式叫做装饰器工厂
# 例如


def log_arg(text='call'):
    def decorator(func):
        @fc.wraps(func)
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


# 而这样需要我们自己定义一个新函数, 不过利用 py 自带库的fc.partial函数就可以
# 直接返回我们需要的这样的函数, 这些默认参数值都是从左到右补全要处理的函数
fn_int_2 = fc.partial(int, base=2)
print(int_2('10000'), fn_int_2('10000'))


###########
# 类 OOP  #
###########


# 类 my_string 继承自 object(公共基类)
# py 和 cpp 类似, 允许多根继承, 也就是括号里面
# 可以有多个类型
# 继承的子类 用父类来 isinstance 判断为真
# 类名后面的括号可省, 缺省下为 object
class my_string(object):
    # class 里面的非 private 权限的属性一般也用 getter 和 setter 方法
    # class 里面的变量, 方法都是属性(毕竟 py 弱类型)
    # class 里面的方法的类型为 bound method(也叫作 class method, 就是通过
    # 类的实例对象访问的)
    # py 还有内置全局函数 hasatrr setattr setattr 来处理对象的属性
    # 因为直接裸写可以接受任何类型(因为 py 是弱类型的)
    # 属性还可以通过 types.MethodType([function], [class instance]) 动态绑定
    # 成类的实例内部的 bound method(也就是第一个参数为 self, 并且通过 instance
    # 调用的时候回自动给出 self 的方法)
    # 很多用于类的全局函数都可以更改类中特殊属性来控制其行为, 例如
    # __delattr__, __iter__, __dir__ 等等

    # __slots__ 可以用 iterable, string, string序列表示
    __slots__ = ('name', '__content', '__index', '__Length')

    # 定义构造器, 第一个参数永远是 self 实例变量, 并且
    # 调用的时候不用指定

    def __init__(self, *strings, repeat=1):
        # 所有类方法的第一个参数在用类实例调用的时候, 都是类的实例对象
        # 一般约定使用 self(或cls) 命名, 相当于 java 中的 this
        # 这里访问的是 __content 属性
        self.__content = ''
        if len(strings) > 0 and not all([isinstance(s, str) for s in strings]):
            print('strings: error argument type')
            return
        if not isinstance(repeat, int):
            print('repeat: error argument type')
            return
        while(repeat > 0):
            for string in strings:
                self.__content += string
            repeat -= 1
        # 用于迭代器
        self.__index = 0
        self.__Length = len(self.__content)

    def __repr__(self):
        """
        类似与 __init__ , py 中的类还有很多这样的特殊属性(方法)
        都是前后以 __ 包围的名称, 有: __init__ 构造器
        __repr__ 用于描述该类的信息, __str__ 跟 __repr__ 特别类似,
        不过是用于打印的信息(比如在 print 的时候使用), 后面调用的时候会给出一些例子
        __class__ 用于记录对象中的类(类型, 类似于 Java 中的 Class 类)
        还有用于迭代器的 __iter__() (有时候我们可以直接用 __iter__ = [another non private
        bound method name] 来使迭代器能够直接被外界访问), 用 py 内置全局函数 iter() 也可以
        直接获取类的实例的迭代器, __iter__ 方法必须要访问一个有 __next__ 的对象.
        其中 __next__ 要求在遍历结束的时候抛出异常StopIteration
        __add__, __sub__, __mul__, __rmul__ 等等是用于重载操作符的, __call__ 用来重载()操作符
        默认情况下 py 的类的实例可以无限制得添加属性, 不过我们可以通过设定 __slots__ 来限制
        实例能够添加的属性, 不过 __slots__ 对继承的子类无效. __slots__ 的好处: 因为默认情况下
        每一个实例都占用一个 dict(__dict__ 属性) 来存储所有属性, 这样很浪费空间, 并且在属性
        值只需要几个固定的情况下有性能浪费. 并且子类如果也设置了 __slots__, 还会自动继承父类
        的 __slots__ (不过如果子类没有显式的更改 __slots__, 则不会继承, 继续使用 __dict__
        存储属性), 所以不要在继承树重复定义 __slots__, 这样会浪费不必要的空间, 并且继承树必须
        从顶至下都有非空 __slots__ 并且没有显式的 __dict__, 不然只要有一个类空 __slots__
        (也就是有 __dict__), 就会导致其子类
        并且我们在获取类中所有属性的时候, 应该使用全局函数 dir(), 因为它在不管是用 __dict__
        还是 __slots__ 都能获取到完整的属性列表, dir() 还可以用于列出模块中的类和全局变量.
        有 __slots__ 就会屏蔽 __weakref__ 和 __dict__, 还可以将 '__dict__' 放入 __slots__
        以达到可以无限制添加属性的目的.
        __slots__ 在类层面上通过为每个变量名创建描述子(descriptor)来实现的, 所以类属性不能由于
        __slots__ 定义的实例变量设置默认值(也就是在类中成员属性), 不然类属性会覆写描述子分配.
        """
        return self.__class__.__name__ + '(__content' \
            '=\'' + self.__content + '\')'

    class InnerBase(object):
        __slots__ = 'foo', 'bar'

    class InnerSubRight(InnerBase):
        __slots__ = 'dick'

    class InnerSubWrong(InnerBase):
        # 不必要的空间浪费
        __slots__ = 'foo', 'bar', 'dick'

    print('占用空间大小比较:',
          sys.getsizeof(InnerSubRight()),
          sys.getsizeof(InnerSubWrong()))

    def __str__(self):
        """
        用于打印该类(的实例)的信息
        """
        return self.__content

    def __iter__(self):
        # 一般都是在类中实现了 __next__ , 所以直接返回 self
        return self

    def __next__(self):
        # 迭代器核心方法, 可用内置全局方法 next()访问
        if self.__index < self.__Length:
            self.__index += 1
            # print(self.__index)
            return self.__content[self.__index - 1]
        else:
            # 一定要重置 index
            self.__index = 0
            raise StopIteration

    def func(self):
        # 类中的叫方法, 全局的叫函数
        # 因为方法还能访问实例对象中的属性(数据)
        # py 类中方法没有静态的说法, 方法既可以通过实例访问
        # 也可以直接用类名访问, 但是方法路遥想要访问实例对象
        # 中的数据, 必须指定第一个参数为 self, 并且通过实例
        # 对象调用该方法的时候, 第一个参数 self 会自动指定,
        # 如果是通过类型调用, 那需要额外指定 self 为一个实例
        # 对象
        print(self.__content)
        # 调用外部函数
        hanoi(3, 'A', 'B', 'C')
        # 如果用 self 调用成员方法, 相当于隐式得为该方法提供了第一个
        # self 参数, 下面的语句其实是 self.__private_func(self)
        # self.__private_func()
        # 而对于 __private_func 这种不需要 self 的方法(可理解为 static)
        # 不能使用实例来调用, 又因为权限为私有, 所以在类内部可以访问
        my_string.__private_func()

    def __private_func():
        # 类中 __ 开头的变量(属性/数据)和方法(也算是变量)
        # 是私有的, python 明显限制外部环境对类中私有方法和
        # 私有变量的调用, 这种没有 self 参数的方法可以视为
        # 静态方法
        print('private func in my_string')

    def class_function(count, self_instance):
        print('static func')
        # 使用 global 变量并不需要 global 关键字
        print(f1())
        # 如果在这一 scope 中更改了一个和全局变量名称相同的局部变量
        # 在作为局部变量使用前作为全局变量使用将会报错, 要么就一直当
        # 全局变量使用, 要么一直当局部变量使用
        # print(f2())
        # 这将会屏蔽 global 变量
        f2 = lambda: 'f2 in class_function'  # noqa E731
        print(f2())
        # 对 global var 进行 reassign 的时候必须先使用 global 关键字
        # 声明 global 变量, 才能对该 global 变量进行调用
        global f3
        f3 = lambda x: x + 1  # noqa E731
        print(f3(1))

    def read(self):
        # do something
        feedback = False
        if not feedback:
            # 抛出异常
            # py 中的异常都是 BaseException(一般从它的子类 Exception 继承)
            # 各种Error 和 Warning 都是继承自 Exception
            raise Exception('some exception raised by read')

    @classmethod
    def class_function_foo(cls, x):
        # classmethod property 就是一个很有趣的装饰器的例子
        # 第一个参数用于接受类的实例对象, 习惯命名为 cls
        # classmethod property/decorator 就是用于把类函数转化为类方法
        # classmethod(class function) -> class method
        # 调用 my_string.class_function_foo(x), 等价与
        # my_string().class_function_foo(x), 也就是说 cls 默认下为
        # 构造器全用默认值创建的对象
        # py 的class method 和 C++/Java 的 static method 不一样
        # 如果要用类似于 C++/Java的 static method, 可以用装饰器
        # staticmethod, 其实就是 跟这种情况相比, 没有了 cls, 等于
        # 有没有实例调用这个函数, 该函数都不依赖类的对象中的任何属性
        # 刚好跟这种情况反过来了.
        print('the input is:', x)

    @property
    def content(self):
        # property 装饰器也很有趣, 可以直接通过访问属性
        # 来调用 getter 和 setter, 这里的 content 是类方法
        # 当然也可以是类函数
        # 其中 @property 修饰的是 getter 方法, 并且其方法名就是
        # 类外面能够直接通过实例调用
        # 这里访问的是一个只读属性(property)
        # 如果需要修改(setter), 还需要设置一个 @content.setter
        # 的类方法/类函数
        # 用 property 修饰的方法/函数返回的是一个 property 属性(attribute)
        # 一种特殊的属性
        return self.__content

    @content.setter
    def content(self, new_value):
        # 必须和 @property 同名的一个方法名/函数名称, 不过 property 还可以
        # 设置指定的 setter 和 getter
        # 不过 py 是不支持函数重载的, 不过这里因为装饰了, 外面的装饰器不一样
        # setter 是有上面 property 生成的另外一个装饰器
        # 总的来说就是实现了一个 Descriptor, 核心是用的 __get__ 和 __set__
        # 这两个装饰器返回的就是 Descriptor 对象, 例如 __get__(self, instance, owner),
        # self 就是属性对象, instance 就是属性对象(如果是直接从 owner 获取, 这里可以为 None)
        # 而 owner 就是含有该属性对象的一个任意对象, 这个不能为空
        # 例如 temp = Temp() 中含有 Descriptor (选择子对象) value 作为属性, 则
        # 调用访问 temp.value 是相当于 value.__get__(value, temp) (这样是只读),
        # 而 temp.value = xxx 这样的 reassign 就相当于 value(value, xxx)
        if not isinstance(new_value, str):
            raise ValueError('new_value must an instance of str!')
        self.__content = new_value
        self.__index = 0
        self.__Length = len(self.__content)


# 创建一个 my_string 的实例变量(instance)
s = my_string('string', repeat=3)
print(s)
# 下面两种方法 func 调用是等价的
s.func()
my_string.func(s)
# 但是对于这种不需要 self 的方法, 不能通过实例变量
# 调用, 因为默认会给上一个 self 参数. 感觉 py 这类
# 中成员方法的调用比较鸡肋
# s.class_function(k1, s)
my_string.class_function(1, s)
# 可以自由的给实例变量绑定任意属性
s.name = 'my_string instance'
print(s.name)
# 还可以用 del 操作参数类的实例中的属性
del s.name
print("hasattr(s, 'name'):", hasattr(s, 'name'))
# 通过 property 装饰的 setter 修改内容
try:
    s.content = 'str'
    print('s =', s)
    s.content = 100
except ValueError as v:
    print(v)
# 下面的语句是非法的, 因为 __ 开头的是私有的
# 不过 py 还是没有强制隐藏, 某个 py 的解释器上的实现是把该
# 私有函数改名为 _[class name][private function or var name]
# 这取决于解释器具体实现, 反正别想干这种事, 不过这 py 的权限管理
# 确实有点鸡肋, 很多都不是强制性的, 完全取决于惯例约束
# s.__private_func()
# _my_string__private_func()

# class 中的一些特殊属性
# 默认的 print(s) 相当于 print(str(s))
print(s)
print(str(s))
print(repr(s))

# 迭代器
it = iter(s)
print(it, [x for x in s])
print([x for x in s])

# py 可以通过在类声明的时候在类名后面用括号来继承一个或多个类,
# 但是因为 py 是动态类型语言, 所以不需要严格的通过继承来表示
# 某个类是另外一个类的子类, 只需要与某个类有相同的成员方法
