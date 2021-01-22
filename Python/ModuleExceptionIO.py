#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#########################
#                       #
# 模块, 异常 和 IO      #
# 2018.2.12             #
#########################

# 导入 hello 模块会无参数运行该模块, 并且
# 这时候 hello 中的 __name__ 为 'hello',
# 这就是模块名
# 模块搜索路径为当前目录, 安装的所有第三方
# 模块, sys.path 便是记录的所有目录, 可以
# sys.path.append('想额外添加的路径')
# 也可以设置 PYTHONPATH 环境变量
# 这里 tk 是一个包, dcmmc 是包 tk 里面的包, hello 是包 dcmmc
# 的一个模块, 调用包的时候同时会调用包里面的 __init__.py
# 这里相当于导入了 tk 的__init__, dcmmc 的 __init__
# 还有 hello 这三个模块
# 并且只要要求 tk 在模块搜索路径里面就好了
import tk.dcmmc.hello
# 还可以 from tk.dcmmc import hello, from 的包还可以用 . 和 .. 还有
# ..[dir name](Intra-package References)
from tk.dcmmc import hello
from contextlib import closing
import urllib.request
from FuncClass import s
# abstract syntax trees
import ast
import logging
import unittest
import doctest
from io import StringIO, BytesIO
import sys
import os
# 高级文件操作
import shutil
# 序列化
import pickle

tk.dcmmc.hello.test()
hello.test()
tk.some_func_in_tk()
# 模块中 _ 和 __ 开头的函数或变量按照惯例都是私有的
# 实践中不要写出下面的代码
tk.dcmmc.hello.__private_func()
print(tk.dcmmc.hello._var)


############################################
# 常用的标准库                             #
# re 正则表达式                            #
# copy 复制                                #
# math, cmath 数学                         #
# decimal, fraction, 无损分数和整数        #
# sqlite3 数据库                           #
# os, os.path 文件系统                     #
# gzip, bz2, zipfile, tarfile 压缩文件     #
# csv, netrc 各种文件格式                  #
# xml                                      #
# htmllib                                  #
# ftplib, socket                           #
# cmd 命令行                               #
# pdb                                      #
# profile, cProfile, timeit                #
# collections, heapq, bisect 数据结构      #
# mmap                                     #
# threading, Queue 并行                    #
# multiprocessing                          #
# subprocess                               #
# pickle, cPickle                          #
# struct                                   #
############################################

# 因为 py 语言的动态性, 我们还可以用 build-in 函数 eval 和 exec
# 动态执行语句
# eval(source, globals=None, locals=None, /)
# globals 和 locals 是命名空间参数
a = 0
local = dict(a=2)
glob = {}
# 行为将会在 local 中更改, 而不能影响原有的变量
eval("a + 1", glob, local)
print(a, list(local.values())[0])
# 可以用 exec 来对原有的变量进行更改
# exec(source, globals=None, locals=None, /)
# 默认locals 为空的时候, 会把新创建的变量创建在当前命名空间
exec('b = a + 1')
print(b)  # noqa F821
# 还可以将代码编译成字节码
# compile(str, filename, mode)
c = compile("a+2", "", 'eval')
eval(c)
c = compile("b=a+2", "", 'exec')
exec(c)
print(b)  # noqa F821

#########################
# 常用模块和示例        #
#########################

##############
# sys 模块   #
##############

# sys.argv 返回该程序接受到的命令行参数的列表
# sys.exc_info() 显示最近的异常的信息
try:
    x = 1 / 0
except Exception:
    print(sys.exc_info())

# 标准输入输出流
# sys.stdin
# sys.stdout
# sys.stderr
# 这三个都跟文件类似, 有 write 和 read 这些方法

# 退出 sys.exit(status), 通过抛出 SystemExit 异常
# 来退出

# sys.platform 表示当前平台, linux/darwin/win
# sys.version 和 sys.version_info 返回 Python版本信息
# 的 str 和 tuple

#############
# os 模块   #
#############
# 注意 py 有一些库中的成员在不同系统下不一样
# 例如 os.uname 在 win 下就没有

# os.name 返回操作系统类型 posix/nt
# os.name sys.platform 和 platform.system() 的区别:
# 分别是检查当前是否有特定系统用的模块来确定系统,
# 在 python 库的编译阶段就已经通过配置文件确定好系统了,
# 在运行阶段通过运行一系列其他函数来确定系统

# os.uname() 返回操作系统详细信息
# os.environ 记录当前环境变量的 dict

# 操作文件和目录的函数一部分放在os模块中，一部分放在os.path模块中
# 查看当前路径的绝对路径, 说实话, 这 abspath 有点蠢,
# 就是把这个相对路径和当前目录的绝对路径 join 再一起...
print('当前路径的绝对路径:', os.path.abspath('.'))
# 把两个目录拼接在一起, 不要直接用 + 拼接, 因为
# join能够对于不同系统采用不同的 dir seperator
print(os.path.join('/tmp', 'testDir'))
# 同样的道理, 拆分路径可以用 os.path.split()
# os.path.splitext() 可以直接得到扩展名
# os.path.isdir() 和 os.path.isfile() 顾名思义
# os.path 主要是提供一些判断或者读入
# 而 os 是调用系统 api 对底层进行真正写入

# 创建一个目录
if os.path.exists('/tmp/testDir'):
    # 删除一个目录
    os.rmdir('/tmp/testDir')
os.mkdir('/tmp/testDir')
# 删除一个目录
os.rmdir('/tmp/testDir')
with open('/tmp/testFile', 'w'):
    pass
# 文件重命名
os.rename('/tmp/testFile', '/tmp/test.txt')
# 列出所有文件
# 就是返回一个 str 的 list, 并是不什么 file 对象
# 这点有点蠢
print(os.listdir('/tmp'))
# os 在 *nix 下还有 chmod chown 之类的操作

# 但是 os 竟然没有复制操作, 原因是复制文件并非由操作系统提供的系统调用
# 理论上我们可以通过文件读写来进行复制, 不过有点麻烦
# shutil 模块提供了 copyfile() 和其他用于解压和复制文件的实用方法
if os.path.exists('/tmp/test2.txt'):
    os.remove('/tmp/test2.txt')
shutil.copyfile('/tmp/test.txt', '/tmp/test2.txt')

# 获取所有文件扩展名为 txt 的文件
print([x for x in os.listdir('/tmp') if os.path.isfile(os.path.join('/tmp', x))
       and os.path.splitext(os.path.join('/tmp', x))[1] == '.txt'])

#############
# IO 读写   #
#############
# 同步 IO
# CPU 会一直等待直到 IO 读写结束

# 打开文件, 只读
# mode 类似于 C:
# b: binary
# r: read(default)
# t: text(default)
# w: truncating the file and write(先把文件内容清除再写入)
# a: append(如果文件存在, 就从文件最末尾处开始增量写入)
# x: create a new file and open if for writing
# open() 还可以指定关键字参数 encoding 和 errors 表示遇到编码错误
# 后怎么处理, 一般是直接忽略 'ignore'
with open('./ModuleExceptionIO.py', 'rt', encoding='utf-8',
          errors='ignore') as f:
    # 只读一行
    # 也可以用 read() 读取所有内容
    # 如果不能确定文件大小，反复调用read(size)比较保险
    print(f.readline())
    # 调用readlines()一次读取所有内容并按行返回list
    # for line in f.readlines():
    # 去掉空白字符再打印
    # print(line.strip())
# 写入
with open('./test.txt', 'a') as f:
    f.write('test file\n')

# 像open()函数返回的这种有个read()方法的对象，在Python中统称为file-like Object
# 例如内存中的字节流, 网络流...
# py 有一个好处就是不需要继承特定类, 只需要有 read() 就行

# 在内存中读写 string 和 bytes: StringIO, BytesIO
print('##############################')
strF = StringIO('Hello\nWorld')
# 移动到 end of stream
strF.seek(0, 2)
strF.write('\nPython')
# 注意上面 write 之后文件指针到了最后面
# 用 seed(0) 将指针移到最前面
strF.seek(0)
# 写入和读取都跟文件一样
while True:
    line = strF.readline()
    if line == '':
        break
    print(line, end='')
print()
strF.close()

with BytesIO() as f:
    f.write('中文'.encode('utf-8'))
    print(f.getvalue())

# 序列化(pickling) 和 反序列化(unpickling)
# 类似于 Java 中的 serialization
# py 的 pickle 模块提供了序列化的支持
# pickle.dumps() 方法把任意对象序列化成一个 bytes,
# 还可以用 pickle.dump() 序列化到 file, 第二个参数指定
# 写入文件
d = dict(name='Bob', age=20, score=88)
bs = bytes(pickle.dumps(d))
print(bs)
del d
# 用 pickle.loads() 和 pickle.load() 分别从 bytes 和 file-like object
# 中反序列化, 返回一个对象
d = pickle.loads(bs)
print(d)

# web 开发经常会遇到在多种编程语言之间传递对象, 我们一般
# 把对象序列化为 JSON 或 XML, JSON 不仅可阅读性好而且
# 比 XML 更快
# py 的内置库 json 提供了对 JSON 的支持
# json 中的 loads load dump dumps 跟 pickle 行为很相似
# 并且 JSON 规定编码为 utf-8, JSON 的格式也跟 py 的
# 字面量很像
# 不过自定义类默认都不支持 json 序列化, 因为 json 就是封装了
# 一些基本类型的字面量, 不过我们可以通过在 dump 方法中
# 指定包装函数来把我们的类转化成 json 支持的类型

##########################################
# 前面这些 IO 操作都是同步 IO,           #
# 下面介绍异步 IO                        #
##########################################

##########
# 异常   #
##########

#######################################################
# 上述行为如果是用于给消费者执行的话                  #
# 是相当危险的!                                       #
#######################################################

# 抽象语法树 (AST)
tree = ast.parse("a+2", "", "eval")
ast.dump(tree)
# 还可以直接对语法树进行修改
tree.body.right.n = 3
ast.dump(tree)
# 还可以使用安全的操作, 只能使用基本值的操作
ast.literal_eval("[10.0, 2, True, 'foo']")

# 断言, 用于调试, bool exp 为 False 的时候抛出 AssertionError


def foo(n: int):
    n = int(n)
    assert n != 0
    return 10 / n


# 除了断言, logging 在处理大型项目的时候更加重要
# 大型项目普遍使用单片测试(测试驱动开发, Test-Driven Development, addr., TDD)
# 例如我们实现一个自己的 Dict
class Dict(dict):
    r"""
    我们还可以使用文档测试(doctest), 模块 doctest 将会自动提取类中的
    注释中的代码, 严格按照注释中的
    Python交互式命令行的输入和输出来判断测试结果是否正确, ... 可表示
    中间一大堆输出
    Example
    >>> d1 = Dict()
    >>> d1['x'] = 100
    >>> d1.x
    100
    >>> d1.y = 200
    >>> d1['y']
    200
    >>> d2 = Dict(a=1, b=2, c='3')
    >>> d2.c
    '3'
    >>> d2['empty']
    Traceback (most recent call last):
        ...
    KeyError: 'empty'
    >>> d2.empty
    Traceback (most recent call last):
        ...
    AttributeError: 'Dict' object has no attribute 'empty'
    """
    def __init__(self, **kw):
        super().__init__(**kw)

    def __getattr__(self, key):
        try:
            return self[key]
        except KeyError:
            raise AttributeError(r"'Dict' object has no attribute '%s'" % key)

    def __setattr__(self, key, value):
        self[key] = value


# 对于一个健壮的类, 我们需要对其进行单元测试以确保其正确性
# 编写单元测试也是一门艺术, 需要尽量覆盖所有易错情况
# 单元测试可以继承自unittest.TestCase
# 而且单元测试一般单独放在 testxxx.py 之类的文件中
class TestDict(unittest.TestCase):
    def test_init(self):
        """
        测试构造器
        @Argument
            self 实例对象
        """
        d = Dict(a=1, b='test')
        self.assertEqual(d.a, 1)
        self.assertEqual(d.b, 'test')
        self.assertTrue(isinstance(d, dict))

    def test_key(self):
        d = Dict()
        d['key'] = 'value'
        self.assertEqual(d.key, 'value')

    def test_attr(self):
        d = Dict()
        d.key = 'value'
        self.assertTrue('key' in d)
        self.assertEqual(d['key'], 'value')

    def test_keyerror(self):
        d = Dict()
        with self.assertRaises(KeyError):
            d['empty']

    def test_attrerror(self):
        d = Dict()
        with self.assertRaises(AttributeError):
            d.empty

    def setUp(self):
        """
        unittest.TestCase 还有两个重要的方法, setUp
        和 setDown 分别用于每调用一次测试方法的时候执行
        例如我们的测试需要启动数据库, 每个测试方法
        都要打开然后关闭测试用力就需要写很多冗余的
        相同代码
        """
        print('setUp...')

    def tearDown(self):
        print('tearDown...')


# 最简单的运行单元测试的方法就是使用 main()
# 也可以在命令行 python -m unittest [testxxx.py]
if __name__ == '__main__':
    unittest.main()
    # 只有在命令行直接运行该模块的时候才会运行 doctest
    doctest.testmod()

# try except:
try:
    s.read()
# 如果不需要获得异常的实例, as [var name] 可以省略
except Exception as ex:
    print('except:', ex)
    # 使用 loggin 库将错误日志存入文件中
    logging.exception(ex)
    raise Exception('new Exception...') from ex
    # 如果没有 from [origin_exc] 那么就是在处理异常过程中抛出的
    # **新异常**, 如果有 from [origin_exc] 那么就是显式的表明新
    # 异常是由原异常引起的
    # raise Exception('new Exception...')
# 可选 else 子句, 当上述异常没有一个出现时, 将会自动执行 else
# 如果有异常被捕获, 就不会执行
else:
    print('else 子句')
# finally 子句也是可选的
finally:
    print('finally...')
    # 类似于 try-resource 语句 with as (as 可省略)
    # scope 结束之后会自动关闭 f
    # 为了适应 with 语句, 类必须要实现上下文管理器,
    # 具体的, 就是实现 __enter__ 和 __exit__ 方法
    # as 就是接收 __enter__ 返回的值
    with open('README.md') as f:
        for x in f:
            print(x, end='')
    # 对了没有实现 __enter__ 和 __exit__ 但是实现了
    # close 的类, 还可以使用contextlib.closing 来封装成
    # 上下文管理器(context manager)
    with closing(urllib.request.urlopen('http://www.baidu.com')) as url:
        html = url.read()
        print(html[:100])
