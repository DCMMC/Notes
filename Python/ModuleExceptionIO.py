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

# try except:
try:
    s.read()
# 如果不需要获得异常的实例, as [var name] 可以省略
except Exception as ex:
    print('except:', ex)
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
    # 上下文管理器
    with closing(urllib.request.urlopen('http://www.baidu.com')) as url:
        html = url.read()
        print(html[:100])
