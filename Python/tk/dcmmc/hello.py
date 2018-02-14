#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
a test module
文件中第一个字符串就是文档注释
可以用任意一种表示字符串的风格 "", '', '''''', """"""
一般推荐用最后一种
模块中的 doc 文档可以通过 __doc__ 访问
像这些 __包围的特殊变量名, 我们应该尽量避免
我们的变量使用这些风格
"""

# __author__ 字段记录模块作者
__author__ = 'DCMMC'

# 导入 sys.py
# 相当于 sys 的命名空间在本命名空间下可见, 不过
# 访问里面的变量 函数 类等等都需要 sys. 前缀
# 也可以直接 from sys import [name] 将 sys 中
# 的指定 [name] 的成员(函数 类 变量) 导入到当前
# 命名空间. 可以用通配符 * (不过不推荐, 因为这样
# 就不知道到底有没有覆盖掉自己的函数, 变量的名称)
import sys


def test():
    # sys.argv 是一个 list 记录调用该 py 文件的时候的参数
    # 其中第一个元素一定是 'hello.py', 也就是该文件名
    args = sys.argv
    if len(args) == 1:
        print('hello, world.')
    elif len(args) == 2:
        print('hello, world.', args[2])
    else:
        print('too many arguments.')


# __name__ 是一个特殊变量, 当直接运行这一个 py 文件
# 时, __name__ 为 '__main__'
if __name__ == '__main__':
    test()
else:
    ###############################################################
    # 导入该模块(也就是该文件时), __name__ 就是该                 #
    # 文件名也就是 'hello'. 并且会自动执行该模块,                 #
    # 所以这里经常用 __name__ 判断是导入还是直接                  #
    # 运行该模块(无参数运行)                                      #
    ###############################################################
    print("__name__ ==", __name__)

# 函数和变量的作用域:
# py 并没有严格的作用域权限管理, 只是约定: _ 和 __ 开头的变量和函数
# 都是 private(私有)的, 编程上不应该引用其他模块中的 private 函数或
# 变量

_var = 'some private variable'


def __private_func():
    print('private function in module tk.dcmmc.hello')
