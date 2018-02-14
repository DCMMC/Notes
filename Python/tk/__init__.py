"""
__init.py__ 用于指示这是一个包, 这就是说包里面才是真正的模块文件
而不是包本身
并且 __init.py__ 可以是空文件
为了加快读取模块的速度, 每个模块目录都会有 __pycache__ 用于存储预编译好的 pyc 文件
"""

print('package tk')


def some_func_in_tk():
    print("some function in package tk")
