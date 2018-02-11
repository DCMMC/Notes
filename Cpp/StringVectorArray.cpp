/**
* C++ Primer 5th Edition
* Chapter 3 Strings Vectors Arrays
* @author DCMMC
* @time 2018.1.18 ~ 2018.
*/

#include <iostream>
#include <string>

using namespace std;

class My_istream {
    public:
    int member = 0;
    My_istream() {
        
    }
    // 类型转换操作符
    operator void*(void) const {
        return (void *)1;
    }
};

// entry
int main(void) {
    /**
     * namespace
     * 使用 using 关键字可以引入特定命名空间的全体或部分成员, 相当于将该命名空间的所有或部分名称(成员名称)在当前源代码中创建一个别名.
     * 如果使用 using [namesace]; 导入全体名称时产生冲突, 可以在 [namespace] 后面使用 scope operator (i.e. :: 操作符) 来
     * 单独使用某个名称, using [namespace]::[name]; (这样比较安全)
     * 
     * > **头文件中不应该出现 using **, 因为 #include 预处理操作其实就是相当于把目标源代码直接复制代替 #include 位置, 所以
     * > 如果头文件中包括 using , 就会使得所有引用该头文件的源代码都使用上 using declaration, 如果该源文件中并不需要使用该 
     * using declaration 就会受到污染.
     * 
     * string
     * string有多种初始化方式:
     * 1. 默认为 empty string
     * 2. 使用 = operator 从string literal 复制
     * 3. 使用 = operator 从 string 对象复制
     * 4. 使用 () operator 调用构造器(这叫直接形式初始化, direct forms initialization)
     * 
     * > 所以像 string str = string(10, 'c'); 这样的变量声明并初始化语句, 相当于先通过直接初始化调用构造器创建了一个临时变量,
     * > 然后再通过 复制形式初始化拷贝了这个临时变量对象. (不推荐使用这种方式初始化)
     * 
     * 头文件 string 中有一个全局函数 istream& getline(istream&, string) 用于从 istream 中读取一行 string
     * 并且类 string 还重载了很多操作符, 其中 == 操作符判断两个 string 内容是否相同(区别大小写)
     * 
     * istream 的 >> 操作符对 string 的读入默认以空白字符分隔的字符串(leading whitespace, e.g. spaces, newlines, tabs)
     * 
     * > istream 的 >> 操作符在遇到 EOF 的时候返回 operator() 返回0, 也就是说 >> 本来返回的不是0, 是一个 istream & 引用类型,
     * > 但是 istream 重载了 operator void *(until C++11, C++之后是重载的 operator bool), 这是一个类型转换(type conversion)
     * > 操作符, 适用于while, if等判断条件的时候, 会自动的调用该操作符. 如果 istream 遇到了EOF 或 Ctrl+Z, 将会跑出一个异常,
     * > 截获异常之后, 就会更改 istream 的一个记录状态(state)的成员变量, operator void *或 operator bool 就是用于判断这些标志
     * > 是否正常, 如果不正常, 就会返回 0(或 false).
     * 
     * ostream 中的 endl 算子(manipulator) 用于输出回车并且刷新缓冲区(flush the buffer, 确保所有要输出但是还在内存中的内容都被输出并且清空 buffer).
     * 
     * string 的 assignment operator (= 号赋值操作) 区别于初始化的 = sign, assignment operator 只是将 string 变量中的内容替换掉.
     * string 还重载了 + 号操作符用于连接(append/concatenation)两个字符串并返回一个新的 string 对象. operator + 的右操作符有很多重载类型.
     * 
     * 头文件 cctype 中有很多判断字符的函数.
     * 
     * > cctype 是C语言的库 ctype.h 的C++版本, C++继承了大部分 C 的库, 不过命名方式有区别. C语言的库 [name].h 对应 C++ 的 c[name],
     * > e.g. stdlib.h 对应 cstdlib 并且C++中C的库的C++版本都加了命名空间 std, 而 C 没有命名空间. C++程序员应该统一使用C++的库, 而不是
     * > 采用 name.h 的C语言库.
     * 
     * C++ 新增 range-base for (for-each) 来更加好的遍历访问.
     * for (declaration : expression)
     *      statement
     * 
     * > range for 有值拷贝和传引用两种区别, 注意区分.
     * 
     * string 的 size 函数返回 string::size_type, 和其他 STL 一样中的类一样, 为了使得库中的类型更好的使用 machine-dependent manner
     * (也就是为了移植性), 很多健壮的库都采用这种方式. 这会给类库消费者来带一些冗杂的感觉, 不过通过使用C++11的 auto 可以让关键字自动得帮我们
     * 推导.
     * 
     * string 同样重载了 operator [], 不过string的 [] 越界访问的行为是 **未定义** 的(UB).
     * 
     * ## vector
     * 一般被称为**容器** (container).
     * 
     * vector 其实就是封装了 resized-array , 重载了很多有用的操作符. vector 使用 class template, 编译器处理模板的过程称为 **instantiation**
     * (实例化). 
     * 
     * > 一些古老的编译器使用的模板声明格式与新版有点不一样, 再需要模板类型也是一个含有模板类型的类时, 旧版的编译器需要分隔开相邻的 >, 以免编译器
     * > 误认为是 >> 操作符, e.g. vector<vector<int>> 在使用旧版编译器需要写成 vector<vector<int> >.
     * 
     * List initialization a vector (C++11)
     * 可以用 a list of zero or more initial element
     */

    My_istream is = My_istream();
    // 如果没有重载类型转换操作符, 将会报错 My_istream 无法转换到 bool
    if (is) {
        cout << "variable \"is\" is true.\n";
    }

    cout << "\nPress any key to continue...\n";
	getchar();
    return 0;
}
