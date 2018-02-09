/**
* C++ Primer 5th Edition
* Chapter 2 Variables and Basic Types
* @author DCMMC
* @time 2018.1.6 ~ 2018.1.18
*/

#include <iostream>
#include <typeinfo> // typeid
#include "header.h"

using namespace std;

// declaration but not define
// extern int std::headerInt;
// definition, 重定义, 直接 CE(Compile Error)了
// extern int std::headerInt = 20;

int main(void) {
	/**
	* Primitive Build-in Types 可以分为两种：Arithmetic Types 和 void 类型(没有有关的值, 一般用于说明函数返回值) 
	* 其中 Arithmetic Types 又可以分为三种:
	* bool 类型， 标准没哟规定bool的实现大小， 任何非0整数赋值给bool类型都为 true（true转化为整型为1），而 整数 0 则为false。
	* integer类型：字符类型（标准规定基本字符 char 至少能容纳机器中的基本字符集，而 wchar_t 则最少能容纳机器的最大扩展字符集，
	* char16_t char32_t 为 Unicode 扩展字符集），short, int, long, long long(C++14). 标准没有规定这些类型的准确大小, 
	* 因为为了移植在多种平台上, 只规定了这些类型的最小大小(这里的大小指的是位数), 并且这些类型的相对表达范围不能小于比它小的
	* 类型(e.g. 标准保证 int 的大小一定不能小于short )
	*
	* > integer types 中除了 字符类型 之外, 默认都是 signed, 字符类型中 char之外都不分 unsigned 和 signed, plain char 
	* 在不同实现上面有可能代表 signed char 也有可能代表 unsigned char, 所以如果需要把char当做数值来用的时候必须显式的指出
	* 用的是 signed 或 unsigned
	*
	* > 标准没有规定 signed 的具体范围, 只是规定差不多正数负数均分 
	* > integer type 一般用途使用int 和 long long (long的大小经常和int一样)
	*
	* floating-point types: float(standard guarantees minimum precision is 6 significant digits, 一般是 
	* 1 machine word), 
	* double(same as float, 10 significant digits, 一般是 2 machine word), long double(extended-precision,
	* special-purpose, * 10 significant digits, 不同实现不一样)
	* 
	* > 一般用途使用 double, 有时候 double在某些机器上面比 float更快, 而long double的精度用不上并且有运行时性能损耗
	*/

	/**
	 * Type Conversions
	 * 隐式的:
	 * bool 和 其他 Arithmetic Types的转换如上面所说
	 * float-point types converts to integer types 将被趋零截尾(truncate)
	 * integer types 与 float point types 的相互转化可能损失精度
	 * 对于 signed 的溢出, 是未定义(undefined behavior, UB)的
	 * 对于 unsigned的溢出, 将会得到原始值被目标类型所能表示的值得个数的取模的余数(也就是补码和原码的关系)
	 * 
	 * 同理表达式的值得转换也如此
	 * 
	 * Expressions involving unsigned types
	 * 含有unsigned类型的表达式中的 signed 类型会自动转化为 unsigned , 类似于上述signed的值复制给 unsigned 一样.
	 * 如果减法表达式中含有 unsigned, 一定要保证表达式的值非负
	 */

	double doubleFromBool = true;
	cout << doubleFromBool << "\n";

	unsigned char c = -1; // -1 % 256 = 255

	/**
	 * 应该尽量避免使用 UB 或 Implementation-defined Behavior(这样会导致程序 nonportable )
	 */
	signed char c2 = 256; // UB

	cout << (int)c << ", " << (int)c2 << "\n";

	int intValue = 0x1000;
	c = intValue;
	cout << (int)c << "\n";

	/**
	 * Literals 字面量
	 * integer literals 默认是 signed decimal, 如果加上 prefix 0或0x(0X), 则表示 八进制(octal)/16进制(hexadecimal)
	 * 字面量, 而 8/16进制字面量可以是 signed 也可以是 unsigned.
	 * 字面量的类型为支持的类型中最小能容纳它的类型, 例如不加任何前缀的整数字面量支持的类型为 int, long, long long(都是
	 * signed), 而 8/16进制还包括上述三个类型的 unsigned 类型.
	 * 
	 * > 虽然字面量可能储存在 signed 类型中, 但是所有的整型字面量都是 非负的, -100中的 minus sign是一个操作符, 并不是字面量
	 * 的一部分, minus sign表示将后面的字面量的值取负.
	 * 
	 * 浮点型字面量默认为 double 类型, 可以采用 decimal point 或 exponent specified using scientific notation(e/E)
	 * decimal point表示的时候可以适当的省略整数或小数部分, e.g. 3. , .001 , 4E7 
	 * 
	 * character string literals 的类型为 array of constant chars, 而且编译器会自动在 string literals后面append
	 * 一个 '\0', 所以存储string literals的数组长度一定要比其 apparent size 多1.
	 * 两个邻接 string literals 中只有 空白字符(空格, tab, 回车)的时候会合并成一个 string literal.
	 * 
	 * escape sequences 转义字符, 用于表示 nonprintable chars(backspace, control characters, etc) 和某些在C++
	 * 中有特殊含义的字符.
	 * 以反斜杠(backslash)开始, 后接字符或x加多个hexademical digits(e.g. \x404 ) 或 1~3个 octal digits
	 * 
	 * 还可以通过添加后缀(suffix)或 prefix 强制说明字面量的类型, e.g. 500LL 为 long long类型
	 * 
	 * 对于 char literals, char16_t, char32_t, wchar_t的 prefix分别为 u, U, L(也可用于 string literals)
	 * 而 u8 可用于 utf-8的 string literals.
	 * unsigned的 suffix为 u/U, long 和 long long的 suffix分别为 l/L和ll/LL, unsigned的suffix还可以跟他们组合
	 * 起来使用. float和long double的suffix分别为 f/F和l/L
	 * 
	 * 字面量都是const的(右值, rvalue)
	 */

	// ! int octal = 09;

	/**
	 * Varialbes
	 * variable provide named storage that programs can manipulate
	 * types determines the size and layout of the variable's memory, the range of values that can stored
	 * within that memory, and the set of operations can be applied to the variable. We can refer to variables
	 * as "objects" interchangeably.
	 * 
	 * variable definitions:
	 * [base type specifier] [list of variable names(technically, declarators) separated by commas, 
	 * optionally provide an initial value] [ends with a semicolon]
	 * 
	 * 对于同一语句的多变量声明和定义, 顺序为从左到右, 立即生效, 所以同一语句中右边的变量可以使用左边的变量作为初始值.
	 * 
	 * 可以将 literials, 函数返回值, 表达式用来 initialization 一个 variable,也可以在变量名后面直接用 () 或 {} 初始化.
	 * e.g. int a(10);
	 * 
	 * > Initialization is NOT assignment, 在C++, = 在初始化(给予正在创建的变量一个初始值)和赋值(将变量中的值替换)
	 * 一个变量是两个不同的操作符.
	 * 
	 * List Initialization (C++11)
	 * 将 value 放在 curly braces中, e.g. int a = { 0 }; 或 int a{ 0 };
	 * 如果将花括号内的值用来初始化variable将 lose data, 编译器就会 reject the initialization(规定是这样的, 不过不同的
	 * 编译器的具体实现不一样, 在 MSVC 2017中严格按照标准, 会阻止编译进行, 新版的 gcc 也已经按照标准来了, 不过 gcc 4.8
	 * 没有实现这一特性, 只是一个警告)
	 * 
	 * Default Initialization
	 * build-in type variable defined **outside any function** body are default initialized to **zero**,
	 * while build-in type variable defined inside a function are default uninitialized(Undefined Behavior)
	 * Each class control how initialize objects of that class type. 也就是每个类的实例变量的默认初始化行为由该类
	 * 来控制.
	 * 
	 * Variable Declarations and Definitions
	 * To support separate compilation, C++ distinguishes between declarations definitions.
	 * A declaration make a name known to the program, while a definition create associated entity(allocate
	 * storage and may provide initial value).
	 * 
	 * Variables must be defined exactly once but can be declared (use **extern** keyword) many times.
	 * 
	 * 如果添加 extern 但是显式的初始化了, 就会覆盖掉 extern的作用, 变成了 definition了. 但是在g++ 7.2.1中, 该行为
	 * 会导致 CE(Complier Error). 因为不允许重复 declaration, 毕竟 显式的初始化的话, 就相当于 declaration and definition
	 * 
	 * Identifiers (标识符)
	 * 由 数字, 字母, 下划线组成, 不能以下划线开头, 区分大小写, 不能为保留字(keywords 和 Alternative Operator Names(and, 
	 * and_eq, etc)), 并且区分大小写.
	 * 标识符最好不要用两个下划线或一个下划线接着一个大写字母开头, 因为这两类都是标准库使用的标识符风格
	 * 
	 * Conversions for Variable Names 命名惯例
	 * 变量使用小写字母开头, 类用大写字母开头, 单词间用_风格或者使用驼峰式风格.
	 * 
	 * Scope of Names
	 * names declared in outer scope can be redefined in an inner scope.
	 * 不过可以通过 在变量名前加上 "::" 显式得使用 global scope中被inner scope redefined 的变量, "::" scope operator
	 * 有 empty left-hand side 时, 就是从当前的 global scope 中 request to fetch name
	 * 
	 * Compound(复合, 合成) Types
	 * A compound type is a type that is defined in terms of another type.
	 * 
	 * References
	 * 
	 * > new standard(C++11) introduced a new kind of reference: "rvalue reference", 现在这里提到的严格来说
	 * 应该是 "lvalue reference".
	 * 
	 * reference 相当于给 object 创建别名(Alias), 只需要在 [name being declared]前面加上 "&".
	 * a reference is just another name for an already existing object, references are not objects.
	 * reference 必须被初始化, 并且用来初始化的变量的类型必须和reference表示的类型 **完全一致** (*向后引用*, 例外, 不过
	 * nonconst type 可以用来初始化 const type reference).
	 * 对reference的所有操作等同于对reference绑定的变量的操作.
	 * reference最大的用处在于函数传参的时候用来 passed by reference, 有点像指针, 不过是不会没有指向对象(空指正)
	 * 安全的指针, 因为 passed by value的函数传参方式会将参数复制一遍值, 对实参的赋值不会影响函数外真正的传入参数.
	 * 而reference能做到在函数内部对参数的赋值同时影响到外部调用函数时作为传入参数的变量的值.
	 * 
	 * 用指向相同类型的reference来初始化另外一个reference, 实际上是把用来初始化的reference绑定的object绑定给正要
	 * 创建的reference
	 * 
	 * Pointers
	 * 
	 * pointer不同于 reference, pointer和其他build-in types有很多共同点. 例如复制, 初始化, 也是一个 object.
	 * pointer 功能类似于 reference, 也是用于间接访问其他 objects.
	 * 
	 * pointer是一个很底层的东西, 在汇编中的寻址经常用到, pointer存储着 object 的address, 也就是指向 object.
	 * 因为 reference 不是object, 所以 pointer不能指向 reference. 用来初始化的变量的类型必须和 pointer 表示的
	 * 类型 **完全一致** (*向后引用*, 例外, 不过 nonconst type 可以用来初始化 pointer to const) 或者 字面量.
	 * 
	 * pointer的声明(declaration)和定义(definition)类似于reference, 都是在 varibale name前面加上一个 "*"作为
	 * declarator. 作为初始化的值常常需要使用 "&" 取地址操作符(address-of operator)来取得 object 的地址.
	 * 用来访问所指向的 object 需要使用 "*" dereference operator.
	 * 对指针变量直接赋值(assignment)能够使它指向别的 object 或 invalid value.
	 * 
	 * 指针的底层表示就是一个整型.
	 * 
	 * 空指针: nullptr 字面量(C++11)是一种特殊的类型, 它可以转化为任意类型的指针. 在以前的标准或C语言中, 一般使用NULL
	 * (在 cstdlib 头文件中)表示空指针.
	 * 
	 * 指针的加法为 要加的值乘以指针指向的类型的大小(字节数).
	 * 
	 * void *指针
	 * 一种特殊的可以指向所有类型的指针. 一般用于比较地是否相同, 不能操作其指向的对象, 因为不知道其指向的对象的准确类型. 所以必须强制转化为其他
	 * 指向确切类型的指向才能用.
	 * 
	 * reference和pointer都不是 base type, 所以一行声明和定义可以声明和定义多个不同类型的变量,
	 * e.g. int i = 100, *pti = &i, &refI = i; 不能单纯得把 含有 *和&(type modifier, 并且 * 不限制个数)
	 * 的declarator的变量的类型. 这两个类型修饰符只能作用于单个变量.
	 * 因为 reference 不是object, 所以不存在 pointer to reference, 但是可以有 reference to pointer.
	 * 
	 * 虽然 int* 和 int *表示都没有错误, 不过更加推荐使用后者, 不管用哪种, the important thing is to choose a style 
	 * and use it consistently.
	 * 
	 * const Qualifier
	 * const object 必须被显式得初始化.
	 * const type 能够使用其对应的 non const version的不会更改其object的操作(definition 不算更改其object)
	 * 所以nonconst和const可以相互转换, 不过const type不能作为初始值初始化 nonconst reference或 nonconst pointer,
	 * pointer to const or reference to const is pointer or reference that they **think** they pointer or
	 * refer to const type
	 * 
	 * nonconst type 可以用来初始化 对应的 const 类型的引用类型. e.g. int i = 10; const int &ri = i;
	 * 也可以用 字面量, 表达式的值来初始化 const reference, 甚至还能像 build-in arithmetic type 一样隐式转换.
	 * e.g. double dVal = 2.3; const int &ri = dVal; (相当于 const int temp = dVal; const int &ri = temp;
	 * 这里的 temp 其实是 编译器生成的 unamed object, 同理 ri还可以绑定 double reference )
	 * 
	 * A reference to const may refer to an **object that is not const**
	 * 
	 * 同理 pointers to const 和 reference to const 有着相同的行为, 所以, pointer/reference to const 不能保证他们指向/引用
	 * 的变量的值一定不会改变.
	 * 
	 * 不过因为 const pointers 也是 objects, 所以可以在声明定义的时候, 在 "*" 后面加上 const, 代表该 pointer 是
	 * const pointer (区别于 pointer to const). const pointer和其他 const objects一样, 必须在初始化的时候显式的
	 * 指定其值, 并且不能再次更改其值了.
	 * e.g. int *const constPtr = &i; // ! constPtr = &i2; // error const Pointer 不能更改其值了
	 * 
	 * 对于含有多个 modifiers 并且有 const的情况, 可以采用从右往左理解变量的类型.
	 * 
	 * 对于指针类型, 如果指针本身的值不能改变(也就是 const pointer), 那这个const 就是 **top-level const** (所以top-level
	 *  const 可以表示任意 const types 的const的类型), 如果能指向一个const object, 那么这个 const 就是 **low-level const**
	 * 
	 * > 对于 const 变量, 使用 `&` 操作符取得的指针为 low-level pointer, i.e., 指向常量的指针
	 * 
	 * Constant Expression
	 * 
	 * 常量表达式的值不会改变, 并且在编译器就能确定(用于编译器优化). 字面量就是一种常量表达式.
	 * 
	 * 一个 object或 表达式 是否是 constant expression 取决于其类型(是否用 const 或 constexpr 修饰)和初始化
	 * (initializer, 是否是用常量表达式来初始化的)
	 * e.g. const int i = get_size(); 和 int j = 10; 都是不是 constant expression.
	 * 
	 * 对于大型系统, 有时候很难判断变量是否是用 constant expression来初始化的, 所以 C++11 新增 constexpr 关键字
	 * 用于显式得指定表达式为常量表达式. 如果 declare the variable in a constexpr declaration, 那么变量会隐式得
	 * 声明为 (top-level) const的, 并且我们可以在 constexpr variable 的 initializer 中使用 constexpr function.
	 * e.g. constexpr int size = size(); // 之后 size函数是 constexpr的, 编译才能通过.
	 * 
	 * > 为了便于编译器计算表达式, constexpr 只能用于只含有 **literal types** 的表达式, 包括 arithmetic types,
	 * > references 和 pointers. 像自定义类和STL中的所有类都不是 literal types.
	 * > 并且对于 constexpr pointers和references, 初始化他们的时候有很多的限制, 对于 constexpr pointers, 我们只能
	 * 用 nullptr literal, 字面量(i.e., constant expression) 0, 和 **在固定地址的对象(像函数内部的所有对象都不是
	 * 固定地址的对象, 只有函数外部的对象才是固定地址的)**. 同理 constexpr reference也只能用于固定地址的对象
	 * 
	 * 所以 constexpr int *p = nullptr; 和 const int *q = nullptr; 不同, 前者是 constant pointer, 后者是pointer
	 * to const.
	 * 
	 * type alias(synonym, 别名)
	 * 
	 * 对于复杂的程序, 一个类型往往会特别的冗杂(特别是在C++这种语言上面), 所以需要使用 typedef 或者 using(C++11) 来创建
	 * 类型的别名
	 * 
	 * typedef的用法类似于变量的声明, 只不过前面多了一个 typedef 关键字并且没有 initializer.
	 * e.g. typedef vector<string> *strVec; 相当于创建了类型 vector<string> *的别名strVec.
	 * 
	 * using 的用法为: using [别名] = [类型];
	 * 
	 * 直接像宏展开那样将类型别名展开是不可取的.
	 * 
	 * The auto type specifier
	 * 对于很多表达式的值, 我们有时候不好甚至不能推断其值的确切类型, C++11添加了 auto 关键字, 用于编译器在变量声明和定义
	 * 语句中自动通过 initializer 的值来推断(deduce)变量的适合类型.
	 * 
	 * 使用 auto 作为其 type specifier 必须有 initialier(显式的初始化器)
	 * 
	 * decalaration can involve only a single base type, the initializers for all the variables in the 
	 * declaration must have types that are consistent with each other.
	 * 
	 * 有时候 auto 的类型推断并不总是准确, 所以 auto 的类型推导遵循以下几个原则 (normal initialization rules):
	 * 1. 对于引用类型作为 initializer 的auto类型, 编译器会自动使用该引用所引用的类型, 而不是引用类型; 如果需要使推断出来的
	 * auto 为引用类型, 可以在 auto 后面显式的加入 `&` , e.g. auto& g = ci;
	 * 2. auto 会自动忽略 initializer 的 top-level consts, 但是 low-level consts会被保留(例如常量指针); 如果需要显式的
	 * 推断出 top-level const, 直接在 auto 前面显式的加上 const 关键字即可, e.g. const auto f = ci;
	 * 3. 如果对 auto 推导显式的加上 `&` (就像 1. 所描述的那样), auto 推导不会忽略 top-level consts, e.g. const int ci = 0;
	 * auto &ref = ci, *p = &ci; // ref被推导出来的类型为 const int&, p 为 const int *
	 * 4. 对于同一个变量声明表达式中定义多个 auto 类型变量, 如果对于他们的 initializer 的推导不一样, 编译器将会报错(即使是有无
	 * const 修饰, double和int之类的情况, 所以说 auto 不是像宏一样单纯的直接替换)
	 * 
	 * **decltype 关键字(C++ 11) (发音 deck-ell type)**
	 * 有时候我们只想要使用编译器推断出我们给出的一个表达式的类型来定义变量, 而不需要必须显式的给出 initializer
	 * 使用 decltype(expression) 就可以让编译器为我们推断出 expression 的类型并且把它直接作为类型使用.
	 * 
	 * 有点类似 auto 的类型推导, 不过在 top-level const 和 references 的推导上面与 auto 的推导有点区别:
	 * 1. 如果 decltype 要推导的表达式是变量, decltype 将会返回 变量的类型, **包括 top-level const 和 references**.
	 * 并且 decltype 推导出来的类型的变量声明也要按照该类型的规则, e.g. decltype 推导出引用类型, 用它来声明变量的时候,
	 * 和正常的引用类型声明一样, 必须显式的初始化.
	 * 2. 如果 decltype 要推导的表达式不是变量, 将推导该表达式生成的对象的类型(e.g. 函数, 变量的算法运算之类的), 如果表达式产生
	 * 的是引用类型的对象, 则 decltype 推导出来的也是引用类型. 通俗的讲, 如果 decltype 中的表达式能够作为赋值(assignment)语句的
	 * 左边, i.e. lvalue, *向后引用*, e.g. int r = 0; decltype(r + 0) 推导出来的是 int, 因为 r + 0 = 9; 这样的赋值语句非法,
	 * 而 int *p = &r; decltype(*p) 推导出来的是 int&, 因为 *p = 1; 这样的赋值语句合法, decltype 将返回引用类型.
	 *
	 * > 在变量名外面加一层或多层括号是表达式而不是变量, 并且是可以作为**左值**(*向后引用*)的表达式, e.g. int r = 0; 
	 * > decltype( (r) ) 和 decltype( ((r)) ) 都是返回 int &, 而 decltype(r)返回 int
	 * 
	 * 类定义可以使用 struct 或 class 关键字, 并且类定义语句最后面必须加上, 因为这个语句其实就类似于变量声明,
	 * 只不过在类定义的时候直接声明是一个不好的习惯(因为这样可读性不强), 不过在使用 struct 定义结构体的时候倒是
	 * 这种用法还算多(因为一般结构体就相对简单, 并且结构体里面的成员都一定是 public 的, 没有其他访问权限可选).
	 * 
	 * C++11允许类中的实例成员在定义的时候初始化(in-class initializer而以前是不允许的), 成员变量默认的隐式的初始化会遵循前面
	 * 所讲的 Default Initialization, e.g. struct Clazz { int count = 0; }
	 * 
	 * > 并且为了区分变量初始化和函数原型声明, 类成员变量初始化只能使用 { } 这样的 list initialization 或 = 符号初始化,
	 * > 不能使用 () 初始化
	 * 
	 * 头文件(Header File)
	 * 头文件当中一般包括类定义, const 和 constexpr 变量, 以及全局函数的原型.
	 * 
	 * 为了避免头文件被重复引用多次(能够), 一般使用 #ifndef #ifdef (这两个是header guards) 和 #define 预处理器. 
	 * 在编译之前预先处理一下源代码.
	 * 
	 * > 预处理器变量名称不需要遵循 C++ 变量命名规范, e.g. #define int 100; // 将源代码中的 int 全部替换成 100
	 * 
	 * > 为了避免预处理变量名与其他变量名冲突, 一般使用全部大写来命名.
	 * 
	 * 
	 */

	// list init
	// ! int listInit{ 3.14 }; // error/warning: narrowing conversion required
	int listInit0 = { 3 }, listInit1(3.6);
	cout << listInit0 << ", " << listInit1;

	extern int headerIntOut;
	cout << "\nheaderInt: " << headerIntOut <<"\n";

	//标准库使用的命名风格, 用户程序为了避免冲突, 请不要使用
	int __Do_not_use__ = 100;
	__Do_not_use__ ++;

	// C++ Alternative Opearator Names
	cout << (5 not_eq 6) << "\n";

	// outer scope i
	int i = 5, sum = 0;
	i++;
	// inner scope i
	for (int i = 0; i < 10; i++)
		sum += i;

	// create alias of i
	int &refVal = i;
	// refVal2 is bound to the object which refVal is bound, i.e., to i
	int &refVal2 = refVal;
	cout << "refVal2: " << refVal2++ << "\n";
	cout << "i: " << i <<"\n";
	double doubleVal = refVal2;
	
	// const int reference to literals(bcz literals is const)
	const int &constRef = 29;
	doubleVal = constRef;
	doubleVal++;

	int *pi = &i;
	// ! double *pd = pi; // error

	// void pointer, 必须要强制转化为 int* 才能用
	void *voidPtr = pi;
	cout << "voidPtr: " << *((int*)voidPtr) << endl;

	// reference to pointer
	int *&refPtr = pi;
	pi = &headerIntOut;
	*refPtr = 1;
	
	cout << "bufSize: " << bufSize << "\n";

	// A reference to const may refer to an object that is not const
	double dVal = 2.3;
	const int &ri = dVal;
	cout << "ri: " << ri << "\n";
	const int &ri2 = i;
	cout << "ri2: " << ri2 << "\n";
	/**
	 * 虽然 reference to const 不能用 const reference来改变所引用的 nonconst object 的值, 不过依然可以修改
	 * 这个 nonconst object
	 */
	i = 250;
	cout << "ri2: " << ri2 << "\n";

	double d = 3.2;
	double &intRef = d;
	// const reference 可以绑定到各种 nonconst types, 甚至可以像 build-in arithmetic types 一样隐式得转换.
	const int &intConstRef = intRef;
	intRef = intConstRef;

	//C++11 constexpr
	// headerIntOut必须是固定地址的object, 也就是外函数体外的对象
	// ptConstExpr 是 constant pointer to const int.
	constexpr const int *ptConstExpr = &headerIntOut;
	headerIntOut = *ptConstExpr;

	// 别名
	typedef char *cstr;
	/**
	 * pstring 相当于是限定 cstr 为const, 也就是 pstring 是 constant pointer to char*, 也就是 char *const *
	 */
	const cstr *pstring = nullptr;
	dVal = (double) (long long)pstring;

	// ! auto &refI = i, *ptrRi = &ri; // i是 nonconst, ri 是 const

	// decltype
	decltype( i ) sum2 = 0;
	decltype( i + 0) sum3;
	decltype(6.8) sum4 = 8; // 这里可以有隐式得类型转换
	// ! decltype( (i) ) refSum; //refSum是 reference, 必须显式的初始化
	// 在 gnu g++ 上的 typeid 获取的类型是简称, 并且没有区分引用类型, 没有 msvc 上的人性化
	decltype( ((i)) ) refSum2 = sum2;
	cout << "typeid: decltype(i) = " << typeid(sum2).name() << ", decltype(((i))) = " 
		<< typeid(refSum2).name() << ", decltype(i + 0) = " << typeid(sum3).name()
		<< ", decltype(6.8) = " << typeid(sum4).name() << "\n";
	cout << "type of sum2 " << (typeid(sum2) == typeid(int) ? "is " : "is not ") 
		<< "same as int\n";
	cout << "type of refSum2 " << (typeid(refSum2) == typeid(int&) ? "is " : "is not ") 
		<< "same as int\n";
	decltype(&i) ptrI = nullptr;
	cout << "type of decltype(&i) is " << typeid(ptrI).name() << "\n"; 

	cout << "\nPress any key to continue...\n";
	getchar();
	return 0;
}
