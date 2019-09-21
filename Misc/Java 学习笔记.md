---
title: Java 学习笔记
tags: Java,入门,笔记
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

# 绪论

## OOP三原则：
* 封装（encapsulation） 
* 继承（inheritance） 
* 多态（polymorphism）

## OOP导论：（原理和ADT类似） 
### 访问控制
限制类的内部访问边界：public ：后面接的元素对所有用户都可用，private ：除类型创建者和类型的内部方法外不能访问，protected ：与private相当，不过继承的类可以访问protected，默认的访问权限（包访问权限）：类可以访问同一个包中的其他类的成员，在包之外，这些成员如同指定了private一样。

### 继承
创建类的副本，当源类（基类、超类或父类）发生改变时，导出类（集成类或子类）也发生改变。 
使导出类和基类产生差异：1.直接在导出类中添加新的方法（导出类与基类的关系：“像是一个”） 2.改变现有基类的方法（覆盖）（导出类与基类的关系：“是一个”）导出类可用于纯粹替代基类。 
伴随多态的可互换对象：对于一个泛化的对象，Java（以及其他OOP语言）的编译器采用**后期绑定**的方法处理泛化的对象，当对象发送消息的时候，被调用的代码直到程序运行的时候才被确定。且该动态绑定在Java中是默认行为。 

### 向上转型
将导出类（子类）看作是它的基类（泛化的）的过程。 

### 单根继承结构
除C\++外，所有的OOP语言，所有的类最终都继承自单一的基类，（这个基类就叫Object）即单根继承结构中的所有对象都具有一个共用接口。

### 容器
用于存储对象，在任何需要时都可以扩充自己以容纳你置于其中的所有东西。如List（用于存储序列）、Map（关联数组）、Set（每种对象类型只持有一个）...... 
e.g. ArrayList（数组序列）和LinkedList（链表序列），在序列中插入一个项目，LinkedList比ArrayList的开销要小，而随机读取时ArrayList比LinkedList的开销要小，接口List所带来的抽象，把在容器之间进行转换时对代码产生的影响降低到最低限度。 

### 参数化类型（JavaSE5新特性）
编译器可以定制作用于特定类型上的类，只接纳和取出特定对象的容器。参数化类型在Java中称为范型，用一对尖括号中间包含类型信息，通过这些特征可以识别对范型的使用。 
JavaSE5之前，容器的存储对象都只具有Object类型（Java中的通用类型），这样可以存储所有单根继承结构的类型，可是向上转型为Object再向下转型可能会转型到错误的类型，除非知道确切的类型（不然会消耗更多的时间来处理异常和额外的检查时间）。 

### 对象的创建和生命期
在C++中，将对象置于堆栈或静态存储区域中，对象的存储空间和生命周期可在编程时确定。在Java中在称为堆的内存池中动态的创建对象，并由Java的垃圾回收机制自动回收，这样可以有效的避免内存泄漏。 


# (P)1 Java基础

## (p)1.1 Java基本元素
空白符（而且Java是一种格式自由的语言）、标识符（Java大小写敏感，SE8不推荐用 `_` 作为标识符）、字面值（literal）、注释（Java有文档注释）、运算符（SE8引入 `::` 用于创建方法或构造函数引用）、分隔符以及关键字（Java保留 `const` 和 `goto` 关键字但没有用）

> p.s. 可以用代码块替代statement

标识符(变量名)由Unicode字母(包括中文字符), 下划线, 美元符号和数字, 其中 **数字不能用作标识符的开头**, 并且 **下划线和美元符号开头都是不推荐的**.

> 标识符大小写敏感, 不能为**Java关键字, false, null或true**

## (p)1.2 数据结构

### (p)1.2.1 基本数据结构

Java是强类型化的语言（和C一样）。
基本数据类型：
* byte（8bits，在使用网络或文件的数据流时很有用）
* short（16bits，最不常用）
* int（32bits，最常用） 
* long（64bits 字面量通常用后缀L或l）
* char（16bits unicode码） float（32bits 字面量后缀f或F） 
* double（64bits，适用于保持精度的运算，最常用 字面量后缀D或d） 
* boolean （true false并没有对应的数值，而且0不代表假 非0不代表真 和C/C++不一样）

类型 | 所占空间 | 包装器类型 | 默认值(只有当变量作为类的成员(域变量)时才给定默认值,不适用于局部变量(i.e.在某个方法内))
------------- | ------------- | ------------ | ------------
boolean |   -         | Boolean                              |   false
char       |  16bits  | Character                          |    '\u0000'(null)(unicode编码)
byte       |  8bits    |  Byte                                  |   (byte)0
short      |  16bits  |  Short                                |   (short)0
int          |  32bits  |  Integer                              |   0
long       |  64bits  |  Long                                 |   0L
float       |  32bits  | Float                                   |   0.0f
double   |  64bits  | Double                                |   0.0d
void       |   -          | Void

> p.s. Java的所有类型都是有符号的

### (p)1.2.2 自动包装机制

在有需要的时候，Java会隐式的自动把基本数据类型包装成包装器类型。
p.s. JDK7新增二进制表示整数：需要加上前缀0b或者0B e.g. int x = 0b1010；JDK7还允许在数字间（包括小数）加入一个或者多个下划线方便记忆大数字，在编译的时候会自动删除 e.g. int x = 123_456\_\_7\_\_\_\_9;（尤其是在二进制数字中可以作为四个四个的分隔符）

### (p)1.2.3 高精度数字

 Java提供了两种用于高精度计算的类:BigInter和BigDecimal(不是基本类型)
 
### (p)1.2.4 字面量（literal）

* 八进制：0前缀 
* 十六进制：0X或0x前缀  
* 二进制（SE7）：0b或0B前缀 

JDK7新增二进制表示整数：需要加上前缀0b或者0B e.g. int x = 0b1010；
JDK7还允许在数字间（包括小数）加入一个或者多个下划线方便记忆大数字，在编译的时候会自动删除 e.g. int x = 123_456\_\_7\_\_\_\_9;（尤其是在二进制数字中可以作为四个四个的分隔符）

字符型字面量：转义字符：\ddd （d为八进制数） \uxxxx（x为十六进数）

变量声明：Java支持动态声明。Java不支持方法内部声明的变量在该方法内部被内层定义覆盖（和C不一样），不过方法可以覆盖所属类中的域变量。

### (p)1.2.5 类型转换

Java能自动执行扩展转换，窄化转换需要显式的使用指派操作符强制转换（可能会造成移除）。浮点型窄化转换会趋零截尾。
Java也是截尾转换, 若要舍入，使用java.lang.Math.round()方法.
表达式中会发生自动类型提升，所有的byte short char自动提升到int，整数表达式的值都会自动提升到int  e.g. byte b = 50；b = b * 2；这时就会报错，必须（byte）强制转换。
含有float（double）的表达式的值会被提升到float（double）。

### (p)1.2.6 数组：（可以使用int[] arr也就是使用int arr[]的形式）
p.s. 数组类中的length成员字段只是反映数组最初设计时所能包含的元素数量。
可以手动分配某一维的每一个元素的大小，使同一维度的元素的长度不同，创建不规则数组。
数组初始化的时候可以使用表达式。

p.s. Java不支持（可以修改的）指针（因为在JVM中使用指针不安全嘛），不过可以调用native C++代码使用指针233333

### (p)1.2.7 作用域
Java支持动态声明。Java的基本类型和对象的引用的生命周期和C/C++一样(因为在堆栈中),但是Java不支持方法内部声明的变量在该方法内部被内层定义覆盖（和C不一样），不过方法可以覆盖所属类中的域变量。
 i.e. 
 ~~~ java
 void someMethod() {
 	int x = 12；
		{
			int  x = 96；
		}
	}
~~~

Java的对象不具备和基本类型一样的生命周期,他们可以存活于作用域之外.Java有一个垃圾回收器用来监视new创建的所有对象,当不在需要时自动销毁.

## (p)1.3 对象

### (p)1.3.1 字段和方法

可以在类中设置两种类型的元素：字段（数据成员）和方法(成员函数).字段可以是任意类型的对象(需要引用和初始化(用new初始化))和任意基本类型.普通字段不能在对象间共享,给对象赋值:objectReference.member 
e.g. 
~~~ java
class DataOnly {
	int i; //自动赋默认值0
	} 
DataOnly data = new DataOnly();
data.i = 47;//DataOnly为类,data为对象,i为字段.
~~~
而在某个方法内定义的局部变量不初始化是和C/C++一样得到垃圾数据,不过Java编译时会对这种未初始化的局部变量视为错误.
Java支持在定义类成员变量的地方为其赋初始值（可以调用某个方法来提供初始值）（C++不行）.

方法,参数和返回值 ReturnType methodName(/\*Argument list\*/) {/\*Method body方法体\*/} 方法名和参数列表(合称方法签名)唯一的标识出某个方法.
> p.s. Main的参数列表是从所接收的参数开始的,而非是从程序名称开始(所以和C/C++不同)，args.length为参数个数，

Java中的方法只能作为类的一部分来创建，方法(非static的)只能通过对象才能被调用。i.e. objectName.methodName(arg1,…);
e.g.  x = a.fun();这种调用被称为发送消息给对象  (fun()是消息,a是对象)
参数列表中必须指定每个所传递对象的类型及名字.关键字return的用法和在C/C++中的函数是一样的.

### (p)1.3.2 名字可见性

用反转的域名来表示包名(类库（library）)
e.g. `com.google.system.android`.

调用某个在同一源文件中的类,可以在任意位置定义该类(不一定要调用前定义).如果类位于其他的文件中,用关键字import来导入所在的类库(包) e.g. import java.util.ArrayList;(这是Java标准类库里面的构件,所以不用写一大段的反转域名,java.util是类库,ArrayList是其中的一个类),如果要导入util中的所有类,可以 import java.util.\*;使用通配符`*`.

### (p)1.3.3 static关键字
适用于
* 只想为某个特定域分配单一存储空间
* 希望某个方法不与包含它的类的任何对象关联在一起(即使没有创建对象,也能够调用这个方法)

e.g. class SaticTest {
		static int i = 47; } 
如果创建了两个StaticTest对象,StaticTest.i也只有一份存储空间,这两个对象共享同一个i;也可以直接通过其类名直接引用，e.g. StaticTest.i++;
	对于静态的方法,也可以用ClassName.mehod()直接引用.在静态的方法中不能引用非静态的方法或字段.
无论创建多少个对象，静态数据都只占用一份存储区域，static关键字不能应用于局部变量，只能作用与域。如果一个域是静态的基本类型域且没有初始化，编译器会给他自动初始化为标准类型的标准初值，如果是对象引用，那么它的默认初始化值就是null。

### (p)1.3.4 注释

`/**/`和`//`的用法和C/C++一样, ==javadoc== 可以用于提取注释的工具,输出一个html文件. 所有javadoc命令都只能在 ==/\*\*(两个\*)开头\*/== 结尾的注释中.

使用javadoc的方式:嵌入html,或使用文档标签.
	独立文档便签:/**@标签内容…*/ ;
	行内文档标签(怎么感觉像是注解):可以在源代码中的任何位置,以@开头,但是要在花括号内.e.g. class Demo {@override void fun() {} }
	三种类型的注释文档:类,域和方法注释,都放在它们的前面.
	e.g.
``` java
//: object/Hello.java
	import java.util.*;
	
/**  A class comment
	 *@author DCMMC
	 *@version 1.0
	 */
	public class Hello {
		/** A field comment */
		public int i;
		/** A method comment 
		 @param args arrays of string arguments
		 @throws exceptions No exceptions thrown
		*/
		public static void main(String[] args) {
			System.out.println("hello");
			System.out.println(new Date());
		}
} /*Output:(55% match) // /*Output标签表示输出的开始部分将有这个文件生成,55%match是向测试系统(???作者写的基于ant的测试系统???)说明程序的输出与输出预期只有55%的相关性
	hello
	Wed Oct 05 14:39:36 MDT 2017
	*///:~
```

> p.s. javadoc只能为public和protected成员进行文档注释,private和包内可访问成员的注释会被忽略掉,所以输出结果中看不到它们(可以用-private进行标记,这样输出也能看到private成员的注释)

可在javadoc注释总内嵌html语法,在文档注释中,位于每一行的星号和前导空格都会被javadoc丢弃.
	标签： 
1. @see fully-qualified-classname#mathod-name 引用其他类的文档 在生成的文档中一个具有超链接的"See Also(参见)"条目
1.  {@link package.class#member label} 与@see类似,只是它作用于行内,而且用"label"作为超链接文本
3. {@docRoot} 生成文档根目录的相对路径,用于文档树的显示超链接.
4. {@inheritDoc} 从当前这个类的最直接的基类中继承相关文档到当前的文档注释中
5. @version version-information 如果javadoc命令行使用了-version标记,那么就从生成的html文档中特别提取出版本信息.
6. @author author-information 可以使用多个标签(必须连续放置)
7. @since 指定程序代码最早使用的版本
8. @param parameter-name description 为参数写描述,description是可以延续数行的文本,终止于新的文档标签出现,下同
9. @return description 描述返回值的含义
10. @throws fully-qualified-class-name description对异常类进行说明
11. @deprecated 指出一些就特性已由改进的新特性所取代,如果是使用一个标记为@deprecated的方法,编译器会发出警告.

[Java编程语言编码定](http://www.oracle.com/technetwork/java/codeconv/index.html)

## (p)1.4 运算符

Java的运算符、关系操作符、逻辑运算符（Java的逻辑运算符不能作用于非布尔类型，因为布尔类型的值不是按照0和非0的）、按位操作符、移位操作符（>>在Java中叫（有符号）右移操作符，若符号为正则在高位插入0，若符号为负则在高位插入1，Java还多了一种无符号右移操作符>>>不管正负全部用0插入；对于char、byte和short的无符号右移操作符，会先转换（提升）为int，再右移，然后截断再转换回去）和C/C++基本一致,基本类型支持所有操作符,=,==,!=这三个操作符可以支持所有的对象,String类支持+(字符串连接)和+=操作符.

用Long和Integer中的toBinaryString方法可以查看他们的二进制

> p.s.用==和!=比较对象的引用的时候比较的是是否指向同一个对象(类比C/C++的指针)。(准确的来说就是比较左右值这两个引用的hashCode)

当字符串后面紧跟着一个+而+后面紧跟着一个非String类型的元素时,就会尝试把这个非String类型的元素转换为String.

操作符的优先级,记不住的话就用括号明确规定计算顺序.

和C/C++一样，Java的表达式中常量不能作为左值。

对于对象的赋值,因为实际是将对象的引用的复制,这期中一个引用使该对象的值改变的时候,另外一个引用也受到影响(因为他们指向同一个对象).
Java中的Boolean类型不能比较大小,因为他们没有数字值.

用方法equals()比较对象的实际内容(适用于所有对象). e.g. obj1.equals(obj2) 但是当新建的类中没有定义equals方法时，默认还是比较其是否指向同一对象。

### 短路
在Java中的逻辑运算表达式中，若表达式未执行完的时候就已经可以肯定表达式的值了，那么后面的（浪费时间的）运算将不会执行。e.g. test1(0) && test2(2) && test3(3) 中test1(0)为true test2(2) 为false 那么整个表达式的值一定就是false了，所以不会执行test3了。
 Java也可以在直接常量后面加与直接常量相关的的某些字符 e.g. long l = 100L;
可以使用Integer和Long类的静态方法toBinaryString()把整型的二进制形式展现出来。

科学计数法：java和C类似

Java没有sizeof操作符
溢出的时候Java并不会报错。

## (p)1.5 控制语句

### (p)1.5.1 选择语句
结构化语句：
if（condition）
	statement；
else
	statement2；
switch(expressoin) { 
	case value1 : 
		//statement sequence
		break;
	case value2 :
		//statement sequence
		break;
	…
	default :
	//default statement sequence
}
p.s. JDK 7开始，expression可以是String类型了。但是String分支更加耗时，可以不用就不用。
相对于使用一系列嵌套的if语句，switch效率更高。
迭代语句：
while（condition）
	statement；
do 
	statement；
 while（condition）；
for（initialization；condition；iteration）statement；
foreach（SE5）：for（type itr-var ： collection）statement；//这里的 每一个itr-var都只是拷贝，改变他们不会影响原数组

### (p)1.5.2 foreach语法（Java SE5）
自动产生数组的每一项的序列
e.g. float arr[] = new float[10];
for(float x : arr)
	st;
这里的x会会在循环中自动的从arr[0]到arr[9]按顺序的初始化。
String类中有个toCharArray方法对字符串返回字符数组。String类有个split（String）方法可以按照参数的字符串分隔开一个字符串并且返回字符串数组。


### (p)1.5.3 转跳语句：continue break return （异常其实也算）
Java的类goto语法：
break label；label只能放在代码块（就有标签的代码块必须具有break语句）前面，break常用于退出多个循环
continue  label；label只能放在迭代语句前
label（任意合法的标识符）: statement… 
e.g. 
label1 :
outer-iteration {
	inner-iteration {
	break;
	continue;
	continue label1;//转到label且进入紧接着label的循环
	break label1;//中断且不会进入到label所指的循环
	}
}

## (p)1.6 初始化与清理

### (p)1.6.1 构造器

Java引入了C++的构造器（constructor）的概念，这是个在创建对象的时候自动调用（initialize）的特殊方法。
构造器采用与类相同的名称，在构造器中可以调用其他方法（支持**向前调用**）。
e.g. class Constructor｛//构造器也是static方法，因为类是在其任何static成员被访问的时候加载的
	Constructor（args…）{//构造器没有返回值（其他方法即使是void的也可以返回一个值）
	｝
}
构造器的参数可在创建对象的时候接收相应参数。不接收任何参数的构造器叫默认构造器（无参构造器）。
如果方法没有定义构造器，编译器会自动创建默认构造器。

用this(arg-list)调用指定的构造器,使用他们有助于减少代码重复量,有助于结构化代码.
不过this()的构造器相对于那些包含所有内联初始化代码的构造器来说,执行速度要慢一些.而且对于比较短的构造器,this()并不能节省加载时间,反而会增加在方法间转跳花费的时间.
所以this()适合于大量初始化代码的构造器.
> p.s. 同一个构造器中不能同时使用super() 和 this() 因为它们都位于构造器的第一条语句.

### (p)1.6.2 方法重载

为了让一个类中同时存在多个不同参数类型的构造器或方法。只要有独一无二的参数列表就可以重载，即使只是参数的顺序不同（不推荐这样，会使代码难以维护）
涉及基本类型的重载：常整数会被当作**int**，参浮点数会被当作**double**；如果传入的参数小于方法中声明的形参，则会被自动扩展转换（提升）为最接近（**最具体的（Specific）**，i.e. 继承的子类 e.g. Integer比Object更具体）的类型；如果传入的参数大于方法中声明的形参，必须在代码中强制类型转换（窄化转换），不然编译器会报错。
方法重载的时候 char类型的参数比较特殊:如果有参数恰好接受char的方法 那个会重载为这个方法 如果没有恰好符合的 则直接提升为int

### (p)1.6.3 this关键字

只能在方法内部使用，表示对调用方法的那个对象的引用，编译器会为类内的每一个字段和类中方法的引用添加this关键字，不用自己写。

this常用于return语句，返回对当前对象的引用e.g. return this; 

this还可以用于将其自身传递给外部方法；this也可用于在构造器中调用构造器（this(args…);就当然于调用构造器）；

this可以用于在方法中创建的与类的域中同名的局部变量内层定义覆盖了类的域中的变量，这时候可以用this访问 e.g. int x；｛int x = 0；this.x = 5;｝。

p.s. 对this的调用（this（args…）语句）必须是构造器中的第一个语句且只能调用一个，用this访问对象的域可以不用放在构造器的第一个语句。

static的含义：static方法就是没有this（**这是非静态的变量**）的方法，在static方法的内部不能调用非静态方法（不过可以传递一个对象的引用到静态方法里面，然后通过这个引用调用非静态方法和访问非静态字段），并且可以在没有创建任何对象的前提下，仅仅通过类本身来调用static方法，在类中置入static方法就可以访问其他static方法和static域。

### (p)1.6.4 终结处理和垃圾清理

Java的垃圾回收只会释放那些经由new分配的内存，如果对象获得了一块特殊的内存区域，就不能被垃圾回收了。Java允许在类中定义一个名为finalize()的方法：一旦垃圾回收器准备好释放对象占用的存储空间，将首先调用其finalize方法，并且在下一次垃圾回收动作发生时才会真正回收对象占用的内存。

p.s. finalize()方法只能负责通过某种创建对象以外的方式为对象分配了存储空间（e.g. 在Java中使用**本地方法**（C/C++）调用malloc()等函数分配的内存）。**终结函数无法预料，常常是危险的，总之是多余的。（《Effective Java》）**

pps. 无论是垃圾回收还是终结在并未内存耗尽的情况下都不一定发生，因为这些会浪费时间。

可以用finalize()来验证**终结条件**：
System.gc();用于强制进行终结动作（force garbage collection & finalization）（不然导致垃圾回收动作的执行）

垃圾回收机制：
一种简单但速度很慢的垃圾回收制度（Java没用过这种）：引用记数技术：每个对象都有个引用计数器，垃圾回收器会在含有全部对象的列表上遍历，当发现引用计数器为0的时候，就会释放其占用的空间。但是对象之间存在这循环引用可能出现“对象应该被回收，但是记数却不为0”。

Java使用的垃圾回收技术：对于任何活的对象，一定能追溯在其存活在堆栈或静态存储区之中的引用。所以Java会遍历所有引用，直到根源与堆栈和静态存储区的引用所形成的所有网络全部被访为止，找到所有活的对象。在这种方式下，JVM将采用一种自适应的垃圾回收技术（取决于不同的Java虚拟机的实现）。
有一种做法叫做**停止-复制（stop-and-copy）**，i.e.先暂停程序的运行（所以它不属于后台回收模式），然后将所有存活的对象从当前堆复制到另一个堆，没有复制的都是垃圾，当对象被复制到新堆时，他们是一个挨着一个的，所以新堆保持紧凑排列。所有指向它的这些引用都必须修正，对于堆或静态存储区的引用可以直接被修正，其他指向这些对象的引用在遍历的过程中才能被找到。p.s 但是这样会降低效率，而且在没有多少垃圾的时候很浪费。有些Java实现会把堆中分配几块较大的内存（块），复制动作发生在这些大块内存之间。
为了避免垃圾少的时候**停止-复制**过于浪费，垃圾少的时候会转换到**标记-清扫（mark-and-sweep）**模式，这种方式相当慢，从堆栈和静态存储区出发，遍历所有的引用，进而找到所有存活的对象，每当找到一个存活对象，就会给对象设一个标志，全部标记工作完成的时候，清理动作就会开始，没有标记的对象将被释放，最后在重新整理对象，使他们连续。

有了块之后，垃圾回收器在回收的时候就可以往废弃的块里拷贝对象了，每个块都用相应的**代数**来记录它是否存活。如果块在某处被引用，其代数就会增加。垃圾回收器会定期进行完整的清理动作：大型对象仍然不会被复制（只是其代数会增加），内含小型对象的那些块则被复制并整理。

**即时（Just-In-Time，JIT）编译器**技术：把程序全部或者部分翻译成本地机器码，当需要转载某个类的时候，编译器会先找到其.class文件，将该类的字节码装入内存。
	1. 即时编译器编译所有代码，缺点：这种加载动作散落在整个程序生命周期内，而且会增加可执行代码的长度（字节码要比即使编译器展开后的本地机器码小很多）
	2. 惰性评估：即时编译器只在必要的时候才编译代码。代码每次被执行的时候就会做一些优化，所以执行的次数越多，它的速度就越快。

### (p)1.6.5 初始化

在类的内部，变量定义的不论散布在何处，都会在任何方法前（包括构造器）被调用之前得到初始化（包括自动初始化）。
在声明引用的时候不初始化，而在使用对象之前再初始化称为**惰性初始化**。

静态数据的初始化：
静态初始化只会在对象第一次创建（或者第一次访问静态数据）的时候初始化，此后就不会再次初始化了。

显式的静态初始化：
Java允许将多个静态初始化动作组织成一个特殊的静态子句（静态块）
e.g. 在类的字段的位置处：
static ｛
st1;
st2;
…｝这段代码跟静态字段的初始化一样，只会在首次生成这个类的对象的时候或者首次访问这个类的静态数据成员的时候（即使从未生成过那个类的对象）执行。他们的作用域只在这对｛｝中。

非静态实例初始化：
{
st1;
st2;
…
｝这种语法对于匿名内部类的初始化是必须的，实例初始化在每次创建对象的时候在所有方法（包括构造器）之前执行。他们的作用域只在这对｛｝中。
p.s. 显式的静态初始化的语句在非静态实例初始化中的语句之前执行（就算在代码中的同一个类里面显式的静态初始化在非静态示例初始化之后）

数组初始化：
子类数组对象可以赋值给基类数组引用 但是这种向上转型会包含原来子类对象类型的规则，这就是只允许放入子类或者子类的导出类的对象。
int[] array;（符合Java的风格 int[]表明是一个int型数组）
或者 int array[];(符合C和C++程序员的习惯）
数组的初始化类似于C/C++
Java可以直接将一个数组赋值给另外一个数组（只是复制了引用）
Java会检查数组是否越界，若越界则会抛出异常(i.e. 运行时错误）（消耗资源，无法禁用这一功能）。
可以用new创建一个确定数组元素个数的数组，并且每个元素都会赋初始基本数据标准初始值。
e.g. ClassOrBasicDataType[] ArrayName = new ClassOrBasicDataType[counts];
或者ClassOrBasicDataType[] ArrayName = new ClassOrBasicDataType[] { elements1,elements2 …elements_n };//element ClassArray usually use new Class(args…)
可以用Arrays.toString()方法把一维数组（非引用数组）转化为形如"[元素1,…,元素n]"的字符串。
对于引用数组的打印，如果数组元素所属的类中没有定义toString()方法的话，默认行为就是打印：类的名字@对象的地址(hashCode) 
数组下的length可以返回数组的元素个数，e.g. array.length
如果创建了一个非基本类型的数组，那么就是创建的一个引用数组。如果直接使用数组中的空引用，就会在运行时产生异常。

### (p)1.6.6 可变参数列表

可以使用Java所有的类都直接或间接继承于Object类，可以创建Object数组作为参数，兼容所有类型的数组
method（Object…args）｛
｝这里的args将会是一个以Object为元素类型的、参数个数为元素个数 的数组,甚至object数组可以直接作为参数（args就是这个数组）传递给方法(其他数组直接作为参数的时候要（Object）强制转换)。（Object可以换成任意的基本数据类型或者任意的类）
亦可以写成method(Object elements1,…,Object elements_n,Object … args)  “…”只能在最后一个元素后面（跟C/C++一样）。（这里的args一样的也是一个Object类的数组）
getClass()方法属于Object的一部分
可变参数列表可以和**自动包装机制（会按照形式参量的要求把实际参数有选择的自动转型，e.g. 把int转型为Integer包装器）**同时配合。
对于**方法重载**，有些时候因为有个可变参数，java会对容易产生模糊（ambiguous）的地方报错，这时候在方法列表添加一个非可变参数，就可以解决问题。

### (p)1.6.7 枚举类型（Java SE5）
enum关键字：
e.g. Access enum Test｛element1,…,element_n｝//Access：public protected private 或者包访问权限
和C/C++类似。不过，在创建enum时，编译器会自动添加一些特性：
1. 创建toString()方法（一种特殊方法），一边可以方便的用print显示某个enum实例的名字
2. 创建ordinal()方法，用来表示某个特定enum常量的声明顺序
3. 创建static values(）方法，用来按照enum常量的声明顺序，产生由这些常量构成的数组。
enum可以配合switch语句使用


枚举类型不能在方法内定义也不能实例化 不过枚举类依然是类类型枚举为类添加构造器 实例变量和方法
public static enum-type[] values（） 
public static enum-type valueOf(String str)
枚举类型可以用在foreach 和switch中
每个枚举常量都是所属的枚举类型的对象,且都是final static的:
每个枚举常量都相当于调用了枚举类型的构造器 而且对于默认构造器枚举常量有没有括号都会调用默认构造器
枚举类型不能实例化
枚举类都继承自java.lang.Enmu类:
Enum中的方法:
final int ordinal() 返回枚举常数的序列值
final int compareTo(enum-type e) 比较调用常数的序列值与e的序列值的大小关系,小于e就返回负值,等于就返回0,大于返回正数 调用常数必须与e是相同的枚举 (可以使用Object类中定义的equals方法 比较是否为同一枚举类的相同序列 也可以使用==比较这两个枚举引用是否相同)

## (p)1.7 访问权限控制

在重构时，类库（library）开发者会更改实现甚至更改域，而类库的消费者（客户端程序员）则不想发生这样的变动。
为了解决这一问题，Java提供了访问权限修饰词，从最大权限到最先权限依次为：public protected 包访问权限（没有关键词） private。

### (p)1.7.1 包（package） ：库单元

用import导入包中的类：==import 包路径.类名;== 或者 ==import 包路径.\*== 导入包中的所有类
e.g. ==import java.util.Arraylist;== 这样就可以直接用Arraylist()调用了而不需要打java.util.Arraylist()。

Java默认把未归入包中的文件放在该目录的未命名包（默认包）中，所以同一个文件夹下的未归入包中的类互相具有包访问权限。

每个.java文件都是一个编译单元，每个编译单元内最多可以有一个public类（或者interface，反正interface和class中最多只能有一个是public）（而且该public类的名字必须和文件名一模一样），且该类名必须和文件的名字相同（包括大小写）。

非public类在包之外都是看不到的。main方法必须是public的.
在编译.java文件时，其中的每个类都会有一个.class输出文件。
用package语句指定构件（.java文件）从属于一个包，且必须是文件中除注释外的第一句程序代码。把构件放入一个包中，相当于给他一个单一的名字空间。

package的名字一般是反顺序的Internet域名（包名不能含有关键字），并把包的名字的据点转换为反斜杠，依次作为路径。e.g. org.github.packagename 转化为 org/github/packagename
p.s. 注意为代码树的根目录添加一个环境变量。

要直接使用import导入类中的静态方法，需要用 ==import static== 导入，e.g. ==import static net.mindview.util.Print.\*;== 将会导入util包下的Print类中的所有静态方法。

> java在command-line下加载class的时候也是必须按照class的全限定名称加载.
如果所在目录或者名称不完整的话, 就会提示 `找不到或无法加载主类`.
> + 包与系统文件夹一一对应
> + 运行class时，需要进入完整包名的第一个包所在的上一级目录，java才能将相应的class文件找出来
> 
> e.g. 某个要运行的类的全限定名称为 tk.dcmmc.project.Demo
则要运行这个类必须cd到tk的上一级目录, 然后再执行 ==java tk.dcmmc.project.Demo== 才行

### (p)1.7.2 包访问权限（没有关键字 或者叫做friendly）

包中的其他类对这个成员都有访问权限，但是对于包之外的所有类，这个成员是private 的。

### (p)1.7.3 public 接口访问权限

类库开发者想要把某些方法（接口）公开给客户端开发者常用public关键字声明接口访问权限。

### (p)1.7.4 private 私有访问权限

除了包含该成员的类之外，其他的类都无法访问这个成员。可用于构造器，阻止别人直接访问这个构造器上的例子或者创建该类的对象而只能调用该类中的静态方法。

构造器都是private的类不能用在别的类中用new创建，然而在别的类中却可以使用new创建这个类的数组（我知道为什么了：因为创建类的数组的时候并没有创建类的实例对象，类似于创建了C中的 ==int \*\* pt；pt =  (int \*\*) malloc (n1 \*int \*)== 这里并没有创建int类型的数据成员）。

### (p)1.7.5 protected：继承访问权限

protected也提供包访问权限，对于不在包中的成员，只有继承类可以访问。
extends关键字：从现有类中继承：e.g. class Foo extends Bar ｛｝//Foo为要创建的类，Bar为已经存在的类，Foo为子类，Bar为基类。且创建Foo的对象的时候会隐式得调用Bar的构造器。可以在Foo中直接访问Bar中的protected和public方法
接口和实现：
访问权限的控制常被称为具体实现的隐藏，把数据和方法包装进类中，以及具体实现的隐藏，共称为封装。

为了清楚起见，可以把按照public protected 包访问权限 private的顺序放置成员。
类（除了内部类）不可以是private的或者protected的。如果不希望任何人用new创建该对象，可以对类中的构造器使用private。（不过可以在类中创建一个静态方法（这样可以方便统计创建了多少个对象）返回该类这个类型的对象，可以在返回的时候用new创建，也可以在类的域的位置创建一个静态的该类的对象（可以用private static创建，这样就只能创建一个对象）然后用这个方法返回这个对象的引用。）


## (p)1.8  复用类

### (p)1.8.1 组合语法：

在类中创建别的类的对象

### (p)1.8.2 继承语法

子类会自动继承基类的非private方法和字段（在不同包的话只能继承public访问权限和protected访问权限的方法和字段）。
但是父类中的非private方法如果调用了private字段却可以正常访问private域.
组合比继承更能取得动态灵活性，组合比继承用得更多。

### (p)1.8.3 super关键字

可以在子类中覆盖基类中的方法和字段（签名和返回值要一模一样才会覆盖，而且权限只能保持一样或者提升，而不能降低权限），这时候用**super关键字（非静态的）**（和this可以配合使用）**(超类访问**)访问基类中被替换的方法 e.g. super.methodName();
Java SE5可以用｛@Override｝行内注解，在覆盖某个方法时，可以在方法前加上@Override，如果@Override后面的方法错误的写成了重载而不是覆盖的时候，编译器会报错。
调用超类的构造器（i.e. super(args…);）,只能放到第一行.
调用超类的方法：super.methodName(args…);

### (p)1.8.4 初始化基类

子类会隐式得继承基类的无参数的构造器的内容，插在子类无参数构造器的内容的前面。

子类不会继承基类的含参构造器. 带参数的基类构造器在子类中调用必须显式的使用super关键字放在子类构造器的第一行。

### (p)1.8.5 代理：（有点像组合和继承的复合）

（Java并不直接支持代理，不过JetBrains IDEA就支持这个）
在原本的基类和子类中创建一个代理层（也是个class，而且这个代理层不是一个继承与基类的子类），在代理层new一个基类的对象的引用，在这个代理层中重新实现基类的所有方法（连方法名字参数都弄成一样），并把需要额外添加的方法和域加进去，这样再把这个代理层作为基类继承给别人，这样别人就不能看到新加的方法的具体实现了。

### (p)1.8.6 正确清理

结合try子句和finally子句（后面会详细讲），避免使用finalize（）方法。

### (p)1.8.7 向上转型（upcast）

在子类中使用基类的引用来作为参数的某个方法，同样可以把子类的引用作为参数传递给该方法。这时候会把子类的引用自动向上转型为基类再把这个参数传递给该方法。所以向上转型总是很安全的。

> tips：组合技术比较常用，而继承只在必须使用向上转型的时候必须使用。

### (p)1.8.8 final关键字（常量数据、方法和类）

编译时常量（必须是基本数据类型）：编译器可以把常量值带入任何可能用到它的计算式中（final变量也可能初始化为一个其他方法的返回值，这时候编译器就不能知道他的值了），在编译时执行计算。**类不需要初始化就能直接读取final字段。**

当对对象引用使用final时，表示改引用只能指向初始化时指向的对象，而不能再把它改为指向另外一个对象，而这个对象其自身是可以被修改的。

按照惯例：用static且final的域用大写表示。

空白final：声明为final但又未给定初始值的域，但必须在使用前得到初始化。

final参数：在参数列表以声明的方式将参数指明为final，即在方法中无法更改该参数引用所指向的对象。

final方法：把方法锁住，防止任何继承类修改它的含义；（在过去的Java实现中，final的方法告诉编译器可以将对他们的所有调用都转为内嵌调用（跟C的inline类似），这能消除方法调用的时候的开销，提高效率，不过在现在的实现中，虚拟机会自动优化并去掉某些效率反而会降低的额外的内嵌调用。）private方法就属于final方法。

final类：表示该类不能被继承。final的域可以根据个人意愿选择是否是final。

### (p)1.8.9 类的加载

只有在创建类的第一个对象或者访问static域或static方法时才会加载类的class字节码，初次使用的时候也是static初始化的时候（按照定义类的书写顺序加载）
子类加载时会自动加载基类。
对象引用的默认值是null--这是所指向的对象的内存是二进制零值。null是一个特殊的量。


## (p)1.9 多态

多态通过分离做什么和怎么做，从另一个角度将接口和实现分离开。
一个基类继承出多个互相有差异的导出类，在把导出类作为本来接收基类的参数的方法的参数的时候，自动向上转型，这时候方法可能不能知道到底接收的是哪个子类向上转型产生的基类。

### (p)1.9.1 方法调用绑定

前期绑定：在程序执行前将一个方法调用同一个方法主题关联起来。（C与其他面向过程的语言的默认方法）

后期绑定（多态）（动态绑定，运行时绑定）：运行时根据对象（向上转型之前的导出类）的类型进行绑定（对应向上转型之前的导出类中的方法），这就Java的方法调用机制实现。Java中除了static方法和final方法（private方法也属于final方法）之外，其他所有的方法都是后期绑定。这样每次调用一个已经向上转型的对象中的方法时会自动识别该子类并调用子类中的该方法。后期绑定可以迭代。

> p.s. 对于没有从基类继承的private或者final方法，在子类中定义与这个方法相同签名和返回类型的方法不叫覆盖，对于子类来说只是创建了一个新的方法而已。所以自己明确是要覆盖基类的方法时最好加上一个@Override行内注释。

缺陷：域和静态方法：在子类被向上转型之后，任何域访问操作都是编译器解析的（所以不是多态的），所以直接访问该对象中已经被子类覆盖的域时，得到的却是基类中该域的值。而在子类的方法中访问域的时候得到的却是子类的域的值，在子类的方法中访问超类的域必须显式得使用super访问。但是实践中很少发生这种事，因为通常所有域都会设置成private，只能通过方法的副作用（也就是返回值）访问。

静态的方法也不具有多态，静态的方法是域类而且是域单个的对象关联的。（所以构造器也不具有多态）所以向上转型之后静态的方法将会只用基类的静态方法。

### (p)1.9.2 构造器和多态

> tips. 在使用默认构造器创建子类的对象时，最先执行基类默认构造器的内容，然后才是显式的静态初始化和非静态实例初始化和域的初始化内容按照书写顺序执行（pps. 不论顺序，显式的静态初始化永远比非静态实例初始化先执行），然后该是该子类的默认构造器的内容。

### (p)1.9.3 继承与清理

当覆盖被继承类的清理方法（在书中选用dispose（）方法）时，必须调用基类版本的清理方法，否则基类的清理动作就不会发生。p.s. 销毁顺序应该和初始化的顺序相反，先销毁导出类再销毁基类。
要清理成员对象中存在域其他一个或多个对象共享的情况，必需使用引用计数器来跟踪仍旧访问着共享对象的对象数量了。

### (p)1.9.4 构造器内部的多态方法的行为

肯定会招致灾难。在构造器中唯一能安全调用的是本类中的final方法（同样适用于private方法）。Java对于方法里面调用别的方法支持向前引用（i.e. 要调用的方法的定义可以在调用它的方法的后面）。

### (p)1.9.5 协变返回类型（Java SE5）

被覆盖方法可以返回基类方法的返回类型的某种导出类型。
向下转型与运行时类型识别（RTTI）：向下转型是不安全的，Java会对所有转型进行运行时类型识别以确保是我们希望的那种类型，否则会抛出ClassCastException（类转型异常）。


## (p)1.10 接口

基类只是为子类创建一些接口而这些接口往往没有实际内容，具体内容都在子类中覆盖，称为抽象类。
抽象方法：e.g. ==abstract void f();== 必须有 ==abstract== 关键字，并且可以不用写方法体的内容。包含抽象方法的类叫抽象类，同样用 ==abstract== 限定。

如果抽象类中的抽象方法同样没有提供定义，那么也必须用 ==abstract== 限定。

> p.s. 抽象类中如果没有指明任意一个构造器的话，编译器默认分配默认构造器，但是，当有其他重载构造器时，**必须显式的声明默认构造器**

### (p)1.10.1 接口

==nterface== 关键字产生一个完全抽象的类（用interface 替代 class关键字），不提供任何的具体实现，只创建方法名、参数列表和返回类型（接口不能有构造器）。接口中的域隐式的是 ==static== 和 ==final== 的。==interface== 中的方法和接口（嵌套的接口）被自动隐式的声明为 ==public== 的(所以在接口的具体实现时, 必须**显示**的声明要覆盖的接口中的方法为 ==public== 的)。

> p.s. 由于toString()方法是根类Object的一部分，interface创建的类会隐式的添加toString接口。

使用 ==implements== 关键字（替代 ==extends== 关键字的位置）表示为 ==interface== 声明其接口的具体实现，并且 ==interface== 中的域也能访问还能覆盖，还可以在类中添加新的接口。（必须实现所有的接口，此时这个类就是普通的类了）

> p.s. interface的实现中用不了super关键字

### (p)1.10.2 策略设计模式

对算法的封装，创建一个能够根据所传递的参数对象的不同而具有不同行为的方法。i.e. 弄一个基类，然后多态（继承）出很多具体方法，再在要执行的类中写一个接收其基类作为参数的方法。

### (p)1.10.3 完全解耦

假设类 ==B== 中某方法 ==m(A argA)== 的参数要求为某一类A的对象，所以称方法 ==m()== 和类 ==A== 耦合。
而如果此时类 ==C== 具有和类A相同的接口元素，复用 ==B.m(A)== (也就是想把 ==C== 作为 ==m()== 的参数)时被禁止。
解决这种问题可以把A用接口的方式重构，然后每次为了在创建新的类的需要复用该方法的时候，重新实现这个接口（适配器设计模式），解决代码复用性差的问题。

**适配器设计模式**
主要作用是在新接口和老接口之间进行适配。i.e. 创造一个适配器类实现这个接口，然后在新方法的方法体中调用老方法。（就像在二脚插座上实现三角插座，在三角插座的方法中调用二脚插座）。

### (p)1.10.4 Java中的伪多重继承（本来Java只有单根继承结构）

可以将多个接口组合在一起。只有 ==interface== 能够多重继承其他多个 ==interface==, e.g. ==interface IntSub extends IntSuper1, IntSuper2;==
要从一个非接口的类继承，只能从一个类去继承，其余的基元素都必须是接口，把所有的接口名置于implements关键字后面用逗号分隔开。
e.g. A extends B implements C,D,E { }

> p.s. 当B和C和D和E都有同样的一个方法时(在C D E中有default的方法),最终会按照A的方法的内容.如果是A implements B,C 且B C都有default的相同签名的方法,那么会报错.

使用接口的核心原因：能向上转型为多个基类型，防止客户端程序员创建该类的对象，并确保这只是创建一个接口。

### (p)1.10.5 适配接口

Java SE5 的 ==Scanner== 类的构造器接受一个 ==Readable== 接口（就像接受基类作为参数一样，作为 ==Readable== 接口的实现的类也可以做作为参数传递进去），==Readable== 单独为 ==Scanner== 创建的，以使得Scanner不必将其参数限制为某个特定的类，如果你创建了一个新的类，并且想让Scanner可以作用于它，那么就可以用这个类来实现Readable接口（接口中只有一个read（方法））。

在Java SE5之前，可以用interface的域都是static和final的特性来创建类似于enum的东西。

### (p)1.10.6 初始化接口中的域

接口中定义的域**不能**是空白final，必须在定义的时候就初始化，不过可以被非常量表达式初始化。

### (p)1.10.7 嵌套接口和嵌套类

接口可以嵌套在类或其他接口中，在类中嵌套的private的接口可以被public的类实现（但是在类A中嵌套的类B只能在A中使用，除非在A中弄一个public的字段创建一个B的引用，或者创建一个getB（返回B的引用）的方法和一个reciveB（接收一个B的引用到的一个private的B的引用的字段）的方法，创建两个A的对象的引用，然后用一个引用的reciveB方法去接收另一个引用的getB方法的返回值）。而且实现类可以嵌套在其他类中，嵌套在类里面的类要创建一个方法来返回这个子类的对象。

### (p)1.10.8 工厂方法设计模式

完全与接口的实现分开。
创建一个Service接口和一个FactoryService接口，在FactoryService中创建一个能够返回Service的方法。每当要实现这个Service时 ，就顺便实现一个FactoryService。

**Consumer**：优先选择类而不是接口，总类开始，如果接口的必需性变得非常明确，那么就进行重构。接口是一种重要的工具，但是他们容易被滥用。


## (p)1.11 内部类

将一个类的定义放在另外一个类的定义内部，这就是内部类。

内部类作为某个接口的实现或其他类的继承，在没有继承外部类的情况下，提供了访问外部类的窗口。

如果要实现对多个类的多重继承，只能使用内部类实现。

当类与接口（或者接口与接口）发生方法名冲突的时候，必须使用内部类来实现。用接口不能完全的实现多重继承，用接口配合内部类才能实现真正的多重继承。

### (p)1.11.1 成员内部类

只能从外部类的非静态方法（非静态方法可以直接new InnerClassName();或者new OuterClassname.InnerClassName(); ）用new直接创建该外部类中的某个内部类的对象（不能在外部类的静态方法中直接用new创建内部类的对象, 因为new 该内部类的时候会自动隐式的调用this, 因为该内部类和它的外围对象相关联），**最好**指明这个对象的具体类型  i.e. OuterClassName.InnerClassName
默认只调用内部类，就算有同名的外部的类都没用，根本创建不了外部的同名类的对象了,外部的同名类直接被这个同名的内部类**覆盖**了

成员内部类**不允许**定义静态变量（**只能创建静态常量变量** i.e. static final …），也不允许声明静态方法。不过可以访问外围类中的所有成员。

当生成一个内部类的对象时，此对象与制造它的外围对象（enclosing object）之间就有了联系，所以它能访问其外围对象的所有成员。i.e. 内部类（非静态）拥有外围类中所有元素的访问权。因为内部类对象会暗暗的连接到它的外部类对象上。但是嵌套类（静态内部类）就不需要对外部类的引用。

在类外**不可直接**生成局部内部类（**保证局部内部类对外是不可见的**）。要想使用局部内部类时需要生成对象，对象调用方法，在方法中才能调用其局部内部类。通过内部类和接口达到一个强制的弱耦合，用局部内部类来实现接口，并在方法中返回接口类型，使局部内部类不可见，屏蔽实现类的可见性。

### (p)1.11.2 ==.this== 和 ==.new==

可在内部类的的方法中使用 ==OuterClassName.this== 生成外围类的引用。这一点在编译器就被知晓并收到检查，因此没有任何运行时开销。

在其他类中创建某个内部类的对象（包括外围类的**静态方法**内创建内部类的对象的引用），必须在new表达式中提供对其他外部类对象的引用。e.g.Outer o = new Outer(); Outer.Inner I = o.new Inner();也可以在外围类中创建一个方法返回内部类的对象的引用。

内部类与向上转型：没看出有啥差别 就是在用一个内部类来实现一个接口 然后向上转型。

### (p)1.11.3 在方法和作用域内的内部类(局部内部类)


在方法的作用域内创建一个完整的类。可以在同一个子目录下的任意类中的内部类也使用这个局部内部类的标识符，不会引起命名冲突。

**在任意作用域内嵌入一个内部类**
可以在方法中的结构化语句中的代码块（e.g. if（b）｛class Inner｛…｝｝ p.s. 这**不能**说明类的创建是有条件的，编译的时候已经编译过了）中创建一个内部类，该内部类具有代码块作用域。

一个内部类被嵌套多少层都不重要，它能透明的访问所有它所嵌入的外围类的所有成员。

### (p)1.11.4 静态内部类（嵌套类）

也就是局部内部类加上个static。静态内部类可以定义静态（包括嵌套类）或者非静态的成员。静态内部类只能访问外部类的静态成员，不能访问非静态成员。外部类可以直接访问静态内部类中的静态方法 e.g. InnerClassName.methodName(args…); 外部类访问内部类的非静态方法需要实例化内部类（i.e. 创建内部类的对象）

生成一个静态内部类不需要外部类成员，可以直接生成：Outer.Inner in = new Outer.Inner();这实际上使静态内部类成为了一个顶级类。

嵌套类可以作为接口的一部分，放在接口中的任何类都自动的是public和static的。

> tips ：  要测试一个类A，可以在这个类中嵌套一个类Test，然后在这个嵌套类中创建要测试的类的引用，这样打包的时候直接删除A$Test.class就可以了。

### (p)1.11.5 匿名内部类

匿名类没有名字,不能创建构造器（包括重载）。**（匿名内部类就是方法中的类，其实匿名内部类在编译的时候会在构造器中会拷贝这些外部定义的对象，在匿名内部类中对这些对象的改动都不会对外部中的对象造成影响，为了避免外部类对这些对象的修改，所以强制要求是final的）**

e.g. return new Super() {类的内容…};创建一个继承自基类Super（已经存在的一个类）的匿名类对象，通过new表达式返回的引用被自动向上转型为基类的引用。
e.g. Inner method（final int var）｛return new Inner（） ｛｝｝
e.g. void method（）｛class Inner｛｝｝

在匿名内部类中（**花括号内**）使用一个在外部定义（比如外部方法中的参数）的对象，那么编译器会要求其**参数引用是final的**。匿名内部类别的特性和成员内部类一样，不能创建除了static final之外的static静态成员。对于匿名类来说，显式实例初始化的实际效果就是构造器，但是因为不能重载实例初始化，所以只能实现一个这样的构造器。

### (p)1.11.6 闭包与回调

闭包（closure）是一个可调用的对象，它记录了一些来自于创建它的作用域的信息。内部类就是面向对象的闭包，它不仅包含外围类对象，还自动拥有一个指向此外围类对象的引用。内部类就可以用来回调。

应用程序框架是被设计用来解决某类特定的一个类或一组类，控制框架是一类特殊的应用程序框架，它用来解决响应事件的需求，主要爱用来响应事件的系统被称作事件驱动系统。应用程序设计中常见的问题之一就是图形用户接口（GUI），它几乎完全是事件驱动的系统。Java Swing就是一个控制框架，使用了大量的内部类。

模版方法设计模式：模版永远不变，模版中有很多可覆盖的方法，不同的动作来实现这个模版。

命令设计模式：接收用户传递给程序的参数。

### (p)1.11.7 内部类的继承

内部类的构造器必须连接到指向其外围类对象的引用。必须在导出类的构造器中接收一个外围类的引用作为参数，并且在构造器内使用如下语法：enclosingClassReference.super();

**内部类不会被覆盖**：如果创建了一个内部类，然后继承其外围类并重新定义此内部类时，内部类并不会被覆盖。可在继承的子类中用 ==extends== **明确继承内部类**并覆盖内部类基类中的方法。

内部标识符：每个类都会产生一个.class文件。内部类的class文件命名：OuterClassName$InnerClassName.class 如果内部类是匿名的那么就会简单的用数字表示InnerClassName.

### (p)1.11.8 JDK8对接口的修改

JDK8为接口新增了默认方法（在接口中为方法指定默认实现）的新功能：1.提供一种扩展接口的方法，而不破坏实现了这个接口的类的代码。2.在接口中指定本质上可选的方法，也就是如果接口的实现并没有覆盖这个接口作为占位符性质的方法，也没问题。（因为有些实现可以不需要接口中的某些方法）
在需要提供默认实现的方法前面加上default关键字。
不过接口还是不能使用实例变量，里面定义的所有变量都是final的。
p.s. 类实现的优先级永远高于接口的默认实现。如果一个类要同时实现两个包含相同的方法签名的接口，编译器会报错。
在子级接口中调用父级接口中的默认实现时，需要使用super关键字显式的调用：InterfaceName.super.methodName()
JDK8中接口还可以定义一个或多个静态方法,可以不用实现接口或接口的实例:InterfaceName.staticMethodName();




## (p)1.12 持有对象(容器)

**容器**（Collections）提供了比数组更加完善的方法来保存对象，容器可自动的调整自己的尺寸。

Java SE5新增一个注解：｛==@SuppressWarnings("unchecked")==｝表示只有有关“不受检查的异常”的警告信息都会被抑制。

通过声明ArrayList<参数类型>（）这种泛型可以在编译期防止将错误类型的对象放置到容器中，参数类型可以为多个，参数类型就是类的名字。向上转型也能作用于泛型。

Java的容器类类库分两种：==Collection==和==Map==

### (p)1.12.1 Collection

（可在尖括号中接收一个引用参数作为泛型）一个独立元素的序列，Collection可以直接用foreach。

List（子类：ArrayList（长于随机访问，但是在List中间插入和移除元素比较慢） LinkedList（跟更快的在List中插入和删除元素，但是随机访问慢））必须按照插入的顺序保存元素，
Set（子类：HashSet TreeSet LinkedHashSet）不能有重复元素而且HashSet最快但是无顺序，TreeSet按照比较结果的升序保存对象，LinkedHashSet按照被添加的顺序保存对象，
Queue（只能在容器的一段插入对象，在另一端移除对象）按照排队规则来确定产生的顺序（通常与它们被插入的顺序相同）。
对于每个键，Map只接收保存一次。

List接口在Collection的基础上添加了大量方法，可以在List中插入和移除元素：add(int index,Integer element)或者add（Integer e）插入对象，get(int index)访问这些对象，size()知道有多少个数据添加了进来，set（int index，Integer element）修改某一个元素，remove(Object o)方法删除某个数据，contains（Object o）确定那个List中是否有该元素，返回boolean类型，containsAll（Colllection<?> c）确实是否有c中的所有元素，indexOf(Object o)返回该对象在List中的索引编号，重载方法addAll（）可以在List中间插入新的列表，subLIst（int fromIndex，int toIndex）从List中复制出一个片段，removeAll（Collection c）移除List中和c共有的元素（基于Object.equals（）的比较），isEmpty()返回是否为空的boolean结果，clear（）清除所有的元素，toArray（）返回一个Object数组。ArrayList保存的是Object。List类是抽象的。

### (p)1.12.2 Collections工具库和Arrays工具库

Collections.sort（list）简单升序排序，Collections.shuffle(List, Random)随机打乱顺序;
可以这样声明：List\<Apple> apples = new LinkedList\<Apple>(); 因为ArrayList LinkedList都是List的子类，这里向上转型了。

Arrays.asList()方法接收一个数组或者逗号分隔的元素列表（使用可变参数），并将其转化为List对象（不是ArrayList 是他们的基类List）。Arrays.asList方法输出的是List，但是其底层表示是数组，因此不能调整大小，如果试图用add() delete()方法在这个List上会得到运行时错误“Unsupported Operation”。

p.s. Arrays.asList()如果接收的所有参数都是一个基类型Super的不同子类，那么就会产生Super的List的引用，如果这时候赋值给了一个Super的基类Root的List的引用，不会再次向上转型，直接报错。除非List\<Root> list = new Arrays.\<Root>asList(new Sub1(),new Sub2());告诉Arrays.asList要向上转型为Root。（这叫显示参数说明）

不过使用Collections.addAll(RootRefence，SubRefence1，SubRefence2，…)（在RootRefence的末尾插入新的列表）就可以，它可以连续向上转型很多次，因为第一个参数已经确定好了目标类型。
Collections.addAll(Collection对象的引用，元素…)方法（注意Collections类有个s）可以接收Collection类的引用加一个数组或者逗号分隔的元素列表（使用可变参数），往Collection类中添加元素。


### (p)1.12.3 LinkedList：LinkedList添加了可以使其用作栈、队列或双端队列的方法。
getFirst（）和element（）都是返回列表的头（第一个元素），如果List为空，则抛出NoSuchElementException，peek（）与这两个方法稍微有点差异，在列表为空的时候返回null。
removeFirst（）与remove（）完全一样，他们移除并返回列表的头，在列表为空的时候抛出NoSuchElementException，poll（）稍有差异，它们在列表为空的时候返回null。
addFirst（）将某个元素插入列表的头部。
offer（）与add（）和addLast（）相同，它们都是将某个元素插入列表的尾部。
removeLast（）移除并返回列表的最后一个元素。

### (p)1.12.5 Stack（栈）

栈通常是指后进先出（LIFO）的容器，又是栈也被称为叠加栈，因为最后压入栈的元素，第一个弹出栈。
LinkedList具有能够直接实现栈的所有功能的方法，因此可以直接作为栈使用。
class Stack\<T> { }尖括号（泛型）告诉编译器这是一个参数化类型，其中的类型参数，即在类被使用时将会被实际类型替换的参数，就是T。这个Stack类声明：定义一个可以持有T类型对象的Stack。在类的内部的容器声明也可以使用\<T>表示持有T对象。
push（T v）即LinkedList中的addFirst（v），peek（）即LinkedList中的getFirst（），pop即LinkedList中的removeFirst（），empty（）即LinkedList中的isEmpty（）。

### (p)1.12.6 Set

Set不保存重复的元素。Set最常被使用的是测试归属性，可以很容易的查询某个对象是否在某个Set中。Set（除TreeSet）具有和Collection一模一样的接口：
add（） addAll（） remove（） contains（） containsAll（） removeAll（）
默认TreeSet是按照字典序排序的，要想按照字母顺序（不区分大小写）排序，可以向TreeSet的构造器中传入String.CASE_INSENTIVE_ORDER比较器（比较器就是建立排序顺序的对象）。

### (p)1.12.7 Map

（可在尖括号中接受两个引用参数（<键类型，值类型>）作为泛型，子类：HashMap（最快的获取元素的方式但是不按顺序） TreeMap（按照比较的结果保存键） LinkedHashMap（按照插入的顺序保存键，同时还保留了HashMap的查询速度））一组成对的“键值对”对象，允许使用键来查找值。映射表（字典）允许我们使用另一个对象来查找某个对象，它也被称为“关联数组”。用方法Map.put(key，value)插入键和值，Map.get(key)将返回与该key相关联的value，containsKey（） containsValue（）方法分别查看Map中是否存在对应的键和值，keySet（）和values（）返回油所有键组成的Set的所有value组成的List。
Map可以很容易的扩展到多维：将其值设置为Map（或者其他容器），e.g. 要跟踪拥有多个宠物的人：Map\<Person,List\<Pet>>;

###(p)1.12.8 Queue

队列是一种先进先出（FIFO）的容器。LinkedList可以作为用作Queue的一种实现。
offer（）将一个元素插到队尾失败返回false，peek（）在不移除的情况下返回队头，为空时返回null，poll（）移除并返回队头，队列为空的时候返回null。

PriorityQueue（JavaSE5）：优先级队列声明下一个弹出元素是最需要的元素（具有最高优先级的），这跟普通的Queue按照等待时间来确定下一个弹出的元素的方式不同。
默认的排序是使用对象在对列中的自然顺序（按照ASCII（unicode）升序的顺序）。PriorityQueue确保当调用peek（） poll（） remove（）的时候，获取的元素是队列中优先级最高的元素。构造器PriorityQueue<>(int initsize，Comparator comparator)；Comparator可以用Comparator.reverseOrder()（JavaSE8放在了Comparator类里面，而SE5是放在了Collection里面）反序比较器，默认的是Comparator.naturalOrder（）比较器，这时候只能使用peek（）和poll（）方法来打印出排序后的结果，用print（）直接打印的会出错。

### (p)1.12.4 容器的打印

数组需要使用方法Arrays.toString()来产生数组的可打印表示。而容器可以直接被println print printf这些方法打印。
Collection：输出结果：[元素1，元素1，…，元素n]
Map：输出结果：{键1=值1，键2=值2，…，键n=值n}

**迭代器：(iterator)**

只是使用容器，而不关心容器的类型，这样更加通用。它的工作是遍历并选择序列中的对象。迭代器通常被称为轻量级对象，创建它的代价很小，不过功能也少。

Java的Iterator只能单向移动，使用Collection.iterator（）方法要求容器返回一个Iterator类（和容器一样，也能用尖括号指定泛型）的对象的引用。

Iterator类中的方法：
hasNext（）检查序列中是否还有下一个元素（返回boolean），next（）得到序列中的下一个元素，remove（）将迭代器最近返回（所以一般调用remove（）前先调用next（））的元素删除（可选方法）。

迭代器统一了对容器的访问方式。
ListIterator：ListIterator是一个更加强大的Iterator子类型，它只能用于List类的访问。ListIterator支持双向移动。

listIterator（）方法产生一个指向List开始处的ListIterator，并且可以通过调用listIterator（n）方法创建一个一开始就指向列表索引为n的元素处的ListIterator。
nextIndex（）返回下一个元素的序列索引，previousIndex（）方法返回最近访问的那个元素的索引（也就是nextIndex（） - 1），hasPrevious（）返回时候前面还有元素，
set（Integer e）把最近访问的那个元素替换为e，previous（）访问上一个元素。

**Collection和Iterator：**

对于Collection类型，Collection比使用Iterator要更加简单，可以直接使用foreach。而要实现一个不是Collection的外部类时，实现Collection接口（要extends AbstractCollection\<T>,这样就不能在继承别的类而需要自己实现所有要用到的方法）会很麻烦（里面的抽象方法很多而且还包括iterator（）），不如直接使用Iterator。
这时最好的方法就是在这个外部类中加入一个iterator方法就可以了返回一个Iterator\<>的匿名内部类。

使用创建iterator方法的方式能使耦合度更加小。

**Foreach与迭代器：**

Java SE5引入了新的被称为Iterable的接口，该接口包含一个能够产生Iterator的iterator（）方法，并且Iterable接口被foreach用来在序列中移动。因此如果创建了任何实现Iterable的类，都可以用于foreach语句。（声明类的时候implements Iterable\<T>，T指泛型）返回值同样也可以使用\<T>作为泛型
所有的Collection类（不包括所有的Map）都是Iterable类型的。
p.s. 数组不是Iterable。
System.getenv（）返回一个Map，entrySet（）产生一个由MapEntry的元素构成的Set，并且这个Set是一个Iterable，可以用foreach。
e.g. for（Map.Entry entry : System.getenv().entrySet()）{ entry.getKey()获得键…entey.getValue()获得值 }
适配器发惯用法：如果现在有一个Iterable类，要想添加一个或多个在foreach中使用这个类的方法。可以继承该类，在构造器中显式的调用基类的构造器（super（参数）;），然后创建一个或多个返回包含iterator方法的返回Iterable\<T>类的方法。e.g. for（Collection\<T> c :  Class.YourMethod）{ }
新建一个ArrayList包装asList的返回结果能够防止改变低沉数组的顺序，e.g. List\<String> list = new ArrayList\<>(Arrays.asList(array));

> p.s. 新程序不应该使用过时的Vector Hashtable Stack。



## (p)1.13  通过异常处理错误


Java使用异常来提供一致的错误报告模型，使得构件能够与客户端代码可靠的沟通。异常机制将保证能够捕获这个错误，只需在统一在一个地方（异常处理程序）处理错误，能够跟程序执行部分分开。

### (p)1.13.1 基本异常

异常情形是指阻止当前方法或作用域继续执行的问题。抛出异常时，Java会使用new在堆上创建异常对象，当前执行路径被终止，从当前环境中弹出对异常对象的调用，此时，异常处理机制接管程序，并开始寻找异常处理程序继续执行。关键字==throw==用来抛出异常对象，能够抛出任意类型的Throwable（异常类型的根类）对象。在作用域中的==throw==或着==return==语句后面的语句都是无法访问的语句。

异常参数:
标准异常类都有两个构造器：默认构造器和接收字符串作为参数的构造器。

### (p)1.13.2 捕获异常

监控区域：
一段可能产生异常的代码，并且后面跟着处理这些异常的代码。
可以设置一个try块（关键字try）来捕获异常：try ｛//Code that might generate exceptions ｝这样只需在一个地方就可以捕获所有异常。

异常处理程序：
异常处理程序紧跟在try块后面，用关键字catch表示：try ｛//Code that might generate exceptions｝catch(Type id1) ｛//Handle exceptions of Type1｝catch(Type2 id2) ｛//Handle exceptions of Type2｝//etc.. 

p.s. try子句中必须含有能够（可能）抛出对应的Type的异常，在try子句抛出对应的时候执行对应的catch子句，finally子句：异常的统一出口。永远都能在方法被中止或程序被终结的时候执行，就算前面执行了return，finally子句的内容依然会执行，如果finally子句包含return语句则会覆盖掉原来的return。

强烈建议独立使用 try/catch 和 try/finally 语句块，不要把try catch finally 放在一个层次上面，这样可以提高代码的清晰度。（try catch和finally不能单独使用）

### (p)1.13.3 终止与恢复

终止模块：
直接终止当前方法或作用域；

恢复模块：
异常处理程序是修正错误，然后重新尝试调用问题的方法，并认为第二次能成功。要想实现该恢复行为：要么在遇见错误的时候不抛出异常而是通过调用方法来修正错误，要么把try块写到while循环里面，这样就不断的进入try块直到得到满意的结果。

恢复性的处理程序需要了解异常抛出的地点，增加了耦合程度和代码的编写及维护难度。

### (p)1.13.4 创建自定义异常

要自定义异常类，必须从已有的异常（Throwable类）来继承（可以继承Exception）。错误信息最好送到System.err标准错误流单中，因为System.out也许会被重定向。

可以调用Throeable类声明的printStackTrace（）方法打印从方法调用处直到异常抛出处的方法调用序列，无参的话会把信息输出到标准错误流（System.err）中，也可以printStackTrace（System.out）把信息输出到System.out，并自动的被捕获和显示在输出中。

### (p)1.13.5 异常与记录日志：java.util.logging.*

Logger.getLogger()方法创建一个String参数相关联的Logger对象（通常与错误相关的包名和类名），这个Logger对象会将其输出发送到System.err。使用Logger.severe（String）向Logger写入一个severe（严重级别）事件。printStackTrace（）默认是将信息输出到System.err并打印，可以使用重载的printStackTrace（java.io.PrintWriter）把错误信息输出到PrintWriter对象中，PrintWriter对象可以用new PrintWriter(new StringWriter);创建。
可以对异常类型添加新的功能，不过一般都用不上。

### (p)1.13.6 异常说明

Java强制开发者对会抛出异常的方法的声明处使用异常说明（使用关键字throws注意有s）：void f（args） throws Exception1，Exception2 … ｛…｝

如果方法声明中没有包含throws,那么说明方法除了自动从RuntimeException继承的异常外，没有其他的异常。

调用使用了异常捕获（throws）的方法时必须有try-catch子句对其捕捉或声明以便抛出。对于RuntimeException错误，默认系统会自动抛出并终结程序，不过也可以自己用try--catch捕获，而且可以在catch中指定执行的操作。

在定义抽象基类和接口的时候，可以在实际上不会抛出这个异常的情况下声明该方法会抛出异常，为异常先占个位子，以后就可以抛出这种异常而不用修改已有的代码，这样派生类或接口实现就能够抛出这些预先声明的异常。这种在编译时强制检查的异常被称为被检查的异常。

捕获所有异常：通过不过异常类型的基类Exception（事实上还有其他的基类，不过Exception是同编程活动相关的基类）就可以了：catch（Exception e） ｛｝

最好把它放在异常处理程序列表的末尾，以防止抢在其他程序之前先把异常捕获了。
因为Exception是与编程有关的所有异常的基类，所以它只有从其基类Throwable继承的方法：String getMessage（）获取详细信息和String getLocalizedMessage（）用本地语言表示的详细信息 和Object基类就有的toString（）方法返回对Trowable的简单描述。
从Object根类继承的方法：getClass（） getSimpleName（）
同时还有void printStackTrace（） void printStackTrace（PrintSteam） void printStackTrace（java.io.PrintWriter）（默认无参是输出到标准错误流，后面两个重载版本可以自定义流）打印Throwable和Throwable的调用栈轨迹。getStackTrace()返回栈轨迹。调用栈显示了把你带到异常抛出地点的方法调用序列。Throwable fillInStackTrace（）用于在Throwable对象的内部记录栈帧的当前状态。

### (p)1.13.7 栈轨迹

printStackTrace（）所提供的信息可以通过Exception.getStackTrace（）方法直接访问，这个方法返回一个由栈轨迹中的元素（这个元素的类型为StackTraceElement，而且StackTraceElement中有个方法getMethodName（）可获取该栈轨迹元素的方法名）所构成的数组，其中每一个元素都表示栈中的一帧。元素0是栈顶元素，并且是调用序列中最后一个方法调用（这个Throwable被创建和抛出之处）。数组最后一个元素（栈底）是调用序列中的第一个方法调用。

### (p)1.13.8 重新抛出异常

重抛异常会把异常抛给上一级环境中的异常处理程序，同一个try块的后续catch子句会被忽略。而且，异常对象的所有信息都得以保持，所以只是把当前异常对象重新抛出，那么printStackTrace（）方法显示的将是原来异常抛出点的调用栈信息，并非是重新抛出点的信息。要想更新这个信息，可以调用fillInStackTrace（）方法，这将返回一个Throwable对象（一般都要强制类型转化），它是通过把当前调用栈的信息填入原来的那个异常对象而建立的。

### (p)1.13.9 异常链：有时候需要捕获一个异常的时候再抛出另外一个异常，而且希望把原始的异常的信息保存下来，这被称为异常链。

Java SE5新增所有Throwable的子类在构造器中都可以接受一个cause（因由）对象作为参数，这个cause用来表示原始异常，把原始异常包装成新的异常，用Throwable getCause（）来获取原始异常。在Throwable的子类中，只有三种基本的异常类提供了带cause的参数的构造器：Error（用于JVM报告系统错误） Exception 和 RuntimeException，如果要把其他类型的异常链接起来，只能使用initCause（Throwable cause）（所有的Throwable类都有）方法把cause和当前创建的异常链接（添加在一起）以来而不能用构造器。

### (p)1.13.10 RuntimeException

对于RuntimeException（或者任何从它继承的异常）都不需要异常说明（所以RuntimeException是个特例，别的异常都要进行异常说明），输出会自动报告给System.err。如果RuntimeException没有被捕获，那么程序被强行终止前还会自动调用printStackTrace。被捕获的话你就可以自定义你要做什么了。

### (p)1.13.11 finally子句

finally子句也只能配合try子句一起使用。无论异常是否抛出，finally总能执行。当要把除内存之外的资源（e.g. 已打开的文件或网络连接 在屏幕上画的图形…）恢复到它们的初始状态时，就要用到finally子句。
当涉及break continue return 的语句中，finally都会得到执行。

### (p)1.13.12 遗憾:异常丢失

在一个try子句中镶嵌了一个try-finally块而且这try（-catch）-finally块中的try子句和（catch子句）和finally子句都能抛出异常，而在外层的try子句如果被catch捕获，则只能捕获到finally子句产生的异常，内层的try子句的异常就丢失了，直到Java SE8都没有修复这个缺陷。还有直接把return放在finally子句中，这样可以屏蔽所有同一级别的try抛出的异常，无法把异常传递到异常处理程序而直接退出该方法。

### (p)1.13.13 异常的限制

当覆盖方法时，只能抛出基类方法的异常说明里列出的那些异常（包括基类方法声明的异常的基类异常）。派生构造器会隐式的声明基类方法中构造器的异常声明，而且还可以额外的添加其他异常声明。
子类中的派生类方法可以不抛出基类方法中声明的异常，但必须显示的写出所有基类方法中的异常声明。
如果处理的一个对象创建的时候是向上转型了，那么编译器会要求你正确的捕获基类的异常。
重载是根据方法签名（i.e. 方法名和参数）的，不能通过异常说明来重载方法。
但是派生类构造器不能捕获基类构造器抛出的异常。（没看懂）

### (p)1.13.14 构造器

如果构造器抛出了异常，那么类中的清理行为也许就不能正常工作了。

e.g. 在构造器中打开文件。不能在构造器中使用来关闭抛出异常时的文件，因为不论是否抛出异常，finally子句都会执行。

对于在构造阶段可能会抛出的异常，并且要求清理的类，最安全的使用方式就是使用嵌套的try子句。基本规则是：对于创建需要清理的对象之后，立即进入一个try-finally语句块。
对于对象构造不可能失败的类的创建使用可以直接使用try-finally子句，可以不需要catch。

### (p)1.13.15 异常匹配

抛出异常的时候，异常处理系统会按照代码的书写顺序找到最近的处理程序，当找到匹配的处理程序之后就不会继续查找了，派生类的异常对象也可以匹配其基类的处理程序。

异常处理的一个重要原则：只有在你知道怎么处理异常的情况下才捕获异常。有时候异常声明带来的强制性捕获异常并非什么好事（对于大型工程来说更是如此），这会导致程序员仓促的异常编写处理程序（有时候直接用RuntimeException（某种你不知道处理的异常类型的引用）;把不知道处理的异常包装进RuntimeException//@since 1.4）。

### (p)1.13.16 JDK7对新增的异常特性

1.带资源的try语句 
2.多重捕获(在一个catch子句的参数中使用|运算符把多个异常放在一起,且每个多重捕获参数都被隐式的声明为final) 
3.更精确的重新抛出(只能抛出满足条件的异常:有关联的try代码块抛出,并没有被前面的catch子句处理,并且是参数的子类型或者超类型)


## (p)1.14 字符串

字符串操作是计算机程序设计中最常见的行为。

String对象是不可变的，String中每一个看起来会修改String值的方法，实际上都是创建了一个全新的String对象，以包含修改后的字符串内容，而最初的String对象并未改变。

### (p)1.14.1 重载的 ==+== 和StringBuilder（Java SE5）

对于创建字符串对象的时候，用+连接的多个常量会在编译器就直接转化为一个常量。如果=号右边是一个常数的话，就会进入内存池，查找是否已经有了该对象，有的话就直接引用已有的对象，没有才创建。
String对象具有只读特征，所以指向它的任何引用都不可能改变它的值，所以一个String对象可以有很多的别名（也就是其他指向它的引用）。但是这也带来了效率问题。

Java仅有的两个重载的操作符是+和+=，而且不允许程序员重载任何操作符。

（Java SE5之前用的是StringBuffer）
在用重载的+进行字符串连接的时候，每连接一个，编译器都会隐式得创建一个StringBuilder对象并调用StringBuilter.append（）。使用重载的+操作符不如直接显示的创建StringBuilder对象并调用append（）方法进行连接，如果实现知道最终的字符串的大小，还可以预先指定StringBuilder的大小以避免多次重新分配缓存。

StringBuilder中的方法：append（） delete（int from，int to）（从第from个字符后面开始删一直删到第to个字符（包括to）） insert（） replace（） substring（） reverse（）（逆序） toString（）

> 无意识的递归:
如果想创建一个toString（）方法，在重载的+后面接this想打印地址，this会发生自动类型转换，调用this.toString()方法，这就会一直递归下去直到栈溢出。需要把this改成super.toString（）。

### (p)1.14.2 String对象具备的一些基本方法

length（） charAt（int index） getChars（） getBytes（）复制char或byte到目标数组中 toCharArray（）equals（） （比较内容）equalsIgnoreCase（）compareTo（）（按照字典顺序比较String内容，比较结果为负数 零或正数，大小写不等价） contains（） contentEquals（）（与参数（CharSequence或者StringBuffer）比较内容是否完全一致） regionMatcher（）（比较两String指定的区域是否一模一样的内容，重载版本还可以忽略大小写） startsWith（String） （判断字符串是否以参数中的字符串开始的） endsWith（String）（判断字符串是否以参数中的字符串结尾）indexOf（）（返回参数中指定的char或者String在字符串中的起始索引，如果不包含就返回-1）lastIndexOf（）（与indexOf（）差不多，不过是从后向前搜索） substring（）（参数为起始索引，返回一个由起始索引指定的子字符串）subSequence（）返回的是CharSequence和substring（）类似 concat（String）（将参数中的String连接到原始String后面，并且返回新的字符串）replace（）替换字符或CharSequence，如果没有发生替换过程，则返回原始的String对象 matches（）检查是否和参数中的内容（这个参数常常是用正则表达式表示的字符串）一模一样replaceAll（） replaceFirst（） toLowerCase（） toUpperCase（） trim（）（将字符串两端的空白字符删除后，返回新的String对象）valueOf（）返回一个表示参数内容的String intern（）为每一个字符序列生成一个且仅生成一个String引用。比如要创建一个字符串“string”用“string”.intern（）创建的时候，会先检查内存池中是否有了“string”为内容的对象（通过equals（）方法确定），有就直接返回这个已有的对象的引用，否则将此String对象添加到内存池中再返回它的引用。


### (p)1.14.3 格式化输出

Java SE5新增了和C语言一模一样的printf（），不过Java有重载的+了（SE5还没有，不知道后面哪个版本加上去的）。
Java SE5引入了System.out.format（）可用于PrintSteam或PrintWriter对象，用法和printf（）一样。
所有新的格式化功能都由java.util.Formatter类处理，创建Formatter类的对象时，要在构造器的参数中指定要输出在哪里。
Formatter类中的format方法 的类型转换说明符（接在%后面）：c表示Unicode b表示Boolean（对于非布尔值，只要不是null输出都是true） s表示String h表示散列码（hashCode，十六进制）d十进制整数 f十进制浮点数 e科学计数法的浮点数 x十六进制整数
String.format（）：静态方法，类似于C中的sprintf（） 


### (p)1.14.4 正则表达式

`\d`表示一位数字，在Java中`\\`的意思是插入一个正则表达式的反斜杠，所以如果需要表示一位数字，那么正则表达式应该是`\\d`，不过换行和制表符之类的东西只需要用单反斜杠：\n\t\r\f  (f是换页) 因为单斜杠表示转移字符
如果只想插入一个普通的反斜杠，那要写成`\\\\`（4个\）
普通的括号也好加//
要表示一个或多个之前的表达式，应该（直接）使用+，所以要表达不同字符+要用`\\+`表示，将+转义成普通字符。 e.g.` -?\\d+` 表示负号开头后面跟着以为或者多位数字
|表示或操作
括号有这将表达式分组（捕获组）的效果 e.g.` (-|\\+)?` 表示可能有-或者+或者都没有（？的效果）
可以用String中的matches（）和split（）（重载的split（CharSequence input，int limit）中的limit表示最多的分割数量）这两个正则表达式工具。
字符组：
. （点）表示任意字符 （字面意思的点好就要用\\.）
[abc]表示a、b或者c（和a|b|c一样的效果）
[a-cK-Z]表示a到c或者K到Z的任意字符
[^abc]除了a b c之外的任意字符
[a-k&&[abc]]表示a b c中任意一个字符
[abc[def]]表示a、b、c、d、e、f中的任意字符
 \w 表示单词字符（包括数字）   \W 表示非单词字符
\s 空白符（空格 tab 换行 换页 回车） \S 非空白符（[^\s]）
\d 数字（0到9） \D 非数字
POSIX 字符集 (US-ASCII only)：
\p{ASCII}    所有ASCII字符（[\x00-\x7F]）
\p{Punct}  标点符号
\p{Cntrl}  控制字符（[\x00-\x1F\x7F]）
\p{Space} 空白字符（[\t\n\r\x0B]）
逻辑操作符：
XY 表示Y跟在X后面
X|Y X或Y
（X）  捕获组。可以在表达式中用\i引用第i个捕获组
边界匹配器：
^   一行的开始
$ 一行的结束
\b 词的边界的的的那个单词字符
\B 非词的边界的那个非单词字符
\G 前一个匹配的结束
量词：（？ + ｛｝）
量词描述了一个模式吸收输入文本的方式：
贪婪型：贪婪表达式会为所有可能的模式发现尽可能多的匹配。
勉强型：用问好来指定（也就是在贪婪型表达式的后面加上一个问号），这个量词匹配满足模式所需的最少字符数。
占有型：用加号来指定（也就是在贪婪型表达式的后面加上一个加号）。只有在Java语言中才可用，所以用的不多。当正则表达式被应用于字符串时，它会产生相当多的状态，以便在匹配失败的时候可以回溯。而占有型量词并不保存这些中间状态，因此他们可以防止回溯。它们常常用于防止正则表达式失控，因此可以使正则表达式执行起来更有效。
设表达式X（通常必须要用括号括起来）
贪婪型示例：
X?   匹配 一个或零个X
X\*  匹配零个或多个X
X+ 匹配一个或多个X
X｛n｝ 匹配恰好n次X
X｛n，｝ 匹配至少n次X
X｛n，m｝ 匹配至少n次，最多m次X
接口CharSequence从CharBuffer String StringBuffer StringBuilder类之中抽象出了字符序列的一般化定义：
interface CharSequence {
	charAt(int i);
	length();
	subSequence(int start,int end);
	toString();
}
多数正则表达式操作都接收CharSequence类型的参数。
Pattern和Matcher类：
位于java.util.regex包。用static Pattern.compile（String）方法将用String类型的正则表达式生成一个Pattern对象。把想要检索的字符串传入Pattern对象的matcher（）方法，matcher（）方法会返回一个Matcher对象，该对象有很多功能：boolean find（） matches（） lookingAt（） groupCount（） group（） start（） end（） relpaceFirst（） relpaceAll（） appendReplacement（StringBuffer subf，String replacement）执行渐进式的替换，渐进式得将替换结果拼接到subf中  appendTail（StringBuffer sbuf）在执行了一次或多次appendReplacement（）之后，调用appendTail可以将输入字符串余下的部分复制到subf中。
p.s. lookingAt和matches都只有在输入的最开始部分就匹配成功才会成功，lookingAt可以只成功匹配输入的开头一部分，matches要匹配整个输入才会成功。
reset（String）将现有的Matcher对象应用于一个新的字符序列 使用不带参数的reset（）可以将Macher对象重新设置到当前字符序列的其实位置。

组：

组是用括号划分的正则表达式，组0表示整个表达式，组1表示第一对括号括起来的，以此类推。
Matcher对象提供一些获取组信息的方法：
public int groupCount（）返回该匹配器的模式中的分组数目，第0组不包括在内。
public String group（）返回前一次匹配操作（e.g. find（））的第0组的匹配结果。
public String group （int i）返回前一次匹配操作期间指定的组号的匹配，如果匹配成功，但是指定的组没有匹配输入字符串的任何部分，则会返回null。
public int start（int group）返回前一次匹配操作中寻找带的组的起始索引。
public int end（int group）返回前一次匹配操作中寻找到的组的最后一个字符索引加1的值。
Pattern标记：
Pattern的compile（）方法还有个重载版本：Pattern Pattern.compile（String regex，int flag）
flag（模式标记）来自下面的Pattern类中的常量：（多个flag用|或操作符组合）
Pattern.CASE_INSENSITIVE (?i)           大小写不敏感（只有US-ASCII能进行）
Pattern.UNICODE\_CASE (?u)             与Pattern.CASE_INSENSITIVE一起使用，大小写不敏感的匹配将按照Unicode标准相一致的方式进行
Pattern.MULTILINE (?m)                    在多行模式下，表达式^和$还额外表示一行的开始和结束。默认没有该flag时^和$仅匹配完整字符串的开始和结束
Pattern.COMMENTS (?x)                     空格符和以#开头直到行末的注释都会被忽略掉。
Pattern.CANON_EQ                           匹配将考虑规范的等价性：当且仅当他们的完全规范分解相匹配时，就认为他们是匹配的
Pattern.DOTALL (?s)                          点号匹配所有字符，包括行终结符。默认情况下，点号表达式不匹配行终结符
Pattern.UNIX_LINES (?d)                       在.(点号) ^ $行为中，只识别行终结符\n（也就是unix的行终结符）
可以直接在正则表达式中使用上面有括号的标记，只需要将括号中的字符插入（也要用括号括起来）到正则表达式中希望起作用的位置即可。
最常用的是：（？ium）

可以使用Integer.parseInt(String) Double.parseDouble(String) 将字符串转化为Integre和Double。

### (p)1.14.5 输入

BufferedReader可以将String转化为可读的流对象。
读入行：java.io.\*; String BufferedReader.readLine()方法
Java SE5新增的Scanner类中的nextLine nextInt nextDouble…还有next（regex）（无参数就是返回定界符分割后的字符串）用于找到下一个输入分词进行正则表达式的匹配结果（所以正则表达式中不能含有定界符）   可以轻松的从输入对象中读取行 整数 浮点数 每个基本类型（除char外）都有对应的next方法（包括BigDecimal和BigInteger）。Scanner还有hasNext方法用来判断下一个输入分词是否是所需的类型：hasNextInt hasNextDouble …还有hasNext（String regex）可以使用正则表达式来扫描，无参数则是返回是否存在下一个定界符分割后的字符串
Scanner定界符：
默认情况下，Scanner根据空白字符对输入进行分词。
可以用正则表达式使用useDelimaiter（）方法指定分定界符。delimiter（）方法可以返回当前正在作为界定符使用的Pattern对象
StringTokenizer：（已经过时了）
在Java还未引入正则表达式（J2SE 1.4）和Scanner类（Java SE5）之前，分割字符串的唯一方法就是使用StringTokenizer。


## (p)1.15 类型信息

传统的类型转换（用（Type）进行类型转换）由RTTI确保类型转换的正确性，如果执行了一个错误的类型转换，就会抛出ClassCastException异常

运行时类型信息（RTTI）使得可以的在程序运行时发现和使用类型信息。
在运行时查询某个泛型引用所指向的对象的确切类型。

### (p)1.15.1 Class对象：（所有的Class对象都属于Class类）

类型信息在运行时的表示是由Class对象完成的，它包含了与类有关的信息。Java使用Class对象来执行其RTTI。每当编译了一个新类，就会产生一个Class对象（而且会被保存在一个同名的.class文件中）。

JVM使用类加载器的子系统来产生Class类的对象。类加载器子系统可以包含一条类加载器链，但是一般情况下只有一个原生类加载器（加载的是来自本地盘的可信类 i.e.Java API类 等）（如果要从网络上或者其他地方加载，需要在类加载器链中挂接额外的类加载器）

所有类都在对其第一次使用时多态的加载到JVM中，e.g. new创建类的对象时会调用构造器。

如果类尚未加载，默认的类加载器可以根据类名查找.class文件（某个附加类加载器可能会在数据库中查找字节码），类的字节码被加载的时候，会接收验证，以确保其没有被破坏，并且不包含不良的Java代码。

**获得Class对象：**

Class类中的静态方法forName（String）可以根据参数（参数为全限定名（包含包名））取得Class对象的引用，如果没有这个类的对象的引用，就会加载它，然后返回Class对象的引用。如果没有指定的类的.class文件存在，就会抛出ClassNotFoundException异常。
对于已经有了的对象，可以使用Object类中的getClass（）方法取得该对象的Class对象的引用。

Class类中的方法：getName（）获得全限定名 getCanonicalName（）（JavaSE5）获得全限定名（我也不知道和getName有啥区别。。）
getSimpleName（）产生不含包名的类名 isInterface（）判断该Class类是否是个接口 getInterfaces（）返回该Class类所包含的所有接口的Class对象，并且是以Class对象数组的形式，可以使用foreach  getSuperclass（）返回该类的直接基类  newInstance（） 尝试实现虚拟构造器（如果该类是虚拟类，就不能创建实例，就会抛出异常），返回Object引用（在没有泛型的情况下），但是这个Object引用指向该Class类对应的对象，用newInstance创建的类必须显式的带有默认的构造器 （因为newInstance不接受任何参数）

### (p)1.15.2 类字面常量：（推荐使用）

除了forName（）和getClass（），Java提供类字面常量更简单更安全的生成对Class对象的引用。
e.g. <标识符>.class

类字面常量可以用于普通类、接口、数组以及基本数据类型，对于基本数据类型的包装器类，可以使用里面的标准字段TYPE，TYPE是一个指向对应的基本数据类型的Class对象。
(e.g. Integer.TYPE区别与Integer.class这是包装器类的Class对象)

可以用object.getClass().isPrimitive()判断是否为基本数据类型的Class对象

### (p)1.15.3 对于类的使用

加载（由类加载器执行，为字节码创建一个Class对象），然后链接（验证类中的字节码，为静态域分配存储空间，解析这个类创建的对其他类的所有引用），最后初始化（执行静态初始化器和静态初始化块，如果该类具有超类，还会对其初始化）（初始化会被延迟到对静态方法（包括构造器）或非常数静态域进行首次引用时才执行）
仅使用.class语法来获得的对类的引用不会引发初始化。如果类没有被初始化而直接通过Class类的引用来访问非final字段或方法，就会在访问非final字段或方法之前就进行链接和初始化。

### (p)1.15.4 泛化的Class引用

可以在Class后用<>指定更具体的泛型，强制编译器执行额外的类型检查。
在Java SE5中Class<\?>优先于Class，即使他们是等价的，因为他们都接受所有类型所以不会产生编译器错误，Class<?>突出说明你就是要选择这样一个非具体可以接收所有类型的引用。
泛型可以使用通配符来限定为一个范围不是某一个具体的类型，
e.g. Class<? extends Number>比Class<Integer>范围要放宽一些。
将泛型语法用于Class对象时，newInstance（）将返回该对象的确切类型而不是Object类。
要对getSuperclass（）返回的超类使用泛型语法，只能用通配符声明为某个类getSuperclass作用于的类的超类 e.g. Class<? Super FancyToy> up = FancyToyClass.getSuperclass();
新的转型语法：（很少使用）如果你存储了一个Class引用，并希望以后通过这个引用来执行转型，就能有点用。
Java SE5在Class类中新增cast（）方法，能够将参数（某对象的引用）转型为该Class引用的类型并返回其引用：
e.g. House h =  houseClass.cast(b);（b为house的某个导出类的对象的引用，houseClass为house的Class对象的引用）

### 1.15.5 instanceof关键字（二元操作符）

判断引用所指向的对象（注意是对象而不是对象的引用）是不是某个特定类型的实例，返回布尔值，可用在转型之前的判断。
e.g. objref instanceof Type（前面接的是引用名，后面是类名）（会在编译的时候试图类型转换来进行类型检测，并不具有灵活性）Type如果是泛型的化，不能是特定类型的，因为运行时根本不知道泛型信息。
Class.isInstance（Object obj）方法（也是返回布尔值）提供了动态（更好的适应泛型）的测试对象的途径。（一般isInstance方法比instanceof操作符要用的多）判断obj对应的对象是否为该Class对应的对象的某个实例或者子类（判断在这个obj与该Class类代表的对象是否类型兼容）
Class.isAssignableFrom(Class<?> cls) 判断ClassSub是否为参数的某个实例。判断参数（类或者接口）是否和这个Class类相同或这是是这个Class类的超类或者superinterface。

### (p)1.15.6 反射：运行时的类信息

RTTI只能识别出编译时就已知的类型。（必须在运行时从别的硬盘或者网络上下载的字节码中的类型信息，就不能识别）
java.lang.reflect类库与Class类一起提供了反射的支持，该类库包含了Field Method Constructor 类，这些类的对象在JVM运行的时候创建，用来表示未知类中对应的成员。
可以用Constructor创建新的对象，用get（）和set（）读取和修改与Field对象关联的字段，用invoke（）方法调用与Method关联的方法。
对于RTTI来说，编译器在编译时打开和检查.class文件，对于反射机制来说，在运行时打开和检查.class文件。
Class.forName（）生成的结果在编译时是不可知的，所以所有的方法特征签名信息都是在执行的时候被提取出来。

Class类中关于反射的方法：
Constructor\<?>[]  getConstructors()    返回该Class对象表示的类的所有public构造器（构造器的权限默认是个类相同）的Constructor数组。
Class\<\?>[]  getDeclaredClasses()   返回该Class对象表示的类中的所有声明的内部类或内部接口
Field[]  getFields()返回该Class对象表示的类中的所有可访问的public字段的Field数组
Method[] getMethods() 返回该Class对象表示的类及其基类 superinterface中的所有public的方法的Method数组。
getClassLoader（）获得该Class对象的类加载器
动态代理：（没看懂，也不是日常使用的工具）
常规的代理是把要代理的接口实现通过构造器把这个实现包装进代理，并且在代理中执行这个实现中的相应方法还可以多加入一些别的操作。
通过调用静态方法Proxy.newProxyInstance(ClassLoader loader,@NotNull Class\<\?>[] interfaces,@NotNull InvocationHandler h)可以创建动态代理（interfaces是你希望实现的代理要实现的接口列表 ，h是InvocationHandler接口的实例）

### (p)1.15.7 空对象：
可以创建一个Null接口，这使得instanceof可以探测空对象
反射可以调用所有的非公共访问权限的东西。
对于Method和Filed ，他们都可以通过他们中的方法setAccessible（Boolean）轻松的把他们设置的到权限（如果参数为true就获取权限）。
并且使用public Object invoke（Object obj，Object… args）方法使用args指定的参数调用Method表示的底层方法（如果参数要求为空args也可以为空），obj表示该Method表示的底层方法所属的类的对象（如果这是个static方法，obj可以为null，而且如果这个static方法对应的类还没有初始化，就会在调用前先初始化）。
如果Method表示的底层方法返回基本数据类型，会适当的执行自动包装机制，如果是基本数据类型的数组，那就会直接返回基本类型的数组，如果方法是void类型的，那么就会返回null。
只有final域的字段不会被反射修改，不过反射修改他们的时候并不会抛出异常，只不过实际上并不会发生任何修改。

RTTI有时能解决效率问题，不过没必要过早的关注效率问题，首先应该让程序运行起来。


## (p)1.16 泛型(@since JDK5)

泛型实现了参数化类型的概念，但是Java的泛型存在一些局限。（e.g. Java的泛型的类型参数不支持基本类型，不过有自动包装机制啊）
泛型可以使在编程的时候暂时不指定类型，在使用这个类时，再用实际的类型替换此类型参数。由编译器来保证类型的正确性。

### (p)1.16.1 元组类

将一组对象直接打包存储于其中的一个单一对象（就是在泛型中使用多个类型参数），这个容器对象允许读取其中元素，但是不允许向期中存放新的对象（通过把元素字段声明为final）。

堆栈类：
自己实现了一下。
泛型接口：字面意思。。

### (p)1.16.3 泛型方法

就是把返回值前面加上\<Type>这样的泛型说明就行了（Type可以用在参数列表和方法体中和返回类型中）。
只要能用泛型方法就用泛型方法，用泛型方法取代将整个类泛型化。
static方法不能访问泛型类的类型参数，要使static方法使用泛型能力，必须使其成为泛型方法。
在使用泛型方法时，通常不用指明参数类型，编译器会自动为我们找出具体的类型（类型参数判断）。（貌似推断不是很智能。。）

### (p)1.16.4 显式的类型说明

有时候类型参数判断比较蠢，只能显式的说明类型：必须是方法所属的类型或者就在所属的类中使用的话可以用this再加点加尖括号中间打上类型说明再加方法名：
e.g. New.\<Person>.set()或者this.\<Integer>.set()
。。。总之真蛋疼 这样显式的说明还不一定有用。。（Java SE8）因为在编译器扫描方法调用进行方法解析的时候并不知道任何形式参数的具体信息，如果这时候参数是一个泛型方法的泛化的返回值，这个泛化的类型（假设是<T>中的T）T就会默认的被认为是Object（除非显式的类型说明）

可变参数与泛型方法：字面意思。。

用于Generator的泛型方法：就是在Generator类后面加一个<T> 跟其他类一样的。。没区别

简化元组：利用类型推断。

可以利用泛型构造出复杂的模型出来。

### (p)1.16.5 擦除（使我们不能准确的知道具体类型信息）

Class.getTypeParameters()将返回该Class所对应的对象的类型参数构成的TypeVariable对象数组（也就是在类的声明中尖括号中的符号 比如Class Deme\<T> {}中的T）
所以残酷的现实是在泛型代码内部，无法获取某个特定实例的实际的类型参数的信息，这些具体的类型信息都被擦除了。只能知道用作参数占位符的标识符。
所以在比较ArrayList\<Integer> 和 ArrayList\<String>这两个类的Class对象的时候就会认为是相同的。
然而C++在模版被实例化时，模版代码就能知道其模版参数的类型。
为了解决擦除的这一问题，必须协助泛型类，给定泛型类的边界（这里Java重写了extends），告知编译器只能接收遵循这个边界的类型（也就是显示的在类型参数中使用extends关键字 e.g. <T extends Cls>，多边界：<T extends Interface1 & Interface2> 
在泛型类型的继承中，可以使用边界将参数类型的范围逐渐缩小（只能缩小）

### (p)1.16.6 迁移兼容性

因为泛型在Java中不是一开始就有的成分，所以擦除是不得已的折中方法。泛型类型只有在静态类型检查期间才出现，在这之后，程序中的所有泛型类型都会被擦除，e.g.List<T>会被擦除陈List，普通的类型变量在未指定边界的情况下会被擦除为Object。
迁移兼容性就是在编写泛型代码的时候，还要往前兼容SE5之前的非泛型类库。擦除就允许了非泛型和泛型代码的共存。
所以，无论何时，使用泛型时，对于class Foo\<T> { T var;} Foo\<Cat> f = new Foo\<>()；这类代码时，必须提醒自己：它就是一个Object。
用非泛型类去继承泛型类是可以的，不过在使用的时候可能就会发出警告，这时候可以在警告所在的方法前加入@SuppressWarnings("unchecked")注解忽略这一处的警告。

在泛型中创建数组，用public static Object newInstance(Class\<\?> componentType,int length)实际上并没有拥有参数中类型的具体信息,拥有的只是typeclass对应的对象的Object数组（向上转型了），而且这个typeclass是实例化的时候提供的类型标签，所以要在前面添加一个（T[]）强制类型转换。这时候也会收到警告，使用@SuppressWarnings("unchecked")注解忽略这一处的警告。
因为有了擦除，使用泛型中的方法返回一个泛型中的类型参数中的类的时候，编译器会隐式的插入一个（T）强制转换。（假设T就是那个参数类型）

因为擦除，所以任何在运行中需要知道确切类型信息的操作都将无法工作。
为了解决部分的问题：可以显式的传递具体的类型的Class对象（引入类型标签）。
instanceof操作符可以换成动态的Class.isInstance（）方法 。用Class.newInstance()创建新实例。

### (p)1.16.7 泛型数组

创建一个被擦除类型的新数组，然后对其转型（其实可以不用）。但是这可能会引起unchecked警告，使用@SuppressWarnings("unchecked")注解忽略这一处的警告。
所以在泛型类内部创建泛型数组的话，不如直接创建Object[]而不要创建T[]，反正都会被擦除成Object[]，直接使用Object[]就不会忘记了这个擦除导致的运行时类型了。
要创建特定实例对应的数组，必须向泛型类传递类型标签，然后使用（T[]）Array.newInstance（）。

### (p)1.16.8 通配符

泛型容器不能直接向上转型，因为不能知道泛型类型有关的参数（因为泛型容器不支协变类型，而数组支持）。e.g. List\<Fruit> flist = new ArrayList\<Apple>();是不允许的。
只能使用通配符在两种类型间建立向上转型关系型：List<？ extends fruit> flist = new ArrayList<Apple>();
然而，最坑的是，add（）的参数也会变成？ extends Fruit，编译器不能了解这里需要Fruit的哪个具体类型，所以他不接受任何类型的Fruit。。。
而contains（）和indexOf（）的参数却是Object，因此不涉及通配符，就可以接受任何类型的Fruit。。
所以，应该使用超类型通配符：List\<？ super Apple> flist = new ArrayList\<Apple>() 这样就可以安全的存放Apple以及Apple的子类了，但是还是不能存放Apple的基类Fruit，因为编译器不知道是Apple的具体那个基类。

无界通配符：LIst<?>表示使用了泛型的某种特定类型的非原生List类型。

捕获转换:
在方法内部，如果需要使用一个无界通配符的类型的确切类型，就会发生捕获转换。

### (p)1.16.9 使用泛型的一些问题

任何基本类型都不能作为类型参数。不过可以借助自动包装机制（SE5）
由于擦除的原因，一个类不能实现同一个泛型接口的两种变体，这两种变体擦除之后就会成为相同的接口。
虽然擦除会导致类型参数的转型并不起作用，但是对于readObject这种返回必须转型的对象，就必须只用转型了，哪怕是泛型的转型。
由于擦除的原因，重载方法不能按照类型参数来区分。
古怪的循环泛型（GRG）：e.g. class CuriouslyRecurringGeneric extends GenericType\<CuriouslyRecurringGeneric> {}创建一个继承自以这个导出类作为类型参数的泛型类型。
这样可以产生使用导出类作为其参数和返回类型的基类。
自限定类型（惯用法）（这不是可强制执行的，也就是可以把这种SelfBounded类作为原生类使用）：
e.g. class SelfBounded\<T extends SelfBounded\<T>> { /… }
这样可以强制在继承关系中要求使用SelfBounded时的类型参数只能是基类SelfBounded的导出类。常常用在GCG中。e.g. class A extends SelfBounded\<A> {//… }
还可以将自限定\<T extends SelfBounded\<T>>用于泛型方法。

### (p)1.16.10 参数协变

自限定类型的价值在于他们可以产生协变参数类型：方法类型参数随着子类而变化。
协变返回类型（SE5）：导出类方法可以返回比它覆盖的基类方法更具体的类型。
使用基类方法参数的子类作为导出类方法的参数，在非自限定类型中就是重载，而在自限定类型中就是覆盖。
动态类型安全：（感觉现在没啥用了）
因为可以向SE5之前的代码传递泛型容器，所以可以使用java.util.Collections下的静态方法checkedCollection（） checkedList（） checkedMap（） checkedSet（） checkedSortedMap（） 和 checkedSortedSet（） 动态检查容器。

异常：
泛型类不能直接或者简介继承自Throwable，但是，类型参数可以继承自Throwable。

### (p)1.16.11 混型

混合多种类，在混型中修改某些东西，将会应用于混型所应用的所有类型之上。
C++能够轻松的实现混型，然而Java的擦除特性导致只能使用接口来产生混型效果。
Java可以使用与接口混合（使用代理）的方式产生混型效果。
装饰器设计模式也是一种实现混型的有局限的一种方案。
使用动态代理创建一种跟贴近混型模型的机制：

### (p) 1.16.12 简化语法

JDK7开始，可以缩短创建泛型类的实例的语法：e.g.Gen<String> gen = new Gen<>(args); //<>被称为菱形运算符，告诉编译器要去推断
//感觉也就是SE5的那个自动类型判断嘛。。而且不是很智能的感觉。。辣鸡擦除特性。。




## (p)1.17 lambda表达式(@since JDK8)

lambda表达式本质就是一个匿名(未命名)方法.而且这个方法不能独立执行,只能用于实现由函数式接口(仅包含一个抽象画方法的接口)定义的另一个方法.因此,lambda表达式会产生一个匿名类.lambda表达式也常被称为闭包.
函数式接口定义了lambda表达式的目标类型,lambda表达式只能用于其目标类型已被指定的上下文中.
e.g. interface MyNumbre {
double getValue();
} MyNumber就是一个函数式接口,其功能由getValue()定义
函数式接口可以指定Object定义的任何公有方法,例如equals,而不影响其作为函数式接口的状态.Object的公有方法被视为函数式接口的隐式成员,以newi函数式接口的实例会默认自动实现它们.
lambda表达式在Java语言中引入了一个新的操作符: `->`(lambda操作符) 左侧指定了表达式需要的所有参数(如果不需要参数,则使用空的参数列表) 右侧制订了lambda体.
e.g. () -> Math.random() * 100 e.g. (n) -> (n % 2) == 0 也可以显式的指定参数类型为int n,不过很多时候参数的类型是可以推测出来的. 如果要显式的声明参数类型,就要为所有的出声明类型

当把一个lambda表达式赋给一个函数式接口(包括泛型函数式接口)引用 变量初始化 return语句 方法参数 类型转换 ?运算符 数组初始化器 lambda表达式本身 等提供了目标类型的上下文 时,就相对于为定义了lambda表达式的目标类型的上下文.

为了在目标上下文中使用lambda表达式,抽象方法的类型和lambda表达式的类型必须兼容,返回值必须兼容,可能抛出的异常必须能被方法接收.
当lambda参数只有一个的时候,可以省略掉括号: n -> (n % 2) == 0 不过还是统一都使用扩号好一点.

块lambda表达式:
lambda操作符右侧可以有一个代码块构成.不过在lambda中必须显式的使用return语句来返回值.

lambda操作符右侧的返回值相当于对应的函数式接口中的成员方法的返回值类型,而整个lambda相当于一个实例化了函数式接口的实例的引用.

作为参数传递lambda表达式:
参数类型必须是lambda表达式对应的函数式接口的类型.而且这个lambda表达式太长的话看起来比较笨拙.

lambda表达式可以显式的或隐式的访问this变量 访问并设置其外城作用域内定义的静态变量 实例 .但是,当lambda表达式使用其外城作用域内定义的局部变量时,称为变量捕获,在这种情况下,lambda表达式只能使用实质上的final的局部变量(也就是第一次赋值之后（而且第一次赋值不能是在lambda表达式中）,值不再发生变化的变量，虽然没有显式的指明是final的)或者就是final变量（也就是显式的声明为final的）

方法引用：貌似这些方法引用都是用在方法参数中作为兼容的函数式接口的实例的引用.
静态方法的方法引用：
ClassName::methodName (`::`是JDK8新增的分隔符)
只要是与目标类型兼容(方法返回值 参数 异常)的任何地方都可以使用这个方法引用
和lambda表达式一样,ClassName::methodName方法引用的返回值就是这个函数式接口的一个实例的引用.(所以如果作为参数,那么这个参数应该是函数式接口类型的,而不是该静态方法的返回值)
e.g. public static\<T> T max(Collection\<?extendsT> coll,Comparator<?superT> comp)中的Comparator就是一个函数式接口,可以直接用方法引用出来一个ClassName::instanceMethodName(这里的instanceMethodName正好要和Comparator函数式接口中的方法兼容)直接作为参数comp,因为方法引用与Comparator兼容.
实例方法的方法引用:objRef::methodName
实例方法的方法引用:
ClassName::instanceMethodName

泛型方法的方法引用: 泛型方法指定为方法引用时,类型参数放在::之后 方法名称之前,泛型类指定为方法引用时,类型参数在::之前,类名之后.

构造器引用:
Classname::new
用于数组的构造器引用:type[]::new
只要函数式接口中的方法和构造器兼容(参数相同 返回值就是构造器所属的类)就行

预定义的函数式接口:
JDK8中的新包java.util.function





## (p)1.18 注解(元数据) (Java SE5)

Java支持在源文件中风嵌入补充信息,这类信息被称为注解 e.g. @Override
注解通过基于接口的机制创建的:
@interface MyAnno {
String str();
int val();
} 
这里的注解中的方法更加像成员变量
注解不能包含extends子句
所有的注解都自动扩展了Annotation（java.lang.annotation包中）接口，该超接口重写了hashCode（） equals（） toString（） 方法 还指定了annotationType（）方法用于返回表示调用注解的Class对象
所有类型（类 方法 域变量 参数 枚举常量）的声明都可以有与之关联的注解，注解本身也可以被注解。注解放在声明的最前面。
使用注解：
@MyAnno（str = "Annotation Example",val = 100）
public static void myMeth() {}
所以注解中的方法更加像域变量

### (p)1.18.1 指定保留策略:
保留策略决定在什么配置丢弃注解(他们被封装到java.lang.annotation.RetentionPolicy枚举中): SOURCE CLASS(p.s. 局部变量声明的注解不能存储在.class文件中) RUNTIME
保留策略通过Java内置注解@Retention指定: @Retention(retention-policy)  retention-policy只能是上面这三个枚举常量中的一个,如果没有为注解指定保留策略,将会使用默认策略CLASS
对Class Method Field Constructor 对象调用\<A extends Annotation> getAnnotation(Class\<A> annoType)方法(如果没找到注解 e.g.注解的保留策略不是RUNTIME 就会返回null) (要配合反射一起使用) 也可以用Annotation[] getAnnotations()获取所有Annotation
  JDK5以来，除了getAnnotation和getAnnotations 还有AnnotatedElement接口中定义的getDeclaredAnnotations返回调用对象中存在的所有非继承类注解，isAnnotationPresent（Class <? extends Annotation> annoType）返回annoType指定的注解与调用对象相关联就返回true 否则返回false
JDK8新增getDeclaredAnnotation和getAnnotationByType和getDeclaredAnnotation方法(后面两个方法自动使用重复注解)

### (p)1.18.2 使用默认值:
可以在成员声明后面添加default子句: e.g. @interface MyAnno { String str() default "Default";}

### (p)1.18.3 标记注解: e.g. @Override
标记注解也就是不包含任何成员,唯一的目的就是标记声明.可以用AnnotatedElement接口(Method Field Class Constructor类实现了该接口)定义的isAnnotationPresent()方法确定标记注解是否存在

### (p)1.18.4 单成员注解:
当注解中只包含一个成员,在使用注解的时候可以直接使用()设置成员的值 e.g @Retention(RetentionPolicy.RUNTIME) 就可以使用单值语法(只要没有默认值的成员只有一个的都可以用单值语法)
只要使用单成员注解,成员的名称就**必须**是**value**

### (p)1.18.5 内置注解:
Java的内置注解中有9个用于一般用途:
来自java.lang.annotation包的4个:
@Retention 指定注解的保留策略 只能用于注解其他注解
@Documented 标记接口,用于通知某个工具--注解将被文档化 只能注解其他注解
@Target 用于指定可以应用注解的声明的类型,被设计为只能注解其他注解.它只有一个参数(必须来自ElementType):ANNOTATION_TYPE 另外一个注解 CONSTRUCTOR 构造器 FIELD 域变量 LOCAL_VARIABLE局部变量 METHOD 方法 PACKAGE 包 PARAMETER 参数 TYPE 类 接口或枚举 TYPE_PARAMETER 类型参数(JDK8) TYPE_USE 类型使用(JDK8)
如果要指定多个值:@Target (  {ElementType.FIELD,ElementType.LOCAL_VARIABLE}) 如果不使用@Target 那么除了类型参数之类,注解可以应用于任何声明
 @Inherited 标记注解,只能用于另外一个注解声明 而且只 影响用于类声明的注解 @Inherited会导致超类的注解被子类继承

来自java.lang包的5个:
@Override  标记注解,只能用于方法.带有它的方法必须覆盖超类中的方法
@Deprecated 标记注解,用于指定声明是过时的,并且已经被更新的形式取代
@FunctionalInterface (JDK8新增)标记注解 用于接口,指出被注解的接口是函数式接口.函数式接口是指仅包含一个抽象方法的接口
@SafeVarargs 标记注解 只能用于varargs方法和final构造器,指示没发生与可变长参数相关的不安全操作.可以用于抑制"未检查不安全代码"
@SupressWarnings 用于指定能抑制一个或多个编译器可能会报告的警告.使用字符串形式表示的名称来指示要被抑制的警告.

JDK8开始 Java注解除了可以在声明处使用外,还可以用于类型注解:方法的返回类型(放在方法声明前) 方法内this的类型 强制转换 数组级别 被继承的类 throws子句 泛型 

p.s. 注解数组级别(JDK8): e.g @TypeAnno String @MaxLen(10) [] @NotZeroLen [] str; @MaxLen注解了第一级类型  @NotZeroLen注解了第二级类型 @TypeAnno注解的是变量类型

### (p)1.18.6 重复注解:
JDK8新增能够在相同元素上重复引用注解的特性.
可重复注解必须用@Repeatable进行注解.其value域指定了重复注解的容器类型（Class<?extends Annotation> value();）,也就是重复注解类型的数组.要创建重复注解,需要创建容器注解,然后将注解的类型指定为@Pepeatable注解的参数.
为了使用getAnnotation方法访问重复注解,需要使用容器注解(就是那个用@Repeatable注解的那个单成员注解(只有一个需要重复注解的那个注解的数组作为返回值的成员方法))而不是重复注解.
获取重复注解的另一种方式是使用JDK8添加到AnnotatedElement中的新方法:
\<T extends Annotation> T[] getAnnotationsByType(Class\<T> annoType)和getDeclaredAnnotationByType()

一些限制:注解不能继承另外一个注解 注解声明的所有方法都必须不带参数 ,注解不能被泛型化.注解方法不能指定throws子句


## (p)1.19 I/O 

基于文本的控制台IO对于实际的Java编程确实用处不大.
流(java.io包定义的类层次中实现的 基于缓冲和基于通道的IO在java.nio及其子包中定义):
Java程序通过流执行IO,流通过Java的IO系统链接到物理设备.所有流的行为方式都是相同的.
 
字节流(@since 1.0)和字符流(@since 1.1)
> 在最底层,所有的IO仍然是面向字节的.

### (p)1.19.1 字节流

字节流通过两个类层次定义:InputStream 和 OutputStream (这两个都是顶层的抽象类)
java.io中的字节流:
Buffered(缓冲) ByteArray(字节数组) Data(Java标准数据类型) File(文件) Filter(同于实现InputStream和OutputStream) Object(对象) Piped(管道) 
\+ 后缀InputStream和OutputStream (Input都是读取 Output都是写入)

PrintStream 包含print()和println()的输出流 PushbackInputStream (支持1字节"取消获取"输入流,这种流向输入流返回1字节) SequenceInputStream (由多个按顺序依次读取的输入流组合而成的输入流)

### (p)1.19.2 字符流
字符流通过底层的两个抽象类Reader和Writer定义.这两个抽象类处理Unicode字符流
java.io中的字符流类:Buffered CharArray File Filter(过滤的读取或写入器) Piped(管道) String 后接Reader或Writer分别表示xx输入流(读取)或输出流(写入)
InputStreamReader(将字节转换成字符的输入流) OutputStreamWriter(将字符转换成字节的输出流) LineNumberReader (计算行数的输入流) PushbackReader(允许字符返回至输入流的输入流)


上面这些类全部都继承了Reader 或 Writer 或 InputStream 或 OutputStrem 中的 read和write（int byteval）（write只能写入byteval的低八位）方法

Java.lang.System类中预定义了三个流变量:in out err 他们都是public static final的
System.in是InputStream类型的对象 System.out和System.err都是PrintStream类型的对象
BufferReader是支持缓冲的输入流:BufferedReader(Reader inputReader)
为了获得Reader的子类InputStreamReader:InputStreamReader(InputStream inputStream) 而java.lang.in就是InputStream类的一个引用
所以创建一个与键盘连接的BufferReader对象:BufferedRader br = new BufferedReader(new InputStreamReader(System.in));

为了从BufferReader对象读取字符 使用 int read()  throws IOException 将字符作为整数值返回,如果达到流的末尾,就返回-1
读取字符串:String readLine() throws IOexception 
PrintWriter类:PrintWriter(OutputStream outputStream,boolean flushingOn) flushingOn为true表示每次调用print()方法 println方法时刷新输出流
使用PrintWriter可以使实际的应用程序更容易国际化.

### (p)1.19.3 读写文件

FileInputStream(String fileName) throws FileNotFoundException 文件不存在的时候就会抛出
FileOutputStream(String fileName) throws FileNotFoundException 不能打开文件或不能创建文件 就会抛出 打开输出文件时 先前存在的同名文件将被销毁
FileNotFoundException 属于IOException的子类
p.s. 对于存在安全管理器的情况(e.g. applet) 可能会抛出SecurityException

### (p)1.19.4 关闭文件

void close() throws IOException 关闭文件会释放为文件分配的系统资源 如果关闭文件失败会导致内存泄漏
在JDK7之前 当不需要文件时显示的调用close方法(FileInoutStream和FileOutputStream中都实现了) JDK7之后可以使用带资源的try语句
读取文件:可以使用FileInputStream中的int read() throws IOException  一个字节一个字节的读取 到达文件末尾时返回-1

JDK7新增自动资源管理（ARM） :  可用于自动关闭不再使用的资源,这样就不需要显式的调用close关闭资源了
try (resource-specification) {
	//use the resource
}
resource-specification是用来声明和初始化资源的语句 而且在resource-specification中声明的资源被隐式的声明为final 可以用分号分隔多个资源声明
p.s 至于哦那些实现了AutoCloseable接口的资源,才能使用带资源的try语句




## (p)1.20 多线程编程

在基于进程的多任务处理中,程序是调度程序能够调度的最小代码单元.
在基于线程的多任务环境中风,最小的可调度代码单元是线程:
进程是重量级任务,线程是轻量级任务.
单线程系统使用轮询时间循环模型.
线程的状态:运行 挂起(suspended) (可以被恢复(resumed)) 堵塞(blocked) 任何时候都可以终结线程,终结.
每个线程都有优先级(为整数):优先级决定上下文切换.
决定上下文切换发生时机的规则:1.线程自愿放弃控制(显示的放弃控制权,休眠或者I/O之前堵塞),这是会检查所有其他线程,并准备运行的线程中优先级最高的那个线程会获得CPU资源 2.线程被优先级更高的线程取代(抢占式多任务处理)
p.s. 相同优先级的两个线程竞争CPU资源:Windows以循环方式自动获取CPU资源,其他系统优先级相等的线程必须自愿的向其他线程放弃控制权,否则其他线程就不能运行.这里可能引发移植问题.
相同优先级的子线程共享CPU
因为多线程为程序引入了异步行为,所以必须有强制同步的方法.
每个对象都有自己的隐式监视器,如果调用对象的同步方法,就会自动进入对象的隐式监视器.一旦某个线程位于一个同步方法中,其他线程就不能调用同一对象的任何其他同步方法.

### (p)1.20.1 Thread类和Runnable接口

Thread类中的方法:getName() setName() setPriority()设置优先级(优先级在MIN_PROIORITY到MAX_PRIORITY之间也就是1到10) getPriority()获取线程的优先级 isAlive() 确定线程是否仍在运行 join() 等待进程终结，否则一直运行该方法 run() 线程的入口点 static sleep() 挂起当前线程一段时间(sleep(long millis)单位ms sleep(long millis,int nanos)第二个参数为纳秒单位,挂起时间为millis+nanos) start() 通过调用线程的run()方法启动线程 static Thead currentThread()返回调用它的线程的引用 toString() 输出由线程名称 优先级 所属线程组构成的Steing[] main方法的优先级默认为5
主线程(程序启动时自动创建):其他子线程都是从主线程中产生的,主线程必须是最后才结束执行的线程,因为它要执行各种关闭动作.
创建线程:实现Runnable接口或者扩展Thread类
为了实现Runnable接口,类只需要实现public void run()方法,run方法为程序中另外一个并发线程的执行建立了入口点.当run方法返回的时候,这个线程就会结束.
Thread类的其他构造器:Thread(Runnable threadOb,String threadName) 新线程的名称由threadName决定法,在创建了新线程之后,只有调用线程的start方法,线程才会运行,本质上,start方法执行对run方法的调用
扩展Thread类：需要重载run方法 
两种方法都需要在构造其中调用start或者run方法
如果不需要对Thread类进行修改或增强的话 推荐使用实现Runnable接口的方法来创建线程.

### (p)1.20.2 线程优先级：
高优先级的线程会获得更多的cpu时间，具有高优先级的线程可能会取代低优先级的线程（e.g. 从休眠或者等待IO中恢复的高优先级线程会取代低优先级的线程）
理论上,具有相同优先级的线程应当得到相等的cpu时间,不过不同的环境上的jvm实现不一定相同,为了安全起见,具有相同优先级的线程应当是不是释放控制权.
如果线程依赖于抢占式行为,经常会引起相同优先级的线程的不一致性.
同步:
如果没有采取什么方法阻止多个线程在相同时间调用同一对象的同一方法,就是竞态条件,这些线程会互相竞争以完成方法.
当多个线程需要访问共享资源时,在给定时刻只有一个线程可以拥有监视器(用作互斥锁的对象),其他企图进入加锁监视器的线程都会被挂起,直到第一个线程退出监视器.
在方法前使用synchronzied关键字限定,使之成为同步方法,一旦线程进入一个实例的同步方法,所有其他线程就不能再进入相同实例的任何同步方法.
所有对象都有与自身关联的隐式监视器,为了进入对象的监视器,只需要调用使用synchronized关键字修饰过的方法.为了退出监视器并将对象的控制权交给下一个等待线程,监视器的拥有者只需简单得从同步方法返回.
synchronized语句:对于不能修改源代码的没有同步方法的类（也就是没有针对多线程设计的）,可以通过把方法调用放在synchronized(objRef) { /…}中，实现同步方法。synachronized代码块确保对objRef对象的成员方法调用是会在当前线程成功进去objRef的监视器之后发生。

### (p)1.20.3 线程间通信：
进程间通信可以更细微级别的控制。
为了避免轮询检测,Java提供wait() notify() notifyAll() 方法(这些方法是Object类中的final方法)进行更巧妙的线程间通信.
wait方法通知线程放弃监视器并进入休眠,直到其他一些线程进入同一个监视器(也就是同一个对象)并调用notify() notifyAll() 方法.
notify() 唤醒调用相同对象的wait方法的线程.
notifyAll方法唤醒调用相同对象的wait方法的所有线程,其中的一个线程将得到授权访问.
p.s. 理论上,wait方法会等待直到调用notify方法或notify方法,但是极小可能会由于假唤醒而被唤醒,Oracle推荐在一个检查线程等待条件的循环中调用wait方法
死锁(deadlock):当两个线程循环依赖一对同步对象时,就会发生死锁.多线程程序偶尔被锁住的时候应当首先检查是否是死锁.

不推荐使用那个suspend方法和stop方法和resume方法(suspend和stop方法在Java2中有时会导致严重错误,如果为关键数据加锁的线程用suspend挂起那么这些锁无法释放,其他等待这些资源的线程会被死锁;resume又必须配合suspend一起使用;如果在线程正在向关键数据结构写入数据且只完成了部分发生变化的数据时使用stop方法可能导致数据结构处于损坏状态,stop会导致释放调用线程的所有锁,其他等待相同锁的线程就会使用这些损坏的数据)

可以用wait和notify方法实现。

获取线程状态：
Thread.State getState()  (State是Thread中的一个枚举类型)
返回值:BLOCKED(线程因为正在等待需要的锁而挂起执行) NEW(线程还没有运行) RUNNABLE(线程要么当前正在执行要么在获得CPU访问权之后执行) TERMINATED(线程已经完成执行) TIMED_WAITING(线程挂起一段指定的时间,例如调用sleep wait join(重载的)这些含有明确等待时间的方法) WAITING (线程因为等待某个动作而执行挂起 e.g. 调用wait 和join (没有明确时间的版本))
不过有可能调用getState方法之后 线程的状态就发生了改变

线程不要创建太多,不然花费在上下文切换的CPU时间会比执行程序的实际时间更长







## applet

java.awt 抽象窗口包
java.applet applet通过GUI框架与用户进行交互 主类需要继承自applet
applet每次必须重新绘制输出时都会调用paint(Graphics g)方法,所以必须在主类中覆盖该方法.

在IDEA中调试applet需要配置一下配置器,use the default one provided by IntelliJ in the application bin directory called appletviewer.policy
而且从JDK7开始,所有applet都需要签名才能在web浏览器中运行,比如在Java控制面板中调整安全设置.也可以直接用在cmd中使用appletviewer运行

applet程序不是从main入口处运行的,用户IO不是使用Java的IO流类完成的,而是使用GUI框架提供的接口.

## (p) 杂项

### `transient`和`volatile`修饰符
+ 实例变量声明为`transien`t表明存储对象的时候实例变量的值将不需要永久保存.

+ `volatile`告诉编译器变量可以被程序的其他部分随意修改:在多线程程序中,有时候多个线程共享相同的变量时,处于效率方面的考虑,每个线程自身可以保存这种共享变量的私有副本.真正的变量副本(主变量副本在各个时间被更新,例如进入同步方法时)这样有时效率不高,为了确保变量的主副本总是反映自身的当前状态,可以将变量修改为volatile告诉编译器必须总是使用volatile变量的主副本(至少总是保持所有私有版本和最新的主副本一致) 此外,访问主变量的顺序必须和所有私有副本相同,以精确的顺序执行.

`instanceof`操作符: objref instencesof type (type是Class类型)

`strictfp`: Java 2之后 浮点计算模型稍微宽松了一点(e.g. 在计算期间新模型不需要截断特定的中间值) strictfp修饰类 接口 或方法 确保采用java 1.0的浮点计算模式 修饰接口和类相当于修饰了其中的所有成员方法.
不过这个关键字极少使用…

### 本地方法
偶尔你想调用非Java语言编写的子例程.
Java提供了`native`关键字,用于声明本地代码方法.大部分本地代码都是用C语言编写的,将C代码集成到Java程序中的机制被称为Java本地接口(JNI).
声明为`native`的方法没有方法体,调用含有该方法体的本地动态链接库为方法提供方法体
使用System.loadLibrary(String filename)调用本地动态链接库(这句代码一般放在static代码块中)
本地代码的问题:安全性问题 可移植性丢失

### assert
assert condition; 或 assert condition:expr; expr是要传递给AssertionError构造器的值(字符串)
断言.用于证实在测试期间遇到了某些期望的条件,如果condition为false则抛出AssertionError异常
为了在运行时启用断言检查,必须指定-ea选项 可以用-da指定执行代码时禁用断言






### 紧凑API配置文件:(JDK8)
将API库的子集组织陈所谓的紧凑配置文件:compact1 compact2 compact3 每个配置文件以前一个配置文件为基础(e.g. compact3 包含整个compact2)
这样的优势在于:用不到完整库的应用程序不需要下载整个库,减小了库的大小,让一些Java应用程序能够在无法支持完整Java API的设备上运行.降低了加载程序的时间.
使用-profile选项指定是否只使用了紧凑配置文件中定义的API:javac -profile profilename Programname e.g. javac -profile compact2 Test.java





# 类库


## 字符串类库

Java中的String类操作都不会改变原字符串,只有StringBuffer和StringBuilder(效率比StringBuffer高点)能够保存可以进行修改的字符串.
这三个类都在java.lang类库中定义.而且都是final的.都是实现了CharSequence接口.

String类中的构造器可以构造出类似于ASCII字符集的8位字节数组 : String(byte chrs[]) 和 String(byte chrs[], int startIndex, int numChars)
> p.s.String类从输出创建字符串之后修改数组内容不会改变String对象（因为是复制过去的）

String（int codePoints[],int startIndex,int numChars）支持扩展的unicode字符集.codePoints是包含Unicode代码点的数组.

int length() 获取String字符串包含的字符数量

对于String,有重载的`+`运算符.

可以直接使用字符串字面值创建String对象

static String valueOf()针对所有基本类型和Object类型重载,返回对应的String对象的引用

char charAt(int where)

void getChars(int sourceStart,int sourceEnd,char target[],int targetStart) 子串为索引从sourceStart到sourceEnd-1之间的字符

getBytes()

toCharArray()

boolean equals(Object str) 比较字符串内容(区分大小写)

boolean equalsIgnoreCase(String str) 大小写不敏感  注意区分equals和==的区别:==比较的是两对象的引用是否一样,不过对于同一字面量创建的String对象,他们的引用是相同的

regionMatches() 比较两字符串特定部分是否相同

startsWith() 
endsWith() 相当于regionMatches的特定形式

int compareTo(String str) String实现了Compareable<T>接口定义的方法
int compareToIgnoreCase()

int indexOf() 查找第一次出现的索引
Int lastIndexOf() 查找最后一次出现的索引

String substring() 提取字串

String concat(String str) 连接字串

replace()

String trim()返回调用字符串的副本,并移除开头和结尾的所有空白字符

toLowerCase

toUpperCase

static String join(CharSequence delim,CharSequence… strs)
static String join(CharSequence delim,Iterator<? extends CharSequence> elemnts) 连接字符串 (JDK8),delim作为每个字符串之间的分隔符

int coidePointAt(int i)返回i指定的位置的Unicode代码点(应该就是因为char已经有点装不下所有的字符了呗,所以就扩展到了int(int中超过unicode的char的范围的就叫增补字符),这就是返回unicode码,这个代码点就是除了unicode中除了ASCII的字符)
int codePointBefore(int i)
int codePointCount(int start,int end)
boolean cotains(CharSequence str) 包含
boolean contentEquals(CharSequence str)和boolean contentEquals(StringBuffer str) 内容相同
static String format(String fmstr,Object… args) 和 static String format(Local loc,String fmstr,Object… args) 返回格式化的字符串
boolean isEmpty()
boolean matches(String regExp) 返回正则表达式是否匹配
int offsetByCodePoints(int start,int num) 返回调用字符串中超过start索引开始num个数量代码点的代码点的索引
String replaceFirst(String regExp,String newStr)
String replaceAll(String regExp,String newStr)
String[] split()
CharSequence subSequence(int startIndex,int stopIndex)

StringBuffer:
在构造器不是指定size的那个时,StringBuffer会(额外)预留16个字符的空间,再次分配空间很耗时的.而且频繁分配空间容易产生内存碎片

length()

int capacity() 返回已分配的容量

void ensureCapacity(int minCapacity) 创建StringBuffer对象之后,可以使用这个方法设置缓冲区大小,阻止不必要的内存分配次数可以提高性能,出于效率方面的考虑可能会分配比minCapacity更大的缓冲区.

void setLength(int len) 设置StringBuffer对象中字符串的长度 (len非负,如果len小于原来存储的字符串的大小,超出长度的字符都会丢失,大于就会添加null字符)

charAt()

void setCharAt(int where,char ch)

getChars()

append()

insert()

reverse() 颠倒StringBuffer的字符

delete()
deleteCharAt()

replace()

substring()

appendCodePoint()
codePointAt()
codePointBefore()
codePointCount()
indexOf（）
lastIndexOf()
offsetByCodePoints()
subSequence()

void trimToSize() 要求为调用对象减小字符缓冲区的大小,以更适应当前内容.

StringBuilder (JDK5)

StringBuilder不是同步的,这意味着它不是线程安全的.但是StringBuilder有更高的性能.




## java.lang包



java.lang是惟一一个会被自动导入到所有程序的包

抽象类Number是所有基本类型包装器(除了Boolean)的超类

浮点型:
一些常量:NaN非数字 POSITIVE_INFINITY 正无穷大 NEGATIVE_INFINITY负无穷大 BYTES使用字节数表示的宽度(JDK8) SIZE位宽

statc long doubleToLongBits(double num) 返回与num对应的 与IEEE兼容的双精度位模式
statc long doubleToRawLongBits(double num) 返回与num对应的 与IEEE兼容的双精度位模式,保留NaN值

整型:
static decode(String str)可以把str转换成对应的整型并返回
static parseByte(String str) 可以返回与str(内容是十进制数字)等价的数字 Byte可以换成其他的整型名称
static parseByte(String str,int radix)可以返回与str(内容是radix进制数字)等价的数字 Byte可以换成其他的整型名称
static int toUnsignedInt() 把参数作为无符号整数返回(JDK8)
Short中的: static short resverseByte(short num)交换num的高字节(高8位)和低字节(低8位)并返回结果
Integer中:
static int bitCount(int num)返回num中已设置的位数
static int compareUnsigned(int num1,int num2) (JDK8)对num1和num2进行无符号比较
static int divideUnsigned(int dividend,int divisor) 作为无符号值返回无符号除法dividend/divisor的结果(JDK8)
static Integer getInteger(String propertyName)返回与propertyName指定的环境属性相关联的值,如果失败就返回null
static Integer getInteger(String ptopertyName,int default) 返回与propertyName指定的环境属性相关联的值,如果失败就返回default
static int highestOneBit(int num)  确定已设置(也就是设置为1的)的最高位的位置,如果全是0,就返回0
static int lowerstOneBit(int num) 确定已设置的最低位的位置
static int numberOfLeadingZeros(int num) 返回num中第一个设置的高阶位之前高阶0位的数量.如果num为0就返回32
static int numberOfTrailngZeros(int num) 返回num中的第一个设置的低阶为之前低阶0位的数量,如果num为0就返回32
static int parseUnsignedInt(String str) 无符号整型…(JDK8)
static int parseUnsignedInt(int dividend,int divisor)
static int reserseBytes(int num) 颠倒num中字节的顺序并返回
staric int rotateLeft(int num,int n) 返回将num向左旋转n位的结果
static int signum(int num) num为负数就返回-1 为正数就返回1 为0就返回0
static String toBinaryString(int num)
toHexString()
toOctalString()返回对应的八进制表示的字符串

Character类:
char charValue() 返回该对象封装的char值
isDefined() 返回是否是Unicode定义的
isDigit() 返回是否是数字字符
isIdentifierIgnorable() 返回参数是否在标识符中应当被忽略
isISOControl() 返回是否是ISO控制符
isJavaIdentifierPart() 返回是否允许参数作为Java标识符的一部分(第一个字符除外)
isJavaIdentifierStart() 返回是否允许参数作为Java标识符的第一个字符
isLetter()
isLetterOrDigit()
isMirrored() 返回参数是否为镜像的Unicode字符(也就是将文本从右向左反向显示的字符)
isTitleCase() 返回参数是否是Unicode表特格式字符
toTitleCase() 返回与参数等价的标题形式
forDigit(int num,int radix) 返回与参数num值关联的数字字符,转换进制由radix指定
digit(char digit,int radix) 将digit按照指定的进制返回对应的整数

代码点:
JDK5开始,Java支持的Unicode字符集扩展到了0到10FFFF也就是需要32位的Unicode,大于FFFF的字符叫补充字符
Java使用两个char表示一个附加字符,第一个char叫高代理,第二个char叫低代理
代码点就是32位Unicode对应的值

isBmpCodePoint() 是否属于BMP字符
isHighSurrogate() 高代理
isLowSurrogate() 低代理
isSupplementaryCodePoint() 扩充字符
isSurrogatePair(char highCh,char lowCh) 返回是否highCh和lowCh能够构成有效的代理对
isValidCodePoint() 有效的代码点
static int toChars(int cp,char target[],int loc) 将cp中的代码点转换成等价的char形式,将结果储存在target中,从loc开始保存.如果cp能够用单个char表示就返回1否则返回2

### Void类

Void类只有域变量TYPE,用来保存对void类型的Class引用.不能创建Void的实例.

### Process类

抽象类用来封装进程,也就是执行的程序.Process主要用作由Runtime类的exec()方法创建的对象类型或ProcessBuilder类的Start()方法创建的对象类型的超类.
void destory() 终止进程
Process destoryForcibly() 强制终止进程.返回对进程的引用(JDK8)
int exitValue() 返回从子进程获得的退出代码
int waitFor() 返回进程返回的退出代码,该方法知道调用进程终止时才会返回.

### Runtime类

Runtime类封装了运行时环境,不能实例化Runtime对象.不过可以调用静态的Runtime.getRuntime()方法获得对当前Runtime对象的引用.获得对当前的Runtime对象的引用就可以用一些方法来控制JVM的状态和行为了.
applet和其他不信任的代码如果调用了Runtime类定义的任何方法,就会引起SecurityException异常.
void addShutdownHook(Thread thrd) 将thrd注册为Java虚拟机在终止时运行的线程.
Process exec(String progName) throws IOException 作为独立的进程执行progName执行的程序,返回描述新进程的Process类型的对象.
Process exec(String progName,String environment[]) throws IOException 在environment指定的环境中,作为独立的进程执行progName指定的程序,返回描述新进程的Process对象
Process exec(String comLineArray[]) 作为独立的进程执行comLineArray中字符串指定的命令行
void exit(int exitCode) 中断执行并将exitCode的值返回给父进程,约定0表示正常终止.
long freeMemory() 返回JVM可以使用的空闲内存的近似字节数
void gc() 开始垃圾回收
void halt(int code)立即终止Java虚拟机.不运行任何终结线程或终结器.code的值将返回给调用进程.
void runFinalization() 为那些不再使用但是还没有被回收的对象调用finalize()
long totalMemory()
void traceInstructions(boolean traceOn) 根据traceOn的值打开或关闭指令跟踪
void traceMethodCalls(boolean traceOn) 根据traceOn的值打开或关闭方法调用跟踪.
p.s. exec可以执行其他环境的程序(e.g. 在windows上可以调用notepad)

### ProcessBuilder类

构造器中的args指定被执行的程序的名称和命令行参数.
List<String> command() 返回对List对象的引用,List对象包含程序的名称和参数.对List的修改会影响调用对象.
ProcessBuilder command(List<String> args) 将对象的名称和参数设置为由args指定的值.对List对象的 修改会影响调用对象.返回对调用对象的引用.
ProcessBuilder command(String… args) 同上
File directory() 返回调用对象的当前工作目录.如果该目录与启动此进程的Java程序的目录相同就返回null.
Map<String,String> environment() 返回调用对象相关联的环境变量.
ProcessBuilder inheritIO() 使被调用进程为标准IO流使用与调用进程相同的源和目录.
ProcessBuilder.Redirect redirectError() 作为ProcessBuilder.Redirect对象返回标准错误的目标.
ProcessBuilder redirectError(File f) 将标准错误的目标设置为指定文件,返回对调用对象的引用.
ProcessBuilder redirectError(ProcessBuilder.Redirect target) 将标准错误的目标设置为target指定的目标,返回对调用对象的引用.
boolean redirectErrorStream() 如果标准错误已经被重定向到标准输出流就返回true 如果标准错误被分离 就返回false
ProcessBuilder redirectErrorStream(boolean merge) 如果merge为true 就将标准错误流重定向到标准输出,如果为false,标准错误流将被分离,这是默认状态.返回对调用对象的引用.
PeocessBuilder.Redirect  redirectInput() 作为ProcessBuilder.Redirect对象返回标准输入的源.
ProcessBuilder redirectInput(File f) 将标准输入的源设置为指定的file 
Process start() 开始调用对象指定的进程,也就是运行指定的 程序.

ProcessBuilder.Redirect类:这个抽象类封装了连接到子进程的IO源或目标
to(File f)
from(File f)
appendTo(File f) 重定向到附加文件
file()
type()返回ProcessBuilder.Redirect.Type枚举值

### System类

如果操作被安全管理器禁止,很多方法就会抛出SecurityException异常
arraycopy()
static String clearProperty(String which) 删除由which指定的环境变量,返回原来与which关联的值
static Console console() 返回与JVM关联的控制台.如果JVM当前没有控制台,就返回null.
static long currentTimeMillis() 以毫秒返回当前时间,从1970年1月1日午夜开始计时
static void exit(int exitCode) 中断执行,并将exitCode 的值返回给父进程(通常是操作系统) 0表示正常终止
static void gc() 开始垃圾回收
getenv() 返回当前环境变量
static Procertis getProperties() 返回与Java运行时系统关联的属性 e.g. user.dir获得当前的路径
static Procertis getProperties(String which) 返回与Java运行时系统关联的属性,没有就返回null
static String getProperties(String which,String default) 返回与which相关联的属性,如果没有找到期望的属性就返回default
static SecurityManager getSecurityManager() 安全管理器
static int identityHashCode(Object obj) 返回obj对象的表示散列码
static Channel inheritedChannel() 返回Java虚拟机继承的通道,没有通道继承就返回null
static String lineSeparator() 返回包含行分隔符的字符串(因为*nix和windows的行分隔符有区别嘛)
load(String libraryFileName) 加载有由libraryFileName指定的文件中的动态库,必须指定完整路径
loadLibrary(String libraryName) 加载名为libraryName的动态库
String mapLibraryName(String lib)返回特定与平台的库名
static long nanoTime() 获得系统最精确的计时器并返回某些任意启动点以来以纳秒表示的时间值.计时器的精确度未知.
setErr(PrintStream eStream) 将标准err流设置为eStream
setProperties()
setSecurityManager()

环境属性:
too many…

### Object类

Object clone() throws CloneNotSupportedException 创建一个新的与调用对象相同的对象

### Cloneable接口

只有实现了Cloneable接口的 类才能被clone()复制,否则抛出CloneNotSuportedException
不过这种精确副本也有很多潜在的危险性:副本与原始对象具有完全相同的内容,所以在副本中所引用的对象的内容的修改同样会改变原始对象.

### Class类

Class类型的对象是在加载类时自动创建的.
static Class<?> forName(String name,boolean how,ClassLoader) 返回给定全名的Class对象.如果how为true就初始化对象.
ProtectionDomain getProtectionDomain() 返回与调用对象关联的保护域.

### ClassLoader类
抽象类ClassLoader定义了加载类的方式.可以创建扩展ClassLoader的子类,但是在正常情况下不需要这么做.

### Math类

double atan2(double x,double y) 返回正切值由x/y指定的角度
double cbrt(double arg) 返回arg的立方根
exp(double arg) e的arg次方
expm1(double arg) e的arg-1次幂
log1p(double arg) arg+1的自然对数
scalb(arg,factor) arg乘以2的factor次幂
abs() 绝对值
ceil(arg) 返回大于或等于arg的最小整数
floor(arg) 返回小于或等于arg的最大整数
floorDiv(dividend,divisor)返回不大于dividend/divisor的结果的最大整数(JDK8)
floorMod()
nextAfter(float arg,double toward) 从arg的值开始,返回toward反向的下一个值.如果arg==toward就返回toward (因为这些浮点型都有最小精度的,下同)
netDown(val)返回低于val的下一个值(JDk8,val为浮点型)
nextUp(arg) 返回正方向上arg的下一个值(arg为浮点型)
rint(arg) 返回最接近arg的整数值(arg为浮点型)
round(arg) 返回arg只入不舍的最近整数值
ulp(arg) 返回arg的ulp值(arg为浮点型)
addExact(arg,arg)将两数相加并返回(发生溢出就会抛出AritheticException)
copySign(arg,signarg) 返回arg,符号与signarg相同(参数都是浮点型)
decrementExact(arg) 返回 arg-1 溢出就抛出异常
getExponent(arg) 返回arg的二进制表示形式所使用的2的指数(arg为浮点型)
hypot(double side1,double side2) 给定直角边side1 side2返回斜边长
IEEEremainder(dividend,divisor)返回dividend/divisor的余数
incrementExact()
multiplyExact()溢出会抛出异常的乘法
negateExact() 返回相反数 溢出就抛出异常
subtractExact()溢出会抛出异常的减法
double toDegrees(double angle) 弧度转化为度
toRadians(double angle) 将度转化为弧度

StrictMath() 
内容和Math一模一样,不过比Math更精确(也好像就是java1.0的浮点模型),但是性能低一点

### Compiler类

支持创建Java环境,从而将Java字节码编译成可执行代码,常规编程不使用Compiler

### Thread类,threadGroup类 Runnable接口

Runable接口定义了抽象方法run()作为线程的入口点
Thread类:
创建Thread如果没有指定线程组那个新线程和父线程将属于相同的线程组.
Thread中关于优先级的常量:MAX_PRIORITY MIN_PRIORITY NORM_PRIORITY 分别是10 1 5
Thread类中的stop() suspend() resume() countStackFrames() (会调用suspend()) destory() 不建议使用
int activeCount() 返回线程所属线程组中活动的线程的大概数量
checkAccess() 让安全管理器核实当前线程是否能够访问和修改对之调用checkAccess()方法的线程.
currentThread()
dumpStack() 显示线程的调用栈堆
int enumerate(Thread threads[] 将当前线程组中所有的Thread对象的副本放入Thread中,返回线程数)
Map<Thread,StackTraceElement[]> getAllStackTraces()
ClassLoader getContextClassLoader() 返回用于调用该线程加载类和资源的上下文类加载器
Thread.UncaghtExceptionHandler getDefaultUncaughtExceptionHandler()
getID() 返回调用线程的ID
getPriority() 返回优先级
Thread.State getState() 返回调用线程的状态
boolean holdsLock(Object ob) 如果调用线程拥有ob的锁就返回true否则返回false
isDaemon() 守护线程
isInterrupted() 线程被中断
join() 进行等待知道线程终止
setContextClassLoader(ClassLoader cl)
setDaemon(boolean state)
setDefaultUncaughtExceptionHandler(Thread.DefaultUncaughtExceptionHandler e)
static void yield() 调用进程将CPU让给其他线程


### ThreadGroup类

ThreadGroup(String groupName) 创建一个新组,该组将当前线程作为父线程
int activeGroupCount() 返回调用线程为父线程的活动线程组的数量(包括子线程组)
int enumerate(Thread group[]) 将调用线程组(包括子线程组)包含的活动线程放入group数组中
int enumerate(Thread group[],boolean all) 将调用线程组包含的活动线程放入group数组中.如果all为true就把子线程组中的所有线程也被放入group数组中
int enumerate(ThreadGroup group[]) 将调用线程组的子线程组(包括子线程组的子线程组放入group数组中)
int enumerate(ThreadGroup group[,boolean all] 将调用线程组的活动子线程组放入group数组中.如果all为true就把子线程组的所有子线程组也被放入group数组中) 
void interrupt() 调用所有线程(以及所有的子线程组)的interrupt方法
void list() 显示有关线程组的信息
void uncaughtException(Thread thread,Throwable e) 当某个异常未被捕获,调用该方法

ThreadLocal类用于创建线程局部变量,每个线程具有线程局部变量的一个副本
InheritableThread类用于创建可以被继承的线程局部变量

### Package类

getImplementationTitle() 返回调用包的标题
getImplementationVendor() 调用包的实现程序的名称
getImpleentationVersion() 调用包的版本号
static Package[] getpackages() 返回调用程序当前知道的所有包
String getSpecificationTitle() 调用包规范的标题
getSpecificationVendor() 调用包规范的所有者的名称
is AnnotationPresent(Class<? extends Annotation> anno) 如果anno描述的注解与调用对象有关就返回true
 isCompatibleWith(String verNum) 如果verNum小于或等于调用包的版本号就返回true
isSeled() 如果调用包被密封就返回true
isSealed(URL url) 如果调用包相对于url密封就返回true

RuntimePermission类 与Java的安全机制有关

### Throwable类

getStackTrace()返回一个堆栈帧数组

SecurityManager类

StackTraceElement类
描述单个的堆栈帧(stack frame)
StackTraceElement(String className,String methodname,String fileName,int line) 如果没有有效的行号,line会使用一个负值,并且line为-2表示这个堆栈帧引用一个本地方法
isNativeMethod()

### Enum类

调用Enum的clone方法会抛出CloneNotSupportedException
final int compareTo(E e) 比较同一枚举中的两个常量的顺序值,调用常量的顺序值比e小就返回负数.
Class<E> getDeclaringClass 返回枚举常量的类型
name() 枚举常量的名称
ordinal() 枚举常量在常量列表中的位置的值

Classvalue类 用于为类型关联一直值.针对非常特殊的应用设计,不用于常规编程.

CharSequence接口
default IntStream chars() 返回调用对象中字符的一个流(JDK8)
default IntStream codePoints() 代码点的IntStream流

Comparable接口
compareTo(T obj)

Appendable接口
append方法

### Itreable接口

所有被用于foreach的类都要实现Iterable接口
Iterator<T> iterator() 为调用对象包含的元素返回迭代器
default void forEach(Consumer<? super T> action) (JDK8) 对于迭代的每一个元素,执行由action指定的代码(Consumer是JDK8新增的一个函数式接口)
default Spliterator<T> spliterator() 返回被迭代序列的Spliterator(JDK8)

### Readable接口

指示对象可以用作字符的源
int read(CharBuffer buf) throws IOException 将字符读入buf中,返回读取的字符数 如果遇到EOF就返回-1

### AutoClaseable接口

该接口对带资源的try语句提供了支持.
该接口只定义close() 方法
该方法关闭调用对象,释放调用对象可能占用的所有资源.在带资源的try语句的末尾会自动调用该方法.

Thread.UncaughtExceptionhandler接口
该静态街头希望处理未捕获异常的类实现.
只有一个方法:void uncaughtException(Thread thrd,Throwable exc) thrd是对生成异常的线程的引用,exc是对异常的引用.

### java.lang子包

java.lang.reflect 反射相关
java.lang.annotation Annotation接口 Elementtype枚举 RetentionPolicy枚举 
java.lang.invoke 支持动态语言 
java.lang.instrument 定义了能够被用于为程序执行的各个方面添加工具的特性
java.lang.management 为JVM和执行黄金提供了管理支持
java.lang.ref 为垃圾回收过程提供了更灵活的控制



## java.util包



集合(容器)框架(Collections Framework) :
集合只能存储引用,所以基本数据类型都要自动装箱成包装器类.

接口:
Collection(顶层接口,扩展了Iterable接口) List(扩展自Collection)  Queue(扩展自Collection) Set(扩展自Collection) SortedSet(扩展Set以处理已排序的集合) Deque(扩展Queue以处理双端队列) NavigableSet() (扩展SortedSet以基于最接近匹配原则检索元素)
Comparator接口用于比较两个对象 Iterator ListIterator Spliterator(用于并行处理的)接口用来枚举集合中的对象
RandomAccsee接口表明列表支持高效随机的元素访问

### Collention接口

Iterator<E> iterator() 返回调用集合的一个迭代器
void clear() 移除调用容器中的所有元素
default Stream<E> parallelStream() 返回一个使用调用容器作为元素来源的流.该流能够支持并行操作(JDK8)
default boolean removeIf(Predicate<? super E> predicate) 从调用集合中移除满足predicate指定条件的那些元素(JDK8) (Predicate是一个函数式接口)
default Spliterator<E> spliterator() 返回调用集合的Spliterator(JDK8)
default Stream<E> stream() 返回一个使用调用容器作为元素来源的流.该流是顺序流(JDK8)
Object[] toArray()
<T> T[] toArray(T array[] ) 如果包含调用集合中元素的数组.数组元素是集合元素的副本.如果array的长度大于等于容器中元素的数量,超出部分的元素数组设置为null;array的长度小于容器中元素数量,就分配必须大小的新数组并返回这个新数组.

### List接口

ListIterator<E> listIterator(int index) 返回从index指定的索引位置开始将元素返回迭代器
default void sort(Comparator<? super E> comp) 使用comp指定的比较器排序列表(JDK8)
default void replaceAll(UnaryOperator<E> opToApply) 使用opToApply函数获得的值更新列表中的每一个元素(JDK8) (UnaryOperator是JDK8新增的一个函数式接口)

### Set接口

如果用add()向Set添加重复的元素,add会返回false.

### SortedSet接口:
以升序进行排序了的.
SortedSet<E> headSet(E end) 返回的SortedSet对象包含已排序调用组中那些小于end的元素.
SortedSet<E> tailSet(E start) 返回的对象包含排序组中大于或等于start的元素.

### navigableSet接口:
E ceiling(E obj) 在对象中查找大于等于obj的最小元素,没找到就返回null
Iterator<E> descendingIterator() 返回一个从最大元素向最小元素移动的迭代器(也就是返回一个反向迭代器)
E floor(E obj) 小于等于的最大元素
NavigableSet<E> headSet(E upperBound,boolean incl) 返回小于upperBound的所有元素,如果incl(也就是include的缩写)为true那么返回的元素中就包含等于upperBound的那个元素
higher(obj) 大于obj的最大元素
lower(obj)小于obj的最小元素
E pollFirst() 返回第一个元素(也就是最小值)并移除这个元素
E pollLast() 返回并移除最后一个元素

### Queue接口
elelment() 返回队列头部的元素,不移除该元素.队列为空就抛出NoSuchElelmentException
boolean offer(E obj) 试图将obj添加到队列中
E peek() 返回队列头部的元素,不移除该元素.如果队列为空就返回null
E poll() 返回并移除头部元素,队列为空就返回null
E remove() 移除并返回队列头部元素

### Deque接口
双端队列既可以像queue一样FIFO也可以像stack一样FILO
addLast() addFirst() 如果超出了容量就会抛出IllegalStateException
getFirst() getLast() 返回但不移除元素,如果队列为空就抛出NoSuchElementException
offerLast() offerFirst() 添加元素,满了就返回false
peekFIrst() peekLast() 返回但是不移除元素,为空就返回null
pollFirst() pollLast() 返回并且移除元素,为空就返回null
removeFirst() removeLast() 返回并且移除元素,为空就抛出NoSuchElelmentException
boolean removeFirstOccurrence(Object obj) 移除第一次出现的obj对象,如果根本就没有obj就返回false

### 容器类
标准容器类不是同步的.
标准容器类:AbstractCollection(实现了Collection的大部分方法) AbstractList(扩展AbstractCollection并实现List大部分方法) AbsrtractQueue(扩展AbstractCollection并实现Queue大部分方法) AbstractSequentialList(扩展AbstractList,用于顺序访问) AbstractSet(扩展AbstractCollection)
LinkedList(扩展AbstractSequentialList,链表) ArrayList(扩展AbstractList,动态数组) 
ArrayDeque(扩展AbstractCollection并实现Deque,动态双端队列)
EnumSet HashSet(乱序) LinkedSet(有序) TreeSet LinkedHashSet(继承自HashSet,有序)
PriorityQueue 基于优先级的队列(扩展AbstractQueue) (根据构造器中的Comparator比较器进行比较) (可通过PriorityQueue对象中的comparator方法返回对象使用的比较器,返回null表示使用的是自然顺序)
遗留类(也重新设计了,不被推荐使用)
Vector Stack Hashtable

这些容器类都重写了toString方法.

trimToSize()可以将容器的容量调整为当前容纳的元素数量
对于所有有默认容量的容器,默认的容量都是16.
对于Hash存储数据,使用add() contains() remove() size()时的速度很快而且不受容器中元素的多少的影响.

HashSet构造器:HashSet(int capacity, float fillRatio) fillRatio为填充率(0.0到1.0),决定哈希组被填充到什么程度就增加容量.默认为0.75

### EnumSet:
static <E extends Enum<E>> EnumSet<E> allOf(Class<E> t) 创建并返回由t指定的枚举中的元素构成的EnumSet
static copyOf(Enum<E> e) 根据c中的元素创建EnumSet
static complementOf(EnumSet<E> e) 创建并返回由e中未存储的元素构成 e.g. e是用一个enum{A,B,C,D}中的A创建的ENumSet,那么complementOf( e)会返回B,C,D为元素的EnumSet
static <E extends Enum<E>> Enum<E>  of(E v, E… varargs) 创建并返回包含v和varargs这些枚举值的EnumSet

### 迭代器:
迭代器是实现了Iterator或ListIterator接口(扩展了Iterator接口,允许双向遍历列表)的对象
Iterable接口是在里面创建了Iterator对象,并且支持并发迭代.
Iterator中的方法:
default void forEachRemaining(Consumer<? super E> action) 遍历并对每个元素执行action指定的动作(JDK8)
hasNext() next() 
default void remove() 移除当前元素.如果在调用next()之前试图调用remove就会抛出IllegalStateException.默认版本抛出UnsupportedOperationException
ListIterator
hasPrevoius() 
previous()
previousIndex()

Splitrerator(JDK8)接口(支持并行编程) 把hastNext和next方法合为一个方法提高效率
int characteristics() 返回调用spliterator的特征,该特征被编码为整数
long estimateSize() 估计剩余的要迭代的元素数并返回.如果由于某些原因得不到元素数就返回Long.MAX_VALUE
default long getExactSizeIfKnow() 如果调用spliterator的特征(characteristics()&SIZED)==0就返回剩余的要迭代的元素数否则返回-1
default boolean hasCharacteristics(int val) 如果val传递了调用对象的特征就返回true
boolean tryAdvance(Consumer<? super T> action) 在迭代的下一个元素上执行action,如果没有下一个元素就返回false (就相当于hasNext和next功能)
Spliterator<T> trySplit() 分割并返回分割后的spliterator引用 失败就返回fasle(什么鬼??) 操作成功的话,原spliterator会迭代序列的一部分返回的spliterator迭代序列的其他部分

RandomAccess接口
没有任何成员,只是为了表明集合支持高效的随机访问其中元素.

映射(Map) 不支持迭代器
接口:Map 将唯一键映射到值 Map.Entry 描述映射中的元素(值键对) SortedMap(扩展Map接口)以升序保存键 NavigableMap(扩展SortedMap) 以处理基于最接近匹配原则的值键对索引

### Map接口

default V compute(K k,BiFunction<? super K,? super V,? extends V> func)  调用func构造一个新值.如果func返回值不是null就把新的值替换掉k对应的旧值,如果func返回null就把原来的值键对移除并返回null(JDK8)
default V computeIfAbsent(K k,Funtion<? super K,? extends V> func) 返回与键k关联的值.如果没有值就通过func构造一个值,并把该配对输入到映射中,返回构造的值.如果无法构造新值就返回null(JDK8)
default V computeIfPresent(K k, BiFunction <? super K,? super V,? extends V>  func) 如果k包含在映射中就通过func构造一个新值替换掉原来的值,然后返回新值,如果func返回的值为null就从映射中删除现有的键和值,并返回null(JDK8)
Set<Map.Entry<K,V>> entrySet() 返回包含映射中所有条目的Set对象,这个组包含Map.Entry类型的对象.这样就可以使用Set迭代了(因为Set实现了Iterator而Map不支持)
default V getOrDefault(Object k,V defVal) 如果映射中包含与k关联的值就返回该值否则返回defVal
default V merge(K k,V v,BiFunction<? super K,? super V,? extends V> func) 如果k没有包含在映射中,就把k和v配对并添加到映射中并返回v.否则func基于原有的值返回一个新值,键被更新为使用这个新值.如果func返回null就从映射中删除现有的键和值并返回null(JDK8)
default void replaceAll(BiFunction<? super K,? super V,? extends V> func) 对调用映射中的每个元素执行func用func返回的结果替换元素,如果在操作过程中删除了元素就会抛出ConcurrentModificationException(JDK8)

### NavigableSet接口

ceilingEntry(K obj) 返回大于等于参数的最小键的条目 没有就返回null
descendingKeySet 返回逆序形式的键的NavigableSet
floorEntry 小于或等于参数的最大键的条目

### 映射类:
AbstractMap EnumMap HashMap TreeMap WeakHashMap(使用带有弱键的哈希表) IdentityHashMap(当比较文档时使用引用相等性,不用于通用目的) LinkedHashMap(扩展了HashMap类,可以按照插入或访问顺序迭代整个映射,在构造器中的Order参数设置为true就使用访问顺序否则默认使用插入顺序)

LinkedHashMap中添加了removeEldestEntry 默认返回fasle而且不执行任何操作,代表保留最久的条目,如果重写为返回true就可以使LinkedHashMap移除映射中最久的条目

### Comparator接口

默认Java使用自然比较器(也就是1在2前面,A在B前面那种)
JDK8中新增default Comparator<T> reversed() 返回调用比较器的逆序版本
static <T extends Comparator<? super T>> Comparator<T> naturalOrder()返回颠倒元素的自然顺序比较器
static <T> Comparator<T> nullsFirst(Comparator<? super T> comp)和static <T> Comparator<T> nullsLast(Comparator<? super T> comp)分别返回认为null比任何值小和null比任何值大的比较器
default Comparator<T> thenComparing(Comparator<? super T> thenByComp) 该方法可以返回一个比较器(组合了第一个和第二个比较器之后的比较器),在第一个比较器(也就是现在被调用的这个比较器)比较的两个对象是相同的时再调用第二个比较器比较,thenByComp指定第一次比较返回相等后调用的比较器. (JDK8)

### 集合算法

Collentions(注意有一个s)类中的static方法:
addAll(Collection<? super T> c,T… elelments) 把elements指定的元素插入到c中.
Queue<T> asLifoQueue(Deque<T> c) 返回一个后进先出的Queue
int binarySearch(List<? extends T> list,T value,Comparator<? super T> c) 
int  binarySearch(List<? extends Comparator<? super T>> list ,T value) 
checkedCollection(Collection<E> c , Class<E> t) 返回容器c按照作为元素类型的类型安全的Collection
disjoint(Collection<?> a,Collection<?> b) 比较a和b中的元素.如果两个集合不包含公共元素就返回true否则返回false
shuffle(List<T> list,Random r) 使用r作为随机数的来源随机化list中的元素
Set<T> singleton(T obj) 将单个的obj转换成Set
Collection<T> synchronizedCollection(Collection<T> c)返回基于c的线程安全的集合.
Collection<T> unmodifiedCollection(Collection<? extends T> c) 返回基于c的不可修改的集合

### 

Arrays类:提供了对数组的一些操作
里面的static方法:
<T> List asList(T… array)
binarySearch()从各种数组(必须是已经排序过了的)中二分查找并返回
copyOf()
copyOfRange()
deepEquals() 同于比较两个可能嵌套数组的数组是否相等
deepToString和deepHashCode也适用于这种嵌套数组的数组
fiil()使用特定的值填充数组
sort()以升序排序数组
parallelSort() 并行排序,显著提高速度(JDK8)
Spliterator.OfDouble spliterator(double array[]) 返回整个数组的spliterator (JDK8)
<T> Spliterator spliterator(T array[]) (JDK8)
DoubleStream stream(double array[ ]) (JDK8)
<T> Stream stream (T array[ ]) (JDK8) 
setAll(double array[],IntToDoubleFunction<? extends T> genVal) 为数组的所有元素赋值  (JDK8) 
parallelSetAll(double array[],IntToDoubleFunction<? extends T> genVal) (JDK8) 
parallelPrefix(double array[],DoubleBinaryOperator func) 对数组进行修改使每个元素都包含对其前面的所有元素应用某个操作的累积结果.

### 一些遗留的类和接口:
Enumeration接口 类似于Iterator接口
Vector类 类似于ArrayList 实现动态数组,不过Vector是同步的.
Stack类 已经被ArrayDeque取代
Dictionary类 类似于Map
Hashtable类 与HashMap类似
Properties类 Hashtable的子类 System.getProperties()方法返回的就是这个类的对象,用于保存环境值
Properties类中的store和load方法可以方便的读取和存储文件

## 更多的java.util工具类

### StringTokenizer类
用于解析格式化的输入，实现了Enumeration接口
指定的定界符为空白字符（空格 制表符 换页符 换行符 回车符）也可以在构造器中指定参数delimiters 指定定界符,定界字符串中的每个字符都被认为是有效的定界符，如果delimAsToken为true就在解析字符串时将定界符作为标记返回否则不返回定界符（默认就是不返回的）
hasMoreElements（） nextElement() 方法和hasMoreToken() 和 nextToken()类似

### BitSet类

特殊类型的数组:元素是布尔形式的位值(这样可以为只用01作为开关的设置节省空间嘛)

Optional OptionalDouble OptionInt OptionLong 类 (JDK8) 强烈推荐使用Optional类来替代null
为处理值可能存在可能不存在的场合.这些类的所有构造器都是private的
以前通常用null表示没有值存在,但是如果引用null值就会出现NullPointerException,这样需要频繁的检查null值.
static <T> Optional<T> empty() 返回一个对象,该对象调用isPresent()会返回false
Optional<T> filter(Predicate<? super T> condition) 如果调用对象的值满足condition 返回一个与调用对象相同的值的Optional实例否则返回一个empty的Optional对象
T get()
void ifPresent(Consumer<? super T> func) 如果调用对象存在值就调用func将该对象传递给func否则什么都不会发生
static <T> Optional<T> of(T val) 创建一个包含val的Optional实例并返回val不能是null
static <T> Optional<T> ofNullable(T val) 创建一个包含val的Optional并返回如果val是null就返回一个empty的Optional实例
T orElse(T defVal) 如果调用对象包含值就返回该值否则返回defVal
T orElseGet(Suploer<? extends T> getFunc) 如果调用对象包含值就返回该值否则返回getFunc获取的值
<X extends Throwable> orElseThrow(Suplier<? extends X> excFunc) throws X extends Throws 返回调用对象的值,否则就抛出excFunc生成的异常

orElse就相当于ifPresent和get的综合

### 时间和日期相关的

Date类 该类很多方法都已经不赞成使用了,很多功能已经被Calendar和DataFormater替代
默认构造区使用当前日期和时间初始化对象,Date(long millisec) millisec表示自1970年1月1日午夜以来经历的毫秒数
from() 和 toInstant 可以实现Instant和Date的互化.

Instant类(JDK8)

Calendar类
国际化的提供时间信息的方法.
无公共的构造器.
adstract void add(int which,int val) which必须指定Calendar的一个域变量 e.g. Calendar.HOUR val添加到时间或者日期组成部分中,val为负数的时候为减法操作.
final void clear() 将调用对象中包含的所有时间组成部分清零
static Locale[] getAvailableLocales() 返回一个由Local对象组成的数组,期中包含可以使用日历的地区信息.
static Calendar getInstance() 为默认地区和时区返回Calendar对象.
static Calendar getInstance(TimeZone tz) 为tz指定的市区返回Calendar对象,默认地区
getTimeZone()
getDate()
isSet()
void set(int which,int val) 

GregorianCalendar类
GregorianCalendar类是Calendar抽象类的具体实现
GregorianCalendar新增AD和BC域变量分别表示格林尼治时间的两个纪元。
isLeapYear测试是否为闰年
String getCalendarType 返回被调用的日历的类似 e.g. "gregory表示格林尼治时间"

TimeZone抽象类
用于处理GMT和UTC之间的时差
static String[] 个头AvailableIDs( ) 返回一个表示所有时区名称的String对象数组
static String[] getAvailableIDs(int RawOffset) 返回与GMT时差为timeDelta的所有时区的名称,RawOffset单位为毫秒
abstract int getRawOffset() 返回计算当地时间需要添加到GMT的原始时差(使用毫秒) 这个值不会针对夏令时进行调整

SimpleTimeZone类
TimeZone的一个实例子类.

 Locale类
用于描述地理或者文化上的 区域.
Locale.CHINA表示中国地区的Locale对象
static void setDedault(Locale localeObj) 将JVM使用的默认地区设置为localObj
final String getDisplayCounty() 
final String getDisplayLanguage()
final String getDisplayName() 这些返回人类能够阅读的字符串用于显示国家的名称 语言和地区的完整描述
statc Locale getDefault() 获得默认地区

### Random类
用于生成伪随机数.
double nextGaussian() 返回下一个高斯分布随机数
JDK8新增doubles() ints() longs()分别返回DoubleStream IntStream LongStream 

### Observable类
用于创建可以被程序其他部分观察的子类.但这种子类的对象发生变化时,观察者就会注意到,观察者必须实现Observer接口,但观察者注意到被观察者的某个变化时,会调用update方法
如果被观察对象发生变化时就必须调用setChange()方法,当准备通知观察者这一变化时就必须调用notifyObservers()方法,这会导致被观察对象的update方法被调用.
所以在notifyObservers之前一定要setChange

addObserver将参数添加到观察调用对象的对象列表中
clearChanged 调用该方法会将调用对象的状态返回为"未改变"

Observer接口:
函数式接口:void update(Observable observObj,Object arg) arg是notifyObservers()传递的值

Timer 和 TimerTask
在将来的某些时候安排执行任务.
可以用来创建在后台运行的 等待特定时间的线程.

TimerTask实现了Runnable接口,不过run是抽象方法,所以要被重写.
boolean cancel() 终止任务.成功就返回true
long scheduledExecutionTime() 返回所安排任务的最后一次执行的时间.

Timer类用于安排工作,被安排的任务必须是TimerTask类的实例.
默认构造器创建的Timer对象作为正常线程运行.如果重载的版本中isDaemon为true就执行守护进程,只要程序的剩余部分仍在继续执行,守护进程就会执行.
void cancle() 取消定时器线程
int purge() 从定时器队列中删除已经取消的任务
void schedule(TimerTask TTask,long wait) TTask被安排在wait(单位毫秒) 之后执行
void schedule(TimerTask TTask,long wait,long repeat) TTask被安排在wait(单位毫秒) 之后执行然后以repeat指定的时间间隔重复执行.

Currency类
封装了货币有关的信息

### Formatter类
格式化输出
void flush() 刷新格式化缓冲区,这会导致缓冲区中当前所有输出都写入目标.主要用于与文件绑定的Formatter对象
可以用close显示的关闭Formatter对象也可以用带资源的try自动释放Formatter的资源
相对于C语言新增的格式说明符:%h(参数的哈希码) %b(布尔值) 
%t(时间,对应的参数必须是Calendar Date Long 或long) 后缀:a(星期名简称) A(星期名全称) b(月份名简称) B(月份名全称) c(标准格式:天 月份 日期 小时:分钟:秒数 时区 年)
D(月/日/年) F(年-月-日) k(小时 0-23) l(小时1-12) m(月份01-13) M(分钟 00-59) p(以小写形式表示本地时间am或pm) r(小时:分钟:秒数(12小时制)) R(小时:分钟:秒数(24小时制)) z (相对于UTC的时差) Z(地区名) 

逗号标志:显示大数字的时候用逗号三位三位隔开:e.g. &,.2f来输出一个455555322.34为455,555,322.34

使用参数索引:
在格式说明符%后面加上n&(n表示后面的args中的第几个索引) e.g. "%3$d %1$d %2$d",10,20,30 输出为30 10 20

### Scanner类
格式化输入,关闭也可以使用带资源的try语句.
构造器可以使用Reader或Path(指定的目录下的该文件)或File或者InputStream(一般都是用System.in)或ReadableByteChannel并用String charset来指定编码方式 作为输入源
默认的定界符是空白符 也可以使用Scanner useDelimiter(String pattern)来指定定界符(可以用InputStream )
String findInLine(Patttern pattern)可以根据正则表达式在文本下一行中搜索指定的字段(该方法单独与所有的定界符组) 一旦读取到pattern指定的字符串,读取出下一个标记
e.g. Age:28  findInLine("Age:");然后再用next输出就是28了
p.s. next() 不吸收定界符(e.g. 空字符) nextLine() 会吸收回车符

String findWithinHorizon(Pattern pattern,int count) 在后序count个字符中查找指定的模式,找到就返回匹配的模式.
Scanner skip(Pattern pattern) 如果与pattern匹配就跳过当前编辑,并返回对调用对象的引用,否则抛出NoSuchElementException

ResourceBundle(抽象类) ListResourceBundle PropertyResourceBundle类
用于帮助程序国际化.
ResourceBundle类用于管理地区敏感的资源的集合(e.g. 显示程序中用户界面的字符串可以定义多套用于各种语言的翻译过的字符串,每套翻译过的字符串都在自己的资源包中.可以加载适用于当前地区的资源包)
资源包(也就是ResourceBundle的子类)通过基名(家族名字)进行识别.在基名后节接两个小写语言代码用于指定语言和两个大写国家代码(用下划线分隔)(e.g.SampleRB_zh_CN 表示SampleRB资源包中的中文版本,中国地区) (也就是命名为自定义名_语言代码_国别代码.properties)
getResourceBundle() 参数可以是ListResourceBundle的一个子类也可以是.property文件(推荐用.property文件) (不用写下划线之后的东西e.g. Sample_zh_CN要直接写成Sample)
!!!!getBundle必须使用相对于classpath的全限定的名称!!!!!!!!!!!!!!!!!!!!!e.g.com.DCMMC.test.SampleRB!!!!而且,里面的中文竟然要用unicode码表示,不能直接用中文..反人类啊..e.g.停止要写成\u505c\u6b62
getString(String k)
ResourceBundle有两个子类:PropertyResourceBundle和ListResourceBundle(抽象类)(需要实现protected adstract Object[][] getContents 返回一个二维数组用于表示资源的键值对,键必须是字符串)

### java.util中的其他使用工具类:
Base64 用于支持Base64编码.JDK8新增Encoder和Decoder嵌套类
DoubleSummaryStatistics 支持编译double值.可以获得平均值 最小值 最大值 记数 和 (JDK8)
EventListenerProxy 扩展了EventListener类(事件监听器)
EventObject 所有事件类的超类
FormatterFlags 定义了Formattable接口使用的格式标志
IntSummaryStatistics 支持编译int值(JDK8)
Objects 操作对象的各种方法
PropertyPermssion 管理属性许可
ServiceLoader 提供了一种查找服务提供者的手段
StringJoiner 支持连接CharSequence对象 可以包含分隔符 前缀 后缀(JDK8)
UUID 封装并管理全球唯一标识符(UUID)
接口:
EventListner 表明类是事件监听器
Formattable 提供格式化字符串

### java.util的子包:
java.util.concurrent java.util.concurrent.atomic  java.util.concurrent.locks 用来支持并行编程.这些包为使用java内置的同步特性提供了高性能的替换方法.java.util.concurrent还提供了Fork/Join框架(JDK7)
java.util.function 为lambda表达式定义了一些预定义的函数式接口
java.util..jar 提供了读取和写入Jar文件的能力
java.util.logging 记录日志
java.util.prefs 提供了对用户选择的支持,通常用于程序配置
java.util.regex 正则表达式
java.util.spi 提供对服务提供者的支持
java.util.stream 提供来了Java的流的API(JDK8)
java.util.zip 提供了ZIP个GZIP格式的读写


## java.io


区分I/O流和JDK8新增的流API

### File类 (不过在新的NIO中的Path接口可以替换File的大多数功能) 

java.io中大多数类用于操作流而File不是.File类直接处理文件和文件系统.i.e. File类没有指定如何从文件检索信息以及如何想文件中储存信息,而是描述文件本身的属性.
在Windows中可用\作为路径分隔符,如果要使用Windows的路径分隔符的话,必须使用\\双斜杠(因为要转义)
delete()
renameTo()
toPath()
getParentFile() 对于new File("file")这种使用相对路径的File类必须使用getAbsolutePath获得对应的完整路径的FIle
String[] list() 返回File对象对应的文件夹下的所有文件或子文件夹.
String[] list(FIlenameFilter FFobj) FIlenameFilter接口(函数式接口)的类的对象筛选出匹配的文件(夹)
File[] listFIles() 类似于list
mkdir() 创建文件夹
mkdirs() 创建完整的路径,包括所有父目录(递归创建)

AutoCloseable(JDK5) Closeable(JDK7,扩展了Closable ) Flushable接口 (JDK5)
AutoCloseable 接口用来实现带资源的try语句的close方法
Flushable接口 flush方法可以强制将缓冲的输出写入与对象关联的流中.

Java中基于流的IO构建在4个抽象类之上:InputStream OutStream Reader Writer (这些都实现了Closabled接口)
字节流:
InputStream (实现了Closeable接口)
int available() 返回当前可读取的输入字节数
void mark(int numBytes) 在输入流的当前位置放置标记,该标记在读入numBytes个字节之前一直都有效
void reset() 将输入指针重置为前面设置的标记
boolean markSupported() 
int read() 读入并返回代表下一个可用的整数,到达文件尾就返回-1
long skip(long numByte) 跳过numBytes个字节

OutStream类 (实现了Closeable Flushable)
void flush() 结束输出状态,从而清空所有缓冲区,i.e.刷新输出缓冲区

FileInputStream类 从文件中读入字节
FileOutputStream类 向文件写入字节
ByteArraInputStream类 使用字节数组作为院的输入流,如果没有用mark()设置reset标志,那么使用reset会将流指针设置为流的开头
ByteArrayOutputStream类 默认构造器会创建一个32字节的缓冲区 close()方法对它没有效果,所以不需要用.writeTo()可以把流的内容写入其他的OutputStream流中
过滤的字节流 : FilterOutputStream类  和 FilterInputStream类 方法和InputStream和OutputStream一模一样

缓冲的字节流: BufferedInputStream 和 BufferedOutputStream 和 PushbackInputStream 缓冲IO能够提升性能.
BufferedInputStream 
支持mark和reset
将缓冲区大小设置陈内存页面 磁盘块等大小的整数倍可以明显提高性能(e.g. 8192字节就不错).
BufferedOutputStream类
flush() 会立即写入缓冲区的所有数据.这样只在必要的时候才写入数据,减少写入次数提高性能.
PushbackInputStream类
可以在读取字节后 在将他们回推到流中.
unread() 会将参数会推到PushbackInputStream流中
PushbackInputStream会使创建它的InputStream流中的mark和reset方法无效.所以在使用他们的之前一定要用markSupported方法检查.

SequenceInputStream 类
允许连接多个InputStream对象.

### PrintStream类
System.out就是这个类的对象
实现了Appendable Closable Flushable接口
构造器可以接收Writer或者OutputStream作为参数，boolean autoFlushingOn控制每次调用printfln（） printf（） format（）时是否自动刷新输出缓冲区（默认为false）

Console类（JDK6）//感觉就这个readPassword有点意思(输入的时候不可见)
Console类用于从控制台读取和写入内容，并实现了Flushable接口。
Console没有构造器,只能从System.console()方法获取Console对象.
p.s. 如果没有可用的Console存在时,System.console()返回null,所以首先要确保返回的这个Console对象不是null(想IDEA Eclipse这样的 IDE 都是测试代码时都是没有Console存在的,所以返回的都是null)
输入错误时会抛出IOError错误(Error的子类),而且这种情况一般是出现了灾难性的系统失败.
readLine() 和 readPassword() 方法的参数不为空的那个重载形式只不过是输出提示的话而已(和C语言的scanf完全不一样…)

### 串行化
将对象的 状态写入字节流的过程.(可以使用串行化保存到永久性存储区(e.g. 文件中),还可以使用反串行化来恢复这些对象)
实现远程方法调用(RMI,Remote Method Invocation)也需要串行化.
假如对象X和对象Y互相包含对方的对象的引用,这样的关系组形成了一个环形引用的有向图(指那些对象中包含其他对象的引用,这种关系可以构成有向图)
支持串行化的接口和类:
Serializable接口
只有实现了Serializable接口的类才能够通过串行化功能进行保存和恢复.Serializable接口没有任何的成员,只是简单的用于指示类可以被串行化.Serializable的子类不能保存声明为transient或static的变量
ObjectOutput接口 (扩展了DataOutput和AutoClosable接口,并且支持对象串行化)
write和writeObject方法分别把字节数组和Object对象写入调用流中.
p.s. 
writeObject用于串行化对象,如果遇到错误就会抛出IOException
ObjectOutputStream类 (扩展OutStream并实现了ObjectOutput接口)
ObjectOutputStream(OutputStream outStream) 将向ObjectOutputStream中写入串行化对象的输出流,关闭ObjectOutputStream对象会自动关闭outStream指定的底层流.
ObjectInput接口(扩展了DataInput和AutoClosable接口)
Object readObject() 从调用流中读取对象.用于反串行化对象.
ObjectInputStream类 (扩展了InputStream类并实现了ObjectInput接口)
ObjectInputStream(InputStream inStream) inStream是从中读取串行化对象的输入流.关闭ObjectInputStream会自动关闭inStream指定的底层流.
Externalizable接口
可以为串行化使用压缩和加密技术.
void readExternal(ObjectInput inStream) 和 void writeExternal(ObjectOutput outStream) 分别用于读取对象的字节流和将对象写入期中的字节流.

流的优点
为复杂且笨重的任务提供了清晰的抽象方案.通过组合过滤流类可以动态的构建适应数据传输需求的自定义接口.


## NIO @since 1.4 JDK7时进行了大扩展(有时也被称为NIO.2)

### NIO中的包：
java.nio为NIO系统的顶级包，用于封装各种类型的缓存。(这些缓冲区包含NIO系统所操作的数据)
java.nio.channels 支持通道,通道的本质是打开的IO连接
java.nio.channels.spi 支持通道的服务提供者
java.nio.charset 封装字符集 另外还支持分别将字符转换成字节以及字节转换陈字符的编码器和解码器
java.nio.charset.spi 支持字符集的服务提供者
java.nio.file 提供对文件的支持
java.nio.fie.attribute 提供对文件属性的支持
java.nio.file.spi 支持文件系统的服务提供者

缓冲区用于容纳数据,通道表示打开的IO设备(e.g. 文件或套接字的连接)
为了使用NIO系统,需要获取用于连接的IO设备的通道以及用于容纳数据的缓冲区.然后操作缓冲区,根据需要输入或输出数据.

缓冲区在java.nio包中定义,所有的缓冲区都是Buffer类的子类.
Buffer类定义了当前位置(position,缓冲区中下一次发生读取和写入操作的索引(offset)) 界限(limit,缓冲区最后一个有效位置之后的下一个位置的索引值) 容量(capacity,缓冲区能容纳元素的数量),而且还支持标记和重置.
final Buffer flip() 将调用缓冲区的界限设置为当前位置,并将当前位置设置为0.返回对缓冲区的引用.
isDirect()返回调用的缓冲区是否是定向的,如果是的话那就可以直接对缓冲区进行IO操作.
由Buffer 派生出的缓冲区类:ByteBuffer CharBuffer DoubleBuffer FloatBuffer IntBuffer LongBuffer MappedByteBuffer(是ByteBuffer 的子类,用于将文件映射到缓冲区) ShortBuffer 
这些派生类都提供了get()或者put() 方法,使用allocate方法手动分配缓冲区,使用wrap方法在缓冲区中封装数组,使用slice()创建缓冲区的子序列

### 通道:
java.nio.channels包中定义的.
通道表示IO源或目标的打开的连接.通道实现了Channel接口并扩展了Closable接口.
对支持通道的对象调用getChannel方法可以获取指定类型的通道.
支持通道的对象:DatagramSocket FileInputStream FileOutputStream RandomAccessFile SercerSocket Socket
也可以用Filels类中定义的静态方法获取通道.(e.g. newByteChannel方法获取字节通道,它返回一个SeekableByteChannel对象(这是FileChannel类实现的一个接口))

FileChannel和SocketChannel这类通道支持各种read和write方法
FileChannel提供静态的open方法打开并返回指向文件的通道.map()可以将文件映射缓冲区.

FileChannel中的MappedByteBuffer map(FilChannelMapMode mode,long position ,long size) 可以将通道映射到缓冲区(mode有以下几种值:MapMode.READ_ONLY MapMode.READ_WRITE MapMode.PRIVATE(创建文件的私有副本,并且对缓冲区的修改不会影响底层的文件) )
NIO使用的另外两个实体是字符集和选择器.
字符集定义了将字节映射为字符的方法,可以使用编码器将字符编码成字节,用解码器将字节解码成字符.这些都由java.nio.charset包中定义的类提供.
选择器支持基于键 非锁定的多通道IO.也就是使用选择器可以通过多个通道执行IO.选择器由java.nio.channels包中定义的类支持.

### JDK7 对NIO的增强(尤其是对文件方面的支持)
新增三个新包:java.nio.file java.nio.file.attribute java.nio.file.spi
java.nio.file:
Path接口 继承自Watchable Iterable<Path> Comparable<Path>接口 
resolve() 将相对路径解析为绝对路径(如果参数表示的是相对路径,就把调用Path对象指定的根路径加在参数表示的相对路径前并返回)
boolean endsWith(path) 如果Path对象以path指定的路径结束就返回true(path为String或者Path对象)

### Files类
提供许多操作文件的静态方法.

### Path接口
通过Paths(提供了了很多关于Path的静态方法)中的get方法(其中一个重载版本可以将路径名分成几个String部分)可以返回Path实例

CopyOption类用于表示Files类中的copy方法中复制或移动文件时的参数
StandardCopyOption中定义了几个值:ShandardCopyOption.COPY\_ATTRIBUTES 要求复制文件的属性 StandardCopyOption.NOFLLOW\_LINKS不使用符号链接 StandardCopyOption.REPLACE_EXISTING 覆盖先前存在的

FileAttriubute 文件属性接口 表示文件(夹)的属性 定义在javanio.file.attribute包中 BasicFIleAttributes为顶部接口 封装了一些所有文件系统都通用的一组属性
BasicFileAttributes派生出DosFIleAttributes和PosixFileAttributes接口
可以通过Files中的getFileAttribute和getFileAttributeView方法访问文件属性 (NIO中定义了AttributeView BasicFileAttributeVIew DosFileAttributeView PosixFIleAttributeView 属性视图接口 )

FileSystem FileSystems FileStore类
用于访问文件系统,用FilsSystems甚至可以通过newFileSystem方法获取新的文件系统.FileStore类封装了文件存储系统.

LinkOption 这个表示链接文件的类型(默认都是符号链接,也可以使用Files中定义的常量域NOFOLLOW_LINKS表示阻止符号链接的LinkOPtion对象)
OpenOption是描述打开文件方式的接口,StandardOpenOtopn是OpenOption的一个实现(一个枚举实现),其中定义了一个枚举用来表示文件选项:APPEND表示写入文件的末尾 CREATE表示文件不存在就创建文件 CREATE\_NEW表示文件不存在时在创建文件 DELETE_ON_COLSE 文件被关闭时删除文件 DSYNC对文件的修改会立即写入物理文件,一般为了效率都是对文件的修改都是先进行缓冲,在需要的时候才写入文件的 READ为输入操作打开文件 SYNC对文件或者文件中元数据的修改被立即写入物理文件. WRITE 为写入操作 TRUNCATE_EXISTING 将为输出操作而打开的 之前就存在的文件的长度减少到0

### 使用NIO:
为基于通道的IO使用NIO
为基于流的IO使用NIO
为路径和文件系统操作使用NIO
读写的一般流程:
获取用于封装目标的Path对象(事先要判读是否存在这个文件),然后获取Channel(也就是建立目标的连接),创建Buffer用于读写(在通道中调用map()方法可以将通道映射到缓冲区))
p.s. 往缓冲区中put东西会导致指针向后移动,使用write把缓冲区的内容写进通道之前记得rewind或者flip把指针重置到开头.

### 使用NIO复制文件:
使用Files中的copy方法

### 基于流的IO使用NIO:
NIO.2开始可以使用NIO打开IO流.
使用Files中的newInputStream和newoutputStream方法可以将打开Path指定的文件到输入或输出流中.
DirectoryStream<Path> newDirectStream获取目录流 DirectoryStream实现了Iterable<Path> 接口 所以可以使用foreach迭代(不过DirectoryStream的每个实例只能执行一次foreach循环)
newDirectStream的一个重载版本可以指定过滤模式的String 通配符\*指定匹配0个或者多个任意字符,?通配符指定匹配任意一个字符. [chars]指定匹配chars中的任意一个字符,chars中的*或?被看作常规字符而不是通配字符.还可以使用-指定范围  e.g 
使用Files中的Path walkFileTree(Path root,FleVisitor<? extends Path> fv) 列出目录树
FleVisitor接口中定义的方法:(T是Path或其子类) (SimpleFileVisior简单的实现了该接口)
postVisitDirectory(T fir,IOException exc) 在访问目录之后调用.目录被传递给dir,任何IOException异常都会被传递给exc.如果exc为null表示没有任何错误.返回结果
preVisitDirectory(T dir,BasicFileAttribute attrubs) 在访问目录之前调用.为了继续检查目录,返回FileVIsitResult.CONTINUE
visitFile(T file,BasicFileAttributes attribs) 当访问文件时调用.
visitFileFailed(T file,IOException exc) 当尝试访问文件失败时调用.访问失败的文件由file传递,IOException由exc传递.结果被返回
上面的每个方法都返回FileVisitResult枚举对象:CONTINUE SKIP_SIBLINGS SKIP_SUBTREE TREMINATE
为了遍历目录和子目录,方法应当返回CONTINUE
对于 preVisitDirectory,为了绕过目录及其兄弟目录并阻止调用postVisitDirectory,会返回 SKIP\_SIBLINGS,为了只绕过目录及其子目录,返回SKIP_SUBTREE
为了停止目录遍历 返回TREMINATE





## 联网


只讨论java.net中的核心类和接口.
支持Java联网功能的核心是套接字(又称伯克利套接字):用于识别网络上的端点.通过套接字,一台计算机可以同时为许多不同的客户端提供服务,也能为许多不同类型的信息提供服务.
也就是使用端口完成的,端口是特定计算机上具有编号的套接字.服务器进程监听端口,直到客户端连接到端口.服务器允许同一个端口号接受多个客户端连接,不过每次会话都是唯一的.
为了管理多个客户端连接,服务器进程必须是多线程的,或者具有一些其他多路复用同步的IO方法.

套接字通信是通过协议进行的.IP(Internet Protocol)协议是低级的路由协议,可以将数据分隔到许多小的数据包中,并通过网络将他们发送到某个地址,但不能保证所有包都能发生到目的地.
TCP(Transmission Control Protocol 传输控制协议) 是一个更高级的路由协议,该协议负责将这些数据包健壮得串联到一起,并且为了可靠的传输数据,该协议对数据包进行排序和重新传输.
UDP(User Datagram Protocol 用户数据协议) 位于TCP协议的上层,用于支持快速的 无连接的 不可靠的数据包传输.

一旦建立连接,一个更高层的协议就随之而来:TCP/IP 保留较低的1024个端口用于特定协议(e.g. 端口号21用于FTP 23用于Telnet 25用于e-mail 43用于whois 80 用于HTTP 119用于netnews) 客户端和端口之间的交互方式是由每个协议决定的.
e.g.HTTP协议是web浏览器和服务器用于传输超文本页面和图像的协议.工作原理:当客户端从HTTP服务器请求文件时(也就是点击某个链接时),它简单的以特定的格式将文件名发生到预先定义的端口,读取并返回文件的内容.服务器还通过状态代码进行响应,高数客户端是否能够满足请求及其原因.

Internet 关键部分是地址.在Internet上,每台设备都有一个地址(IP地址).地址的类型:IPv4(4个8位数值) IPv6(8个16位块) Java会自动处理用IPv4还是用IPv6这样的细节.
Internet地址的名称叫域名 www.demo.com 表示为位于COM顶级域中,名为demo,并且www表示位于web请求的服务器.Internet域名通过DNS(Domain Naming Service 域名服务)映射到IP地址.

Java通过扩展已经建立的流IO接口,并通过添加在网络上构建IO对象所需要的特性来支持TCP/IP.Java支持TCP和UDP协议族.
TCP用于网络上可靠的基于流的IO,UDP为更快的,支持点对点的面向数据报的模型.

#### InetAddress类
用于封装数字IP地址及对应的域名.可以同时处理IPv4和IPv6地址.

InetAddress类没有可用的构造器.需要是使用工厂方法创建InetAddress对象.
InetAddress类中提供了工厂方法 getLocalHost getByName getAllByName(用于那些单个名称表示多台机器的情况) getByAddress(IPv4和IPv6都行) 创建对象.

Inet4Address和Inet6Address类(InetAddress的子类)
一般都简单的使用InetAddress

### TCP/IP客户端套接字
TCP/IP套接字用于在Internet主机主机之间实现可靠的 双向的 持续的 点对点的 基于流的连接.可以使用套接字将Java的IO系统连接到其他程序,这些程序可能位于本地主机或Internet的任何其他机器上.
p.s. applet只可以建立到主机(从该主机可以下载applet)的套接字连接.因为让通过防火墙加载的applet访问任意一台机器是危险的.
Java中有两种TCP套接字:
ServerSocket类用于服务器,被设计成坚挺着,等待客户端进行连接,在这之前什么也不做.
Socket类用于客户端,被设计成用于连接到服务器套接字并发起协议交换.

创建Socket对象会隐式的建立客户端与服务器之间的连接.
getPort()返回调用对象连接到的远程端口.如果没有连接就返回0
getLocalPort() 返回绑定到调用对象的本地端口.如果没有连接就返回-1
isBound() 如果套接字被绑定到某个地址就返回true
Socket还实现了AutoClosable接口,可以使用try-resource

### URL类
现在的Internet上流行的不是上面的这些例如whois FTP之类的老式协议,而是WWW.
Web是高层协议和文件格式的松散集合,全部统一于Web浏览器中.URL(Uniform Resource Locator 统一资源定位器) 用于定位网络上的所有资源.
.e.g http://www.demo.com:80/index.html 第一部分是协议(e.g. HTTP FTP file等) 第二部分是主机名或IP地址(e.g. www.demo.com) 第三部分为端口号(HTTP默认的端口就是80) 第四部分是实际的路径(也就是HTTP服务器中的资源的路径,大多数的HTTP服务器会为直接引用目录资源的URL追加名为index.html之类的文件)
URL为唯一标识或寻址Internet上的信息,提供了一种相当容易理解的形式.
URL类为使用URL访问Internet上的资源提供了一套方法.
URL的构造器支持使用整个URL或者将URL的四个部分拆开来创建对象.也允许使用已有的URL作为参考上下文,然后根据上下文创建新的URL
URL中没有设置端口的话,getProtocol会返回-1

为了访问URL对象的实际位或内容信息,可以使用openConnection() 创建URLConnection对象

### URLConnextion类
用于访问远程资源属性的通用类.一旦构造一个到远程服务器的连接就可以使用URLConnection对象在实际传送远程对象到本地之前,检查远程对象的属性.这些属性由HTTP协议规范提供.并且只对HTTP协议的URL对象有意义.
getContentLength() 返回与资源关联的字节大小.如果长度不可得就返回-1
getContentType() 返回在资源中找到的内容的类型,也就是content-type标题字段的值.如果资源不可得就返回null
getDate和getExpiration 返回响应的时间和资源终止的时间和日期(如果有效日期不可的就返回0)
getHeaderFields可以获取包含所有标题字段和值的映射
getInputStream() 返回链接到资源的InputStream对象.可以使用这个流获取资源的对象.

HttpURLConnection类(URLConnection的子类)
同来支持http连接
getRequestMethod() 返回一个字符串,表示生成URL请求的方式.默认是GET也可能是其他方式(e.g. POST)
getResponseCode 返回http响应代码 如果不能得到响应代码就返回-1
getResponseMessage 返回与响应代码关联的响应消息
FollowRedirects 自动重定向

### URI类
URI(Uniform Resource Identifier 统一资源标识符) 实际上 URL是URI的一个子集.
URI代表定位资源的一种标准方式.URL还描述了如何访问资源.

### cookie
在java.net包中包含了帮助管理cookie的类和接口,并且可以用于创建有状态(与之对应的是无状态的)的HTTP会话.
相关的类:CookieHandler CookieManager HttpCookie
接口:CookiePolicy CookieStore 
这里不展开了

### TCP/IP服务器套接字
ServerSocket类用于创建服务器,在发布的端口上监听与之连接的本地或远程客户端程序.
创建ServerSocket对象时,它会在系统中注册自身,表明对客户端连接有兴趣.
ServerSocket类的构造器反映了希望接收连接的端口号,并且(可选)反映了希望端口使用的队列长度.队列长度告诉系统:在简单的拒绝连接之前,可以保留多少个等待连接的客户端连接.默认长度是50.在不利条件下 构造器可能会抛出IOException异常.
在多宿主机上,其中一个构造器可以接收InetAddress参数指定绑定了套接字绑定的IP地址.
accept() 是一个等待客户端发起通信的堵塞调用,然后返回一个常规的Socket对象,该Socket对象用于与客户端进行通信.

### 数据报
TCP/IP 作为提供序列化的 可预测的 可靠的 包形式的数据流,这些优点是有代价的:TCP为处理拥挤网络上的拥塞控制以及数据丢失的悲观预期提供了许多复杂的算法,这一定程度上降低了数据的传输效率.数据包提供了传输数据的另外一种方式.
数据报是在两台机器之间传递的信息包.而且接收到数据报时,既不能保证数据在传输过程中没有被损坏,也不能保证发送者仍然在等待接收响应.
Java通过在UDP协议之上实现数据报:DatagramPacket 对象是数据封装器,datagramSocket对象是用于发生和接收DatagramPacket对象的机制.
DatagramSocket类(实现了AutoClosable接口)
默认构造器创建绑定到本地计算机上任意未使用端口的DatagramSocket对象.
还有一个构造器接受SocketAddress(抽象类,InetSocketAddress实现了这个抽象类,InetSocketAddress将IP地址与端口号封装在一起)作为参数.
这些构造器都会抛出SocketException异常.
void send(DatagramPacket packet) 
void receive(DatagramPacket packet) 
分别向packet指定的端口发送数据包和等待从packet指定的端口接收数据包并返回结果.
setSoTimeout(int millis) 将超时时间设置成millis传递的毫秒数
getLocalPort 返回本地端口号
getPort 返回套接字连接的端口号 如果没有就返回-1
isBound 返回套接字是否被绑定到某个地址

DatagramPacket
没有含有InetAddress参数的构造器用于创建接收数据到byte[]参数指定的缓冲区的DatagramSocket对象.
含有InetAddress参数的构造器用于创建将byte[]中指定的数据发送到参数指定的地址和端口的DatagramSocket
InetAddress getAddress() (为将要接收的数据报)返回源的地址,或(为将要发送的数据报)返回目的地的地址.
byte[] getData() 返回在数据报中包含的数据的字节数组.通常同于在接收数据报之后,从数据报中接收数据.
int getLength() 返回字节数组(由getData()返回)中包含的有效数据的长度,可能不等于整个字节数组的长度.
setLength(int size) 将包的长度设置为size



## JavaFX




### 顶层结构


高性能图形引擎:Prism
高效窗口系统:Glass
多媒体引擎
Web引擎

Scene Graph i.e javafx.scene包
~就是一个由node(Visual Element)构成的分层的树.他能处理输出和被渲染
编写JavaFX Application从Scene Graph开始.
在Scene Graph中单个的元素称为node,每个node都有ID,style class和bounding volume(边界体积?)
除了root这个node之外的node都有且只有一个parent和0个或者多个children,他们还有特效(Effects) 不透明度(Opacity) 转换(Transforms) 事件处理(Event handles e.g. mouse key and input method) 应用程序特定的状态(An application-specific state) 
JavaFx还有图形基元(graphics primitives e.g. text和rectangles) 还有 controls 布局容器(layout contains) 图片 媒体
javafx.scene包中的API可以创建三种类型的内容:
Nodes (2-D 3-D Shapes images media embedded web browser text   UI controls   constainers …)
State (Transforms(node的节点和反向) visual effects 其他视觉状态)
Effects 用于改变node的外观的简单对象 e.g. 模糊(blurs) 阴影 颜色调整

JavaFX的一些public APIs
位于顶层的APIs为富客户端(rich client application)提供了丰富的API

JavaFX的Graphics System为硬件图形渲染不足的时候提供高效的软件渲染.
i.e.这两个图形加速管道(Graphics accelerateed pipelines):Prism(可以运行在硬件(DX和OpenGL)或者软件渲染器(renderers)上面,负责光栅化(rasterzation)和渲染JavaFX scenes)和Quantum Tookit(用于将Prism和Glass Windowing Toolkit捆绑在一起以支持他们的上层结构(就是上面那张图),还要管理与渲染和事件处理有关的线程规则)

### Glass Windowing Toolkit
JavaFX 图形栈的最底层.主要负责提供本地操作服务 (e.g. 窗口 计时器 本机操作系统有关的连接)
管理事件队列(使用本地系统事件队列功能来管理进程使用),和JavaFX应用程序运行在一个thread中. 

### Threads
整个JavaFX系统在给定的时间两个或者两个以上的以下的线程：
JavaFX Application Thread 
这是开发者使用的主线程，所有在window中还活着的scene必须从这个线程中访问。
一个scene可以被创造并附属在一个后台线程中，但是当他的root node连接到scene
中任何的对象时，这个scene必须从JavaFX Application Thread中被访问
这使得开发者能够在后台线程中创建复杂的场景图，同事保持实时场景的动画流畅和
快速。
Prism render thread
从事件调度中渲染.eg 它允许frame(框架) N在渲染的时候创建frame N+1
Media Thread
在后台运行,从在JavaFX Application Thread中使用的scene graph同步最新的frames
Pluse
他是一个表明了JavaFX scene graph是对Prism上的scene graph的时间同步的事件.
Pluse长度最大60fps,在animations(动画)运行在scene graph上的时候被发射(fire)
就算没有动画运行,在scene graph改变的时候也会指定一个pluse(脉冲)
当一个pluse被fire时,scene graph上的元素的状态会被同步到渲染层中.
pluse 的使用对于dev来说是处理异步事件的一种方式.
这个功能允许系统在pluse上批处理和执行事件.
Layout和CSS也捆绑到了pluse事件中.很多的scene graph变化会导致多个layout或者
CSS更新,这可能会严重影响性能.系统自动通过一次pluse执行一个layout和CSS通道
(pass)以避免降低效率.dev可以根据需要手动触发(trigger)布局通道以便在pluse之前
进行测(measurements)量.
Glass windowing toolkit负责执行pluse事件.他使用高分辨率的本地定时器来执行.

Media and Images
通过java.scene.media中的APIs实现

Web Component
基于WebKit的JavaFX UI控件,提供Web视图和完整的browser.

CSS
JavaFX CSS提供能在不改变任何代码的情况下对用户界面应用个性化的风格
所有的JavaFX属性名称以"-fx-"这个vendor 前缀(prefix)开头,就算是兼容标准
HTML CSS,因为兼容的一些JavaFX值跟标准HTML CSS相比还是有轻微的改动.

### UI控件
在javafx.scene.control包中
这些控件都是scene graph中的node.

### Layout
布局容器或者窗格用于允许在scene graph上实现UI控件的灵活 动态的布局.
Layout有关的API包括几个自动通用布局模型的容器类(container class):
BorderPane 确定其node的位置(上下左右或者居中)
HBox 组织nodes位于水平的一行中
VBox 组织nodes位于竖直的一列中
StackPane 将nodes放在back-to-front single stack中(最先创建的node放在最前面,
一直往正后方叠加)
GridPane 允许dev创建一个灵活的网格(grid)中的行和列展示nodes中的内容
FlowPane 将nodes按照垂直或者水平排列包装在指定的高度或者垂直界限.水平流式布局:node会逐行放置,必要时会换行.
TilePane 将nodes放在大小均匀的cells或者tiles(瓷贴?)中
AnchorPane 使dev能够创建anchor(锚)node在顶部 底部 左侧 右侧 或布局的中心

2D和3D转换(Transformation)
每个node都可以使用x-y坐标(coordinate)转换
使用javafx.scene.tranform 中的类:
translate 移动一个node按照相对于初始位置的x y z的改变量.
scale 按照比例因子(scale factor)放大或者缩小node的大小(x y z)
shear 旋转一个轴(axis) 使x和y轴不再垂直.node的坐标由指定的乘法器
(multipliers)移动
rotate 围绕scene中指定的点旋转node
addine 执行从一个2D/3D坐标(coordinate)到另外一个2D/3D坐标(coordinate)的
线性映射(linear mapping)同时保持直线 平行线的属性.这个class应该和Translate Scale
Rotate Shear这些转换classes同时使用

Visual Effect
好看..
这些特效主要是基于图像像素的,所以所以他们将scene graph中的一组nodes作为image渲染
然后将特效应用在上面
相关的类:
Drop Shadow 将一个给定内容的阴影应用于给定的内容上
Reflection 在给定的内容下面渲染这个内容的反射版本
Lighting 模拟光源照射在给定的内容上,可以使一个平面物体更加的逼真和3D外观

所有的JavaFX App的主class实现javafx.application.Application抽象类(要实现abstract public void start(javafx.stage.Stage primaryStage))
Application类中还有init()和stop().按照init(默认的版本是空的,主要用于执行各种初始化,不能用于创建stage和构建scene,因为init和应用程序的构造器实在主线程(也叫启动线程)上调用,而start()在JavaFX Application Thread中调用) -> start -> stop(关闭应用程序时会调用,在JavaFX Application Thread中调用)

### 启动JavaFX App

为了运行一个独立的JavaFX App,必须调用Application类定义的public static void launch(String … args)方法
调用launch会开始构造应用程序,之后调用init和start 知道应用程序终止,launch方法才返回.
p.s. 对于使用javafxpackager工具(或者IDE中类似的打包工具)打包的JavaFX App不需要包含对launch的调用.包含launch为了能简化测试和调试.

Stage是顶级容器,所有的JavaFX App都自动能够访问一个Stage(也就是start方法的那个primaryStage参数,这个叫主舞台),还可以创建其他舞台.
至少需要在Stage里面添加一个Scene(javafx.scene包下面的)
Scene是由Node构成的分层的树.(根节点 父节点 子节点 叶节点),scene中的所有node的集合创建出scene graph
所有的node的基类是Node,Parent Group Region Control都是Node的派生类.
p.s. Scene(Parent root,……..) 所有的Layout都是Parent的子类.
一些方法:setScene(Scene myScene) 将myScene设置为该Stage的scene show()显示stage创建的窗口和屏幕

### 布局

JavaFX提供了几个布局窗格,位于javafx.scene.layout包中
Layout
布局容器或者窗格用于允许在scene graph上实现UI控件的灵活 动态的布局.
Layout有关的API包括几个自动通用布局模型的容器类(container class):
BoarderPane 确定其node的位置(上下左右或者居中)
HBox 组织nodes位于水平的一行中
VBox 组织nodes位于竖直的一列中
StackPane 将nodes放在back-to-front single stack中(最先创建的node放在最前面,
一直往正后方叠加)
GridPane 允许dev创建一个灵活的网格(grid)中的行和列展示nodes中的内容
FlowPane 将nodes按照垂直或者水平排列包装在指定的高度或者垂直界限.水平流式布局:node会逐行放置,必要时会换行.
TilePane 将nodes放在大小均匀的cells或者tiles(瓷贴?)中
AnchorPane 使dev能够创建anchor(锚)node在顶部 底部 左侧 右侧 或布局的中心
p.s经常用rootNode.getChildren().add(Node node)将node添加到rootNode的子节点列表中.getChildren()返回ObservableList<Node>
可以使用ObservableList中的addAll方法将多个子节点添加到scene graph中,用remove (Node node)从scene graph中删除控件.

### 事件

javaFX事件的基类是javafx.event包中的Event类,Event类继承了java.util.EventObject.
Event类有几个子类:ActionEvent处理按钮产生的动作事件.
JavaFX为时间处理实质上使用了委托事件模型方法,为处理时间,首先必须注册处理程序,作为时间的监听器.时间发生时,会调用监听器.监听器必须响应事件,然后返回.
事件是通过实现EventHandler接口(泛型函数式接口interface EventHandler<T extends Event>该接口定义了handle(T eventObj) 方法)处理的,该接口也在javafx.event包中.
使用一个处理程序处理来自不同源的事件时,通过调用继承自java.util.EventObject的Object getSource()方法可以获得事件的源.
Event类的其他方法允许获得时间类型 确定事件是否已被消费 消费事件 引发事件 以及获得事件的目标.事件被消费之后,就不会被传递给父处理程序.
在JavaFX中,事件沿着事件分发链处理的,产生事件时,事件被传递给分发链的根节点,然后事件沿着分发链向下传递给事件的目标.目标节点处理完事件后,把时间沿着分发链向上回传,从而给父节点提供了必要时处理时间的机会.这被叫做事件冒泡.链中的节点可以消费时间,这会阻止事件被进一步处理.
p.s. 使用javaFX的App还可以通过实现事件管理器来管理事件,通过调用Node类定义的addEventFilter方法可以向节点添加事件管理器.事件管理器可以消费事件从而阻止事件进一步处理.


### UI控件

在javafx.scene.control包中
这些控件都是scene graph中的node.
主要的控件:
Label 标签
Button 按钮 构造器中的String参数是按钮中显式的消息,按下按钮时,会产生ActionEvent事件.ActionEvent包含在javafx,event包中,通过使用Button 中的final void setOnAction(EventHandler<ActionEvent> handler) (handler一般用匿名类实现或者lambda)可以为此事件注册监听器.
p.s.与所有java事件处理一样,处理程序必须尽快响应时间,否则会显著降低App的效率.对于耗时的操作,必须使用单独的执行线程.

直接在画布上绘制:
JavaFX自动处理渲染任务(e.g.重新绘制窗口),而不需要手动处理.
JavaFX的显示图形对象的方法包含在java.scene.canvas包的GraphicsContext类(不是Node)中.这些方法可以直接在画布上绘制.画布(Node的子类)由java.scene.canvas包中的Canvas类封装.
首先new一个Canvas画布的对象,然后获得引用该画布的GraphicsContext对象(e.g. GraphicsContext getGraphicsContext2D())
GraphicsContext中的方法:
strokeLine() 线段
strokeRect() 矩形 topX topY为左上角的坐标 下同
fillRect() 填充绘制矩形
strokeOval() 绘制椭圆
fillOval() 填充绘制椭圆
strokeText()
fillText()
setFont() 可以设置显示文本的字体和字体大小
getFont() 获得画布使用的字体 默认情况下使用系统字体 可以使用javafx.scene.text包中的Font对象构造新的字体.
setFill(Paint newFill) setStroke(Paint newStroke) 指定填充和笔触 Paint是javafx.scene.paint包中的抽象类,它的子类定义了填充和笔触.e.g. Coloe 简单的描述了一种纯色,其中定义了几个静态域变量 e.g.Color.BULE Color.RED Color.GREEN

javafx.scene.shape包含几个也可以用来绘制各种图形形状的类,e.g.圆形 弧线 直线 他们由node表示,所有可以直接称为scene graph的一部分.

探究JavaFX控件
所有的控件都在javafx.scene.control包中
Image和ImageView
JavaFX的很多控件都允许包含图片 e.g. Label和Button
也可以在scene graph中直接嵌入独立的图片
Image(String url)url为指定的URL或者图片文件的路径(相对路径或者绝对路径).
其他的构造器允许指定像图片的宽度和高度之类的选项. Image没有继承Node,所以不能直接作为scene graph的一部分.
有了Image对象后,就可以使用ImageView来显示它,ImageView直接派生自Node,但是不能直接用作rootNode(Scene的构造器接收到是Parent(Node的一个派生类)作为参数).

向标签添加图片:
Label(String str,Node image) 默认是图片在文本的左边 可以通过setContentDisplay(ContentDisplay position)改变图片和文本的相对位置(使图片在文本的其他方向上).(ContentDisplay 是一个枚举)
Button(String , Node image)
经常需要用this.getClass().getResource("FileName").toExternalForm()来返回源代码所属的目录里面的资源的url.

ToggleButton
开关按钮,有两个状态:按下和释放

ToggleGroup 
可以创建按钮组.
适用于ToggleButton及其子类,也就是一个组只能选中一个.
可以使用selectedToggleProperty().addListener(ChangeListener ) 来对ToggleGroup注册变化事件监听器.

RadioButton
单选按钮.扩展了ButtonBase和ToggleButton
实现了通过setToggleGroup(ToggleGroup tg) 可以将单选按钮添加到开关按钮组中.
fire() 如果按钮之前未被选中,这个方法为将为产生动作事件.(用于按钮组设置默认的选择的按钮)

Separator 创建一条水平或者垂直线(分割线)
setPreWidth方法可以设置宽度值

CheckBox 复选框
直接超类是ButtonBase
JavaFX的复选框支持三种状态:选中 未选中 不确定(又称未定义,默认不开启.需要显示启用该状态)
setAllowIndeterminate() 添加不确定状态

ListView
列表视图,显示选项列表的控件,用户可以从中选择一个或者多个选项.
这是一个泛型类, class ListView<T> T指定了列表视图中存储的选项的类型.通常 这些选项的类型是String
默认构造器用于创建空的ListView
构造器ListView(ObservableList<T> list)允许指定列表中的选项,list定义了一个可观察对象的列表(ObservableList继承自java.util.List,并且位于javafx.colections包中)
要创建ObservableList可以使用FXCollections类中的工厂方法static <E> ObservableList<E> observableArrayList(E … elements)
默认情况下ListView只允许在列表中选择一项(可以通过getSelectionModel().setSelectionMode(SelectionMode.MULTIPILE)修改选择模式,允许多项选择),启用多选模式之后,可以通过两种方式选择选项列表:作为选中索引的列表(索引从0开始),或者作为选中选项的列表.为了获得选中选项的列表,通过对选择模式调用ObservableList<T> getSelectionItems方法
可以通过setPrefHeight() setPrefWidth() 或者setSize 这是优先选择的高度 宽度
使用ListView:
	1. 忽略列表产生的事件,而是在程序需要的时候获得列表中的选择项
	2. 注册变化监听器,监听列表中的变化:对列表调用MultipleSelectionModel<T> getSelectionModel() 返回对该ListView使用的选择模式的引用,然后对该选择模式使用readOnlyObjectProperty<T> selectedItemProperty() 获得选中项属性的引用,对属性定义了选中列表中的元素时将发生什么.通过addListener(ChangeListener)把变化监听器添加给这个属性就可以实现变化事件监听.
ListView的滚动条:当列表中的选项数超过其大小所能显示的选项数时就会自动添加滚动条.

ComboBox
组合框是与列表视图相关的控件.继承了ComboBoxBase.而且值允许单项选择.同样是泛型类.
T getValue() 返回当前选项,如果未被(用户或者程序)选择,那么返回null
setValue(T newValue)设置新选中选项
可以通过setEditable(true) 允许用户编辑选项

ChoiceBox 与ListView和ComboBox有相似之处而且使用起来更加方便

TextField
允许用户输出一行文本.继承了TextInputControl
TextField的一个构造器允许指定文本框的初始内容
setPrefColumnCount(int) 来指定文本框的大小
setPromptText() 在用户需要输入但是尚未输入的空文本框中设置一条提示消息.

TextArea
支持多行文本

PasswordField
用于输入密码

ScrollPane
有时候,控件的内容会超出控件指定的屏幕空间.
通过ScrollPane封装节点即可为任何Node添加滚动功能.
Viewport(视口)是滚动窗格的可视区域,被滚动的内容将在这个区域显示.
可以通过setPrefViewportHeight设置Viewport的尺寸
setPannable(true) 可以允许通过拖动鼠标平移内容(默认是关闭的)
通过setHvalue和setVvalue可以设置滚动条的位置,默认情况下滚动条的位置都是0

TreeView
以树状格式显示数据的分层视图
当树大小超过视图的尺寸时 就会自动提供滚动条
这是个泛型类 泛型类型T指定树中的条目保存的值的类型,一般都是String
构造器TreeView(TreeItem<T> rootNode) 中的TreeItem只能用在TreeView中不能作为独立控件使用.
处理TreeView中选择事件的方式与处理ListView中的选择事件类型,都是通过变化事件监听器完成.(TreeView.getSelectionModel().selectedItemProperty().addListener(…))
getVaule方法可获得TreeItem的值.getParent()可获得某个节点的父节点.getChildren()

效果和变换简介
效果由javafx.scene.effect包中的Effects类及其子类支持.
JavaFX内置效果:
Bloom 增加Node中较亮部分的亮度
BoxBlur 让Node变模糊
DropShadow 让Node后面显示阴影
Glow(double glowLevel) 产生发光效果 glowLevel在0.0到1.0之间 指定光的亮度
InnerShadow (double radius, Color shadowColor) 在Node内显示阴影 radius指定Node内阴影的半径
Lighting 创建光源的阴影效果
Reflection 显示倒影

所有的Node都可以通过setEffect(Effect)设置Node的效果,指定null为参数表示不使用特效

变换
由javafx.scene.transform包中的抽象类Tranform支持
Tranform有4个子类:Rotate Scale Shear Translate  Affine
调用Node定义的ObservableList<Transform> getTransforms() 可获得该变换列表
要添加变换 只需调用add方法把它添加到这个列表中 调用clear()可清楚此列表
也可以设置Node的某个属性直接指定变换: setRotate() 可以设置Node旋转的角度 setScaleX()和setScaleY() 可以设置缩放 setTranslateX() 和setTranslateY() 方法可以平移Node(具体平台也可能支持Z轴平移)

Rotate(double angle, double x, double y) 设置轴点和旋转角度 也可以调用setAngle() setPivotX() setPivotY() 设置

Scale(double widthFactor, double heightFactor) 指定对Node宽度和高度应用的缩放因子. 也建议使用setX和setY在创建对象之后设置这两个因子.

添加工具提示:
当鼠标停留在某个控件中时会显示出来一条短消息,这就是工具提示
调只需用Control中定义的setTooltip(Tooltip tip)就可以设置工具提示,Tooltip封装了工具提示.


HTMLEditor

TitledPane 常常和Accordion一起用组成TitledPane Group,而且每次只显示其中的一个

Hyperlink

Slider

File Chosser

Color Chooser

ProgressBar

ProgressIndicator


WebView WebEngine

Pagination

禁用控件:
scene graph中的任何Node 包括控件 都可以被程序禁用.
调用Node定义的setDisable(boolean)可以设置


响应按下回车或者其他指定的按键的动作:
setOnKeyPressed() 这个函数式接口参数中的方法的参数为KeyEvent,可以用KeyEvent.getCode().equals(keyCode.ENTER) //KeyCode为javafx.scene.input包中的类


这些方法的height和weight的单位都是像素.
setAlignment方法中的Pos参数,用Pos中的静态域作为参数
setFill方法接收Color参数,用Color中的静态域作为参数

JavaFX菜单简介
JavaFX的菜单系统支持几个关键元素:
菜单栏: 应用程序的主菜单
 标准菜单: 可以包含可选择的菜单项,也可以包含其他菜单(子菜单)
上下文菜单: 通常通过右键鼠标激活,又叫弹出菜单
JavaFX的菜单也支持加速符和助记符.使用加速键时 不必激活菜单就可以选择菜单项.使用助记符时 可在菜单显示的情况下,使用键盘选择菜单.除了普通菜单之外JavaFX还支持工具栏(ToolBar类)

javaFX的菜单系统由javafx.scene.control包中的一些列相关类支持:
核心菜单类:
CheckMenuItem 复选菜单项(继承自MenuItem)
Context![enter description here][2]Menu 弹出菜单,通常通过右键鼠标激活
Menu 标准菜单 由一个或多个MenuItem组成
MenuBar 保存应用程序的顶级菜单的对象
MenuItem 填充菜单的对象 
RadioMenuItem 单选菜单项(继承自MenuItem)
SeparatorMenuItem 菜单之间的可视分隔符 (继承自CustomMenuItem)

P.S MenuItem没有继承Node,所以MenuItem的实例只能用在菜单项中,而不能直接加入scene graph中
而且 MenuItem是Menu的超类,所以可以创建子菜单.
要创建子菜单,首先创建一个Menu对象并使用MenuItem填充它,然后把它添加在另一个Menu对象中.
选择菜单项后,会产生动作事件.与所选选项关联的文本将成为这次选择的名称.也可以使用单独的匿名类或者lambda表达式来处理每个菜单项的动作事件.

MenuBar本质上是菜单的容器, 它为应用程序提供主菜单的控件.它继承了Node类
MenuBar只有一个默认构造器, 所以一开始菜单栏为空, 在使用之前需要在其中填充菜单, 一般来说一个app有且只有一个菜单栏.
通常只使用MenuBar中的ObservableList<Menu> getMenus() 获得由菜单栏管理的菜单列表.向这个列表使用add()可以把Menu实例添加到这个菜单列表中.add(int index, Menu menu)中的index从0开始,0表示最左边的菜单

Menu 封装了菜单, 菜单用MenuItem填充.派生自MenuItem

