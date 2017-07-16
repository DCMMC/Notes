---
title: Java 学习笔记
tags: Java,入门,笔记
grammar_cjkRuby: true
---

# 0x00 绪论

## OOP三原则：
* 封装（encapsulation） 
* 继承（inheritance） 
* 多态（polymorphism）

p.s. 可以用代码块替代statement
Java基本元素：空白符（而且Java是一种格式自由的语言）、标识符（Java大小写敏感，SE8不推荐用 `_` 作为标识符）、字面值（literal）、注释（Java有文档注释）、运算符（SE8引入 `::` 用于创建方法或构造函数引用）、分隔符以及关键字（Java保留 `const` 和 `goto` 关键字但没有用）

## 数据结构：
Java是强类型化的语言（和C一样）。
基本数据类型：
* byte（8bits，在使用网络或文件的数据流时很有用）
* short（16bits，最不常用）
* int（32bits，最常用） 
* long（64bits 字面量通常用后缀L或l）
* char（16bits unicode码） float（32bits 字面量后缀f或F） 
* double（64bits，适用于保持精度的运算，最常用 字面量后缀D或d） 
* boolean （true false并没有对应的数值，而且0不代表假 非0不代表真 和C/C++不一样）

> p.s. Java的所有类型都是有符号的

## 字面量（literal）
* 八进制：0前缀 
* 十六进制：0X或0x前缀  
* 二进制（SE7）：0b或0B前缀 

JDK7开始，可在数字字面量中嵌入下划线 `_`增强大数字的可阅读性，编译时编译器会自动去掉。e.g. int x = 123_456\_\_789;

字符型字面量：转义字符：\ddd （d为八进制数） \uxxxx（x为十六进数）

变量声明：Java支持动态声明。Java不支持方法内部声明的变量在该方法内部被内层定义覆盖（和C不一样），不过方法可以覆盖所属类中的域变量。

## 类型转换：
Java能自动执行扩展转换，窄化转换需要显式的使用指派操作符强制转换（可能会造成移除）。浮点型窄化转换会趋零截尾。
表达式中会发生自动类型提升，所有的byte short char自动提升到int，整数表达式的值都会自动提升到int  e.g. byte b = 50；b = b * 2；这时就会报错，必须（byte）强制转换。
含有float（double）的表达式的值会被提升到float（double）。

## 数组：（可以使用int[] arr也就是使用int arr[]的形式）
p.s. 数组类中的length成员字段只是反映数组最初设计时所能包含的元素数量。
可以手动分配某一维的每一个元素的大小，使同一维度的元素的长度不同，创建不规则数组。
数组初始化的时候可以使用表达式。

p.s. Java不支持（可以修改的）指针（因为在JVM中使用指针不安全嘛），不过可以调用native C++代码使用指针233333

## 运算符：
基本和C一样，多了>>>（按位右移0填充，而且只对32bit或64bit数值有意义，short byte char这些都会自动提升（也就是会发生符号扩展））instanceof和->（JDK8新增）运算符
p.s. 因为Java的表达式的自动提升，移位操作符用于byte short 时，要把结果强制转换一下。
右移>>后的最高位会使用右移前的最高最填充（符号扩展）。使用左移和右移操作符可以高效的实现乘以和除以2 。
高阶位1表示负数，0表示正数。
补码=反码（符号位不变）+1
Java使用2的补码存储负数。这样原码1000_0000表示的-0就能变成补码1_0000_0000超出了八位二进制的范围了，而且反码能够很好的计算含有负数的加法。
p.s. 为了减少迷糊性和提升优先级，多使用（）来明确优先级而不是记忆那些复杂的优先级表。

## 控制语句：
### 选择语句：
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

### 转跳语句：continue break return （异常其实也算）
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

## 类
对象 对象的引用
域（实例变量） 方法 构造器 重载（根据方法签名） this关键字
垃圾回收
finalize（）方法 只在对象即将被销毁时才执行，所以不能知道什么时候或者甚至会不会被调用。
调用方法时，值调用会创建值的副本作为实参传递给形参，引用调用直接将实参的引用传递给形参。
访问控制:public 包访问权限 protected private 
static ： 静态方法 静态变量
final ： final常量 final类 final参数 final类  
显式的静态实例初始化 非静态初始化 ： 非静态示例初始化--也是在所有方法（包括构造器）之前执行，显示静态初始化只会只会在第一次访问静态数据的时候初始化，而且不论代码的先后顺序，都在非静态示例初始化之前执行。

String类
重载的+   length（） charAt（） equals（） 
varargs可变长参数（SE5）：
用…表示，而且只能放在参数列表的最后一位。
方法重载与模糊性：

super关键字 this关键字

。。。。。
跳。。。。
。。。。。

## JDK8对接口的修改：
JDK8为接口新增了默认方法（在接口中为方法指定默认实现）的新功能：1.提供一种扩展接口的方法，而不破坏实现了这个接口的类的代码。2.在接口中指定本质上可选的方法，也就是如果接口的实现并没有覆盖这个接口作为占位符性质的方法，也没问题。（因为有些实现可以不需要接口中的某些方法）
在需要提供默认实现的方法前面加上default关键字。
不过接口还是不能使用实例变量，里面定义的所有变量都是final的。
p.s. 类实现的优先级永远高于接口的默认实现。如果一个类要同时实现两个包含相同的方法签名的接口，编译器会报错。
在子级接口中调用父级接口中的默认实现时，需要使用super关键字显式的调用：InterfaceName.super.methodName()
JDK8中接口还可以定义一个或多个静态方法,可以不用实现接口或接口的实例:InterfaceName.staticMethodName();

## 异常
Java的内置异常(也就是未经检查的异常):有点多…不写了…
链式异常(since 1.4):Throwable中有两个重载的构造器:Throwable(Throwable causeEXc) 和 Throwable(String msg,Throwable causeExc) //causeExc是引发当前异常的异常
getCause() 返回引发当前异常的异常,如果不存在就返回null.
initCause() 将causeExc和调用异常关联在一起,并返回对异常的引用.对于每个异常对象只能调用initCause方法一次.如果通过构造器设置了引发异常,那么就不能在使用initCause()方法进行设置了.这个方法主要是用于解决不支持链式调用的遗留异常类的设置问题.

JDK7对新增的异常特性:1.带资源的try语句 2.多重捕获(在一个catch子句的参数中使用|运算符把多个异常放在一起,且每个多重捕获参数都被隐式的声明为final) 3.更精确的重新抛出(只能抛出满足条件的异常:有关联的try代码块抛出,并没有被前面的catch子句处理,并且是参数的子类型或者超类型)


## 多线程编程:
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
### Thread类和Runnable接口:
Thread类中的方法:getName() setName() setPriority()设置优先级(优先级在MIN_PROIORITY到MAX_PRIORITY之间也就是1到10) getPriority()获取线程的优先级 isAlive() 确定线程是否仍在运行 join() 等待进程终结，否则一直运行该方法 run() 线程的入口点 static sleep() 挂起当前线程一段时间(sleep(long millis)单位ms sleep(long millis,int nanos)第二个参数为纳秒单位,挂起时间为millis+nanos) start() 通过调用线程的run()方法启动线程 static Thead currentThread()返回调用它的线程的引用 toString() 输出由线程名称 优先级 所属线程组构成的Steing[] main方法的优先级默认为5
主线程(程序启动时自动创建):其他子线程都是从主线程中产生的,主线程必须是最后才结束执行的线程,因为它要执行各种关闭动作.
创建线程:实现Runnable接口或者扩展Thread类
为了实现Runnable接口,类只需要实现public void run()方法,run方法为程序中另外一个并发线程的执行建立了入口点.当run方法返回的时候,这个线程就会结束.
Thread类的其他构造器:Thread(Runnable threadOb,String threadName) 新线程的名称由threadName决定法,在创建了新线程之后,只有调用线程的start方法,线程才会运行,本质上,start方法执行对run方法的调用
扩展Thread类：需要重载run方法 
两种方法都需要在构造其中调用start或者run方法
如果不需要对Thread类进行修改或增强的话 推荐使用实现Runnable接口的方法来创建线程.
### 线程优先级：
高优先级的线程会获得更多的cpu时间，具有高优先级的线程可能会取代低优先级的线程（e.g. 从休眠或者等待IO中恢复的高优先级线程会取代低优先级的线程）
理论上,具有相同优先级的线程应当得到相等的cpu时间,不过不同的环境上的jvm实现不一定相同,为了安全起见,具有相同优先级的线程应当是不是释放控制权.
如果线程依赖于抢占式行为,经常会引起相同优先级的线程的不一致性.
同步:
如果没有采取什么方法阻止多个线程在相同时间调用同一对象的同一方法,就是竞态条件,这些线程会互相竞争以完成方法.
当多个线程需要访问共享资源时,在给定时刻只有一个线程可以拥有监视器(用作互斥锁的对象),其他企图进入加锁监视器的线程都会被挂起,直到第一个线程退出监视器.
在方法前使用synchronzied关键字限定,使之成为同步方法,一旦线程进入一个实例的同步方法,所有其他线程就不能再进入相同实例的任何同步方法.
所有对象都有与自身关联的隐式监视器,为了进入对象的监视器,只需要调用使用synchronized关键字修饰过的方法.为了退出监视器并将对象的控制权交给下一个等待线程,监视器的拥有者只需简单得从同步方法返回.
synchronized语句:对于不能修改源代码的没有同步方法的类（也就是没有针对多线程设计的）,可以通过把方法调用放在synchronized(objRef) { /…}中，实现同步方法。synachronized代码块确保对objRef对象的成员方法调用是会在当前线程成功进去objRef的监视器之后发生。
### 线程间通信：
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

## 枚举(Java SE5)
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

## 自动包装器 (Java SE5) :
每个包装器类中都有对应的typeValue方法 e.g. int intValue()
而且所有包装器类都有重载的接收String类作为对象的构造器,如果String没有包含有效的数值,就会抛出NumberFormatException异常
p.s. 包装也叫装箱(封装器) 包装器类转成基本数据类型叫拆箱
不过装箱和拆箱效率比基本数据类型低很多,不要滥用

## 注解(元数据) (Java SE5):
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

### 指定保留策略:
保留策略决定在什么配置丢弃注解(他们被封装到java.lang.annotation.RetentionPolicy枚举中): SOURCE CLASS(p.s. 局部变量声明的注解不能存储在.class文件中) RUNTIME
保留策略通过Java内置注解@Retention指定: @Retention(retention-policy)  retention-policy只能是上面这三个枚举常量中的一个,如果没有为注解指定保留策略,将会使用默认策略CLASS
对Class Method Field Constructor 对象调用\<A extends Annotation> getAnnotation(Class\<A> annoType)方法(如果没找到注解 e.g.注解的保留策略不是RUNTIME 就会返回null) (要配合反射一起使用) 也可以用Annotation[] getAnnotations()获取所有Annotation
  JDK5以来，除了getAnnotation和getAnnotations 还有AnnotatedElement接口中定义的getDeclaredAnnotations返回调用对象中存在的所有非继承类注解，isAnnotationPresent（Class <? extends Annotation> annoType）返回annoType指定的注解与调用对象相关联就返回true 否则返回false
JDK8新增getDeclaredAnnotation和getAnnotationByType和getDeclaredAnnotation方法(后面两个方法自动使用重复注解)

### 使用默认值:
可以在成员声明后面添加default子句: e.g. @interface MyAnno { String str() default "Default";}

### 标记注解: e.g. @Override
标记注解也就是不包含任何成员,唯一的目的就是标记声明.可以用AnnotatedElement接口(Method Field Class Constructor类实现了该接口)定义的isAnnotationPresent()方法确定标记注解是否存在

### 单成员注解:
当注解中只包含一个成员,在使用注解的时候可以直接使用()设置成员的值 e.g @Retention(RetentionPolicy.RUNTIME) 就可以使用单值语法(只要没有默认值的成员只有一个的都可以用单值语法)
只要使用单成员注解,成员的名称就**必须**是**value**

### 内置注解:
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

### 重复注解:
JDK8新增能够在相同元素上重复引用注解的特性.
可重复注解必须用@Repeatable进行注解.其value域指定了重复注解的容器类型（Class<?extendsAnnotation> value（）；）,也就是重复注解类型的数组.要创建重复注解,需要创建容器注解,然后将注解的类型指定为@Pepeatable注解的参数.
为了使用getAnnotation方法访问重复注解,需要使用容器注解(就是那个用@Repeatable注解的那个单成员注解(只有一个需要重复注解的那个注解的数组作为返回值的成员方法))而不是重复注解.
获取重复注解的另一种方式是使用JDK8添加到AnnotatedElement中的新方法:
<T extends Annotation> T[] getAnnotationsByType(Class<T> annoType)和getDeclaredAnnotationByType()

一些限制:注解不能继承另外一个注解 注解声明的所有方法都必须不带参数 ,注解不能被泛型化.注解方法不能指定throws子句


I/O :
基于文本的控制台IO对于实际的Java编程确实用处不大.
流(java.io包定义的类层次中实现的 基于缓冲和基于通道的IO在java.nio及其子包中定义):
Java程序通过流执行IO,流通过Java的IO系统链接到物理设备.所有流的行为方式都是相同的.

字节流(@since 1.0)和字符流(@since 1.1):
在最底层,所有的IO仍然是面向字节的.
字节流通过两个类层次定义:InputStream 和 OutputStream (这两个都是顶层的抽象类)
java.io中的字节流:
Buffered(缓冲) ByteArray(字节数组) Data(Java标准数据类型) File(文件) Filter(同于实现InputStream和OutputStream) Object(对象) Piped(管道) 开头的InputStream和OutputStream (Input都是读取 Output都是写入)
PrintStream 包含print()和println()的输出流 PushbackInputStream (支持1字节"取消获取"输入流,这种流向输入流返回1字节) SequenceInputStream (由多个按顺序依次读取的输入流组合而成的输入流)
字符流类:
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

读写文件:
FileInputStream(String fileName) throws FileNotFoundException 文件不存在的时候就会抛出
FileOutputStream(String fileName) throws FileNotFoundException 不能打开文件或不能创建文件 就会抛出 打开输出文件时 先前存在的同名文件将被销毁
FileNotFoundException 属于IOException的子类
p.s. 对于存在安全管理器的情况(e.g. applet) 可能会抛出SecurityException

关闭文件:void close() throws IOException 关闭文件会释放为文件分配的系统资源 如果关闭文件失败会导致内存泄漏
在JDK7之前 当不需要文件时显示的调用close方法(FileInoutStream和FileOutputStream中都实现了) JDK7之后可以使用带资源的try语句
读取文件:可以使用FileInputStream中的int read() throws IOException  一个字节一个字节的读取 到达文件末尾时返回-1

JDK7新增自动资源管理（ARM） :  可用于自动关闭不再使用的资源,这样就不需要显式的调用close关闭资源了
try (resource-specification) {
	//use the resource
}
resource-specification是用来声明和初始化资源的语句 而且在resource-specification中声明的资源被隐式的声明为final 可以用分号分隔多个资源声明
p.s 至于哦那些实现了AutoCloseable接口的资源,才能使用带资源的try语句

applet:
java.awt 抽象窗口包
java.applet applet通过GUI框架与用户进行交互 主类需要继承自applet
applet每次必须重新绘制输出时都会调用paint(Graphics g)方法,所以必须在主类中覆盖该方法.

在IDEA中调试applet需要配置一下配置器,use the default one provided by IntelliJ in the application bin directory called appletviewer.policy
而且从JDK7开始,所有applet都需要签名才能在web浏览器中运行,比如在Java控制面板中调整安全设置.也可以直接用在cmd中使用appletviewer运行

applet程序不是从main入口处运行的,用户IO不是使用Java的IO流类完成的,而是使用GUI框架提供的接口.

transient和volatile修饰符:
实例变量声明为transient表明存储对象的时候实例变量的值将不需要永久保存.
volatile告诉编译器变量可以被程序的其他部分随意修改:在多线程程序中,有时候多个线程共享相同的变量时,处于效率反面的考虑,每个线程自身可以保存这种共享变量的私有副本.真正的变量副本(主变量副本在各个时间被更新,例如进入同步方法时)这样有时效率不高,为了确保变量的主副本总是反映自身的当前状态,可以将变量修改为volatile告诉编译器必须总是使用volatile变量的主副本(至少总是保持所有私有版本和最新的主副本一致) 此外,访问主变量的顺序必须和所有私有副本相同,以精确的顺序执行.

instanceof操作符: objref instencesof type (type是Class类型)

strictfp: Java 2之后 浮点计算模型稍微宽松了一点(e.g. 在计算期间新模型不需要截断特定的中间值) strictfp修饰类 接口 或方法 确保采用java 1.0的浮点计算模式 修饰接口和类相当于修饰了其中的所有成员方法.
不过这个关键字极少使用…

本地方法:
偶尔你想调用非Java语言编写的子例程.
Java提供了native关键字,用于声明本地代码方法.大部分本地代码都是用C语言编写的,将C代码集成到Java程序中的机制被称为Java本地接口(JNI).
声明为native的方法没有方法体,调用含有该方法体的本地动态链接库为方法提供方法体
使用System.loadLibrary(String filename)调用本地动态链接库(这句代码一般放在static代码块中)
本地代码的问题:安全性问题 可移植性丢失

assert:
assert condition; 或 assert condition:expr; expr是要传递给AssertionError构造器的值(字符串)
断言.用于证实在测试期间遇到了某些期望的条件,如果condition为false则抛出AssertionError异常
为了在运行时启用断言检查,必须指定-ea选项 可以用-da指定执行代码时禁用断言

静态导入:
import static 可以用于导入包中的静态方法.比如简写版的print方法.但是不能滥用,不仅Java讲类库组织包包中的目的就是为了避免命名空间的冲突.

用this(arg-list)调用指定的构造器,使用他们有助于减少代码重复量,有助于结构化代码.
不过this()的构造器相对于那些包含所有内联初始化代码的构造器来说,执行速度要慢一些.而且对于比较短的构造器,this()并不能节省加载时间,反而会增加在方法间转跳花费的时间.
所以this()适合于大量初始化代码的构造器.
p.s. 同一个构造器中不能同时使用super() 和 this() 因为它们都位于构造器的第一条语句.


紧凑API配置文件:(JDK8)
将API库的子集组织陈所谓的紧凑配置文件:compact1 compact2 compact3 每个配置文件以前一个配置文件为基础(e.g. compact3 包含整个compact2)
这样的优势在于:用不到完整库的应用程序不需要下载整个库,减小了库的大小,让一些Java应用程序能够在无法支持完整Java API的设备上运行.降低了加载程序的时间.
使用-profile选项指定是否只使用了紧凑配置文件中定义的API:javac -profile profilename Programname e.g. javac -profile compact2 Test.java


泛型：（JDK5）
JDK7开始，可以缩短创建泛型类的实例的语法：e.g.Gen<String> gen = new Gen<>(args); //<>被称为菱形运算符，告诉编译器要去推断
//感觉也就是SE5的那个自动类型判断嘛。。而且不是很智能的感觉。。辣鸡擦除特性。。

类型参数只使用引用类型，不能使用基本数据类型。
基于不同类型参数的泛型类型是不同的。

泛型类相对于使用Object类型引用所有类型的对象再在调用的时候显式的通过RTTI向下转型，泛型能够自动确保类型安全，还消除了手动输入类型转换以及类型检查的需要。

擦除：
桥接方法：在字节中覆盖方法的类型擦除不能产生与超类方法相同的擦除。这种情况，会在子类中生成使用超类类型擦除的方法，并且这个方法调用由子类覆盖的那个类型擦除的方法。（这发生在字节码级别）
模糊性错误：简直就是个坑啊。。
擦除导致的限制：
不能实例化类型参数。
静态成员不能使用在类中声明的类型参数。
不能创建参数类型或者特定参数类型作为元素的数组：e.g. new T[]; e.g. Gen<Integer> gens[]  = new Gen<Integer>[10];而Gen<?> gens[] = new Gen<?>[10];就是可以的,在泛型类中声明泛型引用也是可以的:T[] gens;
泛型不能扩展Throwable.



lambda表达式(JDK8):
lambda表达式本质就是一个匿名(未命名)方法.而且这个方法不能独立执行,只能用于实现由函数式接口(仅包含一个抽象画方法的接口)定义的另一个方法.因此,lambda表达式会产生一个匿名类.lambda表达式也常被称为闭包.
函数式接口定义了lambda表达式的目标类型,lambda表达式只能用于其目标类型已被指定的上下文中.
e.g. interface MyNumbre {
double getValue();
} MyNumber就是一个函数式接口,其功能由getValue()定义
函数式接口可以指定Object定义的任何公有方法,例如equals,而不影响其作为函数式接口的状态.Object的公有方法被视为函数式接口的隐式成员,以newi函数式接口的实例会默认自动实现它们.
lambda表达式在Java语言中引入了一个新的操作符: ->(lambda操作符) 左侧制订了表达式需要的所有参数(如果不需要参数,则使用空的参数列表) 右侧制订了lambda体.
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
ClassName::methodName (::是JDK8新增的分隔符)
只要是与目标类型兼容(方法返回值 参数 异常)的任何地方都可以使用这个方法引用
和lambda表达式一样,ClassName::methodName方法引用的返回值就是这个函数式接口的一个实例的引用.(所以如果作为参数,那么这个参数应该是函数式接口类型的,而不是该静态方法的返回值)
e.g. public static<T> T max(Collection<?extendsT> coll,Comparator<?superT> comp)中的Comparator就是一个函数式接口,可以直接用方法引用出来一个ClassName::instanceMethodName(这里的instanceMethodName正好要和Comparator函数式接口中的方法兼容)直接作为参数comp,因为方法引用与Comparator兼容.
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



类库:


字符串类库:
Java中的String类操作都不会改变原字符串,只有StringBuffer和StringBuilder(效率比StringBuffer高点)能够保存可以进行修改的字符串.
这三个类都在java.lang类库中定义.而且都是final的.都是实现了CharSequence接口.

String类中的构造器可以构造出类似于ASCII字符集的8位字节数组 : String(byte chrs[]) 和 String(byte chrs[], int startIndex, int numChars)
p.s.String类从输出创建字符串之后修改数组内容不会改变String对象（因为是复制过去的）

String（int codePoints[],int startIndex,int numChars）支持扩展的unicode字符集.codePoints是包含Unicode代码点的数组.

int length() 获取String字符串包含的字符数量

对于String,有重载的+运算符.

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

java.lang包

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

Voide类:
Void类只有域变量TYPE,用来保存对void类型的Class引用.不能创建Void的实例.

Process类:
抽象类用来封装进程,也就是执行的程序.Process主要用作由Runtime类的exec()方法创建的对象类型或ProcessBuilder类的Start()方法创建的对象类型的超类.
void destory() 终止进程
Process destoryForcibly() 强制终止进程.返回对进程的引用(JDK8)
int exitValue() 返回从子进程获得的退出代码
int waitFor() 返回进程返回的退出代码,该方法知道调用进程终止时才会返回.

Runtime类:
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

ProcessBuilder类:
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

System类
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

Object类:
Object clone() throws CloneNotSupportedException 创建一个新的与调用对象相同的对象

Cloneable接口:
只有实现了Cloneable接口的 类才能被clone()复制,否则抛出CloneNotSuportedException
不过这种精确副本也有很多潜在的危险性:副本与原始对象具有完全相同的内容,所以在副本中所引用的对象的内容的修改同样会改变原始对象.

Class类:
Class类型的对象是在加载类时自动创建的.
static Class<?> forName(String name,boolean how,ClassLoader) 返回给定全名的Class对象.如果how为true就初始化对象.
ProtectionDomain getProtectionDomain() 返回与调用对象关联的保护域.

ClassLoader类:
抽象类ClassLoader定义了加载类的方式.可以创建扩展ClassLoader的子类,但是在正常情况下不需要这么做.

Math类:
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

Compiler类
支持创建Java环境,从而将Java字节码编译成可执行代码,常规编程不使用Compiler

Thread类,threadGroup类 Runnable接口
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


ThreadGroup类:
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

Package类
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

Throwable类
getStackTrace()返回一个堆栈帧数组

SecurityManager类

StackTraceElement类
描述单个的堆栈帧(stack frame)
StackTraceElement(String className,String methodname,String fileName,int line) 如果没有有效的行号,line会使用一个负值,并且line为-2表示这个堆栈帧引用一个本地方法
isNativeMethod()

Enum类
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

Itreable接口
所有被用于foreach的类都要实现Iterable接口
Iterator<T> iterator() 为调用对象包含的元素返回迭代器
default void forEach(Consumer<? super T> action) (JDK8) 对于迭代的每一个元素,执行由action指定的代码(Consumer是JDK8新增的一个函数式接口)
default Spliterator<T> spliterator() 返回被迭代序列的Spliterator(JDK8)

Readable接口
指示对象可以用作字符的源
int read(CharBuffer buf) throws IOException 将字符读入buf中,返回读取的字符数 如果遇到EOF就返回-1

AutoClaseable接口
该接口对带资源的try语句提供了支持.
该接口只定义close() 方法
该方法关闭调用对象,释放调用对象可能占用的所有资源.在带资源的try语句的末尾会自动调用该方法.

Thread.UncaughtExceptionhandler接口
该静态街头希望处理未捕获异常的类实现.
只有一个方法:void uncaughtException(Thread thrd,Throwable exc) thrd是对生成异常的线程的引用,exc是对异常的引用.

java.lang子包:
java.lang.reflect 反射相关
java.lang.annotation Annotation接口 Elementtype枚举 RetentionPolicy枚举 
java.lang.invoke 支持动态语言 
java.lang.instrument 定义了能够被用于为程序执行的各个方面添加工具的特性
java.lang.management 为JVM和执行黄金提供了管理支持
java.lang.ref 为垃圾回收过程提供了更灵活的控制

java.util包:

集合(容器)框架(Collections Framework) :
集合只能存储引用,所以基本数据类型都要自动装箱成包装器类.

接口:
Collection(顶层接口,扩展了Iterable接口) List(扩展自Collection)  Queue(扩展自Collection) Set(扩展自Collection) SortedSet(扩展Set以处理已排序的集合) Deque(扩展Queue以处理双端队列) NavigableSet() (扩展SortedSet以基于最接近匹配原则检索元素)
Comparator接口用于比较两个对象 Iterator ListIterator Spliterator(用于并行处理的)接口用来枚举集合中的对象
RandomAccsee接口表明列表支持高效随机的元素访问

Collention接口:
Iterator<E> iterator() 返回调用集合的一个迭代器
void clear() 移除调用容器中的所有元素
default Stream<E> parallelStream() 返回一个使用调用容器作为元素来源的流.该流能够支持并行操作(JDK8)
default boolean removeIf(Predicate<? super E> predicate) 从调用集合中移除满足predicate指定条件的那些元素(JDK8) (Predicate是一个函数式接口)
default Spliterator<E> spliterator() 返回调用集合的Spliterator(JDK8)
default Stream<E> stream() 返回一个使用调用容器作为元素来源的流.该流是顺序流(JDK8)
Object[] toArray()
<T> T[] toArray(T array[] ) 如果包含调用集合中元素的数组.数组元素是集合元素的副本.如果array的长度大于等于容器中元素的数量,超出部分的元素数组设置为null;array的长度小于容器中元素数量,就分配必须大小的新数组并返回这个新数组.

List接口:
ListIterator<E> listIterator(int index) 返回从index指定的索引位置开始将元素返回迭代器
default void sort(Comparator<? super E> comp) 使用comp指定的比较器排序列表(JDK8)
default void replaceAll(UnaryOperator<E> opToApply) 使用opToApply函数获得的值更新列表中的每一个元素(JDK8) (UnaryOperator是JDK8新增的一个函数式接口)

Set接口:
如果用add()向Set添加重复的元素,add会返回false.

SortedSet接口:
以升序进行排序了的.
SortedSet<E> headSet(E end) 返回的SortedSet对象包含已排序调用组中那些小于end的元素.
SortedSet<E> tailSet(E start) 返回的对象包含排序组中大于或等于start的元素.

navigableSet接口:
E ceiling(E obj) 在对象中查找大于等于obj的最小元素,没找到就返回null
Iterator<E> descendingIterator() 返回一个从最大元素向最小元素移动的迭代器(也就是返回一个反向迭代器)
E floor(E obj) 小于等于的最大元素
NavigableSet<E> headSet(E upperBound,boolean incl) 返回小于upperBound的所有元素,如果incl(也就是include的缩写)为true那么返回的元素中就包含等于upperBound的那个元素
higher(obj) 大于obj的最大元素
lower(obj)小于obj的最小元素
E pollFirst() 返回第一个元素(也就是最小值)并移除这个元素
E pollLast() 返回并移除最后一个元素

Queue接口
elelment() 返回队列头部的元素,不移除该元素.队列为空就抛出NoSuchElelmentException
boolean offer(E obj) 试图将obj添加到队列中
E peek() 返回队列头部的元素,不移除该元素.如果队列为空就返回null
E poll() 返回并移除头部元素,队列为空就返回null
E remove() 移除并返回队列头部元素

Deque接口
双端队列既可以像queue一样FIFO也可以像stack一样FILO
addLast() addFirst() 如果超出了容量就会抛出IllegalStateException
getFirst() getLast() 返回但不移除元素,如果队列为空就抛出NoSuchElementException
offerLast() offerFirst() 添加元素,满了就返回false
peekFIrst() peekLast() 返回但是不移除元素,为空就返回null
pollFirst() pollLast() 返回并且移除元素,为空就返回null
removeFirst() removeLast() 返回并且移除元素,为空就抛出NoSuchElelmentException
boolean removeFirstOccurrence(Object obj) 移除第一次出现的obj对象,如果根本就没有obj就返回false

容器类:
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

EnumSet:
static <E extends Enum<E>> EnumSet<E> allOf(Class<E> t) 创建并返回由t指定的枚举中的元素构成的EnumSet
static copyOf(Enum<E> e) 根据c中的元素创建EnumSet
static complementOf(EnumSet<E> e) 创建并返回由e中未存储的元素构成 e.g. e是用一个enum{A,B,C,D}中的A创建的ENumSet,那么complementOf( e)会返回B,C,D为元素的EnumSet
static <E extends Enum<E>> Enum<E>  of(E v, E… varargs) 创建并返回包含v和varargs这些枚举值的EnumSet

迭代器:
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

Map接口:
default V compute(K k,BiFunction<? super K,? super V,? extends V> func)  调用func构造一个新值.如果func返回值不是null就把新的值替换掉k对应的旧值,如果func返回null就把原来的值键对移除并返回null(JDK8)
default V computeIfAbsent(K k,Funtion<? super K,? extends V> func) 返回与键k关联的值.如果没有值就通过func构造一个值,并把该配对输入到映射中,返回构造的值.如果无法构造新值就返回null(JDK8)
default V computeIfPresent(K k, BiFunction <? super K,? super V,? extends V>  func) 如果k包含在映射中就通过func构造一个新值替换掉原来的值,然后返回新值,如果func返回的值为null就从映射中删除现有的键和值,并返回null(JDK8)
Set<Map.Entry<K,V>> entrySet() 返回包含映射中所有条目的Set对象,这个组包含Map.Entry类型的对象.这样就可以使用Set迭代了(因为Set实现了Iterator而Map不支持)
default V getOrDefault(Object k,V defVal) 如果映射中包含与k关联的值就返回该值否则返回defVal
default V merge(K k,V v,BiFunction<? super K,? super V,? extends V> func) 如果k没有包含在映射中,就把k和v配对并添加到映射中并返回v.否则func基于原有的值返回一个新值,键被更新为使用这个新值.如果func返回null就从映射中删除现有的键和值并返回null(JDK8)
default void replaceAll(BiFunction<? super K,? super V,? extends V> func) 对调用映射中的每个元素执行func用func返回的结果替换元素,如果在操作过程中删除了元素就会抛出ConcurrentModificationException(JDK8)

NavigableSet接口:
ceilingEntry(K obj) 返回大于等于参数的最小键的条目 没有就返回null
descendingKeySet 返回逆序形式的键的NavigableSet
floorEntry 小于或等于参数的最大键的条目

映射类:
AbstractMap EnumMap HashMap TreeMap WeakHashMap(使用带有弱键的哈希表) IdentityHashMap(当比较文档时使用引用相等性,不用于通用目的) LinkedHashMap(扩展了HashMap类,可以按照插入或访问顺序迭代整个映射,在构造器中的Order参数设置为true就使用访问顺序否则默认使用插入顺序)

LinkedHashMap中添加了removeEldestEntry 默认返回fasle而且不执行任何操作,代表保留最久的条目,如果重写为返回true就可以使LinkedHashMap移除映射中最久的条目

Comparator接口:
默认Java使用自然比较器(也就是1在2前面,A在B前面那种)
JDK8中新增default Comparator<T> reversed() 返回调用比较器的逆序版本
static <T extends Comparator<? super T>> Comparator<T> naturalOrder()返回颠倒元素的自然顺序比较器
static <T> Comparator<T> nullsFirst(Comparator<? super T> comp)和static <T> Comparator<T> nullsLast(Comparator<? super T> comp)分别返回认为null比任何值小和null比任何值大的比较器
default Comparator<T> thenComparing(Comparator<? super T> thenByComp) 该方法可以返回一个比较器(组合了第一个和第二个比较器之后的比较器),在第一个比较器(也就是现在被调用的这个比较器)比较的两个对象是相同的时再调用第二个比较器比较,thenByComp指定第一次比较返回相等后调用的比较器. (JDK8)

集合算法:Collentions(注意有一个s)类中的static方法:
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

一些遗留的类和接口:
Enumeration接口 类似于Iterator接口
Vector类 类似于ArrayList 实现动态数组,不过Vector是同步的.
Stack类 已经被ArrayDeque取代
Dictionary类 类似于Map
Hashtable类 与HashMap类似
Properties类 Hashtable的子类 System.getProperties()方法返回的就是这个类的对象,用于保存环境值
Properties类中的store和load方法可以方便的读取和存储文件

更多的java.util工具类:
StringTokenizer类
用于解析格式化的输入，实现了Enumeration接口
指定的定界符为空白字符（空格 制表符 换页符 换行符 回车符）也可以在构造器中指定参数delimiters 指定定界符,定界字符串中的每个字符都被认为是有效的定界符，如果delimAsToken为true就在解析字符串时将定界符作为标记返回否则不返回定界符（默认就是不返回的）
hasMoreElements（） nextElement() 方法和hasMoreToken() 和 nextToken()类似

BitSet类:
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

时间和日期相关的：
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

Random类
用于生成伪随机数.
double nextGaussian() 返回下一个高斯分布随机数
JDK8新增doubles() ints() longs()分别返回DoubleStream IntStream LongStream 

Observable类
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

Formatter类
格式化输出
void flush() 刷新格式化缓冲区,这会导致缓冲区中当前所有输出都写入目标.主要用于与文件绑定的Formatter对象
可以用close显示的关闭Formatter对象也可以用带资源的try自动释放Formatter的资源
相对于C语言新增的格式说明符:%h(参数的哈希码) %b(布尔值) 
%t(时间,对应的参数必须是Calendar Date Long 或long) 后缀:a(星期名简称) A(星期名全称) b(月份名简称) B(月份名全称) c(标准格式:天 月份 日期 小时:分钟:秒数 时区 年)
D(月/日/年) F(年-月-日) k(小时 0-23) l(小时1-12) m(月份01-13) M(分钟 00-59) p(以小写形式表示本地时间am或pm) r(小时:分钟:秒数(12小时制)) R(小时:分钟:秒数(24小时制)) z (相对于UTC的时差) Z(地区名) 

逗号标志:显示大数字的时候用逗号三位三位隔开:e.g. &,.2f来输出一个455555322.34为455,555,322.34

使用参数索引:
在格式说明符%后面加上n&(n表示后面的args中的第几个索引) e.g. "%3$d %1$d %2$d",10,20,30 输出为30 10 20

Scanner类
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

java.util中的其他使用工具类:
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

java.util的子包:
java.util.concurrent java.util.concurrent.atomic  java.util.concurrent.locks 用来支持并行编程.这些包为使用java内置的同步特性提供了高性能的替换方法.java.util.concurrent还提供了Fork/Join框架(JDK7)
java.util.function 为lambda表达式定义了一些预定义的函数式接口
java.util..jar 提供了读取和写入Jar文件的能力
java.util.logging 记录日志
java.util.prefs 提供了对用户选择的支持,通常用于程序配置
java.util.regex 正则表达式
java.util.spi 提供对服务提供者的支持
java.util.stream 提供来了Java的流的API(JDK8)
java.util.zip 提供了ZIP个GZIP格式的读写


java.io:
区分I/O流和JDK8新增的流API
File类 (不过在新的NIO中的Path接口可以替换File的大多数功能) 
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

PrintStream类
System.out就是这个类的对象
实现了Appendable Closable Flushable接口
构造器可以接收Writer或者OutputStream作为参数，boolean autoFlushingOn控制每次调用printfln（） printf（） format（）时是否自动刷新输出缓冲区（默认为false）

Console类（JDK6）//感觉就这个readPassword有点意思(输入的时候不可见)
Console类用于从控制台读取和写入内容，并实现了Flushable接口。
Console没有构造器,只能从System.console()方法获取Console对象.
p.s. 如果没有可用的Console存在时,System.console()返回null,所以首先要确保返回的这个Console对象不是null(想IDEA Eclipse这样的 IDE 都是测试代码时都是没有Console存在的,所以返回的都是null)
输入错误时会抛出IOError错误(Error的子类),而且这种情况一般是出现了灾难性的系统失败.
readLine() 和 readPassword() 方法的参数不为空的那个重载形式只不过是输出提示的话而已(和C语言的scanf完全不一样…)

串行化
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


NIO @since 1.4 JDK7时进行了大扩展(有时也被称为NIO.2)
NIO中的包：
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

通道:
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

JDK7 对NIO的增强(尤其是对文件方面的支持)
新增三个新包:java.nio.file java.nio.file.attribute java.nio.file.spi
java.nio.file:
Path接口 继承自Watchable Iterable<Path> Comparable<Path>接口 
resolve() 将相对路径解析为绝对路径(如果参数表示的是相对路径,就把调用Path对象指定的根路径加在参数表示的相对路径前并返回)
boolean endsWith(path) 如果Path对象以path指定的路径结束就返回true(path为String或者Path对象)

Files类
提供许多操作文件的静态方法.

Path接口
通过Paths(提供了了很多关于Path的静态方法)中的get方法(其中一个重载版本可以将路径名分成几个String部分)可以返回Path实例

CopyOption类用于表示Files类中的copy方法中复制或移动文件时的参数
StandardCopyOption中定义了几个值:ShandardCopyOption.COPY_ATTRIBUTES 要求复制文件的属性 StandardCopyOption.NOFLLOW_LINKS不使用符号链接 StandardCopyOption.REPLACE_EXISTING 覆盖先前存在的

FileAttriubute 文件属性接口 表示文件(夹)的属性 定义在javanio.file.attribute包中 BasicFIleAttributes为顶部接口 封装了一些所有文件系统都通用的一组属性
BasicFileAttributes派生出DosFIleAttributes和PosixFileAttributes接口
可以通过Files中的getFileAttribute和getFileAttributeView方法访问文件属性 (NIO中定义了AttributeView BasicFileAttributeVIew DosFileAttributeView PosixFIleAttributeView 属性视图接口 )

FileSystem FileSystems FileStore类
用于访问文件系统,用FilsSystems甚至可以通过newFileSystem方法获取新的文件系统.FileStore类封装了文件存储系统.

LinkOption 这个表示链接文件的类型(默认都是符号链接,也可以使用Files中定义的常量域NOFOLLOW_LINKS表示阻止符号链接的LinkOPtion对象)
OpenOption是描述打开文件方式的接口,StandardOpenOtopn是OpenOption的一个实现(一个枚举实现),其中定义了一个枚举用来表示文件选项:APPEND表示写入文件的末尾 CREATE表示文件不存在就创建文件 CREATE_NEW表示文件不存在时在创建文件 DELETE_ON_COLSE 文件被关闭时删除文件 DSYNC对文件的修改会立即写入物理文件,一般为了效率都是对文件的修改都是先进行缓冲,在需要的时候才写入文件的 READ为输入操作打开文件 SYNC对文件或者文件中元数据的修改被立即写入物理文件. WRITE 为写入操作 TRUNCATE_EXISTING 将为输出操作而打开的 之前就存在的文件的长度减少到0

使用NIO:
为基于通道的IO使用NIO
为基于流的IO使用NIO
为路径和文件系统操作使用NIO
读写的一般流程:
获取用于封装目标的Path对象(事先要判读是否存在这个文件),然后获取Channel(也就是建立目标的连接),创建Buffer用于读写(在通道中调用map()方法可以将通道映射到缓冲区))
p.s. 往缓冲区中put东西会导致指针向后移动,使用write把缓冲区的内容写进通道之前记得rewind或者flip把指针重置到开头.

使用NIO复制文件:
使用Files中的copy方法

基于流的IO使用NIO:
NIO.2开始可以使用NIO打开IO流.
使用Files中的newInputStream和newoutputStream方法可以将打开Path指定的文件到输入或输出流中.
DirectoryStream<Path> newDirectStream获取目录流 DirectoryStream实现了Iterable<Path> 接口 所以可以使用foreach迭代(不过DirectoryStream的每个实例只能执行一次foreach循环)
newDirectStream的一个重载版本可以指定过滤模式的String 通配符*指定匹配0个或者多个任意字符,?通配符指定匹配任意一个字符. [chars]指定匹配chars中的任意一个字符,chars中的*或?被看作常规字符而不是通配字符.还可以使用-指定范围  e.g 
使用Files中的Path walkFileTree(Path root,FleVisitor<? extends Path> fv) 列出目录树
FleVisitor接口中定义的方法:(T是Path或其子类) (SimpleFileVisior简单的实现了该接口)
postVisitDirectory(T fir,IOException exc) 在访问目录之后调用.目录被传递给dir,任何IOException异常都会被传递给exc.如果exc为null表示没有任何错误.返回结果
preVisitDirectory(T dir,BasicFileAttribute attrubs) 在访问目录之前调用.为了继续检查目录,返回FileVIsitResult.CONTINUE
visitFile(T file,BasicFileAttributes attribs) 当访问文件时调用.
visitFileFailed(T file,IOException exc) 当尝试访问文件失败时调用.访问失败的文件由file传递,IOException由exc传递.结果被返回
上面的每个方法都返回FileVisitResult枚举对象:CONTINUE SKIP_SIBLINGS SKIP_SUBTREE TREMINATE
为了遍历目录和子目录,方法应当返回CONTINUE
对于 preVisitDirectory,为了绕过目录及其兄弟目录并阻止调用postVisitDirectory,会返回 SKIP_SIBLINGS,为了只绕过目录及其子目录,返回SKIP_SUBTREE
为了停止目录遍历 返回TREMINATE

联网:
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

InetAddress类
用于封装数字IP地址及对应的域名.可以同时处理IPv4和IPv6地址.

InetAddress类没有可用的构造器.需要是使用工厂方法创建InetAddress对象.
InetAddress类中提供了工厂方法 getLocalHost getByName getAllByName(用于那些单个名称表示多台机器的情况) getByAddress(IPv4和IPv6都行) 创建对象.

Inet4Address和Inet6Address类(InetAddress的子类)
一般都简单的使用InetAddress

TCP/IP客户端套接字
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

URL类
现在的Internet上流行的不是上面的这些例如whois FTP之类的老式协议,而是WWW.
Web是高层协议和文件格式的松散集合,全部统一于Web浏览器中.URL(Uniform Resource Locator 统一资源定位器) 用于定位网络上的所有资源.
.e.g http://www.demo.com:80/index.html 第一部分是协议(e.g. HTTP FTP file等) 第二部分是主机名或IP地址(e.g. www.demo.com) 第三部分为端口号(HTTP默认的端口就是80) 第四部分是实际的路径(也就是HTTP服务器中的资源的路径,大多数的HTTP服务器会为直接引用目录资源的URL追加名为index.html之类的文件)
URL为唯一标识或寻址Internet上的信息,提供了一种相当容易理解的形式.
URL类为使用URL访问Internet上的资源提供了一套方法.
URL的构造器支持使用整个URL或者将URL的四个部分拆开来创建对象.也允许使用已有的URL作为参考上下文,然后根据上下文创建新的URL
URL中没有设置端口的话,getProtocol会返回-1

为了访问URL对象的实际位或内容信息,可以使用openConnection() 创建URLConnection对象

URLConnextion类
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

URI类
URI(Uniform Resource Identifier 统一资源标识符) 实际上 URL是URI的一个子集.
URI代表定位资源的一种标准方式.URL还描述了如何访问资源.

cookie
在java.net包中包含了帮助管理cookie的类和接口,并且可以用于创建有状态(与之对应的是无状态的)的HTTP会话.
相关的类:CookieHandler CookieManager HttpCookie
接口:CookiePolicy CookieStore 
这里不展开了

TCP/IP服务器套接字
ServerSocket类用于创建服务器,在发布的端口上监听与之连接的本地或远程客户端程序.
创建ServerSocket对象时,它会在系统中注册自身,表明对客户端连接有兴趣.
ServerSocket类的构造器反映了希望接收连接的端口号,并且(可选)反映了希望端口使用的队列长度.队列长度告诉系统:在简单的拒绝连接之前,可以保留多少个等待连接的客户端连接.默认长度是50.在不利条件下 构造器可能会抛出IOException异常.
在多宿主机上,其中一个构造器可以接收InetAddress参数指定绑定了套接字绑定的IP地址.
accept() 是一个等待客户端发起通信的堵塞调用,然后返回一个常规的Socket对象,该Socket对象用于与客户端进行通信.

数据报
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

JavaFX

顶层结构


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

Glass Windowing Toolkit
JavaFX 图形栈的最底层.主要负责提供本地操作服务 (e.g. 窗口 计时器 本机操作系统有关的连接)
管理事件队列(使用本地系统事件队列功能来管理进程使用),和JavaFX应用程序运行在一个thread中. 

Threads
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

UI控件
在javafx.scene.control包中
这些控件都是scene graph中的node.

Layout
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

启动JavaFX App:
为了运行一个独立的JavaFX App,必须调用Application类定义的public static void launch(String … args)方法
调用launch会开始构造应用程序,之后调用init和start 知道应用程序终止,launch方法才返回.
p.s. 对于使用javafxpackager工具(或者IDE中类似的打包工具)打包的JavaFX App不需要包含对launch的调用.包含launch为了能简化测试和调试.

Stage是顶级容器,所有的JavaFX App都自动能够访问一个Stage(也就是start方法的那个primaryStage参数,这个叫主舞台),还可以创建其他舞台.
至少需要在Stage里面添加一个Scene(javafx.scene包下面的)
Scene是由Node构成的分层的树.(根节点 父节点 子节点 叶节点),scene中的所有node的集合创建出scene graph
所有的node的基类是Node,Parent Group Region Control都是Node的派生类.
p.s. Scene(Parent root,……..) 所有的Layout都是Parent的子类.
一些方法:setScene(Scene myScene) 将myScene设置为该Stage的scene show()显示stage创建的窗口和屏幕

布局:
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

事件
javaFX事件的基类是javafx.event包中的Event类,Event类继承了java.util.EventObject.
Event类有几个子类:ActionEvent处理按钮产生的动作事件.
JavaFX为时间处理实质上使用了委托事件模型方法,为处理时间,首先必须注册处理程序,作为时间的监听器.时间发生时,会调用监听器.监听器必须响应事件,然后返回.
事件是通过实现EventHandler接口(泛型函数式接口interface EventHandler<T extends Event>该接口定义了handle(T eventObj) 方法)处理的,该接口也在javafx.event包中.
使用一个处理程序处理来自不同源的事件时,通过调用继承自java.util.EventObject的Object getSource()方法可以获得事件的源.
Event类的其他方法允许获得时间类型 确定事件是否已被消费 消费事件 引发事件 以及获得事件的目标.事件被消费之后,就不会被传递给父处理程序.
在JavaFX中,事件沿着事件分发链处理的,产生事件时,事件被传递给分发链的根节点,然后事件沿着分发链向下传递给事件的目标.目标节点处理完事件后,把时间沿着分发链向上回传,从而给父节点提供了必要时处理时间的机会.这被叫做事件冒泡.链中的节点可以消费时间,这会阻止事件被进一步处理.
p.s. 使用javaFX的App还可以通过实现事件管理器来管理事件,通过调用Node类定义的addEventFilter方法可以向节点添加事件管理器.事件管理器可以消费事件从而阻止事件进一步处理.


UI控件
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
Context![enter description here][1]Menu 弹出菜单,通常通过右键鼠标激活
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



 


  [1]: ./images/1500169756062.jpg
