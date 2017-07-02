---
title: Gradle 学习笔记 
tags: Java,Gradle
grammar_cjkRuby: true
---

# Fundamentals

## Installation

[Link](https://gradle.org/install)

## The Gradle Wrapper
Wrapper能够方便的控制用户使用正确的版本的build工具. 用别人的==project==的时候一定需要用到Wrapper.

如果project已经建立了Wrapper, 直接`gradlew <task>`就行了. 每一个Wrapper就绑定了指定版本的Gradle, 所以第一次build的时候, 会自动下载安装自动版本的Gradle, 这样使用Wrapper分发的==project==就不需要提前安装==Gradle==了.

### Installation
作为==project==的作者, 你需要把Wrapper安装至你的==project==: `gradle wrapper --gradle-version 4.0 --distribution-type bin )` 
默认的分发类型是==bin==(最小化Gradle Distribution, 但是使用==all==作为distribution-type时, Android Studio或者IDEA能够提供额外的contex information.
还可以用`--gradle-distribution-url`来指定下载Gradle的url.
P.S. 如果没有指定任何的gradle version或者url, 就会使用当前调用wrapper的gradle的版本和url.

可以在以后的迭代版本中更改build.gradle中添加或者修改==Wrapper== task来指定gradleVersion:
~~~
task wrapper(type: Wrapper) {
    gradleVersion = '2.0'
}
~~~
执行gradle wrapper之后就能在==project==中生成==Wrapper generated files==.
这样, 就可以直接在==project==中使用==gradlew==了, ==gradlew==的用法和==gradle==一模一样.

## Creating New Gradle Builds
1. 创建 ==build.gradle== 文件
在 ==\%Project_Root\%== 下创建 ==build.gradle==文件.
输入 ==gradle tasks== 可以查看所有能够用gradle调用的tasks, 后面加上 ==--all== 还会显示所有build.gradle中自定义的tasks.
你可以定义自己的tasks, 修改自带的tasks或者使用plugins提供的tasks.
2. 生成Gradle Wrapper的task
你可以键入 ==gradle help --task wrapper== 来了解 ==gradle wrapper== 的用法.
默认直接 ==gradle wrapper==就可以了.
生成的Wrapper文件大概目录为:
├─.gradle
│  ├─4.0
│  │  ├─fileChanges
│  │  ├─fileHashes
│  │  └─taskHistory
│  └─buildOutputCleanup
└─gradle
    └─wrapper
3. 查看 ==project== 的属性的task
 ==./gradlew properties== 即可查看项目属性
 可以通过在 ==build.gradle== 中添加或者修改相应的属性:
 e.g. 在 ==build.gradle== 中添加一行 ==version='1.0'==
4. 配置Gradle Core Tasks
Gradle自带了一个tasks库, 提供了很多诸如Copy之类的tasks.
e.g. 在 ==build.gradle== 中创建一个task:
~~~
task copy(type: Copy) {
	from 'src'
	into 'dest'
}
~~~
键入 ==gradlew copy== 就能执行了, 还这个task会将当前目录下的src复制成dest并放在当前目录.
5. 使用插件
Gradle包含了很多plugins, 还有更多的插件在 [the Gradle plugin protal](http://plugins.gradle.org/)中.
e.g. ==base== 这个插件中有一个 ==core type== 叫 ==Zip==, 可以提供name和location来创建压缩文件
通过加入 ==plugins== 语句块来添加插件:
~~~
plugins {
    id 'base'
}
~~~
P.S. plugins语句块必要放在build.gradle的最前面, 必须在task语句块前面.
在新建一个task:
~~~
task zip(type: Zip) {
    from 'src'
}
~~~
执行 ==./gradlew zip== 就会自动将src目录打包成 ==\%project_name\%-\%version\%.zip== 的形式到 ==\%project_root\%/build/distribution== 目录下.
P.S. ==clean== task可以用来清除 ==build== 文件夹.

## Dependency Management Basics
大部分项目不是`completely self-contained`, 他们大多都依赖别的项目构建出来的文件(称为 ==dependencies==, build和upload的文件(包括jars, docs)称为 ==publications== ), 这些依赖可能来自于远程服务器 ==Maven== 或者 ==Ivy== respository(依赖库)中, 在一个本地文件夹中, 或者是在 ==multi-project build== 中的另外一个 ==project==  build出来的. Gradle寻找这些依赖的过程叫 ==dependency resolution== (依赖解析).
不过 ==project== 所调用的依赖自己本身也有一些依赖, 这种子依赖称为 ==transitive dependencies==. Gradle会自动找到这些 ==transitive dependencies==.
### 声明依赖
e.g.
~~~ Groovy
apply plugin: 'java'

repositories {
    mavenCentral()
}

dependencies {
    compile group: 'org.hibernate', name: 'hibernate-core', version: '3.6.7.Final'
    testCompile group: 'junit', name: 'junit', version: '4.+'
}
~~~
在Gradle中, 这些依赖都是作为 ==Dependency configuration== 写在 ==build.gradle==中的.
例如 ==Java==这个plugin定义了一些标准configuration:
* compile
* runtime
* testCompile
* testRuntime

定义自定义configuration: [Section 25.3, “Dependency configurations"](https://docs.gradle.org/3.5/userguide/dependency_management.html#sub:configurations)

### 依赖库Repositories
~~~
repositories {
    mavenCentral()
	 jcenter()
}
~~~
这里声明了两个依赖库 == Maven central repository== 和 ==JCenter repository==.
或者声明url来使用远程Maven依赖库:
~~~
repositories {
    maven {
        url "http://repo.mycompany.com/maven2"
    }
}
~~~
或者远程 ==Ivy directory==
~~~
repositories {
    ivy {
        url "http://repo.mycompany.com/repo"
    }
}
~~~
或者在本地文件系统中(支持 ==Ivy== 和 ==Maven== ):
~~~
repositories {
    ivy {
        // URL can refer to a local directory
        url "../local-repo"
    }
}
~~~


### 外部依赖(External dependencies)
声明存储在 ==resposity== (e.g.  Maven central, or a corporate Maven or Ivy repository, or a directory in the local file system)中的依赖:
e.g.
~~~
dependencies {
    compile group: 'org.hibernate', name: 'hibernate-core', version: '3.6.7.Final'
}
~~~
一个依赖是由 ==group==, ==name== , ==version== 确定, 不过取决于不同的依赖库, ==group== 和 ==version== 有时候是可选的.
也可以用简短形式来声明依赖:
~~~
dependencies {
    compile 'org.hibernate:hibernate-core:3.6.7.Final'
}
~~~
For more details: [ Section 25.4, “How to declare your dependencies”.](https://docs.gradle.org/3.5/userguide/dependency_management.html#sec:how_to_declare_your_dependencies)

## More about Tasks
### 创建一个 ==ad-hoc== task


# Java Develpment

## Building Java Applications
### ==java== plugin
一些convertions(惯例):
* 你的==production source code==放在==src/main/java==中
* ==test source code== 放在==src/test/java==中
* 所有在==src/main/resources==的文件会作为resources加入到JAR file
* 所有在==src/test/resources==中的文件都会包括在 ==classpath== 中用来运行test.
* 所有的输出文件都会创建在==build==目录, JAR file会在 ==build/libs==中

一些tasks:
* ==java== plugin自带了一些tasks, 最常用的就是 ==build==, 这个task能够完整的build整个项目, 执行junit 的test, 创建JAR. 
* 还有就是 ==assemble== task, 编译并且将字节码打包成jar, 但是不会进行 ==unite tests==.
* ==check== 编译并且test代码. 有一些plugins还会加入很多的checks, e.g. `checkstyle ` 这个plugin还会运行 ==Checkstyle==.

==gradle init --type java-application== 在当前目录生成gradlew所需要的所有文件以及src的所有目录结构.

### 外部依赖(External Dependencies)
一般一个Java Project都会有一些需要使用外部JAR文件的依赖, 要引用这些依赖, 在Gradle中像外部JAR文件这样的**Artifacts**都位于 ==repository==(依赖库)中.
e.g. 如果要使用 ==the public Maven repository== 只需要在==build.gradle==中添加:
~~~
repositories {
    mavenCentral()
}
~~~
**添加依赖**
如果我们的 ==project==的 ==prodection classes== 需要一个编译时依赖(Compile-time) ==commons collections==, 而在==test classes==中需要一个编译时依赖==junit==. 可以添加一下代码到==build.gradle==:
~~~
dependencies {
    compile group: 'commons-collections', name: 'commons-collections', version: '3.2.2'
    testCompile group: 'junit', name: 'junit', version: '4.+'
}
~~~
### Customizing the project
==Java== plugin在你的项目中加入了很多 ==properties==, 而且这些 ==properties== 都有默认的值, 要改变这些值, 只需要在 ==build.gradle== 中加入相应代码就行了.
e.g. 
~~~
sourceCompatibility = 1.7 //代码兼容的JDK版本
version = '1.0'
jar {
    manifest { //用来定制MANIFEST.MF文件
        attributes 'Implementation-Title': 'Gradle Quickstart',
                   'Implementation-Version': version
    }
}
~~~

### 发布JAR
~~~
uploadArchives {
    repositories {
       flatDir {
           dirs 'repos'
       }
    }
}
~~~
将会把生成的jar文件放在根目录中的repos文件夹下

### 定制JAR的签名
通过配置 [==jar== task](https://docs.gradle.org/3.5/userguide/more_about_tasks.html#sec:configuring_tasks).
~~~ Groovy
jar {
    manifest {
        attributes('Implementation-Title': project.name,
                   'Implementation-Version': project.version)
    }
}
~~~

### Multi-project的构建
首先在源码树的根目录下创建 ==setting.gradle== 文件, 在这个目录中如果存在两个子项目 ==demo== 和 ==model==, 则在 ==setting.gradle== 中添加 `include "demo", "model"`
或者
~~~ Groovy
include "demo"
include "model"
~~~
并且每一个子项目都要包含他们自己的 ==build.gradle== 文件

在主项目的 ==build.gradle== 中声明所有子项目的 `common configuration` :
在 主项目的 ==build.gradle== 中添加 ==subprojects== 代码块, e.g.
~~~
subprojects {
    apply plugin: 'java'

    repositories {
       mavenCentral()
    }

    dependencies {
        testCompile 'junit:junit:4.12'
    }

    version = '1.0'

    jar {
        manifest.attributes provider: 'gradle'
    }
}
~~~
P.S. 这些 ==subprojects== 块中的 ==configuration== 只作用于所有的子项目, 不会作用于 ==root level==.

### *multi-project*中的项目间依赖
在上个例子中, 如果要在子项目 ==demo== 中依赖 ==model== 项目生成的jar, 只需要在子项目 ==demo== 的 ==build.gradle== 中添加:
~~~
dependencies {
    compile project(':model')
}
~~~
P.S. 注意前面的 ==:== 号.

## Building Java Libraries
### 创建一个新项目
执行 ==init== task:
==gradle init --type java-libraries==

### java-library plugin
==Java Libraries== 使用的 ==java-library== plugin相当于 ==Java Application== 使用的 ==java== plugin的一个超集, ==java-library== 在 ==java== 的基础上添加了一些 ==Java Library== 要用到的一些东西. 特别的, ==Java Library== 会暴露一个API给客户端程序员(i.e., consumers of this library).

### API and implementation separation
*A library is a Java component meant to be consumed by other componets.*
==java library== 在 ==multi-project== 中很常见, ==java-library== plugin提供两个 ==configurations== 用来在==build.gradle== 中声明依赖: ==api== 和 ==implementation==.
* ==api== 用来声明将被这个 ==library API== 导出的依赖, 会出现在 ==libraries consumers== 的 ==complie classpath== 中.
* ==implementation== 用来声明只是在这个 ==component== 内部的依赖, 不会出现在 ==libraries consumers== 的 ==complie classpath== 中.

e.g.
~~~ Groovy
dependencies {
    api 'commons-httpclient:commons-httpclient:3.1'
    implementation 'org.apache.commons:commons-lang3:3.5'
}
~~~
这样的好处:
* 当 ==implementation dependencies== 改变的时候, ==library consumers==不需要重新编译.
* 减小 ==classpath== 的大小, 很短的编译时间
* 使用 ==maven-publish== plugin时会有更加清晰的分发, 因为生成POM文件的时候会区别那些是编译时库那些是运行时库.

认识 ==api== 和 ==implementation== dependencies:
==api== 依赖用于但不限于:
* 公共域, 公共方法参数(包括泛型)中的类型
* 超类或者接口中的类型
* 公共注解类型

而 ==implementation== 依赖用于:
* 只用于**方法体**中的类型
* 只用于private成员的类型
* 只出现在内部类中的类型(未来的Gradle会让用户定义那一些 ==packages== 属于 ==public API==)


### ==java-library== plugin的 ==configuration== 关系图
好复杂, 没太看懂, 感觉好多 ==configurations== 我都用不上.
详细请看 [48.4](https://docs.gradle.org/3.5/userguide/java_library_plugin.html)

### 添加API Document
执行 ==javadoc== 操作就行了, 会自动把源码中的javadoc注释生成html到 ==/build/docs/javadoc==, 打开 ==index.html== 即可查看所有的javadoc.


# Create Building Scans
==build scan== 就是一个Gradle提供的可分享的, 便于集中记录一次build, 并且给出build的时候发生了什么和为什么的一个插件. 
可以免费使用.

==Plugin website==:  [Gradle Plugin Portal](https://plugins.gradle.org/plugin/com.gradle.build-scan)

**Usage:**
1. 在==Project==中的==build.gradle==文件中加入以下语句块:
~~~ Groovy
plugins {
    id 'com.gradle.build-scan' version '1.8'  //version后面必须加版本号, 版本号自己去Gradle官网找最新的
}
~~~
2. 在==build.gradle==中加入==license agreement==
~~~ Groovy
buildScan {
    licenseAgreementUrl = 'https://gradle.com/terms-of-service'
    licenseAgree = 'yes'
}
~~~
3. 在` ./gradlew build` 后面加上 ==--scan==


**Enable build scans for all builds (optional)**

在`~/.gradle/init.d`中创建`buildScan.gradle`:
~~~ Groovy
initscript {
    repositories {
        maven { url 'https://plugins.gradle.org/m2' }
    }

    dependencies {
        classpath 'com.gradle:build-scan-plugin:1.6'
    }
}

rootProject {
    apply plugin: com.gradle.scan.plugin.BuildScanPlugin

    buildScan {
        licenseAgreementUrl = 'https://gradle.com/terms-of-service'
        licenseAgree = 'yes'
    }
}
~~~
这样在所有的项目中运行./gradlew build --scan就行了



# *Reference*
* [官方Guide](https://gradle.org/guides#getting-started)
* [the Gradle plugin protal](http://plugins.gradle.org/)
