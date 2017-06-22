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

**Installation**
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



## Create Building Scans
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
