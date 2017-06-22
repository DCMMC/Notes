---
title: Gradle 学习笔记 
tags: Java,Gradle
grammar_cjkRuby: true
---

## Installation

[Link](https://gradle.org/install)

## Create Building Scans
==build scan== 就是一个Gradle提供的可分享的, 便于集中记录一次build, 并且给出build的时候发生了什么和为什么的一个插件. 
可以免费使用.

==Plugin website==:  [Gradle Plugin Portal](https://plugins.gradle.org/plugin/com.gradle.build-scan)

**Usage:**
1. 在==Project==中的==build.gradle==文件中加入以下语句块:
~~~ Groovy
plugins {
    id 'com.gradle.build-scan' version '1.6'  //version后面必须加版本号, 版本号自己去Gradle官网找最新的
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


## *Reference*
* [官方Guide](https://gradle.org/guides#getting-started)
