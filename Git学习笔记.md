---
title: Git学习笔记 
tags: Git,笔记
grammar_cjkRuby: true
---


# Github项目的clone与上传

clone:

> git clone  https://github.com/YourGithubUsername/RepositoryName.git

上传:
~~~
git pull origin master //先用github上同步好github上的最新版本, 不然git push会失败
git add ProjectFolderName //不管是有文件添加还是修改, 都是用add
git commit -m "first commit"
git status //查看状态
git remote add origin https://github.com/YourGithubUsername/RepositoryName.git
git push -u origin master //上传到master branch, 期间会要求输入github的帐号和密码
~~~