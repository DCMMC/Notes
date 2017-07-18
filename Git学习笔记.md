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
git add ProjectFolderName
git commit -m "first commit"
git status //查看状态
git remote add origin https://github.com/YourGithubUsername/RepositoryName.git
git push -u origin master //上传, 期间会要求输入github的帐号和密码
~~~