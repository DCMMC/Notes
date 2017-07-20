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

# 解决Git默认对大小写不敏感的问题

配置git 使其对文件名大小写敏感
git config core.ignorecase false

# Git删除远程服务器上的文件(有时候因为大小写的问题, 远程服务器区分大小写, 本地却不区分, 导致本地根本同步不了)

git rm -r -n --cache SomeFileNameOrDir //加上-n只是预览要删除的内容, 并不会真的执行

git rm -r --cache SomeFileNameOrDir //执行删除命令

git commit -m "Commits you want to say"

git push origin master //同步, 也就是发送删除命令

# *Refence*

[git命令清单](http://www.ruanyifeng.com/blog/2015/12/git-cheat-sheet.html)