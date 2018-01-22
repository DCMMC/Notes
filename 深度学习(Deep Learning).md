---
title: 深度学习(Deep Learning) 
tags: 深度学习, AI, DL, ML,笔记
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
# Notations

# Intro

在现在人工智能(Artificial Intelligence, *abbr.* AI)领域主要需要解决的是那些对人类来说很直观的(Intuitively)但是难以形式化(formally, i.e., mathematic rules)得描述的问题. 本书讨论的一种方案, 可以让计算机通过较为简单的知识来从经验中学习产生更加复杂的层次体系(相互关联的层次概念, hierarchy concepts), 我们称这种方法为深度学习(AI deep learning). 

一些早期的人工智能项目希望通过将整个世界通过人为的硬编码(hard-code)成形式化的语言, 但这个想法的困难实现表明, AI 系统需要能够直接用原始数据中提取模式(extract pattern)的能力来获取(acquire)自己的知识而不是人为的硬编码进去, 这个能力被称为机器学习(Machine Learning). 

逻辑回归(Logistic  Regression)作为一种简单的机器学习算法可以用来判断是否需要剖腹产, 另一种被称为朴素贝叶斯(naive Bayes)的机器学习算法可以用来区分是否是垃圾邮件. 但是, 这两种简单的机器学习算法很大程度上依赖于给定数据的表示(representation), 例如在预测是否需要剖腹产时, 医生必须告诉系统几条与之密切相关的患者的信息(e.g. 是否存在子宫疤痕), 表示患者的每条信息被称为特征(features), 逻辑回归学习这些特征如何与各种结果相关联, 而不能直接想医生一样从核磁共振成像得出结果.  

不管实在 CS 领域还是日常生活领域, 对数据表示的依赖是很正常的现象, 例如要画一条直线将对一下数据分为两类, 在笛卡尔坐标系下显然是不可能的, 但是在极坐标系下就是很简单的.

![Figure 1.1][1] 



# (p) Part 1 Applied Math and Machine Learning Basis


  [1]: ./images/1516606697255.jpg