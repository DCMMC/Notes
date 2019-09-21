---
title: 数论(Number Theory) 
tags: 数论,笔记
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


# (P)1 同余(Congruence)

1. 记 `!$ a \equiv b\  (mod\ n)$` , 表示a, b同余于n.
意思就是a b以n作为除数的余数相等.

> e.g. `!$1 \equiv 8\ (mod\ 7)$`

2. 记 `!$ a | b$`, 表示a能够整除b.

## (p) 引理1.1 

`!$ a \equiv b\ (mod\ n) \Leftrightarrow  n|(a-b)$`

证明:

必要性)
`!$ a \equiv b\ (mod\ n) $`
`!$ \Rightarrow \exists q,\ q^\prime 和\ r,\ 且a = qn + r, b = q^\prime + r $`
`!$ \Rightarrow a - b = (qn + r) - (q^\prime n + r) = (q - q^\prime)n$`
`!$ \Rightarrow n|(a - b) \,. $`

充分性)
`!$ n|(a - b)$`
`!$ \Rightarrow \exists x\ 使\ a - b = xn, i.e., a = b + xn \,. $`
`!$ 假设r是b除以n的余数, 则b = qn + r, 则a = b + xn = (q + x)n + r, i.e., a除以n余r$`
`!$ \Rightarrow a \equiv b\ (mod\ n)  \,. $`



# *Refence*
[Number Theory in Withman College](https://www.whitman.edu/mathematics/higher_math_online/section03.01.html)

在markdown中插入 ==LaTex== 数学公式

### 行内公式

这是行内公式`!$ \Gamma(z) = \int_0^\infty t^{z-1}e^{-t}dt\,. $`

### 块公式

```mathjax!
$$\Gamma(z) = \int_0^\infty t^{z-1}e^{-t}dt\,.$$
```

```mathjax!
$$\int_0^\pi ( 1 - \sin^2 \theta) d \theta =  \int_0^\pi 1 d \theta - \int_0^\pi \sin^3 \theta d \theta $$
$$ = \pi + \int_0^\pi \sin^2 \theta d \cos \theta$$
$$ = \pi + \int_0^\pi ( 1 - \cos^2 \theta) d \cos \theta$$
```