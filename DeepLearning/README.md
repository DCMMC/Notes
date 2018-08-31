## Deep Learning 学习笔记

> `.md` 文件是用小书匠写的, `.ipynb` 文件是 `jupyter notebook`

Github 上加载很慢的话, 可以用 [nbviewer](http://nbviewer.jupyter.org/github/DCMMC/Notes/tree/master/DeepLearning/) 加速访问.

### Fix `boldsymbol` not found issue in `jupyter notebook`

For my instance, add

```
/* Add some extensions to mathjax */
MathJax.Hub.Config({
    TeX: {
        extensions: ["autobold.js"]
    },
});
```

to `/usr/lib/python3.7/site-packages/notebook/static/custom/custom.js`

### PRML errate(勘误)

[github errata prml](https://github.com/yousuketakada/prml_errata)

[ms research](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/05/prml-errata-1st-20110921.pdf)
