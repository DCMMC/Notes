# ThuThesis-Project

LaTeX template used for projects, based on ThuThesis.

Only tested with MikTeX in the Arch Linux.

Big thanks to TUNA and other contributors of ThuThesis.

> More details please refer to the [upstream repository](https://github.com/tuna/thuthesis).

## Get start

Build and then read the `thesis-project.pdf` to learn basic usages of this template.
You can edit `thesis-project.tex` and `thusetup.tex` and build your own project report.

> In specific, you may need change `author`, `course`, `title`, and `studentid` in `thusetup.tex`.

## Build

```bash
make thesis-projct
```

If you're not use make, you should first run `xetex thuthesis.ins` and then run `xelatex thuthesis-project; xelatex thuthesis-project; bibtex thuthesis-project; xelatex thuthesis-project` in powershell or bash.

## Clean the caches

```bash
make clean
```
> More tasks refer to `Makefile`.
