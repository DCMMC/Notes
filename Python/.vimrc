" Powerline Theme
set laststatus=2
set t_Co=256
python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup

" 参考: https://www.jianshu.com/p/bc19b91354ef
" vim 版本必须 >= 8.0, 并且 +python

" 配色
set background=dark
" 不设置这些会导致背景色为纯黑, 贼丑, 设置之后将会是终端(模拟器)的背景色,
" 所以还需要把终端主题改成 solarized
let g:solarized_termcolors=256
let g:solarized_termtrans=1
colorscheme solarized

" Vundle 插件管理
set nocompatible              " required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" alternatively, pass a path where Vundle should install plugins
" call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Add all your plugins here (note older versions of Vundle used Bundle instead of Plugin)
" 要记得在vim中 :PluginInstall 安装这些插件

" 配色
Bundle 'Solarized'
" 格式化注释
" gcb 为 box circled comment
" gcc 为普通注释
" gcu 为撤回注释, 然而垃圾的很, 就是 normal 下的 u
" FIXME: 必须设置为一个 tab 四个空格
Plugin 'DCMMC/caw.vim'
" Plugin 'tyru/caw.vim'
" Plugin 'scrooloose/nerdcommenter'
" 缩进指示线
Plugin 'Yggdroot/indentLine'
" 自动补全括号
Plugin 'jiangmiao/auto-pairs'
" 折叠代码
Plugin 'tmhedberg/SimpylFold'
" Python 自动缩进
Plugin 'Vimjas/vim-python-pep8-indent'
" mypy vim 扩展
Plugin 'Integralist/vim-mypy'
" Plugin 'vim-scripts/indentpython.vim'
" 自动补全
" 还需要安装某些依赖, arch下vim-youcompleteme-git
" ArchLinux 需要yaourt -S vim-youcompleteme-gi && cd ~/.vim/bundle/YouCompleteMe && ./install.sh --all
Bundle 'Valloric/YouCompleteMe'
" 语法检查
Plugin 'w0rp/ale'
" shell in vim
" 安装之后需要手动 cd ~/.vim/bundle/vimproc.vim && make
Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/vimshell.vim'
" 文件树形结构
Plugin 'scrooloose/nerdtree'
" 查看文件树形结构
Plugin 'jistr/vim-nerdtree-tabs'
" 集成 git
Plugin 'tpope/vim-fugitive'
" 异步 shell 执行
Plugin 'skywind3000/asyncrun.vim'
"=== vim-trailing-whitespace将代码行最后无效的空格标红 ===
Plugin 'bronson/vim-trailing-whitespace'
" 实时 md 渲染预览
Plugin 'iamcco/mathjax-support-for-mkdp'
Plugin 'iamcco/markdown-preview.vim'
let g:mkdp_path_to_chrome="google-chrome-unstable"
let g:mkdp_auto_close=0
nmap <F7> <Plug>MarkdownPreview
nmap <F8> <Plug>StopMarkdownPreview
" 更优雅的 buffer 管理
Plugin 'fholgado/minibufexpl.vim'

" 简要命令指南: 'MBEOpen' 打开管理器, ':bun'将当前 buffer 的窗口最小化, ':bw' 关闭当前 buffer 和窗口
" ':b[num]' 打开指定 num 的 buffer 为当前窗口, 注意如果当前只有一个窗口, 但是有多个 buffer, 这时候
" ':q' 会退出 vim, 所以需要用 ':bw' 来关闭
" 如果对当前窗口大小不满意, ':vertical resize+num' 宽度+[num]行,
" ':resize+num', 高度

""""""""""""""""""""""""""""""  
" miniBufexplorer Config  
""""""""""""""""""""""""""""""  
" let g:miniBufExplMapWindowNavArrows = 1  
" let g:miniBufExplMapWindowNavVim = 1  
" let g:miniBufExplMapCTabSwitchWindows = 1  
" let g:miniBufExplMapCTabSwitchBufs = 1   
" let g:miniBufExplModSelTarget = 1  
  
" 解决FileExplorer窗口变小问题  
" let g:miniBufExplForceSyntaxEnable = 1  
" let g:miniBufExplorerMoreThanOne=2  


" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" 我修改过上面这个插件, 因为我只想 python 文件
" 标示行末不必要的空格, 所以改了下 g:extra_whitespace_ignored_filetypes
" 变成 g:extra_whitespace_filetypes
let g:extra_whitespace_filetypes = ['python']


" 分割方向
" :sv <filename> 纵向分割打开
" :vs <filename> 横向分割打开
set splitbelow
set splitright
" 分割快捷键切换
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
" 代码折叠
set foldmethod=indent
set foldlevel=99
" 空格快捷键代码折叠
" press space to fold/unfold code
nnoremap <space> za
vnoremap <space> zf
" 希望看到折叠代码的文档字符串
let g:SimpylFold_docstring_preview=1

" Python PEP8风格缩进
" 编写python代码时会使#(注释)自动回退到行首
" au bufnewfile,bufread *.py, *.java
" \ set tabstop=4 |
" \ set softtabstop=4 |
" \ set shiftwidth=4

" web开发缩进
au BufNewFile,BufRead *.js, *.html, *.css
\ set tabstop=2 |
\ set softtabstop=2 |
\ set shiftwidth=2

" 标示不必要的空白字符
" au BufRead,BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/

" 支持 utf-8
set encoding=utf-8

" 自动补全微调
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>
let g:ycm_complete_in_strings = 1

" 支持Virtualenv虚拟环境
py << EOF
import os
import sys
if 'VIRTUAL_ENV' in os.environ:
  project_base_dir = os.environ['VIRTUAL_ENV']
  activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
  execfile(activate_this, dict(__file__=activate_this))
EOF

" ale 异步语法检查
" 语法高亮
" 让 ale 对 py 文件使用 flake8
" 记得 sudo pip install flake8
let g:ale_linters = { 'python': ['flake8'], }
" Auto-check file for errors on write:
let g:PyFlakeOnWrite = 1
" let g:flake8_show_in_gutter = 1
" 错误表示在整行而不是侧边
let g:flake8_show_in_file = 1
let python_highlight_all=1
syntax on

" 在终端 vim 中默认打开tabs
let g:nerdtree_tabs_open_on_console_startup=1
" 隐藏文件树形结构中的 .pyc
let NERDTreeIgnore=['\.pyc$', '\~$', '__pycache__'] "ignore files in NERDTree

" 显示行号
set nu

" Python 代码格式化
" 需要 sudo pip install yapf
autocmd FileType python nnoremap <LocalLeader>= :0,$!yapf(" --style pep8")<CR>

" 一键执行
map <F5> :call CompileRun()<CR>
func! CompileRun()
    exec "w"
    if &filetype == 'c'
        exec "! gcc  % -o %<; time ./%<"
    elseif &filetype == 'cpp'
        exec "! g++ -std=c++11 % -o %<; time ./%<"
    elseif &filetype == 'java'
        exec "! javac %; time java %<"
    elseif &filetype == 'sh'
        exec "! time bash %"
    elseif &filetype == 'python'
        exec "! time python %"
    endif
endfunc

" 用 ,. 组合键代替 Esc
inoremap ,. <esc>
