" -*- coding:utf-8 -*-
" 基本設定
set notitle
set guifont=SourceCodePro-Regular-Powerline:h13
set guifontwide=SourceCodePro-Regular-Powerline:h13
let g:Powerline_symbols = 'fancy'

" 色の設定
colorscheme jellybeans

" ターミナルタイプによるカラー設定
if &term =~ "xterm-256color" || "screen-256color"
" 256色
    set t_Co=256
    set t_Sf=<1b>[3%dm
    set t_Sb=<1b>[4%dm
elseif &term =~ "xterm-debian" || &term =~ "xterm-xfree86"
    set t_Co=16
    set t_Sf=<1b>[3%dm
    set t_Sb=<1b>[4%dm
elseif &term =~ "xterm-color"
    set t_Co=8
    set t_Sf=<1b>[3%dm
    set t_Sb=<1b>[4%dm
endif

syntax enable
hi PmenuSel cterm=reverse ctermfg=33 ctermbg=222 gui=reverse guifg=#3399ff guibg=#f0e68c

let mapleader="," "キーマップリーダー

set encoding=utf-8
set fileencodings=utf-8,cp-932,euc-jp
set scrolloff=5
set textwidth=0
set nobackup
set autoread
set noswapfile
set hidden
set backspace=indent,eol,start
set formatoptions=lmoq
set vb t_vb=
set browsedir=buffer
set whichwrap=b,s,h,l,<,>,[,]
set showcmd
set showmode
set viminfo='50,<1000,s100,\"50
set modelines=0
set ignorecase
set clipboard=unnamed
set mouse=a
set guioptions=+a
set ttymouse=xterm2

command! Ev edit $MYVIMRC
command! Rv source $MYVIMRC

" ステータスライン
set laststatus=2

" インデントの設定
set autoindent
set smartindent
set cindent
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set smarttab

" 表示関連
" ファイルタイプ毎の設定
if has('autocmd')
    filetype plugin on
    filetype indent on

    autocmd FileType html :set indentexpr=
    autocmd FileType xhtml :set indentexpr=

    filetype plugin on

    " PEP 8 Indent rule
    autocmd FileType python let g:pydiction_location = '$HOME/.vim/bundle/pydiction/complete-dict'
    autocmd FileType python setl autoindent nosmartindent cindent
    autocmd FileType python setl smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
    autocmd FileType python setl tabstop=8 expandtab shiftwidth=4 softtabstop=4 smarttab
    autocmd FileType python set omnifunc=pythoncomplete#Complete
    autocmd FileType python setl textwidth=80 colorcolumn=80
    autocmd FileType python setl foldmethod=indent foldlevel=99
endif

" マルチバイト文字を入力している最中にカーソルの色を変更する
if has('multi_byte_ime') || ('xim')
    highlight CursorIM guibg=Purple guifg=NONE
endif

" □とか○の文字があってもカーソル位置がずれないようにする
if exists('&ambiwidth')
    set ambiwidth=double
endif

" カレントウィンドウにのみ罫線を引く
if has('autocmd')
    augroup cch
        autocmd! cch
        autocmd WinLeave * set nocursorline
        autocmd WinEnter,BufRead * set cursorline
    augroup END
endif

set showmatch
set number
set list
set listchars=tab:»-,trail:-,eol:↲,extends:»,precedes:«,nbsp:%
set display=uhex
set cursorline
set hlsearch

" Esc連打でハイライトを消す
nnoremap <Esc><Esc> :nohlsearch<CR><Esc>

set lazyredraw
set ttyfast

" 移動関連
nnoremap h <Left>
nnoremap j gj
nnoremap k gk
nnoremap l <Right>
nnoremap <Down> gj
nnoremap <Up>   gk

inoremap <C-e> <END>
inoremap <C-a> <HOME>

inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-h> <Left>
inoremap <C-l> <Right>

noremap <Space>j <C-f>
noremap <Space>k <C-b>

nnoremap gb '[
nnoremap gp ']

nnoremap vy vawy

nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

nnoremap ( %
nnoremap ) %

" 編集関連
source $VIMRUNTIME/macros/matchit.vim
set tags+=tags

" insertモードを抜けるとIMEをOFF
set noimdisable
set iminsert=0 imsearch=0
set noimcmdline
inoremap <Silent> <ESC> <ESC>:set iminsert=0<CR>
" inoremap <Silent> <ESC> <ESC>set iminsert=0<CR>

" 行末の時に迷惑
" コンマの後に空白を追加
" inoremap , ,<Space>

"<Leader><Leader>で変更があれば保存
noremap <Leader><Leader> :up<CR>

" XMLの閉じタグを挿入する
augroup MyXML
    autocmd!
    autocmd FileType xml inoremap <buffer> </ </<C-x><C-o>
augroup END

" 保存時に行末の空白を除去する
autocmd BufWritePre * :%s/\s\+$//ge
" 保存時にtabをスペースに変換する
autocmd BufWritePre * :%s/\t/    /ge

"対応する括弧を自動で補完する
" { を入力するたびに ClosePairOn() と ClosePairOff()がToggleする
function! ClosePairOn()
    let b:closepair = 0
    inoremap [ []<Left>
    inoremap { {}<Left>
    inoremap ( ()<Left>
    inoremap " ""<Left>
    inoremap ' ''<Left>
    inoremap ` ``<Left>
endfunction

function! ClosePairOff()
    iunmap [
    iunmap {
    iunmap (
    iunmap "
    iunmap '
    iunmap `
endfunction

function! ToggleClosePair()
    if !exists("g:closepair_status")
        let g:closepair_status = 1
    endif

    if (g:closepair_status)
        let g:closepair_status = 0
        echo "Current: Closing Pair On"
        call ClosePairOn()
    else
        let g:closepair_status = 1
        echo "Current: Closing Pair Off"
        call ClosePairOff()
    endif
endfunction

nnoremap { :call ToggleClosePair()<CR>

filetype off
if has('vim_starting')
    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()
endif

" Execute python script C-P
function! s:ExecPy()
    exe "!" . &ft . " %"
endfunction

command! Exec call <SID>ExecPy()
autocmd FileType python map <silent> <C-P> :call <SID>ExecPy()<CR>

" NERDTree
nmap <Leader>nn :NERDTreeToggle<CR>

"powerline
let g:Powerline_symbols = 'fancy'

" PEP8
map mp :!pep8 %<CR>

"インデントの色
hi IndentGuidesOdd  ctermbg=black
hi IndentGuidesEven ctermbg=darkgrey

" original repos on github
Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/vimfiler'
Bundle 'kana/vim-fakeclip'
Bundle 'kana/vim-smartchr'
Bundle 'thinca/vim-ref'
Bundle 'gmarik/vundle'
Bundle 'mattn/mkdpreview-vim'
Bundle 'h1mesuke/vim-alignta'
Bundle 'thinca/vim-quickrun'
Bundle 'thinca/vim-ref'
Bundle 'scrooloose/syntastic'
Bundle 'Lokaltog/vim-powerline'
Bundle 'mitechie/pyflakes-pathogen'
Bundle 'mattn/vdbi-vim'
Bundle 'ocim/htmljinja.vim'
Bundle 'reinh/vim-makegreen'
Bundle 'lambdalisue/nose.vim'
Bundle 'sontek/rope-vim'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/nerdcommenter'

" Nerd_Commenter の基本設定
let NERDSpaceDelims = 1

" vim-indent-guides
Bundle 'nathanaelkane/vim-indent-guides'
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_guide_size = 4 "インデントの色付け幅
let g:indent_guides_auto_colors = 0 "autoにするとよく見えなかったので自動的に色付けするのはストップ
let g:indent_guides_color_change_percent = 10 "色の変化の幅（？）。パーセンテージらしい
"
"memolist
Bundle 'glidenote/memolist.vim'
map <Leader>mn  :MemoNew<CR>
map <Leader>ml  :MemoList<CR>
map <Leader>mg  :MemoGrep<CR>
"
Bundle 'kien/ctrlp.vim'
Bundle 'mattn/benchvimrc-vim'

"numbers.vim
Bundle 'myusuf3/numbers.vim'
nnoremap <F3> :NumbersToggle<CR>

Bundle 'kien/ctrlp.vim'
let g:ctrlp_cmd = 'CtrlPMixed'
let g:working_path_mode = 'rc'
let g:custom_ignore = {
    \ 'dir': '¥.git¥|vendor/bundle¥|tmp',
    \ 'file': '¥.jpg$¥|¥.jpeg$¥|¥.png$¥|¥.gif$¥|¥.log'
    \ }

" vim-scripts repos
Bundle 'TwitVim'
Bundle 'scratch'
Bundle 'rest.vim'
Bundle 'occur.vim'
Bundle 'Source-Explorer-srcexpl.vim'
Bundle 'trinity.vim'
Bundle 'taglist-plus'
Bundle 'taglist.vim'
Bundle 'snipMate'

" Gundo
Bundle 'Gundo'
nmap U :<C-u>GundoToggle<CR>

Bundle 'sudo.vim'
" non github repos

filetype plugin indent on
