" -*- coding:utf-8 -*-
" 基本設定
set guifont=Ricty:h18
set guifontwide=Ricty:h18

" 色の設定

if &term =~ "screen-256color-bce"
    set t_Co=256
    syntax on
    colorscheme wombat256mod
else
    syntax on
    colorscheme wombat256mod
endif

" 起動時にフルスクリーンにする
if has('gui_macvim')
    set fuoptions=maxvert,maxhorz
    au GUIEnter * set fullscreen
endif

let mapleader="," "キーマップリーダー
set encoding=utf-8
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
    autocmd FileType python setl omnifunc=pysmell#Complete
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

" カーソル行をハイライト
set cursorline
" カレントウィンドウにのみ罫線を引く
if has('autocmd')
    augroup cch
        autocmd! cch
        autocmd WinLeave * set nocursorline
        autocmd WinEnter,BufRead * set cursorline
    augroup END
endif

hi clear CursorLine
hi CursorLine gui=underline
highlight CursorLine ctermbg=black guibg=black

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
inoremap <Silent> <ESC> <ESC>set iminsert=0<CR>

" コンマの後に空白を追加
inoremap , ,<Space>
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

" original repos on github
Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/unite.vim'
Bundle 'thinca/vim-ref'
Bundle 'gmarik/vundle'
Bundle 'mattn/mkdpreview-vim'
Bundle 'Shougo/vimfiler'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'h1mesuke/vim-alignta'
Bundle 'thinca/vim-quickrun'
Bundle 'scrooloose/syntastic'
Bundle 'Lokaltog/vim-powerline'
Bundle 'kana/vim-fakeclip'
Bundle 'mitechie/pyflakes-pathogen'

" vim-scripts repos
Bundle 'Gundo'
Bundle 'QuickBuf'
Bundle 'TwitVim'
Bundle 'scratch'
Bundle 'rest.vim'
Bundle 'occur.vim'
Bundle 'Source-Explorer-srcexpl.vim'
Bundle 'trinity.vim'
Bundle 'pep8'
Bundle 'taglist-plus'
Bundle 'taglist.vim'
Bundle 'snipMate'

" Gundo
nmap U :<C-u>GundoToggle<CR>

" vim-indent-guides
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_guide_size = 4 "インデントの色付け幅
let g:indent_guides_auto_colors = 0 "autoにするとよく見えなかったので自動的に色付けするのはストップ
let g:indent_guides_color_change_percent = 10 "色の変化の幅（？）。パーセンテージらしい
"インデントの色
hi IndentGuidesOdd  ctermbg=black
hi IndentGuidesEven ctermbg=darkgrey

" QuickBUf
let g:qb_hotkey="<Space><Space>"

"Bundle 'Pydiction'
if has('autocmd')
    autocmd FileType python set complete+=k~/.vim/bundle/pydiction/complete-dict iskeyword+=.,(
endif

" non github repos
filetype plugin indent on

