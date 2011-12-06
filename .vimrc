"" 基本設定
" 起動時にフルスクリーンにする
if has('gui_macvim')
  set fuoptions=maxvert,maxhorz
  au GUIEnter * set fullscreen
endif

let mapleader="," "キーマップリーダー
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

set clipboard+=unnamed

set mouse=a
set guioptions=+a
set ttymouse=xterm2

set clipboard=unnamed

command! Ev edit $MYVIMRC
command! Rv source $MYVIMRC

" ステータスライン
set laststatus=2
set statusline=%{expand('%:p:t')}\ %<\(%{expand('%:p:h')}\)%=\ %m%r%y%w%{'[enc='.(&fenc!=''?&fenc:&enc).'][format='.&ff.']'}[%04l,%04c][%p%%]

" 色の設定
set t_Co=256
syntax on
colorscheme wombat256

" インデント
set cindent
set expandtab
set tabstop=2
set shiftwidth=2

" 行番号の表示
set number

" Vundle.vmで管理しているPluginを読み込む
filetype off
if has('vim_starting')
  set rtp+=~/.vim/bundle/vundle/
  call vundle#rc()
endif

Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/unite.vim'
Bundle 'thinca/vim-ref'
Bundle 'gmarik/vundle'

filetype plugin indent on
