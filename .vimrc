" 起動時にフルスクリーンにする
if has('gui_macvim')
  set fuoptions=maxvert,maxhorz
  au GUIEnter * set fullscreen
endif

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
