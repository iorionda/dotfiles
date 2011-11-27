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
set rtp+=~/.vim/bundle/vundle
" call vundle#rc()
Bundle 'qmarik/vundle'

filetype plugin indent on
