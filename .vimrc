"" 基本設定
" 色の設定
set t_Co=256
syntax on
colorscheme wombat256

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
set statusline=%{expand('%:p:t')}\ %<\(%{expand('%:p:h')}\)%=\ %{g:HahHah()}%m%r%y%w%{'[enc='.(&fenc!=''?&fenc:&enc).'][format='.&ff.']'}[%04l,%04c][%p%%]

" インデントの設定
set autoindent
set smartindent
set cindent
set tabstop=2
set shiftwidth=2
set softtabstop=2

if has('autocmd')
  filetype plugin on
  filetype indent on

  autocmd FileType html :set indentexpr=
  autocmd FileType xhtml :set indentexpr=
endif

" 表示関連
set showmatch
set number
set list
set listchars=tab:»-,trail:-,eol:↲,extends:»,precedes:«,nbsp:%
set display=uhex

set cursorline
augroup cch
  autocmd! cch
  autocmd WinLeave * set nocursorline
  autocmd WinEnter,BufRead * set cursorline
augroup END

:hi clear CursorLine
:hi CursorLine gui=underline
highlight CursorLine ctermbg=black guibg=black

:set lazyredraw
:set ttyfast

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

nnoremap <Space><Space> <C-f>
nnoremap <BS><BS> <C-b>

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
" insertモードを抜けるとIMEをOFF
set noimdisable
set iminsert=0 imsearch=0
set noimcmdline
inoremap <Silent> <ESC> <ESC>:set iminsert=0<CR>

" Tabを空白に変換
set expandtab

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
autocmd BufWritePre * :%s/\t/ /ge

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
Bundle 'mattn/hahhah-vim'
Bundle 'scrooloose/nerdtree'

filetype plugin indent on
