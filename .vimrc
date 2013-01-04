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
    autocmd FileType python let g:pydiction_location = '$HOME/.vim/NeoBundle/pydiction/complete-dict'
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

"-------------------------------------------------
" neoNeoBundle.vim
"-------------------------------------------------
set nocompatible
filetype plugin indent off

if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

" original repos on github
NeoBundle 'Shougo/neocomplcache'
" NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimfiler'
NeoBundle 'kana/vim-fakeclip'
NeoBundle 'kana/vim-smartchr'
" NeoBundle 'thinca/vim-ref'
NeoBundle 'mattn/mkdpreview-vim'
NeoBundle 'h1mesuke/vim-alignta'
NeoBundle 'thinca/vim-quickrun'
" NeoBundle 'thinca/vim-ref'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'Lokaltog/vim-powerline'
NeoBundle 'mitechie/pyflakes-pathogen'
NeoBundle 'mattn/vdbi-vim'
NeoBundle 'ocim/htmljinja.vim'
NeoBundle 'reinh/vim-makegreen'
NeoBundle 'lambdalisue/nose.vim'
NeoBundle 'sontek/rope-vim'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'scrooloose/nerdcommenter'

" Nerd_Commenter の基本設定
let NERDSpaceDelims = 1

" vim-indent-guides
NeoBundle 'nathanaelkane/vim-indent-guides'
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_guide_size = 4 "インデントの色付け幅
let g:indent_guides_auto_colors = 0 "autoにするとよく見えなかったので自動的に色付けするのはストップ
let g:indent_guides_color_change_percent = 10 "色の変化の幅（？）。パーセンテージらしい
"
"memolist
NeoBundle 'glidenote/memolist.vim'
map <Leader>mn  :MemoNew<CR>
map <Leader>ml  :MemoList<CR>
map <Leader>mg  :MemoGrep<CR>

NeoBundle 'kien/ctrlp.vim'
NeoBundle 'mattn/benchvimrc-vim'

"numbers.vim
NeoBundle 'myusuf3/numbers.vim'
nnoremap <F3> :NumbersToggle<CR>

NeoBundle 'kien/ctrlp.vim'
let g:ctrlp_cmd = 'CtrlPMixed'
let g:working_path_mode = 'rc'
let g:custom_ignore = {
    \ 'dir': '¥.git¥|vendor/NeoBundle¥|tmp',
    \ 'file': '¥.jpg$¥|¥.jpeg$¥|¥.png$¥|¥.gif$¥|¥.log'
    \ }

" vim-scripts repos
NeoBundle 'TwitVim'
NeoBundle 'scratch'
NeoBundle 'rest.vim'
NeoBundle 'occur.vim'
NeoBundle 'Source-Explorer-srcexpl.vim'
NeoBundle 'trinity.vim'
NeoBundle 'taglist-plus'
NeoBundle 'taglist.vim'
NeoBundle 'snipMate'

" Gundo
NeoBundle 'Gundo'
nmap U :<C-u>GundoToggle<CR>

NeoBundle 'sudo.vim'
" non github repos

" その他
NeoBundle 'Shougo/vimproc', {'build' : {'mac' : 'make -f make_mac.mak',}, }
NeoBundle 'tpope/vim-endwise.git'
NeoBundle 'ruby-matchit'
NeoBundle 'vim-scripts/dbext.vim'

" railsサポート
NeoBundle 'romanvbabenko/rails.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'ujihisa/unite-rake'
NeoBundle 'basyura/unite-rails'

" reference環境
NeoBundle 'thinca/vim-ref'
NeoBundle 'taichouchou2/vim-ref-ri'
NeoBundle 'taq/vim-rspec'

" rubyの設定
if !exists('g:neocomplcache_omni_functions')
  let g:neocomplcache_omni_functions = {}
endif
let g:neocomplcache_omni_functions.ruby = 'RSenseCompleteFunction'

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.ruby       = '[^. *\t]\.\w*\|\h\w*::'

if filereadable(expand('~/rtags'))
  au FileType ruby,eruby setl tags+=~/rtags
endif

"------------------------------------
" vim-rsense
"------------------------------------
"{{{
" Rsense
let g:rsenseUseOmniFunc = 1
let g:rsenseHome = "/usr/local/Cellar/rsense/0.3/libexec"

function! SetUpRubySetting()
  setlocal completefunc=RSenseCompleteFunction
  nmap <buffer>tj :RSenseJumpToDefinition<CR>
  nmap <buffer>tk :RSenseWhereIs<CR>
  nmap <buffer>td :RSenseTypeHelp<CR>
endfunction
autocmd FileType ruby,eruby,ruby.rspec call SetUpRubySetting()
"}}}

"-------------------------------------------------
" neoNeoBundle.vim
"-------------------------------------------------
filetype plugin indent on

" Installation check.
if neobundle#exists_not_installed_bundles()
  echomsg 'Not installed bundles : ' .
        \ string(neobundle#get_not_installed_bundle_names())
  echomsg 'Please execute ":NeoBundleInstall" command.'
  "finish
endif
