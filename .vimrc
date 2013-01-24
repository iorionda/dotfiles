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
set wildmode=list:longest
set wildmenu

" インデントの設定
set autoindent
set smartindent
set cindent
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set smarttab

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

" ruby/rails サポート
NeoBundle 'tpope/vim-rails'
NeoBundle 'ujihisa/unite-rake'
NeoBundle 'basyura/unite-rails'
NeoBundleLazy 'vim-ruby/vim-ruby', { 'autoload' : { 'filetypes': ['ruby', 'eruby', 'haml'] } }
NeoBundleLazy 'skwp/vim-rspec', { 'autoload': { 'filetypes': ['ruby', 'eruby', 'haml'] } }
NeoBundleLazy 'ruby-matchit', { 'autoload' : { 'filetypes': ['ruby', 'eruby', 'haml'] } }
NeoBundleLazy 'Shougo/neocomplcache-rsense', { 'depends': 'Shougo/neocomplcache', 'autoload': { 'filetypes': 'ruby' }}
"
" " original repos on github

NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'tpope/vim-endwise.git'
NeoBundle 'tpope/vim-surround'
NeoBundle 'Shougo/vimproc'
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neosnippet'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimfiler'
NeoBundle 'kana/vim-fakeclip'
NeoBundle 'kana/vim-smartchr'
NeoBundle 'mattn/mkdpreview-vim'
NeoBundle 'h1mesuke/vim-alignta'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'iori-o/vim-powerline'
NeoBundle 'mattn/vdbi-vim'
NeoBundle 'reinh/vim-makegreen'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'glidenote/memolist.vim'
NeoBundle 'myusuf3/numbers.vim'
NeoBundle 'mattn/benchvimrc-vim'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'thinca/vim-singleton'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'majutsushi/tagbar'

" vim-scripts repos
NeoBundle 'scratch'
NeoBundle 'rest.vim'
NeoBundle 'occur.vim'
NeoBundle 'Source-Explorer-srcexpl.vim'
NeoBundle 'trinity.vim'
NeoBundle 'taglist-plus'
NeoBundle 'taglist.vim'
NeoBundle 'Gundo'
NeoBundle 'sudo.vim'
NeoBundle 'project.tar.gz'

"------------------------------------------------
" テキストオブジェクトで置換
"------------------------------------------------
NeoBundle 'kana/vim-operator-replace.git'
NeoBundle 'kana/vim-operator-user.git'
map R  <Plug>(operator-replace)

"------------------------------------------------
"memolist
"------------------------------------------------
map <Leader>mn  :MemoNew<CR>
map <Leader>ml  :MemoList<CR>
map <Leader>mg  :MemoGrep<CR>

"------------------------------------------------
"numbers.vim
"------------------------------------------------
nnoremap <F3> :NumbersToggle<CR>

"------------------------------------------------
" ctrlp
"------------------------------------------------
let g:ctrlp_cmd = 'CtrlPMixed'
let g:working_path_mode = 'rc'
let g:custom_ignore = {
      \ 'dir': '¥.git¥|vendor/NeoBundle¥|tmp',
      \ 'file': '¥.jpg$¥|¥.jpeg$¥|¥.png$¥|¥.gif$¥|¥.log'
      \ }

"------------------------------------------------
" Gundo
"------------------------------------------------
nmap U :<C-u>GundoToggle<CR>

"-------------------------------------------------
" NERDTree
"-------------------------------------------------
nmap <Leader>nn :NERDTreeToggle<CR>


"-------------------------------------------------
" tagbar
"-------------------------------------------------
nnoremap <silent> <F9> :TagbarToggle<CR>
" ctagsはMacVim-kaoriyaの使ってる
let g:tagbar_ctags_bin = '/Applications/MacVim.app/Contents/MacOS/ctags'
" JavaScriptにはjsctagsを使用
let g:tagbar_type_javascript = {
    \ 'ctagsbin' : '~/.nodebrew/current/bin/jsctags'
\ }

"------------------------------------------------
" neocomplcache
"------------------------------------------------
let g:neocomplcache_force_overwrite_completefunc=1
let g:neocomplcache_enable_at_startup = 1

" http://vim-users.jp/2010/11/hack185/
" Plugin key-mappings.
imap <C-k>     <Plug>(neocomplcache_snippets_expand)
smap <C-k>     <Plug>(neocomplcache_snippets_expand)
inoremap <expr><C-g>     neocomplcache#undo_completion()
inoremap <expr><C-l>     neocomplcache#complete_common_string()

" Recommended key-mappings.
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e>  neocomplcache#cancel_popup()

" AutoComplPop like behavior.
"let g:neocomplcache_enable_auto_select = 1
"------------------------------------------------
" rubyの設定
"------------------------------------------------
if filereadable(expand('~/rtags'))
  au FileType ruby,eruby setl tags+=~/rtags
endif

" Rsense "{{{
let g:rsenseHome = "/usr/local/Cellar/rsense/0.3/libexec"
let g:rsenseUseOmniFunc = 1
" }}}

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'

function! SetUpRubySetting()
  setl completefunc=RSenseCompleteFunction
  nmap <buffer>tj :RSenseJumpToDefinition<CR>
  nmap <buffer>tk :RSenseWhereIs<CR>
  nmap <buffer>td :RSenseTypeHelp<CR>
endfunction

autocmd FileType ruby,eruby,ruby.rspec call SetUpRubySetting()
autocmd FileType ruby,eruby,ruby.rspec setl ts=2 sw=2 sts=2 ex
autocmd FileType ruby,eruby,ruby.rspec setl ai nosmartindent cindent
autocmd FileType ruby,eruby,ruby.rspec setl textwidth=80 colorcolumn=80
autocmd FileType ruby,eruby,ruby.rspec setl foldmethod=indent foldlevel=99
"}}}

"------------------------------------------------
" vim-ruby
"------------------------------------------------
compiler ruby
let ruby_space_errors=1

"------------------------------------------------
" coffee-script
"------------------------------------------------
"" syntax + 自動compile
NeoBundle 'kchmck/vim-coffee-script'
" js BDDツール
NeoBundle 'claco/jasmine.vim'

" vimにcoffeeファイルタイプを認識させる
au BufRead,BufNewFile,BufReadPre *.coffee set ft=coffee
" インデントを設定
autocmd FileType coffee setl sw=2 sts=2 ts=2 ex

" taglistの設定 coffeeを追加
let g:tlist_coffee_settings = 'coffee;f:function;v:variable'

"------------------------------------
" vim-coffee-script
"------------------------------------
" 保存時にコンパイル
autocmd BufWritePost *.coffee silent CoffeeMake! -cb | cwindow | redraw!

"------------------------------------
" jasmine.vim
"------------------------------------
" ファイルタイプを変更
function! JasmineSetting()
  au BufRead,BufNewFile *Helper.js,*Spec.js  set ft=jasmine.javascript
  au BufRead,BufNewFile *Helper.coffee,*Spec.coffee  set ft=jasmine.coffee
  au BufRead,BufNewFile,BufReadPre *Helper.coffee,*Spec.coffee  let b:quickrun_config = {'type' : 'coffee'}
  call jasmine#load_snippets()
  map <buffer> <leader>m :JasmineRedGreen<CR>
  command! JasmineRedGreen :call jasmine#redgreen()
  command! JasmineMake :call jasmine#make()
endfunction
au BufRead,BufNewFile,BufReadPre *.coffee,*.js call JasmineSetting()

"------------------------------------
" indent_guides
"------------------------------------
" インデントの深さに色を付ける
let g:indent_guides_start_level=1
let g:indent_guides_auto_colors=0
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_color_change_percent=40
let g:indent_guides_guide_size=1
let g:indent_guides_space_guides=1

hi IndentGuidesOdd  ctermbg=235
hi IndentGuidesEven ctermbg=237

nmap <silent><Leader>ig <Plug>IndentGuidesToggle

"------------------------------------
" HTML
"------------------------------------
NeoBundle 'mattn/zencoding-vim'
NeoBundle 'open-browser.vim'
NeoBundle 'mattn/webapi-vim'
NeoBundle 'tell-k/vim-browsereload-mac'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'othree/html5.vim'
NeoBundle 'pangloss/vim-javascript'

autocmd FileType html :set indentexpr=
autocmd FileType xhtml :set indentexpr=

"------------------------------------
" python
"------------------------------------
NeoBundle 'ocim/htmljinja.vim'
NeoBundle 'mitechie/pyflakes-pathogen'
NeoBundle 'lambdalisue/nose.vim'
NeoBundle 'sontek/rope-vim'

" PEP 8 Indent rule
autocmd FileType python let g:pydiction_location = '$HOME/.vim/NeoBundle/pydiction/complete-dict'
autocmd FileType python setl autoindent nosmartindent cindent
autocmd FileType python setl smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
autocmd FileType python setl ts=8 ex sw=4 sts=4 smarttab
autocmd FileType python setl textwidth=80 colorcolumn=80
autocmd FileType python setl foldmethod=indent foldlevel=99
autocmd FileType python setl omnifunc=pythoncomplete#Complete

" Execute python script C-P
function! s:ExecPy()
  exe "!" . &ft . " %"
endfunction

command! Exec call <SID>ExecPy()
autocmd FileType python map <silent> <C-P> :call <SID>ExecPy()<CR>

" PEP8
map mp :!pep8 %<CR>


"-------------------------------------------------
" javascript
"-------------------------------------------------
NeoBundle 'jiangmiao/simple-javascript-indenter'
NeoBundle 'nono/jquery.vim'
NeoBundle 'jelera/vim-javascript-syntax'
NeoBundle 'teramako/jscomplete-vim'

" simple-javascript-indenter
"-------------------------------------------------
" この設定入れるとshiftwidthを1にしてインデントしてくれる
let g:SimpleJsIndenter_BriefMode = 1
" この設定入れるとswitchのインデントがいくらかマシに
let g:SimpleJsIndenter_CaseIndentLevel = -1

" jquery.vim
"-------------------------------------------------
au BufRead,BufNewFile jquery.*.js set ft=javascript syntax=jquery

" vim-javascript-syntax
"-------------------------------------------------
" DOMとMozilla関連とES6のメソッドを補完
let g:jscomplete_use = ['dom', 'moz', 'es6th']

"syntastic
"-------------------------------------------------
" このようにするとjshintを必ず使ってチェックしてくれるようになる
let g:syntastic_javascript_checker = "jshint"

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
