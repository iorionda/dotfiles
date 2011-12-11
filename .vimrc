"" 基本設定
" 色の設定
" if &term =~ "xterm-256color"
  set t_Co=256
  syntax on
  colorscheme wombat256
" endif

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
set statusline=%{expand('%:p:t')}\ %<\(%{expand('%:p:h')}\)%=\ %{g:HahHah()}%m%r%y%w%{'[enc='.(&fenc!=''?&fenc:&enc).'][format='.&ff.']'}[%04l,%04c/%04L][%p%%]

" インデントの設定
set autoindent
set smartindent
set cindent
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set smarttab

if has('autocmd')
  filetype plugin on
  filetype indent on

  autocmd FileType html :set indentexpr=
  autocmd FileType xhtml :set indentexpr=

  if has('python')
      filetype plugin on
      autocmd FileType python let g:pydiction_location = '~/.vim/bundle/pydiction/complete-dict'
      autocmd FileType python setl autoindent
      autocmd FileType python setl smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
      autocmd FileType python setl tabstop=8 expandtab shiftwidth=4 softtabstop=4
      autocmd FileType python set omnifunc=pysmell#Complete
  endif
endif

" 表示関連
" □とか○の文字があってもカーソル位置がずれないようにする
if exists('&ambiwidth')
  set ambiwidth=double
endif

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

" Vundle.vmで管理しているPluginを読み込む
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
Bundle 'mattn/hahhah-vim'
Bundle 'scrooloose/nerdtree'
Bundle 'Shougo/vimfiler'
if has('python')
  Bundle 'ehamberg/vim-cute-python'
  Bundle 'klen/python-mode'
endif

" vim-scripts repos
Bundle 'QuickBuf'
Bundle 'TwitVim'
if has('python')
  Bundle 'Pydiction'
  Bundle 'pep8'
endif

" non github repos
filetype plugin indent on

" プラグインの設定
" QuickBuf
let g:qb_hotkey="<Space><Space>"

" python-mode
filetype off
filetype plugin indent on
syntax on

if has('python')
    " Disable pylint checking every save
    let g:pymode_lint_write = 0
    " Set key 'R' for run python code
    let g:pymode_run_key = 'R'
    " Load show documentation plugin
    let g:pymode_doc = 1
    " Key for show python documentation
    let g:pymode_doc_key = 'K'
    " Executable command for documentation search
    let g:pydoc = 'pydoc'
    " Load run code plugin
    let g:pymode_run = 1
    " Key for run python code
    let g:pymode_run_key = '<leader>r'
    " Load pylint code plugin
    let g:pymode_lint = 1
    " Switch pylint or pyflakes code checker
    " values (pylint, pyflakes)
    let g:pymode_lint_checker = "pylint"
    " Pylint configuration file
    " If file not found use 'pylintrc' from python-mode plugin directory
    let g:pymode_lint_config = "$HOME/.pylintrc"
    " Check code every save
    let g:pymode_lint_write = 1
    " Auto open cwindow if errors be finded
    let g:pymode_lint_cwindow = 1
    " Auto jump on first error
    let g:pymode_lint_jump = 0
    " Place error signs
    let g:pymode_lint_signs = 1
    " Minimal height of pylint error window
    let g:pymode_lint_minheight = 3
    " Maximal height of pylint error window
    let g:pymode_lint_maxheight = 6
    " Load rope plugin
    let g:pymode_rope = 1
    " Auto create and open ropeproject
    let g:pymode_rope_auto_project = 1
    " Enable autoimport
    let g:pymode_rope_enable_autoimport = 1
    " Auto generate global cache
    let g:pymode_rope_autoimport_generate = 1
    let g:pymode_rope_autoimport_underlineds = 0
    let g:pymode_rope_codeassist_maxfixes = 10
    let g:pymode_rope_sorted_completions = 1
    let g:pymode_rope_extended_complete = 1
    let g:pymode_rope_autoimport_modules = ["os","shutil","datetime"]
    let g:pymode_rope_confirm_saving = 1
    let g:pymode_rope_global_prefix = "<C-x>p"
    let g:pymode_rope_local_prefix = "<C-c>r"
    let g:pymode_rope_vim_completion = 1
    let g:pymode_rope_guess_project = 1
    let g:pymode_rope_goto_def_newwin = 0
    let g:pymode_rope_always_show_complete_menu = 0
    " Load motion plugin
    let g:pymode_motion = 1
    " Load breakpoints plugin
    let g:pymode_breakpoint = 1
    " Key for set/unset breakpoint
    let g:pymode_breakpoint_key = '<leader>b'
    " Autoremove unused whitespaces
    let g:pymode_utils_whitespaces = 1
    " Auto fix vim python paths if virtualenv enabled
    let g:pymode_virtualenv = 1
    " Set default pymode python indent options
    let g:pymode_options_indent = 1
    " Set default pymode python fold options
    let g:pymode_options_fold = 1
    " Set default pymode python other options
    let g:pymode_options_other = 1
    " Enable pymode's custom syntax highlighting
    let g:pymode_syntax = 1
    " Enable all python highlightings
    let g:pymode_syntax_all = 1
    " Highlight "print" as function
    let g:pymode_syntax_print_as_function = 0
    " Highlight indentation errors
    let g:pymode_syntax_indent_errors = g:pymode_syntax_all
    " Highlight trailing spaces
    let g:pymode_syntax_space_errors = g:pymode_syntax_all
    " Highlight string formatting
    let g:pymode_syntax_string_formatting = g:pymode_syntax_all
    " Highlight str.format syntax
    let g:pymode_syntax_string_format = g:pymode_syntax_all
    " Highlight string.Template syntax
    let g:pymode_syntax_string_templates = g:pymode_syntax_all
    " Highlight doc-tests
    let g:pymode_syntax_doctests = g:pymode_syntax_all
    " Highlight builtin objects (__doc__, self, etc)
    let g:pymode_syntax_builtin_objs = g:pymode_syntax_all
    " Highlight builtin functions
    let g:pymode_syntax_builtin_funcs = g:pymode_syntax_all
    " Highlight exceptions
    let g:pymode_syntax_highlight_exceptions = g:pymode_syntax_all
    " For fast machines
    let g:pymode_syntax_slow_sync = 0
endif
