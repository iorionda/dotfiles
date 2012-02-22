" 色の設定
colorscheme wombat256mod
set transparency=0

" フォントの設定
set guifont=Ricty:h18
set guifontwide=Ricty:h18

" ツールバーの非表示(表示したい場合は-=を+=に）
set guioptions-=T

"インデントの色
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=black    guibg=black    ctermbg=1
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=darkgrey guibg=darkgrey ctermbg=2

