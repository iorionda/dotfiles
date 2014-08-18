# キーバインドをEmacsに
bindkey -e

bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
bindkey "\\ep" history-beginning-search-backward-end
bindkey "\\en" history-beginning-search-forward-end

# ctrl-W,  ctrl-b で単語移動
bindkey "^W" forward-word
bindkey "^B" backward-word

# glob(*)によるインクリメンタルサーチ
# zsh 4.3.10 以降じゃないと動かないと思う
bindkey '^R' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward

# Default completion configuration
# 展開する前に補完候補を表示する(Ctrl-iで補完するようにする)
bindkey "^i" menu-complete

bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
bindkey "\\ep" history-beginning-search-backward-end
bindkey "\\en" history-beginning-search-forward-end

## Command Line Stack [Esc]-[q]
bindkey -a 'q' push-line

## Zaw
bindkey '^xb' zaw-git-branches
bindkey '^xd' zaw-cdr
bindkey '^xh' zaw-history

## peco
bindkey '^r' peco-select-history
bindkey '^xg' peco-git-branch-selector
