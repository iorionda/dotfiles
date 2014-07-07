################################################################################
# peco-git-branch-selector
################################################################################
function peco-git-branch-selector() {
    if [ -f ".git" ]; then
      exit
    fi

    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi

    BUFFER=$(git branch | \
             grep -v "*" | \
             eval $tac | \
             peco --query "$LBUFFER")
    BUFFER="git checkout${BUFFER}"
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-git-branch-selector
bindkey '^xg' peco-git-branch-selector
