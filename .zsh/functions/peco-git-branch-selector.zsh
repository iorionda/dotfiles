################################################################################
# peco-git-branch-selector
################################################################################
function peco-git-branch-selector() {
  local tac
  if which tac > /dev/null; then
    tac="tac"
  else
    tac="tail -r"
  fi

  git rev-parse --git-dir >/dev/null 2>&1
  if [[ $? == 0 ]]; then
    git checkout $(git branch | grep -v "*" | eval $tac | peco --query "$LBUFFER")
  fi
  zle reset-prompt
}

zle -N peco-git-branch-selector
bindkey '^xg' peco-git-branch-selector
