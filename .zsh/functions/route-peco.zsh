################################################################################
# route-peco
################################################################################
route-peco() {
  rake --help > /dev/null 2>&1
  if [[ $? == 0 ]]; then
    BUFFER="$(rake routes | peco --query "$LBUFFER")"
    CURSOR=$#BUFFER
  fi
  zle clear-screen
}

zle -N route-peco
bindkey '^xr' route-peco
