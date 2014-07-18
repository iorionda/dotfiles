peco-bundle-gem-open() {
  bundle help > /dev/null 2>&1
  if [[ $? == 0 ]]; then
    local selected_dir=$(bundle show --paths | peco)
    if [ -n "$selected_dir" ]; then
      BUFFER="$EDITOR ${selected_dir}"
      zle accept-line
    fi
  fi
  zle clear-screen
}
zle -N peco-bundle-gem-open

bindkey '^bo' peco-bundle-gem-open
