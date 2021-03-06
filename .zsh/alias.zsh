################################################################################
# ls
################################################################################
alias la="ls -a"
alias lf="ls -f"
alias ll="ls -l"

case ${OSTYPE} in
    darwin* | freebsd* )
        if [ -x "$(which gnuls)" ]; then
            alias ls="gnuls"
            alias la="ls -lhAF --color=auto"
        else
            alias la="ls -lhAFG"
        fi
        ;;
    SunOS)
        if [ -x "`which gls`" ]; then
            alias ls="gls"
            alias la="ls -lhAF --color=auto"
        else
            alias la="ls -lhAF"
        fi
        ;;
    *)
        alias la="ls -lhAF --color=auto"
        ;;
esac

################################################################################
# process
################################################################################
alias 'ps?'='pgrep -l -f'
alias pk="pkill -f"

################################################################################
# du/df
################################################################################
alias du="du -h"
# alias df="df -f"
alias duh="du -f ./ --max-depth=1"

################################################################################
#su
################################################################################
alias su="su -l"

################################################################################
# rlwrap
################################################################################
if [ -x /usr/local/bin/rlwrap ]; then
    alias mysql='/usr/local/bin/rlwrap -a -pRED mysql'
fi

################################################################################
# ag
################################################################################
if [ -x /usr/local/bin/ag ]; then
    alias ag="ag --pager 'less -R'"
fi

################################################################################
# rm
################################################################################
alias rr="command rm -rf"

################################################################################
# file
################################################################################
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

################################################################################
# pushd/popd
################################################################################
alias pd="pushd"
alias po="popd"

################################################################################
# diff
################################################################################
# alias diff=colordiff

################################################################################
# tmux
################################################################################
alias tm="tmux"
alias tma="tmux attach"
alias tml="tmux list-window"

# terminalの中でsshしたら新しいwindowを作成する
if [ "$TERM" = "screen-256color-bce" ];then
    function ssh_tmux() {
        eval server=\${$#}
        tmux new-window -n $@ "exec ssh $@"
    }
    alias ssh=ssh_tmux
fi

# terminalの中でmanをしたら新しいpainを作成する
if [ "$TERM" = "screen-256color-bce" ];then
    function man_tmux() {
        eval server=\${$#}
        tmux split-window "exec man $@"
    }
    alias man=man_tmux
fi

################################################################################
# 展開コマンドのオプション
################################################################################
# extract http://d.hatena.ne.jp/jeneshicc/20110215/1297778049
#
extract () {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xvjf $1    ;;
      *.tar.gz)    tar xvzf $1    ;;
      *.tar.xz)    tar xvJf $1    ;;
      *.bz2)       bunzip2 $1     ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1      ;;
      *.tar)       tar xvf $1     ;;
      *.tbz2)      tar xvjf $1    ;;
      *.tgz)       tar xvzf $1    ;;
      *.zip)       unzip $1       ;;
      *.Z)         uncompress $1  ;;
      *.7z)        7z x $1        ;;
      *.lzma)      lzma -dv $1    ;;
      *.xz)        xz -dv $1      ;;
      *)           echo "don't know how to extract '$1'..." ;;
    esac
  else
    echo "'$1' is not a valid file!"
  fi
}
alias ex='extract'
alias rtags='/Applications/MacVim.app/Contents/MacOS/ctags -R --langmap=RUBY:.rb --sort=yes ~/.rbenv/versions/1.9.3-p327 ~/.rbenv/shims -f ~/rtags '

ls_abbrev() {
    # -a : Do not ignore entries starting with ..
    # -C : Force multi-column output.
    # -F : Append indicator (one of */=>@|) to entries.
    local cmd_ls='ls'
    local -a opt_ls
    opt_ls=('-aCF' '--color=always')
    case "${OSTYPE}" in
        freebsd*|darwin*)
            if type gls > /dev/null 2>&1; then
                cmd_ls='gls'
            else
                # -G : Enable colorized output.
                opt_ls=('-aCFG')
            fi
            ;;
    esac

    local ls_result
    ls_result=$(CLICOLOR_FORCE=1 COLUMNS=$COLUMNS command $cmd_ls ${opt_ls[@]} | sed $'/^\e\[[0-9;]*m$/d')

    local ls_lines=$(echo "$ls_result" | wc -l | tr -d ' ')

    if [ $ls_lines -gt 10 ]; then
        echo "$ls_result" | head -n 5
        echo '...'
        echo "$ls_result" | tail -n 5
        echo "$(command ls -1 -A | wc -l | tr -d ' ') files exist"
    else
        echo "$ls_result"
    fi
}

chpwd() {
    ls_abbrev
}

function do_enter() {
    if [ -n "$BUFFER" ]; then
        zle accept-line
        return 0
    fi
    echo
    ls
    # ↓ おすすめ
    # ls_abbrev
    if [ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = 'true' ]; then
        echo
        echo -e "\e[0;33m--- git status ---\e[0m"
        git status -sb
    fi
    zle reset-prompt
    return 0
}
zle -N do_enter
bindkey '^m' do_enter

################################################################################
# hub
################################################################################
function git(){hub "$@"}

################################################################################
# unzip
################################################################################
alias unzip='/usr/local/bin/unzip -Ocp932'

################################################################################
# peco
################################################################################
alias -g B='`git branch | peco | sed -e "s/^\*[ ]*//g"`'
