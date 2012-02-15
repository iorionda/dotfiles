# users generic .zshrc file for zsh(1)

## Environment variable configuration
typeset -U path
path=(
/bin(N-/)
$HOME/local/bin(N-/)
/usr/local/bin(N-/)
/usr/local/sbin(N-/)
/usr/bin(N-/)
/usr/X11/bin(N-/)
/usr/local/share/python(N-/)
)

typeset -xT SUDO_PATH sudo_path
typeset -U sudo_path
sudo_path=({,/usr}/sbin(N-/))

typeset -U man_path
man_path=(
$HOME/local/share/man(N-/)
/usr/local/share/man(N-/)
/usr/share/man(N-/)
)

typeset -xT RUBYLIB ruby_path
typeset -U ruby_path
ruby_path=(./lib)

typeset -xT PYTHONPATH python_path
typeset -U python_path
python_path=(./lib)

# LANG
export LANG=ja_JP.UTF-8

# less コマンドで使用するエンコード
export LESSCHARSET=utf-8

# エディタの設定
export EDITOR=/Applications/MacVim.app/Contents/MacOS/Vim
alias vi='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/MacOS/Vim "$@"'
alias vim='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/MacOS/Vim "$@"'

# LESSの設定
export LESS='-R'
export LESSOPEN='| ~/local/bin/src-hilite-lesspipe.sh %s'

# キーバインドをEmacsに
bindkey -e

# 新規ディレクトリは755， 新規ファイルは644にする
umask 022

# コマンドライン全てのスペルチェックをする
setopt correct_all

# 上書きリダイレクトの禁止
setopt no_clobber

# synbolic link は実体を追うようにする
setopt chase_links

# sudo も補完対象
zstyle ':completion:*' list-colors di=34 fi=0

# beepを鳴らさない
setopt nolistbeep

# ctrl-W, ctrl-b で単語移動
bindkey "^W" forward-word
bindkey "^B" backward-word

# URLをコピーしたときに自動的にエスケープ
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

################################################################################
# rlwrap
################################################################################
if [ -x /usr/local/bin/rlwrap ]; then
    alias mysql='/usr/local/bin/rlwrap -a -pRED mysql'
fi

################################################################################
# ディレクトリ移動
################################################################################
### ディレクトリ名だけでcdする
setopt auto_cd
### cdで移動した時にpushd(スタック)する
setopt auto_pushd
### ディレクトリが変わったらディレクトリスタックを表示する
chpwd_functions=($chpwd_functions dirs)
### cdコマンドを入力したときにlsを出力する
function chpwd(){ ls -v }
### カレントディレクトリ内に指定したディレクトリがない場合に
### 移動先を検索するリスト
cdpath=(~)
### カレントディレクトリに候補がない場合にだけ cdpath 上のディレクトリを候補にする
zstyle ':completion:*:cd:'tag-order local-directories path-directoris
### cdは親ディレクトリからカレントディレクトリを選択しないので表示させないようにする
zstyle ':completion:*:cd*' ignore-parents parent pwd
### LS-COLORを設定しておく
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
# ファイル保管候補に色を付ける
zstyle ':completion:*:*' list-colors ${(s.:.)LS_COLORS}

################################################################################
# History
################################################################################
### zshプロセス間でヒストリファイルを共有する
setopt no_share_history
### ヒストリファイルにヒストリだけではなく実行時刻と実行時間も保存する
setopt extended_history
### 同じコマンドを実行した時はヒストリに保存しない
setopt hist_ignore_dups
### スペースで始まるコマンドはヒストリに登録しない
setopt hist_ignore_space
### すぐにヒストリファイルに追記する
setopt inc_append_history
### ヒストリを保存するファイル
export HISTFILE=~/.zshhistory
### メモリ上のヒストリ数
export HISTFILESIZE=100000000
### 保存するヒストリ数
export SAVEHIST=$HISTFILESIZE
# 余分な空白は詰める
setopt hist_reduce_blanks
# zsh 4.3.10 以降じゃないと動かないと思う
bindkey '^R' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward
### C-sでヒストリ検索が潰されるので，出力停止・開始用にC-s/C-qを使わない
setopt no_flow_control

################################################################################
# 補完時の挙動の設定
################################################################################
### 補完の初期化
autoload -Uz compinit
compinit

### 補完候補を↑↓←→でも選択できるようにする
### 補完候補が2つ以上なければすぐに補完する
zstyle ':completion:*:default' menu select=2

### 補完候補に色をつける
### 空文字はデフォルトの値を使うという意味
zstyle 'completion:*:default' list-colors ""

## 補完方法の設定。指定した順番に実行する。
### _oldlist 前回の補完結果を再利用する。
### _complete: 補完する。
### _match: globを展開しないで候補の一覧から補完する。
### _history: ヒストリのコマンドも補完候補とする。
### _ignored: 補完候補にださないと指定したものも補完候補とする。
### _approximate: 似ている補完候補も補完候補とする。
### _prefix: カーソル以降を無視してカーソル位置までで補完する。
zstyle ':completion:*' completer _oldlist _complete _match _history _ignored _approximate _prefix

### 色の定義
export TERM=screen-256color-bce
autoload -U colors
colors

Red=%{${fg[red]}%}
Yellow=%{${fg[yellow]}%}
Blue=%{${fg[blue]}%}
Green=%{${fg[green]}%}
Cyan=%{${fg[cyan]}%}
White=%{${fg[white]}%}
Reset=%{${fg[reset_color]}%}
Default=%{${fg[default]}%}

zstyle ':completion:*:messages' format ${Yellow}'%d'${Reset}
zstyle ':completion:*:warnings' format ${Red}'No matches for:'${Yellow}' %d'${Reset}
zstyle ':completion:*:descriptions' format ${Yellow}'completing %B%d%b'${Reset}
zstyle ':completion:*:corrections' format ${Yellow}'%B%d '${Red}'(errors: %e)%b'${Reset}
zstyle ':completion:*:options' description 'yes'

# グループ名に空文字列を指定すると，マッチ対象のタグ名がグループ名に使われる。
# したがって，すべての マッチ種別を別々に表示させたいなら以下のようにする
zstyle ':completion:*' group-name ''
### 補完候補をキャッシュする
zstyle 'completion:*' use-cache yes
### 詳細な情報を表示する
zstyle 'completion:*' verbose yes
### sudo時にはsudo用のパスを使う
zstyle 'completion:sudo:*' environ PATH="$SUDO_PATH:$PATH"
### オブジェクトファイルとか中間ファイルは補完させない
zstyle 'completion:*:*files' ignored-patterns '*?.0' '*?~' '*\#'

### 語の途中でもカーソル位置まで補完
setopt complete_in_word
### カーソル位置は保持したままファイル名一覧を順次その場で表示
setopt always_last_prompt
### globを展開しないで候補の一覧から補完する
setopt hist_expand
### 補完候補がない時にbeepを鳴らさない
setopt no_beep
### 辞書順ではなく数字順に並べる
setopt numeric_glob_sort
### 補完候補一覧でファイルの種別を識別マーク表示(ls -Fの記号)
setopt list_types
### 補完キー連打で順に補完候補を自動で補完s
setopt auto_menu
# 補完候補が複数ある場合に，一覧表示する
setopt auto_list

### コマンドラインでも # 以降をコメントとして扱う
setopt interactive_comments
### 日本語ファイル名など8bitを通す
setopt print_eight_bit
### 明確なドットの指定なしで.から始まるファイルをマッチ
setopt globdots
# 補完リダイレクトやパイプなど，必要に応じて tee や cat の機能が使われる
setopt multios
### 展開する前に補完候補を表示する(Ctrl-iで補完するようにする)
bindkey "^i" menu-complete

################################################################################
# 展開
################################################################################
### カッコの対応を自動的に補完
setopt auto_param_keys
### ディレクトリ名の補完で末尾の/を自動的に付加し，次の補完に備える
setopt auto_param_slash
### ファイル名の展開でディレクトリにマッチした場合，末尾に/を付加する
setopt mark_dirs
# 最後がディレクトリ名で終わっている場合，末尾の / を自動的に取り除かない
setopt noautoremoveslash
### コマンドライン引数で --prefix=/usr みたいな = 以降でも補完する
setopt magic_equal_subst
### 拡張globで補完(~とか^とか． 例えば less *.txt~memo.txt なら memo.txt 以外の*.txtにマッチする)
setopt extended_glob

################################################################################
# ジョブ
################################################################################
### jobsでプロセスIDも表示する
setopt long_list_jobs

################################################################################
# 実行時間
################################################################################
### 実行したプロセスの消費時間が3秒以上なら
### 自動的に消費時間の統計情報を表示する
export PREPORTTIME=3

################################################################################
# エイリアスの設定
################################################################################
### コマンド検索にハッシュテーブルを使用しない
unhash -am '*'

case ${OSTYPE} in
    darwin* | freebsd* )
        if [ -x "$(which gnuls)" ]; then
            alias ls="gnuls"
            alias la="ls -lhAF --color=auto"
        else
            alias la="ls -lhAFG"
        fi
        alias ps="ps -fU$(whoami)"
        ;;
    SunOS)
        if [ -x "`which gls`" ]; then
            alias ls="gls"
            alias la="ls -lhAF --color=auto"
        else
            alias la="ls -lhAF"
        fi
        alias ps="ps -fl -u$(/usr/xpg4/bin/id -un)"
        ;;
    *)
        alias la="ls -lhAF --color=auto"
        alias ps="psfU$(whoami) --forest"
        ;;
esac

### 完全に削除
alias rr="command rm -rf"

### ファイル操作を都度確認する
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

### pushd/popdのエイリアス
alias pd="pushd"
alias po="popd"


### GUU grepがあったら優先して使う
if type ggrep > /dev/null 2>&1; then
    alias grep=ggrep
fi
### デフォルトオプションの設定
export GREP_OPTIONS
### perl-regexpかextend-regexpを設定する
if grep --help | grep -q -- --perl-regexp; then
    GREP_OPTIONS="--perl-regexp $GREP_OPTIONS"
else
    GREP_OPTIONS="--extend-regexp $GREP_OPTIONS"
fi
### バイナリファイルにはマッチさせない
GREP_OPTIONS="--binary-file=without-match"
### 管理用のディレクトリは無視する
if grep --help | grep -q -- --exclude-dir; then
    GREP_OPTIONS="--exclude-dir=.svn $GREP_OPTIONS"
    GREP_OPTIONS="--exclude-dir=.git $GREP_OPTIONS"
fi

### 可能なら色をつける
if grep --help | grep -q -- --color; then
    GREP_OPTIONS="--color=auto $GREP_OPTIONS"
fi

alias grep='grep --ignore-case' $GREP_OPTIONS

alias diff=colordiff
export GIT_PAGER='/usr/local/bin/lv -c -Au8'

alias vim='mvim'
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias view='vim -R'

################################################################################
# プロンプト
################################################################################
autoload -Uz add-zsh-hook
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn hg bzr
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' acrtionformats '(%s)-[%b|%a]'
zstyle ':vcs_info:*bzr:*' use-simple true

autoload -Uz is-at-least
if is-at-least 4.3.10; then
    zstyle ':vcs-info:git:*' check-for-changes true
    zstyle ':vcs-info:git:*' stagedstr '+'
    zstyle ':vcs-info:git:*' unstagedstr '-'
    zstyle ':vcs-info:git:*' formats '(%s)-[%c%u%b]'
    zstyle ':vcs-info:git:*' actionformats '(%s)-[%c%u%b|%a]'
fi

function git_not_pushd() {
if [ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" = "true" ]; then
    head="$(git rev-parse HEAD)"
    for x in $(git rev-parse --remotes)
    do
        if [ "$head" = "$x" ]; then
            return 0
        fi
    done
    echo "NOT PUSHED"
fi
return 0
}

function update_vcs_info_msg() {
psvar=()
LANG=en_US.UTF-8 vcs_info
psvar[2]=$(git_not_pushd)
[[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
add-zsh-hook precmd update_vcs_info_msg
local prompt_smiley="%(?,%{%F{green}%}☺,%{%F{red}%}☹%{%f%})"
local prompt_self="%B%{%F{green}%}(%n@%m)%b%{%f%}"
local prompt_status="%(?,%{%F{green}%}(%?,%{%F{red}%}(%?))%{%f%}"
local prompt_path="%{%F{cyan}%}[%(5~,%-2~/.../%2~,%~)]%{%f%}"
local prompt_date="%{%F{red}%}<%D{%Y-%m-%d %H:%M}>%{%f%}"
local prompt_vcs_info="%{%F{green}%}%B%1(v|%1v|)%{%f%}%b"
local prompt_history="%B%{%F{green}%}[%h]%{%f%}%b"
local prompt_job="%(1j,(%j),)"

case ${UID} in
    0)
        PROMPT
        PROMPT2
        SPROPT
        RPROMPT
        ;;
    *)
        PROMPT="${prompt_history}-${prompt_self}-${prompt_path}-${prompt_smiley} ${prompt_status}-${prompt_date} %# "
        PROMPT2=' >>>'
        #    SPROMPT="${Red}%r is correct? [n, y, a, e]:${Default}"
        RPROMPT="${prompt_vcs_info}"
        ;;
esac

################################################################################
# Virtualenv
################################################################################
### virtualenvのルートディレクトリにする場所
WORKON_HOME=${HOME}/.virtualenvs

### パッケージをvirtualenv環境下にインストール
export PIP_RESPECT_VIRTUALENV=true
### virtualwrapperの読み込み
if [ -f /usr/local/share/python/virtualenvwrapper.sh ]; then
    source /usr/local/share/python/virtualenvwrapper.sh
fi
setopt nonomatch

################################################################################
# GEOS
################################################################################
export GEOS_LIBRARY_PATH='/usr/local/lib/libgeos_c.so'
export GEOS_DIR='/usr/local'

################################################################################
# RVM
################################################################################
# [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # This loads RVM into a shell session.
# show rvm prompt like "1.9.2@gemset_name"
#function rvm_prompt {
#    result=`rvm-prompt v g 2> /dev/null`
#    if [ "$result" ] ; then
#        echo "[$result]"
#    fi
#}
################################################################################
# rbenv
################################################################################
path=($HOME/.rbenv/bin(N) $path)
eval "$(rbenv init -)"

################################################################################
# node.js
################################################################################
export NODE_PATH=/usr/local/lib/node:$NODE_PATH

################################################################################
# Gisty
################################################################################
export GISTY_DIR="$HOME/dev/gists"

################################################################################
# incr
################################################################################
# source ~/.zsh/plugins/incr*.zsh

# # echo Now zsh version $ZSH_VERSION start.

################################################################################
# tmux
################################################################################
# terminalの中でsshしたら新しいwindowを作成する
if [ "$TERM" = "screen" ];then
    function ssh_tmux() {
        eval server=\${$#}
        tmux new-window -n $@ "exec ssh $@"
    }
    alias ssh=ssh_tmux
fi

# terminalの中でmanをしたら新しいpainを作成する
if [ "$TERM" = "screen" ];then
    function man_tmux() {
        eval server=\${$#}
        tmux split-window "exec man $@"
    }
    alias man=man_tmux
fi

################################################################################
# プロジェクト用
################################################################################
alias world="cd ~/local/proj/zerostart/world"
alias lotte="cd ~/local/proj/zerostart/lotte/"
alias aucfan="cd ~/local/proj/zerostart/aucfan/"

alias solr_start="cd $HOME/local/src/apache-solr-3.5.0/example/ && java -Dsolr.solr.home=multicore -jar start.jar"
################################################################################
# 起動時
################################################################################
# if [ ! "$WINDOW" ]; then
#    exec screen -S main -xRR
# fi

