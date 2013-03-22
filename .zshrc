# users generic .zshrc file for zsh(1)

## Environment variable configuration
# LANG
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8

## Default LESS configuration
export LESS='-R'
export LESSOPEN='| ~/local/bin/src-hilite-lesspipe.sh %s'

## Keybind configuration
# emacs like keybind (e.x. Ctrl-a goes to head of a line and Ctrl-e goes
#   to end of it)
bindkey -e

# 素早くvvを入力した時に最後に実行したvimを実行する
bindkey -s "vv" "!vi\n"

# 素早く:qを入力した時に端末を終了する
bindkey -s ':q' "^A^Kexit\n"

# エラーメッセージ本文出力に色付け
e_normal=`echo -e "¥033[0;30m"`
e_RED=`echo -e "¥033[1;31m"`
e_BLUE=`echo -e "¥033[1;36m"`

function make() {
    LANG=C command make "$@" 2>&1 | sed -e "s@[Ee]rror:.*@$e_RED&$e_normal@g" -e "s@cannot¥sfind.*@$e_RED&$e_normal@g" -e "s@[Ww]arning:.*@$e_BLUE&$e_normal@g"
}
function cwaf() {
    LANG=C command ./waf "$@" 2>&1 | sed -e "s@[Ee]rror:.*@$e_RED&$e_normal@g" -e "s@cannot¥sfind.*@$e_RED&$e_normal@g" -e "s@[Ww]arning:.*@$e_BLUE&$e_normal@g"
}

## Default shell configuration
autoload colors
colors

DEFAULT=%{${fg[default]}%}
RESET="%{${reset_color}%}"
GREEN="%{${fg[green]}%}"
BLUE="%{${fg[blue]}%}"
RED="%{${fg[red]}%}"
CYAN="%{${fg[cyan]}%}"
WHITE="%{${fg[white]}%}"

case ${UID} in
    0)
        PROMPT="%B%{${fg[red]}%}%/#%{${reset_color}%}%b "
        PROMPT2="%B%{${fg[red]}%}%_#%{${reset_color}%}%b "
        SPROMPT="%B%{${fg[red]}%}%r is correct? [n, y, a, e]:%{${reset_color}%}%b "
        [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
            PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
        ;;
    *)

        # Show git branch when you are in git repository
        # http://d.hatena.ne.jp/mollifier/20100906/p1
        autoload -Uz add-zsh-hook
        autoload -Uz vcs_info

        zstyle ':vcs_info:*' enable git svn hg bzr
        zstyle ':vcs_info:*' formats '(%s)-[%b]'
        zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
        zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
        zstyle ':vcs_info:bzr:*' use-simple true

        autoload -Uz is-at-least
        if is-at-least 4.3.10; then
          # この check-for-changes が今回の設定するところ
          zstyle ':vcs_info:git:*' check-for-changes true
          zstyle ':vcs_info:git:*' stagedstr "+" # 適当な文字列に変更する
          zstyle ':vcs_info:git:*' unstagedstr "-" # 適当な文字列に変更する
          zstyle ':vcs_info:git:*' formats '(%s)-[%c%u%b]'
          zstyle ':vcs_info:git:*' actionformats '(%s)-[%c%u%b|%a]'
        fi

        function _update_vcs_info_msg() {
            psvar=()
            LANG=en_US.UTF-8 vcs_info
            psvar[2]=$(_git_not_pushed)
            [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
        }
        add-zsh-hook precmd _update_vcs_info_msg

        function _git_not_pushed() {
            if [ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" = "true" ]; then
                head="$(git rev-parse HEAD)"
                for x in $(git rev-parse --remotes)
                do
                    if [ "$head" = "$x" ]; then
                        return 0
                    fi
                done
                echo "{?}"
            fi
            return 0
        }

        PROMPT="${RESET}${GREEN}${WINDOW:+"[$WINDOW]"}${RESET}${WHITE}[%D{%Y-%m-%d %H:%M}]${RESET}-%B${BLUE}%n@%m%b${RESET}-%(?,${GREEN}(%?,${RED}(%?))${RESET} %% "
        PROMPT2=" >>>"
        SPROMPT="${RED}%r is correct? [n,  y,  a,  e]:${RESET}"
        RPROMPT="%1(v|%F${CYAN}%1v%2v%f|)${vcs_info_git_pushed}${RESET}${WHITE}[${BLUE}%(5~, %-2~/.../%2~, %~)% ${WHITE}]${WINDOW:+"[$WINDOW]"} ${RESET}"
    ;;
esac

setopt transient_rprompt
## Command history configuration
# zshプロセス間でヒストリファイルを共有する
setopt no_share_history

# ヒストリファイルにヒストリだけではなく実行時刻と実行時間も保存する
setopt extended_history

# 同じコマンドを実行した時はヒストリに保存しない
setopt hist_ignore_dups

# スペースで始まるコマンドはヒストリに登録しない
setopt hist_ignore_space

# すぐにヒストリファイルに追記する
setopt inc_append_history

# ヒストリを保存するファイル
export HISTFILE=~/.zsh_history

# メモリ上のヒストリ数
export HISTFILESIZE=100000000

# 保存するヒストリ数
export SAVEHIST=$HISTFILESIZE

# 余分な空白は詰める
setopt hist_reduce_blanks

# C-sでヒストリ検索が潰されるので，出力停止・開始用にC-s/C-qを使わない
setopt no_flow_control

# コマンド名に / が含まれているとき PATH 中のサブディレクトリを探す
setopt path_dirs

# historical backward/forward search with linehead string binded to ^P/^N
#
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
bindkey "\\ep" history-beginning-search-backward-end
bindkey "\\en" history-beginning-search-forward-end

# glob(*)によるインクリメンタルサーチ
bindkey '^R' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward

## Default directory configuration
# ディレクトリ名だけでcdする
setopt auto_cd

# cdで移動した時にpushd(スタック)する
setopt auto_pushd

# ディレクトリが変わったらディレクトリスタックを表示する
chpwd_functions=($chpwd_functions dirs)

# cdコマンドを入力したときにlsを出力する
function chpwd(){ ls -v }

# カレントディレクトリ内に指定したディレクトリがない場合に移動先を検索するリスト
cdpath=(~)

# カレントディレクトリに候補がない場合にだけ cdpath 上のディレクトリを候補にする
zstyle ':completion:*:cd:'tag-order local-directories path-directoris

# cdは親ディレクトリからカレントディレクトリを選択しないので表示させないようにする
zstyle ':completion:*:cd*' ignore-parents parent pwd

# ファイル保管候補に色を付ける
zstyle ':completion:*:*' list-colors ${(s.:.)LS_COLORS}

# ctrl-W,  ctrl-b で単語移動
bindkey "^W" forward-word
bindkey "^B" backward-word

# back-wordでの単語境界の設定
autoload -Uz select-word-style
select-word-style default
zstyle ':zle:*' word-chars " _-./;@"
zstyle ':zle:*' word-style unspecified

## Prediction configuration
autoload predict-on
#predict-off

## Command Line Stack [Esc]-[q]
bindkey -a 'q' push-line

# Default completion configuration
# 展開する前に補完候補を表示する(Ctrl-iで補完するようにする)
bindkey "^i" menu-complete

# 補完の初期化
autoload -Uz compinit
compinit

# 補完候補を↑↓←→でも選択できるようにする
# 補完候補が2つ以上なければすぐに補完する
zstyle ':completion:*:default' menu select=2

# 補完候補に色をつける
# 空文字はデフォルトの値を使うという意味
zstyle 'completion:*:default' list-colors ""

## 補完方法の設定。指定した順番に実行する。
# _oldlist 前回の補完結果を再利用する。
# _complete: 補完する。
# _match: globを展開しないで候補の一覧から補完する。
# _history: ヒストリのコマンドも補完候補とする。
# _ignored: 補完候補にださないと指定したものも補完候補とする。
# _approximate: 似ている補完候補も補完候補とする。
# _prefix: カーソル以降を無視してカーソル位置までで補完する。
zstyle ':completion:*' completer _oldlist _complete _match _history _ignored _approximate _prefix
zstyle ':completion:*:messages' format ${Yellow}'%d'${Reset}
zstyle ':completion:*:warnings' format ${Red}'No matches for:'${Yellow}' %d'${Reset}
zstyle ':completion:*:descriptions' format ${Yellow}'completing %B%d%b'${Reset}
zstyle ':completion:*:corrections' format ${Yellow}'%B%d '${Red}'(errors: %e)%b'${Reset}
zstyle ':completion:*:options' description 'yes'

# グループ名に空文字列を指定すると，マッチ対象のタグ名がグループ名に使われる。
# したがって，すべての マッチ種別を別々に表示させたいなら以下のようにする
zstyle ':completion:*' group-name ''
# 補完候補をキャッシュする
zstyle 'completion:*' use-cache yes
# 詳細な情報を表示する
zstyle 'completion:*' verbose yes
# sudo時にはsudo用のパスを使う
zstyle 'completion:sudo:*' environ PATH="$SUDO_PATH:$PATH"
# オブジェクトファイルとか中間ファイルは補完させない
zstyle 'completion:*:*files' ignored-patterns '*?.0' '*?~' '*\#'

# 語の途中でもカーソル位置まで補完
setopt complete_in_word
# カーソル位置は保持したままファイル名一覧を順次その場で表示
setopt always_last_prompt
# globを展開しないで候補の一覧から補完する
setopt hist_expand
# 補完候補がない時にbeepを鳴らさない
setopt no_beep
# 辞書順ではなく数字順に並べる
setopt numeric_glob_sort
# 補完候補一覧でファイルの種別を識別マーク表示(ls -Fの記号)
setopt list_types
# 補完キー連打で順に補完候補を自動で補完s
setopt auto_menu
# 補完候補が複数ある場合に，一覧表示する
setopt auto_list

# コマンドラインでも # 以降をコメントとして扱う
setopt interactive_comments
# 日本語ファイル名など8bitを通す
setopt print_eight_bit
# 明確なドットの指定なしで.から始まるファイルをマッチ
setopt globdots
# 補完リダイレクトやパイプなど，必要に応じて tee や cat の機能が使われる
setopt multios
# sudo も補完対象
zstyle ':completion:*' list-colors di=34 fi=0

# zsh 4.3.10 以降じゃないと動かないと思う
bindkey '^R' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward

## 展開
# カッコの対応を自動的に補完
setopt auto_param_keys

# ディレクトリ名の補完で末尾の/を自動的に付加し，次の補完に備える
setopt auto_param_slash

# ファイル名の展開でディレクトリにマッチした場合，末尾に/を付加する
setopt mark_dirs

# 最後がディレクトリ名で終わっている場合，末尾の / を自動的に取り除かない
setopt noautoremoveslash

# コマンドライン引数で --prefix=/usr みたいな = 以降でも補完する
setopt magic_equal_subst

# 拡張globで補完(~とか^とか． 例えば less *.txt~memo.txt なら memo.txt 以外の*.txtにマッチする)
setopt extended_glob

# {a-c}を a b c に展開する
setopt brace_ccl

## jobs
# jobsでプロセスIDも表示する
setopt long_list_jobs

## preport time
# 実行したプロセスの消費時間が3秒以上なら
# 自動的に消費時間の統計情報を表示する
export PREPORTTIME=3

## Default git configuration
export GIT_PAGER='/usr/local/bin/lv -c -Au8'

# 新規ディレクトリは755， 新規ファイルは644にする
umask 022

# コマンドライン全てのスペルチェックをする
# 鬱陶しいから使わない
# setopt correct_all

# 上書きリダイレクトの禁止
setopt no_clobber

# synbolic link は実体を追うようにする
setopt chase_links

# beepを鳴らさない
setopt nolistbeep

# URLをコピーしたときに自動的にエスケープ
# auto-fu.zshと相性が悪いのでコメントアウト
#autoload -Uz url-quote-magic
# zle -N self-insert url-quote-magic

## terminal configuration
## http://journal.mycom.co.jp/column/zsh/009/index.html
unset LSCOLORS

case "${TERM}" in
xterm)
    export TERM=xterm-color
    ;;
kterm)
    export TERM=kterm-color
    # set BackSpace control character
    stty erase
    ;;

cons25)
    unset LANG
    export LS_COLORS='di=01;32:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30'
    zstyle ':completion:*' list-colors \
    'di=;36;1' 'ln=;35;1' 'so=;32;1' 'ex=31;1' 'bd=46;34' 'cd=43;34'
;;

kterm*|xterm*)
    export CLICOLOR=1
    export LSCOLORS=ExFxCxDxBxegedabagacad

    zstyle ':completion:*' list-colors \
    'di=36' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
;;

dumb)
    echo "Welcome Emacs Shell"
    ;;
esac

export EDITOR=vim
export PATH=$PATH:$HOME/local/bin:/usr/local/git/bin
export PATH=$PATH:$HOME/dotfiles/bin
export PATH=$PATH:/sbin:usr/local/bin
export MANPATH=$MANPATH:/opt/local/man:/usr/local/share/man

# Defaults mercurial configuration
#
export HGENCODING=utf-8

REPORTTIME=3

# pip zsh completion start
function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip_completion pip
# pip zsh completion end

## alias設定
[ -f ~/dotfiles/.zshrc.alias ] && source ~/dotfiles/.zshrc.alias

case "${OSTYPE}" in
darwin*)
    [ -f ~/dotfiles/.zshrc.osx ] && source ~/dotfiles/.zshrc.osx ;;
linux*)
    [ -f ~/dotfiles/.zshrc.linux ] && source ~/dotfiles/.zshrc.linux ;;
esac

# local固有設定
[ -f ~/dotfiles/.zshrc.local ] && source ~/dotfiles/.zshrc.local


