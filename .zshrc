# users generic .zshrc file for zsh(1)

# Language Setting
#---------------------------------------
export LANG=ja_JP.UTF-8
# 日本語ファイル名など8bitを通す
setopt print_eight_bit

# Load plugin
#---------------------------------------
## antigen
if [ -f ~/.zsh/antigen/antigen.zsh ] ; then
    source ~/.zsh/zshrc.antigen.zsh
fi

# Editor Setting
#---------------------------------------
export EDITOR=atom

# Prompt setting
#---------------------------------------
source ~/.zsh/theme/iori-theme.zsh

make() {
    LANG=C command make "$@" 2>&1 | sed -e "s@[Ee]rror:.*@$e_RED&$e_normal@g" -e "s@cannot¥sfind.*@$e_RED&$e_normal@g" -e "s@[Ww]arning:.*@$e_BLUE&$e_normal@g"
}
cwaf() {
    LANG=C command ./waf "$@" 2>&1 | sed -e "s@[Ee]rror:.*@$e_RED&$e_normal@g" -e "s@cannot¥sfind.*@$e_RED&$e_normal@g" -e "s@[Ww]arning:.*@$e_BLUE&$e_normal@g"
}

# Command history configuration
#---------------------------------------
# 同じコマンドを実行した時はヒストリに保存しない
setopt hist_ignore_dups
# すぐにヒストリファイルに追記する
setopt inc_append_history
# 余分な空白は詰める
setopt hist_reduce_blanks
# C-sでヒストリ検索が潰されるので，出力停止・開始用にC-s/C-qを使わない
setopt no_flow_control
# コマンド名に / が含まれているとき PATH 中のサブディレクトリを探す
setopt path_dirs

# historical backward/forward search with linehead string binded to ^P/^N
#------------------------------------------------------------------------------
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

# Default directory configuration
#---------------------------------------
# ディレクトリ名だけでcdする
setopt auto_cd
# cdで移動した時にpushd(スタック)する
setopt auto_pushd
# ディレクトリが変わったらディレクトリスタックを表示する
chpwd_functions=($chpwd_functions dirs)
# cdコマンドを入力したときにlsを出力する
chpwd() { ls -v; }
# カレントディレクトリ内に指定したディレクトリがない場合に移動先を検索するリスト
cdpath=(~)
# カレントディレクトリに候補がない場合にだけ cdpath 上のディレクトリを候補にする
zstyle ':completion:*:cd:'tag-order local-directories path-directoris
# cdは親ディレクトリからカレントディレクトリを選択しないので表示させないようにする
zstyle ':completion:*:cd*' ignore-parents parent pwd
# 補完候補を↑↓←→でも選択できるようにする
# 補完候補が2つ以上なければすぐに補完する
zstyle ':completion:*:default' menu select=2
# back-wordでの単語境界の設定
autoload -Uz select-word-style
select-word-style default
zstyle ':zle:*' word-chars " _-./;@"
zstyle ':zle:*' word-style unspecified
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
# zle
zstyle ':filter-select' case-insensitive yes
# sudo も補完対象
zstyle ':completion:*' list-colors di=34 fi=0

## Prediction configuration
autoload predict-on
# 補完の初期化
autoload -Uz compinit
compinit
# 語の途中でもカーソル位置まで補完
setopt complete_in_word
# カーソル位置は保持したままファイル名一覧を順次その場で表示
setopt always_last_prompt
# globを展開しないで候補の一覧から補完する
setopt hist_expand
# 補完候補がない時にbeepを鳴らさない
setopt no_beep
# 補完候補一覧でファイルの種別を識別マーク表示(ls -Fの記号)
setopt list_types
# 補完キー連打で順に補完候補を自動で補完s
setopt auto_menu
# 補完候補が複数ある場合に一覧表示する
setopt auto_list
# コマンドラインでも # 以降をコメントとして扱う
setopt interactive_comments
# 明確なドットの指定なしで.から始まるファイルをマッチ
setopt globdots
# 補完リダイレクトやパイプなど，必要に応じて tee や cat の機能が使われる
setopt multios

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

# Commands
#---------------------------------------
# 新規ディレクトリは755， 新規ファイルは644にする
umask 022
# サスペンド中のプロセスと同じコマンド名を実行した場合はリジューム
setopt auto_resume
# 上書きリダイレクトの禁止
setopt no_clobber
# synbolic link は実体を追うようにする
setopt chase_links
# URLをコピーしたときに自動的にエスケープ
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# source file
#---------------------------------------
source "$HOME/.dotfiles/.zsh/functions/peco-select-history.zsh"
source "$HOME/.dotfiles/.zsh/functions/peco-git-branch-selector.zsh"
source "$HOME/.dotfiles/.zsh/functions/route-peco.zsh"
source "$HOME/.dotfiles/.zsh/functions/peco-bundle-gem-open.zsh"
source "$HOME/.dotfiles/.zsh/functions/notify.plugin.zsh"

case "${OSTYPE}" in
darwin*)
    [ -f ~/.dotfiles/.zshrc.osx ] && source ~/.dotfiles/.zshrc.osx ;;
linux*)
    [ -f ~/.dotfiles/.zshrc.linux ] && source ~/.dotfiles/.zshrc.linux ;;
esac

# local固有設定
[ -f ~/.dotfiles/.zshrc.local ] && source ~/.dotfiles/.zshrc.local

export PATH="/usr/local/heroku/bin:$PATH"
export SYS_NOTIFIER=/usr/local/bin/terminal-notifier
export NOTIFY_COMMAND_COMPLETE_TIMEOUT=5

# Cask の設定
export PATH="/Users/ONDA/.cask/bin:$PATH"
autoload -Uz git-escape-magic
git-escape-magic
