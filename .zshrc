# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
# ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="iori"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(rails ruby git rbenv zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh

# Customize to your needs...

#disable autocorrect
unsetopt correct_all

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

if [[ -f ~/.nodebrew/nodebrew ]]; then
    export PATH=$HOME/.nodebrew/current/bin:$PATH
fi

source $HOME/.dotfiles/.zsh/functions/peco-select-history.zsh
source $HOME/.dotfiles/.zsh/functions/peco-git-branch-selector.zsh
source $HOME/.dotfiles/.zsh/functions/route-peco.zsh
source $HOME/.dotfiles/.zsh/functions/peco-bundle-gem-open.zsh
source $HOME/.dotfiles/.zsh/functions/notify.plugin.zsh

# [TODO]  - 仕事用に分離させなきゃ
export SYS_NOTIFIER=/usr/local/bin/terminal-notifier
export NOTIFY_COMMAND_COMPLETE_TIMEOUT=5
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/Contents/Home
