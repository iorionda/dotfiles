#!/bin/zsh
source ~/.zsh/antigen/antigen.zsh

antigen-use oh-my-zsh
# antigen-update

# from oh-my-zsh/plugins
antigen-bundle git
antigen-bundle rbenv
antigen-bundle autojump
antigen-bundle rails
antigen-bundle ruby

antigen-bundle zsh-users/zsh-syntax-highlighting
antigen-bundle zsh-users/zsh-completions
antigen-bundle zsh-users/zsh-history-substring-search
antigen-bundle zsh-users/zaw

antigen-apply
