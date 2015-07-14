################################################################################
# rbenv
################################################################################
if [ -d ${HOME}/.rbenv ]; then
    export PATH="${HOME}/.rbenv/bin:${PATH}"
    export PATH="/Users/ONDA/.rbenv/shims:${PATH}"

    source "${HOME}/.rbenv/completions/rbenv.zsh"
    rbenv rehash 2>/dev/null
    rbenv() {
    typeset command
    command="$1"
    if [ "$#" -gt 0 ]; then
        shift
    fi

    case "$command" in
        rehash|shell)
        eval `rbenv "sh-$command" "$@"`;;
        *)
        command rbenv "$command" "$@";;
    esac
    }
elif [ -d ${RBENV_ROOT} ]; then
    export PATH="${RBENV_ROOT}/bin:${PATH}"
    export PATH="${RBENV_ROOT}/shims:${PATH}"

    source "/usr/local/Cellar/rbenv/0.4.0/completions/rbenv.zsh"
    rbenv rehash 2>/dev/null
    rbenv() {
    typeset command
    command="$1"
    if [ "$#" -gt 0 ]; then
        shift
    fi

    case "$command" in
        rehash|shell)
        eval `rbenv "sh-$command" "$@"`;;
        *)
        command rbenv "$command" "$@";;
    esac
    }
fi

################################################################################
# pyenv
################################################################################
export PYENV_ROOT="${HOME}/.pyenv"
if [ -d "${PYENV_ROOT}" ]; then
    export PATH=${PYENV_ROOT}/bin:$PATH
    eval "$(pyenv init -)"
fi
# Initialization for FDK command line tools.Mon Jun  8 10:34:10 2015
FDK_EXE="/Users/ONDA/bin/FDK/Tools/osx"
PATH=${PATH}:"/Users/ONDA/bin/FDK/Tools/osx"
export PATH
export FDK_EXE
