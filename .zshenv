################################################################################
# path
################################################################################
typeset -U path
path=(
    $HOME/local/bin(N-/)
    /opt/local/bin(N-/)
    /usr/local/bin(N-/)
    /usr/local/sbin(N-/)
    /usr/bin(N-/)
    /bin(N-/)
    /usr/X11/bin(N-/)
    /sbin(N-/)
    /usr/sbin(N-/)
    )

typeset -xT SUDO_PATH sudo_path
typeset -U sudo_path
sudo_path=(
    /opt/local/sbin(N-/)
    /usr/sbin(N-/)
    )

typeset -U man_path
man_path=(
    /opt/local/share/man(N-/)
    /opt/local/man(N-/)
    $HOME/local/share/man(N-/)
    /usr/local/share/man(N-/)
    /usr/share/man(N-/)
    )

typeset -xT RUBYLIB ruby_path
typeset -U ruby_path
ruby_path=(./lib)

typeset -xT PYTHONPATH python_path
typeset -U python_path
python_path=(
    ./lib(N-/)
)

################################################################################
# rbenv
################################################################################
if [ -d ${HOME}/.rbenv ]; then
    export PATH="${HOME}/.rbenv/bin:${PATH}"
    export PATH="/Users/ONDA/.rbenv/shims:${PATH}"

    source "/usr/local/Cellar/rbenv/0.4.0/libexec/../completions/rbenv.zsh"
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
