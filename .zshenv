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
# AWS
################################################################################
export AWS_RDS_HOME=/usr/local/src/RDSCli-1.10.003
export PATH=$PATH:$AWS_RDS_HOME/bin
export AWS_CREDENTIAL_FILE=$AWS_RDS_HOME/credential-file-path

################################################################################
# GNU Global
################################################################################
export GTAGSCONF=/usr/local/share/gtags/gtags.conf

################################################################################
# nodebrew
################################################################################
export PATH=$HOME/.nodebrew/current/bin:$PATH

################################################################################
# node.js
################################################################################
if [[ -f ~/.nodebrew/nodebrew ]]; then
  export PATH=$HOME/.nodebrew/current/bin:$PATH
fi

################################################################################
# Gisty
################################################################################
export GISTY_DIR="$HOME/dev/gists"
export GISTY_ACCESS_TOKEN=4abc896373018be644eb770af0e3674033cb5837

################################################################################
# PostgreSQL
################################################################################
export PGDATA=/usr/local/var/postgres

################################################################################
# Java
################################################################################
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/Contents/Home
alias blender=/Applications/Blender/blender.app/Contents/MacOS/blender
