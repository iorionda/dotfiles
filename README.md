# Qucik Start

```
$ ./link.sh
```

## Vim

* Install NeoBundle

```
$ mkdir -p ~/.vim/bundle
$ git clone git://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim
```

### rbenv-default-gems

rbenv で ruby をインストールした際に指定した gem を同時にインストールしてくれる plugin


``` bash
% brew install rbenv-default-gems
% cp default-gems $RBENV_ROOT/
```

あとはいつも通りに rbenv install するだけ。
