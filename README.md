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

* Install vim-ruby

```
$ git clone http://github.com/vim-ruby/vim-ruby.git
$ cd vim-ruby/
$ git checkout vim7.3
$ rake package
$ gem install etc/package/vim-ruby-2013.01.15.gem
$ cd bin
$ ruby vim-ruby-install.rb
```
