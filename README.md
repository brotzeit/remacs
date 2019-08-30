# remacs-vterm

This project is an experimental fork of remacs, that contains a port of [emacs-libvterm](https://github.com/akermu/emacs-libvterm). Unlike the module, this project provides built-in libvterm support by using remacs code. It contains the complete emacs code base in order to simplify upstream merges.

# Installation

You need the the correct libvterm version.

``` bash
git clone git@github.com:remacs/remacs-libvterm.git
cd remacs-libvterm/
make install
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib
```

# vterm

Start your terminal with `vterm`.
