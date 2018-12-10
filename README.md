# emacs-config
My config for the DOOM Emacs distribution on Ubuntu 16.04 and 18.04

## Installation

Install DOOM

```bash
sudo apt-get install emacs
git clone -b develop https://github.com/hlissner/doom-emacs ~/.emacs.d
```

Setup DOOM

```bash
~/.emacs.d/bin/doom quickstart
mkdir ~/.doom.d
cp init.el config.el ~/.doom.d/
```

In your .bashrc append

```bash
em() {
    fn=`echo "$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"`
    ~/.emacs.d/bin/doom run $fn
}
alias emacs="em"
```

Done!
