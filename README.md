# doom-config
My config for [doom emacs](https://github.com/hlissner/doom-emacs) for Emacs 27 on Ubuntu 20.04

## Installation

### Install Emacs 27

```bash
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs27
```

### Install Doom

```bash
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
git clone https://github.com/auerl/doom-config ~/.doom.d/
~/.emacs.d/bin/doom install
~/.emacs.d/bin/doom sync
```

### Setup Python/Jupyter IDE

For Ubuntu 20.04

``` bash
sudo apt install python3-pip
sudo apt install python-is-python3
sudo pip install jupyter
sudo pip install 'python-language-server[all]'
```

Done!
