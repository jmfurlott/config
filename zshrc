# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh


#matlab script
function matlabs() {
  /Applications/MATLAB_R2013a.app/bin/matlab "$@";
}
alias matlab=matlabs

function goToVagrant() {
  cd /Users/jmfurlott/vagrant-ansible-provisioning
}

function sshVagrant() {
  cd /Users/jmfurlott/vagrant-ansible-provisioning;
  vagrant ssh;
}

alias vssh=sshVagrant

alias gtv=goToVagrant

export PATH=$PATH:/opt/local/bin
alias adb=/Users/jmfurlott/Documents/android-sdk/platform-tools/adb
alias fastboot=/Users/jmfurlott/Documents/android-sdk/platform-tools/fastboot
export PATH=/usr/local/share/python:$PATH
alias gcx=gcc-4.9

export PATH=/usr/local/mysql/bin:$PATH
export SLACK_TOKEN=xoxp-2347373612-2389971169-2390217197-7615a7 
#GRUNT 
eval "$(grunt --completion=zsh)"


# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"
#ZSH_THEME="geoffgarside"
#ZSH_THEME="nanotech"
#ZSH_THEME='3den'
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=$PATH:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
export PATH=$PATH: /usr/local/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib
