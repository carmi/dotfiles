# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="af-magic"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
alias sml="rlwrap sml"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"


source $ZSH/oh-my-zsh.sh
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git rvm pip virtualenvwrapper python django)


source ~/.bash_aliases

function lt() { ls -ltrsa "$@" | tail; }
function psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }
function fname() { find . -iname "*$@*"; }

# Disable correction
unsetopt correct_all

export ECHO_NEST_API_KEY="ZRASXHLRSX368BR8W"

# Customize to your needs...
export PATH=/home/evan/.rvm/gems/ruby-1.9.3-p194/bin:/home/evan/.rvm/gems/ruby-1.9.3-p194@global/bin:/home/evan/.rvm/rubies/ruby-1.9.3-p194/bin:/home/evan/.rvm/bin:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" 

export WORKON_HOME=$HOME/Dropbox/projects/virtualenvs
export PROJECT_HOME=$HOME/Dropbox/projects
source /usr/local/bin/virtualenvwrapper.sh
