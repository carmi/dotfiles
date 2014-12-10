alias s='git status'
alias dpl='cd ~/Dropbox/wesleyan/spring13/comp321/hw/'
alias vim='mvim -v'
# alias ag='ag --smart-case'

CODE_HOME='~/code'
BREWSTER_HOME="$CODE_HOME/brewster"

# directory aliases
alias br="cd $BREWSTER_HOME/brewster"
alias web="cd $BREWSTER_HOME/web"
alias pup="cd $BREWSTER_HOME/puppetmaster"
alias dots="cd $CODE_HOME/dot-files"
alias ecarmi="cd $CODE_HOME/ecarmi.org-nanoc"

# brewster start aliases
alias es="cd $BREWSTER_HOME; script/elasticsearch start"
alias imagine="cd $BREWSTER_HOME; script/imagine start"
alias riak="cd $BREWSTER_HOME; script/riak console"
alias resque="cd $BREWSTER_HOME; rake resque:dev:pool"

# git aliases
alias dev='br; git checkout develop'
alias stg='br; git checkout staging'
alias prd='br; git checkout production'

# deploy
alias ops-deploy="ssh ops-r01 -t 'screen -dR -S evan-deploy'"

# prod
alias r01-prd="ssh prod-rr-r13 -t 'screen -dR -S evan-prod'"
alias prdrr01="ssh prod-rr-r01 -t 'screen -dR -S evan-prod'"

