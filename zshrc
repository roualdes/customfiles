# function to return current branch name while suppressing errors.
function git_branch() {
    branch=$(git symbolic-ref HEAD 2> /dev/null | awk 'BEGIN{FS="/"} {print $NF}')
    if [[ $branch == "" ]]; then
        :
    else
        echo '('$branch')'
    fi
}

setopt prompt_subst             # allow command substitution inside the prompt
PROMPT='%~ $(git_branch)%% '

export JULIA_NUM_THREADS=4
alias julia='/Applications/Julia-1.8.app/Contents/Resources/julia/bin/julia'
# alias julia='julia -t2 -J/Users/edward/customfiles/PlotsSysimage.so'

alias rest='pmset sleepnow'