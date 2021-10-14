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

export PATH="/Applications/Julia-1.6.app/Contents/Resources/julia/bin:$PATH"
# alias julia='julia -t2 -J/Users/edward/customfiles/PlotsSysimage.so'
