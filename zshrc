# autoload -Uz compinit && compinit
# autoload -Uz bashcompinit && bashcompinit

# function parse_git_branch() {
#     git branch 2> /dev/null | sed -n -e 's/^\* \(.*\)/[\1]/p'
# }

COLOR_DEF=$'%f'
COLOR_USR=$'%F{243}'
COLOR_DIR=$'%F{197}'
COLOR_GIT=$'%F{39}'
setopt PROMPT_SUBST
# export PROMPT='${COLOR_USR}%n ${COLOR_DIR}%~ ${COLOR_GIT}$(parse_git_branch)${COLOR_DEF} $ '
export PROMPT='${COLOR_USR}%n ${COLOR_DIR}%~${COLOR_GIT}${COLOR_DEF} $ '

source <(fzf --zsh)

alias fzf-preview="fzf --preview 'bat --color=always {}' --preview-window '~2,bottom,wrap'"

eval "$(zoxide init zsh)"

# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

path=('/Users/edward/.juliaup/bin' $path)
export PATH

export FZF_DEFAULT_COMMAND='fd --type d --strip-cwd-prefix --follow --exclude .git .'

# CTRL-Y to copy the command into clipboard using pbcopy
export FZF_CTRL_R_OPTS="
  --bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort'
  --color header:italic
  --header 'Press CTRL-Y to copy command into clipboard'"

_fzf_compgen_dir() {
  fd --type d --follow --exclude ".git" . "$1"
}

_fzf_compgen_path() {
  fd --follow --exclude ".git" --exclude "node_modules". "$1"
}


# <<< juliaup initialize <<<
