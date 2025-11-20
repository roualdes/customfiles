COLOR_DEF=$'%f'
COLOR_USR=$'%F{243}'
COLOR_DIR=$'%F{197}'
COLOR_GIT=$'%F{39}'
setopt PROMPT_SUBST
# export PROMPT='${COLOR_USR}%n ${COLOR_DIR}%~ ${COLOR_GIT}$(parse_git_branch)${COLOR_DEF} $ '
export PROMPT='${COLOR_USR}%n ${COLOR_DIR}%~${COLOR_GIT}${COLOR_DEF} $ '

source <(fzf --zsh)

alias fzf-preview="fzf --preview 'bat --color=always {}' --preview-window '~2,bottom,wrap'"

path=('/Users/edward/.juliaup/bin' $path)
export PATH

export FZF_CTRL_R_OPTS="
  --bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort'
  --color header:italic
  --header 'Press CTRL-Y to copy command into clipboard'"

export FZF_ALT_C_COMMAND="fd \
       -t d \
       --max-depth 2 \
       --exclude Library/ \
       --exclude 'Creative Cloud Files/' \
       --exclude Music/ \
       --exclude Movies/ \
       . ~
"

export FZF_ALT_C_OPTS="
       --bind 'tab:change-prompt(> )+reload(fd -a -t d . {})+first'
"

export FZF_CTRL_T_COMMAND="fd \
       --max-depth 2 \
       --exclude Library/ \
       --exclude 'Creative Cloud Files/' \
       --exclude Music/ \
       --exclude Movies/ \
       . ~
"

export FZF_CTRL_T_OPTS="
       --bind 'tab:change-prompt(> )+reload(fd -a . {})+first'
"
