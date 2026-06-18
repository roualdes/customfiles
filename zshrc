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

source /Users/eroualdes/.config/broot/launcher/bash/br

. "$HOME/.local/bin/env"

autoload -Uz zmv
alias mmv='noglob zmv -W'

# Simple calculator. When using "=", quote the expression before executing it.
# See https://www.zsh.org/mla/users/2026/msg00021.html
_vbe_calc_quote() {
    case $BUFFER in
        "="*)
            typeset -g _vbe_calc_expr=$BUFFER
            BUFFER="= ${(q-)${${BUFFER#=}# }}"
            ;;
    esac
}
# Ensure the original, unquoted, expression is put in history.
_vbe_calc_history() {
    return ${+_vbe_calc_expr}
}
_vbe_calc_preexec() {
    (( ${+_vbe_calc_expr} )) && print -s $_vbe_calc_expr
    unset _vbe_calc_expr
    return 0
}
add-zle-hook-widget line-finish _vbe_calc_quote
add-zsh-hook preexec _vbe_calc_preexec
add-zsh-hook zshaddhistory _vbe_calc_history
if (( $+commands[numbat] )); then
    aliases[=]='numbat -e'
elif (( $+commands[qalc] )); then
    aliases[=]='qalc'
else
    autoload -Uz zcalc
    aliases[=]='zcalc -f -e'
fi
