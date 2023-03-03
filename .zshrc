source ~/.zplug/init.zsh
export PATH="$HOME/.local/bin:$PATH"

# Make sure to use double quotes
zplug "zsh-users/zsh-history-substring-search"

# Use the package as a command
# And accept glob patterns (e.g., brace, wildcard, ...)
zplug "Jxck/dotfiles", as:command, use:"bin/{histuniq,color}"

# Can manage everything e.g., other person's zshrc
zplug "tcnksm/docker-alias", use:zshrc

# Disable updates using the "frozen" tag
zplug "k4rthik/git-cal", as:command, frozen:1

zplug "jeffreytse/zsh-vi-mode"
# Supports oh-my-zsh plugins and the like
zplug "plugins/git",   from:oh-my-zsh

# Run a command after a plugin is installed/updated
# Provided, it requires to set the variable like the following:
# ZPLUG_SUDO_PASSWORD="********"
zplug "jhawthorn/fzy", \
    as:command, \
    rename-to:fzy, \
    hook-build:"make && sudo make install"

# Supports checking out a specific branch/tag/commit
zplug "b4b4r07/enhancd", at:v1
zplug "mollifier/anyframe", at:4c23cb60

# Can manage gist file just like other packages
zplug "b4b4r07/79ee61f7c140c63d2786", \
    from:gist, \
    as:command, \
    use:get_last_pane_path.sh

# Rename a command with the string captured with `use` tag
zplug "b4b4r07/httpstat", \
    as:command, \
    use:'(*).sh', \
    rename-to:'$1'

# Note: To specify the order in which packages should be loaded, use the defer
#       tag described in the next section

# Set the priority when loading
# e.g., zsh-syntax-highlighting must be loaded
# after executing compinit command and sourcing other plugins
# (If the defer tag is given 2 or above, run after compinit command)
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug "lib/history", from:oh-my-zsh
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"

zplug "nvbn/thefuck"
zplug "chrissicool/zsh-256color"
zplug "desyncr/auto-ls"
zplug "arzzen/calc.plugin.zsh"
zplug "zdharma-continuum/fast-syntax-highlighting"

# Load theme file
zplug 'sbugzu/gruvbox-zsh', as:theme

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load 

autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# include hidden files.

zplug "hlissner/zsh-autopair", defer:2
# vi mode
bindkey -v
export keytimeout=1
export ALPHAVANTAGE_API_KEY='DH5ZX26E46MYMEXW'

source ~/.outputrc
# load aliases and shortcuts if existent.
source ~/.aliasrc

# Use lf to switch directories and bind it to ctrl-o
lfcd () {
    tmp="$(mktemp)"
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}
bindkey -s '^o' 'lfcd\n'

[ -f "/home/hibiscus-tea/.ghcup/env" ] && source "/home/hibiscus-tea/.ghcup/env" # ghcup-env
