# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/bin

export PATH
export LC_ALL=C
export LANG=C

function show_git_branch {
  if which git >/dev/null; then
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
  fi
}

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]$(show_git_branch)\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w$(show_git_branch)\$ '
fi
$(show_git_branch)

export HISTTIMEFORMAT='%F %T '
