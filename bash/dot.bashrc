# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
umask 022

# User specific environment and startup programs

#-------------------------------------------------------------------------------
# Env. Configuration
#-------------------------------------------------------------------------------

UNAME=${UNAME:-$(uname)}
PATH=${PATH}:${HOME}/bin

case "${UNAME}" in
  CYGWIN*)
    PATH=${PATH}:${HOME}/packer
    PATH=${PATH}:/cygdrive/c/Program\ Files/Oracle/VirtualBox
    PATH=${PATH}:/cygdrive/c/Program\ Files\ \(x86\)/VMware/VMware\ VIX
    PATH=${PATH}:/cygdrive/c/Program\ Files\ \(x86\)/VMware/VMware\ Workstation
    ;;
  Darwin)
    PATH=${PATH}:${HOME}/packer
    PATH=${PATH}:/Applications/VMware\ Fusion.app/Contents/Library
    ;;
esac

export PATH
#export LC_ALL=C
#export LANG=C
export LANG=en_US.UTF-8
export HISTTIMEFORMAT='%F %T '

case "${UNAME}" in
  Darwin)
    export LSCOLORS=gxfxcxdxcxegedabagacad
    ;;
esac

#-------------------------------------------------------------------------------
# Aliases
#-------------------------------------------------------------------------------

case "${UNAME}" in
  CYGWIN*)
    alias ls='ls -F --color --show-control-chars'
    alias screen='screen -U'
    alias apt-cyg='apt-cyg -u'
    if [[ -f /cygdrive/c/cygwin64/bin/git ]]; then
      alias git='/cygdrive/c/cygwin64/bin/git'
    fi
    alias dot="/cygdrive/c/Program\ Files\ \(x86\)/Graphviz2.38/bin/dot.exe"
    ;;
  Darwin)
    alias ls='ls -FG'
    ;;
esac

#-------------------------------------------------------------------------------
# Prompt
#-------------------------------------------------------------------------------

function show_git_branch() {
  type -P git >/dev/null || return 0
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

PS1='\u@\h:\w$(show_git_branch)\$ '

#-------------------------------------------------------------------------------
# SSH Agent
# + based on https://github.com/mitchellh/dotfiles/blob/master/bashrc#L181-L203
#-------------------------------------------------------------------------------

ssh_env=${HOME}/.ssh/environment.${HOSTNAME}

function start_ssh_agent() {
  # remote?
  [[ -z "${SSH_CLIENT}" ]] || return 0

  ssh-agent | sed 's/^echo/#echo/' > ${ssh_env}
  chmod 0600 ${ssh_env}
  . ${ssh_env} > /dev/null

  local ssh_agent_keys=${HOME}/.ssh/agent_keys

  if [[ -f "${ssh_agent_keys}" ]]; then
    local privkey=
    while read privkey; do
      # expand a file path using "~" or "${HOME}"
      eval privkey=${privkey}
      [[ -f "${privkey}" ]] || continue
      ssh-add ${privkey}
    done < ${ssh_agent_keys}
  else
    ssh-add
  fi
}

# Source SSH agent settings if it is already running, otherwise start
# up the agent proprely.

if [[ -f "${ssh_env}" ]]; then
  . ${ssh_env} > /dev/null
  ps -p ${SSH_AGENT_PID} > /dev/null || {
    start_ssh_agent
  }
else
  start_ssh_agent
fi

# static ssh agent sock path

ssh_agent_sock=${HOME}/.ssh/agent.sock.${HOSTNAME}

# based on http://www.gcd.org/blog/2006/09/100/
if ! [[ -L "${SSH_AUTH_SOCK}" ]] && [[ -S "${SSH_AUTH_SOCK}" ]]; then
  ln -fs ${SSH_AUTH_SOCK} ${ssh_agent_sock}
  export SSH_AUTH_SOCK=${ssh_agent_sock}
fi
