# .bash_profile

# Get the aliases and functions
if [[ -f ~/.bashrc ]]; then
  . ~/.bashrc
fi

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
    PATH=${PATH}:/cygdrive/c/Program\ Files\ \(x86\)/Heroku/bin
    ;;
  Darwin)
    PATH=${PATH}:${HOME}/packer
    PATH=${PATH}:/Applications/VMware\ Fusion.app/Contents/Library
    ;;
esac

export PATH
export LC_ALL=C
export LANG=C
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
#-------------------------------------------------------------------------------

SSH_ENV=${HOME}/.ssh/environment
SSH_AGETNT_KEYS=${HOME}/.ssh/agent_keys

function start_ssh_agent() {
  # remote?
  [[ -z "${SSH_CLIENT}" ]] || return 0

  ssh-agent | sed 's/^echo/#echo/' > ${SSH_ENV}
  chmod 0600 ${SSH_ENV}
  . ${SSH_ENV} > /dev/null

  if [[ -f ${HOME}/.ssh/agent_keys ]]; then
    local privkey=
    while read privkey; do
      # expand a file path using "~" or "${HOME}"
      eval privkey=${privkey}
      [[ -f "${privkey}" ]] || continue
      ssh-add ${privkey}
    done < ${SSH_AGETNT_KEYS}
  else
    ssh-add
  fi
}

# Source SSH agent settings if it is already running, otherwise start
# up the agent proprely.

if [[ -f "${SSH_ENV}" ]]; then
  . ${SSH_ENV} > /dev/null
  # ps ${SSH_AGENT_PID} doesn't work under cywgin
  ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
    start_ssh_agent
  }
else
  start_ssh_agent
fi
