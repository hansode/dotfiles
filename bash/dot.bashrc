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
# local-specific bashrc
#-------------------------------------------------------------------------------

if [[ -f "${HOME}/.bashrc.prelocal" ]]; then
  . "${HOME}/.bashrc.prelocal"
fi

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
    PATH=${PATH}:/cygdrive/c/texlive/2014/bin/win32
    ;;
  Darwin)
    PATH=${PATH}:${HOME}/packer
    PATH=${PATH}:/Applications/VMware\ Fusion.app/Contents/Library
    ;;
esac

if [[ -d /opt/axsh/wakame-vdc ]]; then
  PATH=${PATH}:/opt/axsh/wakame-vdc/dcmgr/bin
  PATH=${PATH}:/opt/axsh/wakame-vdc/client/mussel/bin
fi
export PATH
#export LC_ALL=C
#export LANG=C
export LANG=en_US.UTF-8
export HISTTIMEFORMAT='%F %T '
export HISTSIZE=5000
export HISTFILESIZE=10000

case "${UNAME}" in
  Darwin)
    export LSCOLORS=gxfxcxdxcxegedabagacad
    ;;
esac

#-------------------------------------------------------------------------------
# Aliases
#-------------------------------------------------------------------------------

case "${UNAME}" in
  Linux)
    alias ls='ls -F --color'
    ;;
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
# Completion
#-------------------------------------------------------------------------------

function load_completion() {
  local completion_dir=${HOME}/.bash.d/completion
  local completion_targets=${completion_dir}/targets

  if [[ -f "${completion_targets}" ]]; then
    local completion=
    while read completion; do
      completion=${completion_dir}/${completion}
      [[ -f "${completion}" ]] || continue
      . "${completion}"
    done < "${completion_targets}"
  fi
}
load_completion

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

# if you want to disable this function, you should "ssh_env=/dev/null" in ~/.bashrc.prelocal.
ssh_env=${ssh_env:-${HOME}/.ssh/environment.${HOSTNAME}}

function start_ssh_agent() {
  # remote?
  [[ -z "${SSH_CLIENT}" ]] || return 0

  ssh-agent | sed 's/^echo/#echo/' > "${ssh_env}"
  chmod 0600 "${ssh_env}"
  . "${ssh_env}" > /dev/null

  local ssh_agent_keys=${HOME}/.ssh/agent_keys

  if [[ -f "${ssh_agent_keys}" ]]; then
    local privkey=
    while read privkey; do
      # expand a file path using "~" or "${HOME}"
      eval privkey="${privkey}"
      [[ -f "${privkey}" ]] || continue
      ssh-add "${privkey}"
    done < "${ssh_agent_keys}"
  else
    ssh-add
  fi
}

# Source SSH agent settings if it is already running, otherwise start
# up the agent proprely.

if [[ -f "${ssh_env}" ]]; then
  . "${ssh_env}" > /dev/null
  ps -p "${SSH_AGENT_PID}" > /dev/null || {
    start_ssh_agent
  }
else
  start_ssh_agent
fi

function swap_ssh_agent_sock() {
  # static ssh agent sock path
  local ssh_agent_sock=${HOME}/.ssh/agent.sock.${HOSTNAME}
  # ignore?
  [[ "${ssh_env}" == "/dev/null" ]] && return 0

  # based on http://www.gcd.org/blog/2006/09/100/
  if ! [[ -L "${SSH_AUTH_SOCK}" ]] && [[ -S "${SSH_AUTH_SOCK}" ]]; then
    ln -fs "${SSH_AUTH_SOCK}" "${ssh_agent_sock}"
    export SSH_AUTH_SOCK="${ssh_agent_sock}"
  fi
}
swap_ssh_agent_sock

#-------------------------------------------------------------------------------
# local-specific bashrc
#-------------------------------------------------------------------------------

if [[ -f "${HOME}/.bashrc.local" ]]; then
  . "${HOME}/.bashrc.local"
fi
