all: dot.ssh ssh_config

dot.ssh:
	[ -d $(HOME)/.ssh ] || mkdir -m 700 $(HOME)/.ssh

ssh_config:
	[ -f $(HOME)/.ssh/config ] || cp config $(HOME)/.ssh/config
