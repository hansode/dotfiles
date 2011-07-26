DOT_EMACS=$(HOME)/.emacs
EMACS_D=$(HOME)/.emacs.d
REAL_DOT_EMACS=$(EMACS_D)/dot.emacs.el


all:
	echo $(HOME)
	[ -L $(DOT_EMACS) ] || ln -s $(REAL_DOT_EMACS) $(DOT_EMACS)
