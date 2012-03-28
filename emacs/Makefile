DOT_EMACS=$(HOME)/.emacs
EMACS_D=$(HOME)/.emacs.d
REAL_DOT_EMACS=$(EMACS_D)/dot.emacs.el


all: dotfiles subdirs

clean: dotfiles.clean subdirs.clean

dotfiles:
	echo $(HOME)
	[ -L $(DOT_EMACS) ] || ln -s $(REAL_DOT_EMACS) $(DOT_EMACS) && :
	[ -L $(EMACS_D)   ] || ln -s `pwd` $(EMACS_D) && :

dotfiles.clean:
	[ -L $(DOT_EMACS) ] && rm -f $(DOT_EMACS)
	[ -L $(EMACS_D)   ] && rm -f $(EMACS_D)

subdirs:
	cd 3rd-party && make

subdirs.clean:
	cd 3rd-party && make clean
