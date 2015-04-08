all: virc vimrc

virc:
	[ -f $(HOME)/.$@ ] && rm -f $(HOME)/.$@ || :
	[ -L $(HOME)/.$@ ] || ln -s `pwd`/dot.$@ $(HOME)/.$@

vimrc:
	[ -f $(HOME)/.$@ ] && rm -f $(HOME)/.$@ || :
	[ -L $(HOME)/.$@ ] || ln -s `pwd`/dot.$@ $(HOME)/.$@
