all: virc

virc:
	[ -f $(HOME)/.$@ ] && rm -f $(HOME)/.$@ || :
	[ -L $(HOME)/.$@ ] || ln -s `pwd`/dot.$@ $(HOME)/.$@
