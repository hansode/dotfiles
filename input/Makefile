all: inputrc

inputrc:
	[ -f $(HOME)/.$@ ] && rm -f $(HOME)/.$@ || :
	[ -L $(HOME)/.$@ ] || ln -s `pwd`/dot.$@ $(HOME)/.$@
