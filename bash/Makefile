all: bash_profile bashrc

bash_profile:
	[ -f $(HOME)/.$@ ] && rm -f $(HOME)/.$@ || :
	[ -L $(HOME)/.$@ ] || ln -s `pwd`/dot.$@ $(HOME)/.$@

bashrc:
	[ -f $(HOME)/.$@ ] && rm -f $(HOME)/.$@ || :
	[ -L $(HOME)/.$@ ] || ln -s `pwd`/dot.$@ $(HOME)/.$@
