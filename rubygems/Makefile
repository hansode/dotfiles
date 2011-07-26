all: gemrc clean

gemrc:
	sed -e "s,__HOME__,$(HOME),g" gemrc.yml > $@
	cp $@ $(HOME)/.gemrc

clean:
	[ -f gemrc ] && rm gemrc || :
