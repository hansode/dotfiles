all: markdown-mode.el

markdown-mode.el: markdown-mode
	[ -L $@ ] || ln -s markdown-mode/$@ $@

markdown-mode:
	git clone git://jblevins.org/git/markdown-mode.git

clean:
	rm -rf markdown-mode* || :
