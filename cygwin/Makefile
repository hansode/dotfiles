WHOAMI=`whoami`

all: documents downloads dropbox

documents:
	if [ ! -L $(HOME)/$@ -a -d /cygdrive/c/Users/$(WHOAMI)/Documents ]; then ln -s /cygdrive/c/Users/$(WHOAMI)/Documents $(HOME)/$@; fi

downloads:
	if [ ! -L $(HOME)/$@ -a -d /cygdrive/c/Users/$(WHOAMI)/Downloads ]; then ln -s /cygdrive/c/Users/$(WHOAMI)/Downloads $(HOME)/$@; fi

dropbox:
	if [ ! -L $(HOME)/$@ -a -d /cygdrive/c/Users/$(WHOAMI)/Dropbox   ]; then ln -s /cygdrive/c/Users/$(WHOAMI)/Dropbox   $(HOME)/$@; fi
