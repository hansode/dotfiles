all:
	for i in */; do echo ... $$i; (cd $$i; make); done
