# This is a Makefile INCLUDE - it assumes that the variable PACKNAME is defined
#
# It also assumes that the current directory contains a file called VERSION
# containing just the current version of the pack.
#
# It also assumes that there is a file called pack.pl with placeholders for
# the version and package name.

VER=$(shell cat VERSION)
PACKFILE=release/$(PACKNAME)-$(VER).tgz

main: 

packdir:
	mkdir -p $(PACKNAME) $(PACKNAME)/prolog 
	sed -e "s/<VER>/$(VER)/g" < pack.pl | sed -e "s/<PACKNAME>/$(PACKNAME)/g" > $(PACKNAME)/pack.pl
	rsync -ar --delete --exclude '.*' prolog $(PACKNAME)
	cp -p README $(PACKNAME)

pack: packdir
	tar czf $(PACKFILE) $(PACKNAME)
	rm -rf $(PACKNAME)
	git add $(PACKFILE)

push:
	git push && chmod -w $(PACKFILE)

install:
	swipl -g "pack_install('$(PACKFILE)'), halt"

install-git: 
	(git commit $(PACKFILE) || echo 'Local up to date') && (git push || echo 'Remote up to date')
	swipl -g "pack_property($(PACKNAME),download(D)), pack_install(D), halt"
	chmod -w $(PACKFILE)

dist-clean:
