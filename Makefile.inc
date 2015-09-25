# This is a Makefile INCLUDE - it assumes that the variable PACKNAME is defined
#
# It also assumes that the current directory contains a file called VERSION
# containing just the current version of the pack.
#
# It also assumes that there is a file called pack.pl with placeholders for
# the version and package name.

VER=$(shell cat VERSION)
PACKFILE=release/$(PACKNAME)-$(VER).tgz
EPACKFILE=release\/$(PACKNAME)-$(VER).tgz
ZIPFILE=release/$(PACKNAME)-$(VER).zip
DOWNLOAD=https:\/\/raw.githubusercontent.com\/samer--\/prolog\/master\/$(PACKNAME)\/$(EPACKFILE)

main: 

packdir:
	mkdir -p $(PACKNAME) $(PACKNAME)/prolog 
	sed -e "s/<VER>/$(VER)/g" < pack.pl | sed -e "s/<PACKNAME>/$(PACKNAME)/g" | sed -e "s/<DOWNLOAD>/$(DOWNLOAD)/g" > $(PACKNAME)/pack.pl
	rsync -ar --delete --exclude '.*' prolog $(PACKNAME)
	cp -p README $(PACKNAME)
	cp -p ../COPYING* $(PACKNAME)

pack: packdir
	tar czf $(PACKFILE) $(PACKNAME)
	rm -rf $(PACKNAME)
	git add $(PACKFILE)

zip: packdir
	zip -r $(ZIPFILE) $(PACKNAME)
	rm -rf $(PACKNAME)
	git add $(ZIPFILE)

push:
	git push

install:
	swipl -g "pack_install('$(PACKFILE)',[upgrade(true),interactive(false)]), halt"

install-git: 
	(git commit $(PACKFILE) || echo 'Local up to date') && (git push || echo 'Remote up to date')
	swipl -g "pack_property($(PACKNAME),download(D)), pack_install(D,[upgrade(true),interactive(false)]), halt"
	chmod -w $(PACKFILE)

dist-clean:
