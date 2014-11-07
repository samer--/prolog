# ---------------- configuration ----------------------

# if you have multiple SWI Prolog installations or an installation
# in a non-standard place, set PLLD to the appropriate plld invokation, eg
# PLLD=/usr/local/bin/plld -p /usr/local/bin/swipl

PACKNAME=musicbrainz
VER=0.6.5
PACKFILE=release/$(PACKNAME)-$(VER).tgz
# ---------------- end of configuration ---------------

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

install: pack
	swipl -g "pack_install('$(PACKFILE)'), halt"

install-git: 
	(git commit $(PACKFILE) || echo 'Local up to date') && (git push || echo 'Remote up to date')
	chmod -w $(PACKFILE)
	swipl -g "pack_property($(PACKNAME),download(D)), pack_install(D), halt"

dist-clean:
	echo 'Nothing to do'
