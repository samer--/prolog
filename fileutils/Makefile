# ---------------- configuration ----------------------
PACKNAME=fileutils
VER=$(shell cat VERSION)
PACKFILE=release/$(PACKNAME)-$(VER).tgz
# ---------------- end of configuration ---------------

all: pack

packdir:
	mkdir -p $(PACKNAME) $(PACKNAME)/prolog 
	sed -e "s/<VER>/$(VER)/g" < pack.pl | sed -e "s/<PACKNAME>/$(PACKNAME)/g" > $(PACKNAME)/pack.pl
	rsync -ar --delete --exclude '.*' prolog $(PACKNAME)
	cp -p README $(PACKNAME)

pack: packdir
	tar czf $(PACKFILE) $(PACKNAME)
	rm -rf $(PACKNAME)
	git add $(PACKFILE)

install: pack
	swipl -f none -g "pack_install('$(PACKFILE)'), halt"

dist-clean:

install-git: 
	(git commit $(PACKFILE) || echo 'Local up to date') && (git push || echo 'Remote up to date')
	swipl -f none -g "pack_property($(PACKNAME),download(D)), pack_install(D), halt"
	chmod -w $(PACKFILE)
