# ---------------- configuration ----------------------

# if you have multiple SWI Prolog installations or an installation
# in a non-standard place, set PLLD to the appropriate plld invokation, eg
# PLLD=/usr/local/bin/plld -p /usr/local/bin/swipl

PACKNAME=fileutils
VER=0.1.5
PACKFILE=release/$(PACKNAME)-$(VER).tgz
# ---------------- end of configuration ---------------

all: pack


pack:
	mkdir -p $(PACKNAME) $(PACKNAME)/prolog 
	sed -e 's/<VER>/$(VER)/' pack.pl > $(PACKNAME)/pack.pl
	cp -pR prolog $(PACKNAME)
	cp -p README $(PACKNAME)
	tar czf $(PACKFILE) $(PACKNAME)
	rm -rf $(PACKNAME)
	git add $(PACKFILE)

dist-clean:

install: pack
	swipl -f none -g "pack_install('$(PACKFILE)'),halt"

install-git: 
	(git commit $(PACKFILE) || echo 'Local up to date') && (git push $(PACKFILE)) || echo 'Remote up to date')
	swipl -g "pack_property($(PACKNAME),download(D)), pack_install(D), halt"

commit:
	chmod -w $(PACKFILE)
	git commit -a
