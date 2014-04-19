# ---------------- configuration ----------------------

# if you have multiple SWI Prolog installations or an installation
# in a non-standard place, set PLLD to the appropriate plld invokation, eg
# PLLD=/usr/local/bin/plld -p /usr/local/bin/swipl

PACKNAME=callgraph
VER=0.2.1
PACKFILE=release/$(PACKNAME)-$(VER).tgz
# ---------------- end of configuration ---------------

main: 
	make -C c

packfile:
	mkdir -p $(PACKNAME) $(PACKNAME)/prolog 
	sed -e 's/<VER>/$(VER)/' pack.pl > $(PACKNAME)/pack.pl
	cp -p pack.pl $(PACKNAME)
	cp -pR prolog $(PACKNAME)
	cp -p README $(PACKNAME)
	tar czf $(PACKFILE) $(PACKNAME)
	rm -rf $(PACKNAME)
	git add $(PACKFILE)

install: packfile
	swipl -g "pack_install('$(PACKFILE)')"
