# ---------------- configuration ----------------------

# if you have multiple SWI Prolog installations or an installation
# in a non-standard place, set PLLD to the appropriate plld invokation, eg
# PLLD=/usr/local/bin/plld -p /usr/local/bin/swipl

PACKNAME=typedef
VER=0.1.2
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

install:
	swipl -g "pack_install('$(PACKFILE)'),halt"
