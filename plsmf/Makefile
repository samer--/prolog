SMFROOT=/usr/local
GLIB_CFLAGS=$(shell pkg-config --cflags glib-2.0)
GLIB_LIBS=$(shell pkg-config --libs glib-2.0)
TARGET=plsmf

CFLAGS+=$(GLIB_CFLAGS) -I$(SMFROOT)/include
SOBJ=$(PACKSODIR)/$(TARGET).$(SOEXT)
LIBS=$(GLIB_LIBS) -L$(SMFROOT)/lib -lsmf

all:	$(SOBJ)

$(SOBJ): c/$(TARGET).o
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) -o $@ $(SWISOLIB) $< $(LIBS)
	# strip -x $@

check::
install::
clean:
	rm -f c/$(TARGET).o
distclean: clean
	rm -f $(SOBJ)



