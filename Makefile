CONFIG_GENERATED = config.log config.status autom4te*.cache

all:
	(cd src; $(MAKE))

clean:
	rm -f core *~
	(cd src; $(MAKE) clean)

install:
	(cd src; $(MAKE) install)

distclean: clean
	(cd src; $(MAKE) distclean)
	rm -rf $(CONFIG_GENERATED)

realclean: clean
	(cd src; $(MAKE) realclean)
	rm -rf $(CONFIG_GENERATED) configure


