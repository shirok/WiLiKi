CONFIG_GENERATED = config.log config.status

all:
	(cd src; $(MAKE))

clean:
	rm -f core *~
	(cd src; $(MAKE) clean)

install:
	(cd src; $(MAKE) install)

distclean: clean
	(cd src; $(MAKE) distclean)
	rm -f $(CONFIG_GENERATED)

realclean: clean
	(cd src; $(MAKE) realclean)
	rm -f $(CONFIG_GENERATED) configure


