CONFIG_GENERATED = config.log config.status autom4te*.cache
.PHONY : all clean test install distclean realclean

all:
	(cd src; $(MAKE))

clean:
	rm -f core *~
	(cd src; $(MAKE) clean)
	(cd test; $(MAKE) clean)

test:
	(cd test; $(MAKE) test)

install:
	(cd src; $(MAKE) install)

distclean: clean
	(cd src; $(MAKE) distclean)
	(cd test; $(MAKE) distclean)
	rm -rf $(CONFIG_GENERATED)

realclean: clean
	(cd src; $(MAKE) realclean)
	(cd test; $(MAKE) realclean)
	rm -rf $(CONFIG_GENERATED) configure


