CONFIG_GENERATED = config.log config.status autom4te*.cache VERSION
.PHONY: all clean test check install distclean realclean

all:
	cd src; $(MAKE)
	cd doc; $(MAKE)

clean:
	rm -f core *~
	cd src; $(MAKE) clean
	cd doc; $(MAKE) clean
	cd test; $(MAKE) clean

check:
	cd test; $(MAKE) check

install: all
	cd src; $(MAKE) install
	cd doc; $(MAKE) install

distclean: clean
	cd src; $(MAKE) distclean
	cd doc; $(MAKE) distclean
	cd test; $(MAKE) distclean
	rm -rf $(CONFIG_GENERATED)

maintainer-clean: clean
	cd src; $(MAKE) maintainer-clean
	cd doc; $(MAKE) maintainer-clean
	cd test; $(MAKE) maintainer-clean
	rm -rf $(CONFIG_GENERATED) configure
