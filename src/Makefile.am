GOSH = @GOSH@
GAUCHE_CONFIG = @GAUCHE_CONFIG@
INSTALL_TYPE = @INSTALL_TYPE@

COMMON = wiliki.scm \
         wiliki/format.scm \
         wiliki/mcatalog.scm \
         wiliki/macro.scm \
         wiliki/rss.scm \
         wiliki/pasttime.scm 

scmlibdir = $(DESTDIR)`$(GAUCHE_CONFIG) --$(INSTALL_TYPE)libdir`

scmlib_SCRIPTS = $(COMMON) wiliki/msgs.jp

EXTRA_DIST = $(COMMON) wiliki/msgs.jp.euc

wiliki/msgs.jp: wiliki/msgs.jp.euc
	$(GOSH) -ugauche.charconv -e '(call-with-input-file "wiliki/msgs.jp.euc" (lambda (in) (call-with-output-file "wiliki/msgs.jp" (cut copy-port in <> :unit (quote char)))) :encoding "euc-jp")' -Eexit

extract :
	$(GOSH) ./extract.scm wiliki.scm wiliki/msgs.jp.euc

clean :
	rm -f core *~ wiliki/*~

distclean : clean
