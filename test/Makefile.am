GOSH = @GOSH@
TEST_OUT = test.log test.dbm

EXTRA_DIST = test.scm

check : test

test :
	@rm -f $(TEST_OUT)
	$(GOSH) -I$(srcdir) test.scm > test.log

clean :
	rm -f core *~ $(TEST_OUT)

distclean : clean
