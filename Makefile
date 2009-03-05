## $Id: Makefile,v 1.13 2008/02/18 05:34:31 vances Exp $

export HSS_ID="Motivity HSS"
export HSS_VERSION=1.0

export ERL=erl

docdir = ./doc

.PHONY=all
all: beams html

.PHONY=beams
beams:
	cd src && $(MAKE) $@

edocs	= $(addprefix $(docdir)/, overview.edoc)

.PHONY=edoc
edoc:	$(edocs)
	
$(docdir)/%.edoc:	$(docdir)/%.edoc-in
	sed -e 's!%VERSION%!$(HSS_VERSION)!' < $< > $@

.PHONY=html
html:	edoc
	$(ERL) -noshell -run edoc_run application "'hss'" '"."' \
			'[{sort_functions,false}]'
	@cd doc && (sed -e 's!erlang logo!3Gdb logo!' \
			  -e 's!erlang.png!3Gdb.15.png!' < overview-summary.html > t.html; \
			  mv t.html overview-summary.html; \
			  sed -e 's!erlang logo!3Gdb logo!' \
			  -e 's!erlang.png!3Gdb.15.png!' < hss.html > t.html; \
			  mv t.html hss.html; \
			  sed -e 's!erlang logo!3Gdb logo!' \
			  -e 's!erlang.png!3Gdb.15.png!' < hss_app.html > t.html; \
			  mv t.html hss_app.html; \
			  sed -e 's!erlang logo!3Gdb logo!' \
			  -e 's!erlang.png!3Gdb.15.png!' < hss_sup.html > t.html; \
			  mv t.html hss_sup.html; \
			  sed -e 's!erlang logo!3Gdb logo!' \
			  -e 's!erlang.png!3Gdb.15.png!' < hss_server.html > t.html; \
			  mv t.html hss_server.html; \
			  sed -e 's!erlang logo!3Gdb logo!' \
			  -e 's!erlang.png!3Gdb.15.png!' < hss_xml.html > t.html; \
			  mv t.html hss_xml.html; \
			  sed -e 's!erlang logo!3Gdb logo!' \
			  -e 's!erlang.png!3Gdb.15.png!' < milenage.html > t.html; \
			  mv t.html milenage.html)

.PHONY=all_doc
all_doc:	edoc
	$(ERL) -noshell -run edoc_run application "'hss'" '"."' \
			'[{sort_functions,false},{private,true},{todo,true}]'

.PHONY=clean
clean:
	cd src && $(MAKE) $@
	-cd doc && rm -f edoc-info index.html modules-frame.html \
			overview-summary.html packages-frame.html \
			stylesheet.css overview.edoc erlang.png
	-cd test && rm -f *.beam
	-cd test/log && rm -rf *.html ct_run.* variables-ct*

.PHONY=check
check:	beams
	dialyzer -c ebin

.PHONY=test
test:	beams
	run_test -dir test -logdir test/log -pa `pwd`/ebin +W w

