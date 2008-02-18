## $Id: Makefile,v 1.13 2008/02/18 05:34:31 vances Exp $

export HSS_ID="Motivity HSS"
export HSS_VERSION=1.0

ERL = erl

docdir = ./doc

.PHONY=all
all: beams check html

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

