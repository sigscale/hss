## $Id: Makefile,v 1.7 2008/02/17 11:50:10 vances Exp $

ERL = erl
ERLC = erlc
ERLCFLAGS = -v -W -I $(incdir) -I /Users/otpuser/lib/erlang/lib/xmerl-1.1.6/include -o $(beamdir) +debug_info

incdir	= ../include
beamdir	= ../ebin
htmldir	= ../doc

incfiles	= $(incdir)/subscriber_data.hrl $(incdir)/procedures.hrl

erlfiles	= hss.erl hss_app.erl hss_sup.erl hss_server.erl milenage.erl \
			hss_xml.erl

beamfiles	= $(addprefix $(beamdir)/, $(erlfiles:.erl=.beam))

htmlfiles	= $(addprefix $(htmldir)/, $(erlfiles:.erl=.html))

.PHONY=beams
beams:	$(beamfiles) $(beamdir)/hss.app

$(beamdir)/%.beam:%.erl	$(incfiles)
	$(ERLC) $(ERLCFLAGS) $<

$(beamdir)/%.app:	%.app-in
	sed -e 's!%ID%!$(HSS_ID)!' \
			-e 's!%VERSION%!$(HSS_VERSION)!' < $< > $@

.PHONY=clean
clean:
	-rm -f $(beamdir)/hss.app
	-rm -f $(beamfiles)
	-rm -f $(htmlfiles)

