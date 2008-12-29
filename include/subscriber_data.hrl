%%% $Id: subscriber_data.hrl,v 1.14 2008/02/18 05:34:31 vances Exp $
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c) 2008, Motivity Telecom Inc.
%%% 
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 
%%%    - Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    - Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in
%%%      the documentation and/or other materials provided with the 
%%%      distribution.
%%%    - Neither the name of Motivity Telecom  nor the names of its
%%%      contributors may be used to endorse or promote products derived
%%%      from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%---------------------------------------------------------------------
%%%
%%% This header file defines the format of the subscriber data
%%% used in the hss application and stored in the database.
%%%
%%% reference: Organization of Subscriber Data (3GPP TS 23.008)
%%%


%% @type guid() = {Node, Now, Reference}
%% 	Node = node()
%% 	Now = {MegaSecs, Secs, MicroSecs}
%% 	MegaSecs = integer()
%% 	Secs = integer()
%% 	MicroSecs = integer()
%% 	Reference = reference().
%% A globally unique reference used as an index key in permanent
%% subscriber data tables.

%% @type serverCapabilities() = #serverCapabilities{
%% 		mandatory_capabilities = [integer()],
%% 		optional_capabilities = [integer()],
%% 		sCSCFName = [sCSCFName()]}.
%% 	Information to assist the I-CSCF during the process of 
%% 	selecting an S-CSCF for a certain IMS Subscription. 
%%
-record(serverCapabilities,
		{mandatory_capabilities = [],
		optional_capabilities = [],
		sCSCFName}).

%% @type subscriber() = #subscriber{
%% 		subscriberID = subscriberID(),
%% 		privateUserIDs = [privateUserIDs()],
%% 		sharedPublicUserIDs = sharedPublicUserIDs(),
%% 		implicitRegistrationSets = implicitRegistrationSets(),
%% 		serverCapabilities = serverCapabilities()}.
%% 	An entry in the <tt>subscriber</tt> database table storing
%% 	information related to an IMS services subscription.
%%
%% @type subscriberID() = guid().
%% 	Index into the <tt>subscriber</tt> table.
%%
%% @type sharedPublicUserIDs() = [publicUserID()].
%% 	The list of Public User Identities which are shared between
%% 	users in a subscription.
%%
%% @type implicitRegistrationSets() = //stdlib/ordsets:ordered_set().
%% 	The set of sets of Public User Identifiers to be
%% 	registered implicitly.  An identifier may appear
%% 	in only one implicit registration set within the sets.

%%
-record(subscriber,
		{subscriberID,
		privateUserIDs = [],
		sharedPublicUserIDs = [],
		implicitRegistrationSets = [],
		serverCapabilities = #serverCapabilities{}}).

%% @type user() = #user{
%% 		user = user(),
%% 		publicUserIDs = [publicUserIDs()],
%% 		k = k(),
%% 		opc = opc(),
%% 		sequence = sequence(),
%% 		subscriberID = subscriberID()}.
%% 	An entry in the <tt>user</tt> database table storing
%% 	information related to a user of an IMS services subscription.
%%
%% @type privateUserID() = string().
%% 	A unique global identity defined by the Home Network Operator,
%% 	which may be used within the home network to identify the user's
%% 	subscription.  It takes the form of a Network Access Identifier
%% 	(NAI) as defined in
%% 	<a href="http://www.ietf.org/rfc/rfc2486.txt">IETF RFC 2486</a>.
%%
%% @type k() = binary().
%% 	A 128 bit subscriber authentication key (K) known only to the
%% 	HSS and the ISIM/USIM application on the UICC.
%%
%% @type opc() = binary().
%% 	A 128 bit key (OPc) derived from the Operator Variant Algorithm
%% 	Configuration Field (OP) and {@link k(). K} known only to the
%% 	HSS and the ISIM/USIM application on the UICC.
%%
%% @type sequence() = integer().
%% 	A counter identifying the authentication sequence number.
%%
-record(user,
		{privateUserID,
		publicUserIDs = [],
		k,
		opc,
		sequence = 0,
		subscriberID}).

%% @type address() = #address{
%% 		publicUserID = publicUserID(),
%% 		subscriberID = subscriberID(),
%% 		serviceProfileID = serviceProfileID(),
%% 		barringIndicator = barringIndicator(),
%% 		authVisitedNetworkIDs = authVisitedNetworkIDs(),
%% 		servicesUnregState = servicesUnregState(),
%% 		serviceIndication = serviceIndication()}.
%%		An entry in the <tt>address</tt> database table storing
%% 	information for a Public User ID.
%%
%% @type publicUserID() = sipURL() | telURL().
%% 	An address of a user to used by other users to establish communication.
%%
%% @type sipURL() = string().
%% 	A Uniform Resource Locator (URL) having a "sip" Uniform Resource 
%% 	Identifier (URI) as defined in
%% 	<a href="http://www.ietf.org/rfc/rfc3261.txt">IETF RFC 3261</a>.
%% 	
%% @type telURL() = string().
%% 	A Uniform Resource Locator (URL) having a "tel" Uniform Resource 
%% 	Identifier (URI) as defined in
%% 	<a href="http://www.ietf.org/rfc/rfc3966.txt">IETF RFC 3966</a>.
%% 	
%% @type barringIndicator() = boolean().
%% 	Indicates whether or not the Public User Identity is barred
%% 	from any IMS communication (except registrations and de-registrations).
%%
%% @type authVisitedNetworkIDs() = [string()].
%% 	The list of visited network identifiers allowed for roaming.
%%
%% @type servicesUnregState() = boolean().
%% 	Indicates whether the identity has services related to unregistered 
%% 	state or not. 
%%
%% @type serviceIndication() = void().
%% 	A identifies exactly one set of service related transparent data.
%%
-record(address,
		{publicUserID,
		subscriberID,
		serviceProfileID,
		barringIndicator = false,
		authVisitedNetworkIDs = [],
		servicesUnregState = false,
		serviceIndication}).

%% @type profile() = #profile{
%% 		serviceProfileID = serviceProfileID(),
%% 		initialFilterCriteriaIDs = [initialFilterCriteriaID()],
%% 		sharedIFCSetIDs = sharedIFCSetIDs(),
%% 		subscribedMediaProfileID = subscribedMediaProfileID()}.
%% 	An entry in the <tt>profile</tt> database table storing
%% 	a collection of IMS service related information.
%%
%% @type serviceProfileID() = guid().
%% 	Index into the the <tt>profile</tt> table.
%%
%% @type sharedIFCSetIDs() = [integer()].
%% 	Identifies sets of Initial Filter Criteria shared by more than
%% 	one subscriber.
%%
%% @type subscribedMediaProfileID() = integer().
%% 	Identifies a set of session description parameters that the
%% 	subscriber is authorized to request.
%%
-record(profile,
		{serviceProfileID,
		initialFilterCriteriaIDs,
		sharedIFCSetIDs,
		subscribedMediaProfileID}).

%% @type filter() = #filter{
%% 		initialFilterCriteriaID = initialFilterCriteriaID(),
%% 		priority = priority(),
%% 		iprofilePartIndicator = profilePartIndicator(),
%% 		applicationServerName = applicationServerName(),
%% 		defaultHandling = defaultHandling(),
%% 		serviceInformation = serviceInformation(),
%% 		conditionTypeCNF = conditionTypeCNF(),
%% 		servicePointTriggerIDs = [servicePointTriggerID()]}.
%% 	An entry in the <tt>filter</tt> database table storing
%% 	information about service logic in a service profile referred
%% 	to as Initial Filter Criteria (iFC).
%%
%% @type initialFilterCriteriaID() = guid().
%% 	Index into the the <tt>filter</tt> table.
%%
%% @type priority() = integer().
%% 	Indicates the priority of the Filter Criteria.
%%
%% @type profilePartIndicator() = registered | unregistered.
%% 	Indicates if the iFC is a part of the registered or 
%% 	unregistered user profile.
%%
%% @type applicationServerName() = sipURL().
%% 	The SIP address of an Application Server.
%%
%% @type defaultHandling() = session_continued | session_terminated.
%% 	Indicates whether the dialog should be released if the Application
%% 	Server could not be reached.
%%
%% @type serviceInformation() = string().
%% 	Application Server specific information.
%%
%% @type conditionTypeCNF() = boolean().
%% 	Indicates the form of boolean expression in the Trigger Point 
%% 	associated with the FilterCriteria.  It is true for Conjuctive
%% 	Normal Form (CNF) and false for Disjunctive Normal Form (DNF).
%%
-record(filter,
		{initialFilterCriteriaID,
		priority,
		profilePartIndicator,
		applicationServerName,
		defaultHandling,
		serviceInformation,
		conditionTypeCNF = false,
		servicePointTriggerIDs}).

%% @type trigger() = #trigger{
%% 		servicePointTriggerID = servicePointTriggerID(),
%% 		conditionNegated = conditionNegated(),
%% 		group = group(),
%% 		registrationType = registrationType(),
%% 		condition = condition()}.
%% 	An entry in the <tt>trigger</tt> database table storing
%% 	information about a service point trigger in a filter.
%%
%% @type servicePointTriggerID() = guid().
%% 	Index into the the <tt>trigger</tt> table.
%%
%% @type conditionNegated() = boolean().
%% 	Indicates whether the individual SPT instance is negated 
%% 	(logical NOT expression).
%%
%% @type group() = [integer()].
%% 	The grouping of SPTs that will configure the sub-expressions
%% 	inside a CNF or DNF expression. 
%%
%% @type registrationType() = registration | reregistration | deregistration.
%% 	Relevant only to the SIP Method SPT with a value of "REGISTER".
%%
%% @type condition() = {requestURI, RequestURI} | {method, Method}
%% 		| {sipHeader, Header} | {sipHeader, Header, Content} 
%% 		| {sessionCase, SessionCase} | {sessionDescription, Line}
%% 		| {sessionDescription, Line, Content}.
%% 	The condition which will match a SIP message for this trigger.
%% 	`RequestURI', `Method', `Header', `Content', `Line' are string().
%% 	`SessionCase' may be one of `originating_session',
%% 	`terminating_registered', `terminating_unregistered' or
%% 	`originating_unregistered'.
%%
-record(trigger,
		{servicePointTriggerID,
		conditionNegated = false,
		group = [],
		registrationType,
		condition}).

%% @type location() = #location{
%% 		publicUserID = publicUserID(),
%% 		privateUserID = privateUserID(),
%% 		state = state(),
%% 		sCSCFName = sCSCFName(),
%% 		sCSCFDiameter = sCSCFDiameter(),
%% 		authenticationPending = boolean()}.
%%
%% @type sCSCFName() = sipURL().
%% 	The SIP address of an S-CSCF.
%%
%% @type sCSCFDiameter() = sipURL().
%% 	The diameter client address of an S-CSCF.
%%
%% @type state() = registered | unregistered | not_registered.
%% 	The current registration state of the Public User Identity.
%% 	The unregistered state may be a consequence of a terminating call
%% 	or an S-CSCF keeping the user profile stored.  An identity which
%% 	is not registered at an S-CSCF may not appear in the location table.
%%
-record(location,
		{publicUserID,
		privateUserID,
		state,
		sCSCFName,
		sCSCFDiameter,
		authenticationPending = false}).

