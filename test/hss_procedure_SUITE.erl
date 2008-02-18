%%% $Id: hss_procedure_SUITE.erl,v 1.8 2008/02/18 05:34:31 vances Exp $
%%%--------------------------------------------------------------------
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
%%%--------------------------------------------------------------------
%%% Procedure tests
%%%--------------------------------------------------------------------
%%%
-module(hss_procedure_SUITE).

-compile(export_all).

-include("ct.hrl").

-include("../include/subscriber_data.hrl").
-include("../include/procedures.hrl").

-define(SCSCFName, "sip:s-cscf.acme.net").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

%% @spec () -> DefaultData
%% 	DefaultData = [tuple()]
%% @doc Require variables and set default values for the suite.
%%
suite() ->
	[{timetrap, {minutes, 1}}].

%% @spec (Config) -> Config
%% 	Config = [tuple()]
%% @doc Initiation before the whole suite.
%%
init_per_suite(Config) ->
	Config1 = hss_test_lib:init_per_suite(Config),
	SubscriberID = hss_test_lib:subscriberid(Config1),
	PrivateUserID = hss_test_lib:privateid(),
	DefaultID = "sip:" ++ PrivateUserID,
	TelURL = hss_test_lib:telurl(),
	PublicUserIDs = [DefaultID, TelURL],
	S = hss:get_subscriber(SubscriberID),
	PrivateUserIDs = S#subscriber.privateUserIDs,
	RegSets = hss:add_implicit_registration_set(PublicUserIDs,
			S#subscriber.implicitRegistrationSets),
	{atomic, ok} = mnesia:transaction(fun() ->
				mnesia:write(subscriber, S#subscriber{
						privateUserIDs = PrivateUserIDs ++ [PrivateUserID],
						implicitRegistrationSets = RegSets}, write) end),
	hss_test_lib:userid(SubscriberID, PrivateUserID, PublicUserIDs, Config1),
	hss_test_lib:address(SubscriberID, DefaultID, TelURL),
	[{subscriberID, SubscriberID} | Config1].

%% @spec (Config) -> any()
%% 	Config = [tuple()]
%% @doc Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	hss_test_lib:end_per_suite(Config).

%% @spec (TestCase, Config) -> Config
%% 	TestCase = atom()
%% 	Config = [tuple()]
%% @doc Initiation before each test case.
%%
init_per_testcase(_, Config) ->
	{ok, Pid} = hss:start_link([]),
	[{routingInformation, Pid} | Config].

%% @spec (TestCase, Config) -> any()
%% 	TestCase = atom()
%% @doc Cleanup after each test case.
%%
end_per_testcase(_, Config) ->
	Pid = ?config(routingInformation, Config),
	exit(Pid, shutdown).

%% @spec () -> Sequences 
%% 	Sequences = [{SeqName, Testcases}]
%% 	SeqName = atom()
%% 	Testcases = [atom()]
%% @doc Group test cases into a test sequence.
%%
sequences() -> 
	[].

%% @spec () -> TestCases
%% 	TestCases = [Case]
%% 	Case = atom()
%% @doc Returns a list of all test cases in this test suite.
%%
all() -> 
	[ursq, arq, srn, ulq, arq_auts].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

ursq() ->
	[{userdata, [{doc, "User registration status query."}]}].

ursq(Config) ->
	RoutingInformation = ?config(routingInformation, Config),
	SubscriberID = ?config(subscriberID, Config),
	Subscriber = hss:get_subscriber(SubscriberID),
	[PrivateUserID | _] = Subscriber#subscriber.privateUserIDs,
	User = hss:get_user(PrivateUserID),
	[PublicUserID | _] = User#user.publicUserIDs,
	VisitedNetworkIdentifier = "acme.net",
	TypeOfAuthorization = registration,
	Response = hss:user_registration_status_query(PublicUserID,
			VisitedNetworkIdentifier, TypeOfAuthorization,
			PrivateUserID, RoutingInformation),
	#user_registration_status_response{experimentalResult = first_registration}
			= Response.
	
arq() ->
	[{userdata, [{doc, "Authentication request."}]}].

arq(Config) ->
	RoutingInformation = ?config(routingInformation, Config),
	SubscriberID = ?config(subscriberID, Config),
	Subscriber = hss:get_subscriber(SubscriberID),
	[PrivateUserID | _] = Subscriber#subscriber.privateUserIDs,
	User = hss:get_user(PrivateUserID),
	[PublicUserID | _] = User#user.publicUserIDs,
	AuthenticationData = #authenticationData{},
	SCSCFName = "sip:scscf1.acme.net",
	NumItems = 5,
	Response = hss:authentication_request(PublicUserID, PrivateUserID,
			NumItems, AuthenticationData, SCSCFName, RoutingInformation),
	#authentication_request_response{resultCode = success,
			publicUserID = PublicUserID, privateUserID = PrivateUserID,
         numberAuthenticationItems = NumItems,
         authenticationData = AUTNs} = Response,
	NumItems = length(AUTNs),
	autn(User#user.opc, User#user.k, 1, AUTNs, 0).

srn() ->
	[{userdata, [{doc, "S-CSCF registration/deregistration notification."}]}].

srn(Config) ->
	RoutingInformation = ?config(routingInformation, Config),
	DataDir = ?config(data_dir, Config),
	SubscriberID = ?config(subscriberID, Config),
	Subscriber = hss:get_subscriber(SubscriberID),
	[PrivateUserID | AssociatedPrivateIDs]
			= Subscriber#subscriber.privateUserIDs,
	User = hss:get_user(PrivateUserID),
	[PublicUserID | _] = User#user.publicUserIDs,
	SCSCFName = ?SCSCFName,
	ServerAssignmentType = registration,
	UserDataAlreadyAvailable = false,
	Response = hss:scscf_registration_notification(PublicUserID, SCSCFName,
			PrivateUserID, ServerAssignmentType, UserDataAlreadyAvailable,
			RoutingInformation),
	#scscf_registration_notification_response{resultCode = success,
			userProfile = UserProfile,
			associatedPrivateIDs = AssociatedPrivateIDs} = Response,
	Schema = filename:join(DataDir, "CxDataType_Rel7.xsd"),
	{ok, XsdState} = xmerl_xsd:process_schema(Schema),
	{XmlElement, _} = xmerl_scan:string(UserProfile),
	case xmerl_xsd:validate(XmlElement, XsdState) of
		{XmlElement, _} ->
			ok;
		{error, Reasons} ->
			Furi = fun({value_not_anyURI, _}) ->
					false;
				(_) ->
					true
			end,
			false = lists:any(Furi, Reasons)
	end.

ulq() ->
	[{userdata, [{doc, "User location query."}]}].

ulq(Config) ->
	RoutingInformation = ?config(routingInformation, Config),
	SubscriberID = ?config(subscriberID, Config),
	Subscriber = hss:get_subscriber(SubscriberID),
	[PrivateUserID | _] = Subscriber#subscriber.privateUserIDs,
	User = hss:get_user(PrivateUserID),
	[PublicUserID | _] = User#user.publicUserIDs,
	Response = hss:user_location_query(PublicUserID, RoutingInformation),
	#user_location_response{resultCode = success,
			sCSCFName = "sip:s-cscf.acme.net"} = Response.

arq_auts() ->
	[{userdata, [{doc, "Authentication synchronization request."}]}].

arq_auts(Config) ->
	RoutingInformation = ?config(routingInformation, Config),
	SubscriberID = ?config(subscriberID, Config),
	Subscriber = hss:get_subscriber(SubscriberID),
	[PrivateUserID | _] = Subscriber#subscriber.privateUserIDs,
	User = hss:get_user(PrivateUserID),
	[PublicUserID | _] = User#user.publicUserIDs,
	RAND = crypto:rand_bytes(16),
	SEQ = crypto:rand_uniform(0, 16#7FFFFFFFFFF),
	AUTS = auts(SEQ, User, RAND),
	AuthenticationData = #authenticationData{
			authenticationScheme = "Digest-AKAv1-MD5",
			authorizationInformation = <<RAND/binary, AUTS/binary>>},
	SCSCFName = "sip:s-cscf.acme.net",
	NumItems = 5,
	Response = hss:authentication_request(PublicUserID, PrivateUserID,
			NumItems, AuthenticationData, SCSCFName, RoutingInformation),
	#authentication_request_response{resultCode = success,
			publicUserID = PublicUserID, privateUserID = PrivateUserID,
         numberAuthenticationItems = NumItems,
         authenticationData = AUTNs} = Response,
	NewSEQ = (SEQ + 1) rem 16#7FFFFFFFFFF,
	NumItems = length(AUTNs),
	autn(User#user.opc, User#user.k, NewSEQ, AUTNs, 0).
	
%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

auts(SEQ, User, RAND) ->
	OPc = User#user.opc,
	K = User#user.k,
	AK = milenage:f5star(OPc, K, RAND),
	SQN = <<SEQ:43, 0:5>>,
	ConcSQN = crypto:exor(SQN, AK),
	AMF = <<0:16>>,
	MAC = milenage:f1star(OPc, K, RAND, SQN, AMF),
	<<ConcSQN/binary, MAC/binary>>.

autn(OPc, K, SEQ, [#authenticationData{
		authenticationScheme = "Digest-AKAv1-MD5",
		authenticationInformation = <<RAND:16/binary, AUTN:16/binary>>,
		authorizationInformation = RES,
		confidentialityKey = CK,
		integrityKey = IK} | T], N) ->
	<<ConcSQN:6/binary, AMF:2/binary, MAC:8/binary>> = AUTN,
	{RES, CK, IK, AK} = milenage:f2345(OPc, K, RAND),
	SQN = crypto:exor(ConcSQN, AK),
	<<SEQ:43, N:5>> = SQN,
	MAC = milenage:f1(OPc, K, RAND, SQN, AMF),
	autn(OPc, K, SEQ, T, N + 1);
autn(_, _, _, [], _) ->
	ok.

