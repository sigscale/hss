%%% $Id: hss_api_SUITE.erl,v 1.7 2008/02/18 05:34:31 vances Exp $
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
%%% Permanent subscriber data tests
%%%--------------------------------------------------------------------
%%%
-module(hss_api_SUITE).

-compile(export_all).

-include("../include/subscriber_data.hrl").

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
	hss_test_lib:init_per_suite(Config).

%% @spec (Config) -> any()
%% 	Config = [tuple()]
%% @doc Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	hss_test_lib:end_per_suite(Config).

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
	[trigger, filter, profile, subscriber, user].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

trigger() ->
	[{userdata, [{doc, "Create a new service point trigger."}]}].

trigger(_Config) ->
	ServicePointTriggerID = hss_test_lib:triggerid(),
	ServicePointTrigger = hss:get_trigger(ServicePointTriggerID),
	true = is_record(ServicePointTrigger, trigger).

filter() ->
	[{userdata, [{doc, "Create a new initial filter criteria."}]}].

filter(_Config) ->
	InitialFilterCriteriaID = hss_test_lib:filterid(),
	InitialFilterCriteria = hss:get_filter(InitialFilterCriteriaID),
	true = is_record(InitialFilterCriteria, filter).

profile() ->
	[{userdata, [{doc, "Create a new service profile."}]}].

profile(_Config) ->
	ServiceProfileID = hss_test_lib:profileid(),
	ServiceProfile = hss:get_profile(ServiceProfileID),
	true = is_record(ServiceProfile, profile).

subscriber() ->
	[{userdata, [{doc, "Create a new subscription."}]}].

subscriber(Config) ->
	SubscriberID = hss_test_lib:subscriberid(Config),
	Subscriber = hss:get_subscriber(SubscriberID),
	[PrivateUserID] = Subscriber#subscriber.privateUserIDs,
	User = hss:get_user(PrivateUserID),
	SubscriberID = User#user.subscriberID,
	[PublicUserID | _] = User#user.publicUserIDs,
	Address = hss:get_address(PublicUserID),
	SubscriberID = Address#address.subscriberID.

user() ->
	[{userdata, [{doc, "Add a user to a subscription."}]}].

user(Config) ->
	SubscriberID = case catch hss:get_subscriber() of
		#subscriber{} = ExistingSub ->
			ExistingSub;
		_ ->
			hss_test_lib:subscriberid(Config)
	end,
	PrivateUserID = hss_test_lib:privateid(),
	DefaultID = "sip:" ++ PrivateUserID,
	TelURL = hss_test_lib:telurl(),
	PublicUserIDs = [DefaultID, TelURL],
	S = hss:get_subscriber(SubscriberID),
	RegSets = hss:add_implicit_registration_set(PublicUserIDs,
			S#subscriber.implicitRegistrationSets),
	{atomic, ok} = mnesia:transaction(fun() ->
				mnesia:write(subscriber, S#subscriber{implicitRegistrationSets
						= RegSets}, write) end),
	hss_test_lib:userid(SubscriberID, PrivateUserID, PublicUserIDs, Config),
	hss_test_lib:address(SubscriberID, DefaultID, TelURL),
	User = hss:get_user(PrivateUserID),
	SubscriberID = User#user.subscriberID,
	PublicUserIDs = User#user.publicUserIDs,
	Subscriber = hss:get_subscriber(SubscriberID),
	true = lists:member(PrivateUserID, Subscriber#subscriber.privateUserIDs),
	Address1 = hss:get_address(DefaultID),
	SubscriberID = Address1#address.subscriberID,
	Address2 = hss:get_address(TelURL),
	SubscriberID = Address2#address.subscriberID.
	
	
%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

