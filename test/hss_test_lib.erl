%%% $Id: hss_test_lib.erl,v 1.4 2008/02/18 05:34:31 vances Exp $
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
%%% @doc Library of common functions for test suites.
%%%--------------------------------------------------------------------
%%%
-module(hss_test_lib).

-export([init_per_suite/1, end_per_suite/1]).
-export([triggerid/0, triggerids/0, filterid/0, profileid/0, subscriberid/1]).
-export([userid/4, address/3, privateid/0, telurl/0]).

-include("../include/subscriber_data.hrl").
-include_lib("ct.hrl").

-define(MCC, 248).
-define(MNC, 323).

%% @spec (Config) -> Config
%% 	Config = [tuple()]
%% @doc Initiation before the whole suite.
%%
init_per_suite(Config) ->
	ok = application:start(mnesia),
	mnesia:wait_for_tables([schema], 10000),
	Tables = [subscriber,user,profile,address,filter,trigger,location],
	case mnesia:wait_for_tables(Tables, 4000) of
		ok ->
			ok;
		_ ->
			ok = application:stop(mnesia),
			ok = mnesia:create_schema([node()]),
			ok = application:start(mnesia),
			{ok, Tables} = hss:install([node()])
	end,
	ok = crypto:start(),
	ok = application:start(hss),
	OP = crypto:rand_bytes(16),
   [{op, OP} | Config].

%% @spec (Config) -> any()
%% 	Config = [tuple()]
%% @doc Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = application:stop(hss),
	ok = application:stop(crypto),
	ok = application:stop(mnesia).


%% @spec () -> servicePointTriggerID()
%% @doc Create a single trigger.
triggerid() ->
	hss:add_trigger(false, [0], {method, "INVITE"}).

%% @spec () -> [servicePointTriggerID()]
%% @doc Create an example set of triggers.
%%
triggerids() ->
	SPT1 = hss:add_trigger(false, [0], {method, "INVITE"}),
	SPT2 = hss:add_trigger(false, [0], {method, "MESSAGE"}),
	SPT3 = hss:add_trigger(false, [0], {method, "SUBSCRIBE"}),
	SPT4 = hss:add_trigger(false, [1], {method, "INVITE"}),
	SPT5 = hss:add_trigger(false, [1], {method, "MESSAGE"}),
	SPT6 = hss:add_trigger(false, [1], {sipHeader, "FROM", "joe"}),
	[SPT1, SPT2, SPT3, SPT4, SPT5, SPT6].

%% @spec () -> initialFilterCriteriaID()
%% @doc Create a filter.
%%
filterid() ->
	hss:add_filter(0, "sip:as.acme.net", undefined, undefined,
			undefined, true, triggerids()).

%% @spec () -> serviceProfileID()
%% @doc Create a profile.
%%
profileid() ->
	InitialFilterCriteriaID = case catch hss:get_filter() of
		{'EXIT', _} ->
			filterid();
		Filter ->
			Filter
	end,
	hss:add_profile([InitialFilterCriteriaID], [], 0).

%% @spec (Config) -> subscriberID()
%% 	Config = [tuple()]
%% @doc Create a subscriber.
%%
subscriberid(Config) ->
	PrivateUserID = privateid(),
	DefaultID = "sip:" ++ PrivateUserID,
	TelURL = telurl(),
	PublicUserIDs = [DefaultID, TelURL],
	ImplicitRegistrationSets = hss:add_implicit_registration_set(PublicUserIDs,
			hss:new_implicit_registration_sets()),
	SubscriberID = hss:add_subscriber([PrivateUserID], [],
			ImplicitRegistrationSets, #serverCapabilities{}),
	userid(SubscriberID, PrivateUserID, PublicUserIDs, Config),
	address(SubscriberID, DefaultID, TelURL),
	SubscriberID.

%% @spec (SubscriberID, PrivateUserID, PublicUserIDs, Config)
%% 		-> privateUserID()
%% 	SubscriberID = subscriberID()
%% 	PrivateUserID = privateUserID()
%% 	PublicUserIDs = [publicUserID()]
%% 	Config = [tuple()]
%% @doc Create a user.
%%
userid(SubscriberID, PrivateUserID, PublicUserIDs, Config) ->
	K = crypto:rand_bytes(16),
	OPc = milenage:opc(K, ?config(op, Config)),
	ok = hss:add_user(SubscriberID, PrivateUserID, PublicUserIDs, K, OPc),
	PrivateUserID.

%% @spec (SubscriberID, DefaultID, TelURL)
%% 		-> ok
%% 	SubscriberID = subscriberID()
%% 	DefaultID = sipURL()
%% 	TelURL = telURL()
%% @doc Create addresses.
%%
address(SubscriberID, DefaultID, TelURL) ->
	ServiceProfileID = case catch hss:get_profile() of
		{'EXIT', _} ->
			profileid();
		Profile ->
			Profile	
	end,
	ok = hss:add_address(SubscriberID, DefaultID, ServiceProfileID,
			["acme.net"], true, false),
	ok = hss:add_address(SubscriberID, TelURL, ServiceProfileID,
			["acme.net"], false, false).

%% @spec () -> privateUserID()
%% @doc Create private user identity.
%%
privateid() ->
	MCC = integer_to_list(?MCC),
	MNC = integer_to_list(?MNC),
	MCC ++ MNC ++ digits(10) ++ "@" ++ MNC ++ "." ++ MCC
			++ ".imsi.3gppnetwork.org".

%% @spec () -> telURL()
%% @doc Create tel URL public identity.
%%
telurl() ->
	"tel:+1647" ++ digits(7).

%% @spec (N) -> telURL()
%% 	N = integer()
%% @doc Create a digit string N long.
%% @private
digits(N) ->
	digits(N, []).
digits(0, Acc) ->
	Acc;
digits(N, Acc) ->
	digits(N - 1, [crypto:rand_uniform(48, 57) | Acc]).

