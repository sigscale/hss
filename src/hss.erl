%%% $Id: hss.erl,v 1.22 2008/02/18 05:34:31 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2008 Motivity Telecom Inc.
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
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
%%% @doc This library module implements the APIs to the <tt>hss</tt>
%%% 		application.
%%%
-module(hss).
-copyright('Copyright (c) 2008 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.22 $').

%% export the hss oam API
-export([add_subscriber/1, add_subscriber/4]).
-export([add_user/1, add_user/5]).
-export([add_address/1, add_address/3, add_address/6]).
-export([get_subscriber/1, get_user/1, get_profile/1, get_address/1,
		get_addresses/1, get_filter/1, get_trigger/1, get_service/1]).
-export([get_subscriber/0, get_user/0, get_profile/0, get_address/0,
		get_filter/0, get_trigger/0, get_subscriber_id/1]).
-export([add_profile/1, add_profile/3, add_filter/2, add_filter/7]).
-export([add_trigger/3, add_trigger/4]).
-export([remove_profile/1, remove_filter/1, remove_trigger/1]).
-export([remove_subscriber/1, remove_user/1, remove_address/1]).
-export([new_implicit_registration_sets/0,
		add_implicit_registration_set/2, get_implicit_registration_set/2,
		remove_implicit_registration_set/2]).

%% export the private oam hss API
-export([install/1, start_link/1]).

%% export the hss procedure API
-export([user_registration_status_query/5]).
-export([scscf_registration_notification/6]).
-export([user_location_query/2, authentication_request/6]).

-include("subscriber_data.hrl").
-include("procedures.hrl").

-define(WAITFORSCHEMA, 10000).
-define(WAITFORTABLES,  10000).

%%----------------------------------------------------------------------
%%  The hss oam API
%%----------------------------------------------------------------------

%% @headerfile "subscriber_data.hrl"

%% @spec (Subscriber) -> subscriberID()
%% 	Subscriber = subscriber()
%% @doc Create a new subscriber in the database.
%%
add_subscriber(Subscriber) when is_record(Subscriber, subscriber) ->
	SubscriberID = make_ref(),
	case mnesia:transaction(fun() ->
			mnesia:write(Subscriber#subscriber{subscriberID
					= SubscriberID}) end) of
		{atomic, _Result} ->
			SubscriberID;
		Error ->
			error_logger:error_msg(mnesia:error_description(Error)),
			exit(Error)
	end.

%% @spec (PrivateUserIDs, SharedPublicUserIDs, ImplicitRegistrationSets,
%% 		ServerCapabilities) -> subscriberID()
%% 	PrivateUserIDs = [privateUserID()]
%% 	SharedPublicUserIDs = sharedPublicUserIDs()
%% 	ImplicitRegistrationSets = implicitRegistrationSets()
%% 	ServerCapabilities = serverCapabilities()
%%
%% @doc Create a new subscriber in the database.
%%
add_subscriber(PrivateUserIDs, SharedPublicUserIDs,
		ImplicitRegistrationSets, ServerCapabilities) 
		when is_list(PrivateUserIDs), is_list(hd(PrivateUserIDs)),
		is_list(SharedPublicUserIDs),
		is_record(ServerCapabilities, serverCapabilities) ->
	Subscriber = #subscriber{privateUserIDs = PrivateUserIDs,
			sharedPublicUserIDs = SharedPublicUserIDs,
			implicitRegistrationSets = ImplicitRegistrationSets,
			serverCapabilities = ServerCapabilities},
	add_subscriber(Subscriber).

%% @spec (InitialFilterCriteriaIDs) -> serviceProfileID()
%% 	InitialFilterCriteriaIDs = [filterID()]
%%
%% @doc Create a new service profile in the database.
add_profile(InitialFilterCriteriaIDs) when is_list(InitialFilterCriteriaIDs) ->
	Fref = fun(R) when is_reference(R) ->
				true;
			(_) ->
				false
	end,
	case lists:all(Fref, InitialFilterCriteriaIDs) of
		true ->
			ServiceProfileID = make_ref(),
			Profile = #profile{serviceProfileID = ServiceProfileID,
					initialFilterCriteriaIDs = InitialFilterCriteriaIDs},
			case mnesia:transaction(fun() -> mnesia:write(Profile) end) of
				{atomic, _Result} ->
					ServiceProfileID;
				Error ->
					error_logger:error_msg(mnesia:error_description(Error)),
					exit(Error)
			end;
		false ->
			exit(badarg)
	end.

%% @spec (InitialFilterCriteriaIDs, SharedIFCSetIDs,
%% 		SubscribedMediaProfileID) -> serviceProfileID()
%% 	InitialFilterCriteriaIDs = [filterID()]
%% 	SharedIFCSetIDs = [integer()]
%% 	SubscribedMediaProfileID = integer()
%%
%% @doc Create a new service profile in the database.
%%
add_profile(InitialFilterCriteriaIDs, SharedIFCSetIDs,
		SubscribedMediaProfileID)
		when is_list(InitialFilterCriteriaIDs),
		is_list(SharedIFCSetIDs),
		is_integer(SubscribedMediaProfileID), SubscribedMediaProfileID >= 0 ->
	Fref = fun(R) when is_reference(R) ->
				true;
			(_) ->
				false
	end,
	Fint = fun(N) when is_integer(N), N >= 0 ->
				true;
			(_) ->
				false
	end,
	case lists:all(Fref, InitialFilterCriteriaIDs)
				andalso lists:all(Fint, SharedIFCSetIDs) of
		true ->
			ServiceProfileID = make_ref(),
			Profile = #profile{serviceProfileID = ServiceProfileID,
					initialFilterCriteriaIDs = InitialFilterCriteriaIDs,
					subscribedMediaProfileID = SubscribedMediaProfileID,
					sharedIFCSetIDs = SharedIFCSetIDs},
			case mnesia:transaction(fun() -> mnesia:write(Profile) end) of
				{atomic, _Result} ->
					ServiceProfileID;
				Error ->
					error_logger:error_msg(mnesia:error_description(Error)),
					exit(Error)
			end;
		false ->
			exit(badarg)
	end.

%% @spec (SubscriberID, PrivateUserID, PublicUserIDs, K, OPc)
%% 		-> ok
%% 	SubscriberID = subscriberID()
%% 	PrivateUserID = privateUserID()
%% 	PublicUserIDs = [publicUserID()]
%% 	K = k()
%% 	OPc = opc()
%%
%% @doc Create a new user entry in the database.
%%
add_user(SubscriberID, PrivateUserID, PublicUserIDs, K, OPc)
		when is_reference(SubscriberID), is_list(PrivateUserID),
		is_list(PublicUserIDs), is_list(hd(PublicUserIDs)),
		is_binary(K), size(K) == 16, is_binary(OPc), size(OPc) == 16 ->
	User = #user{privateUserID = PrivateUserID,
			publicUserIDs = PublicUserIDs,
			k = K, opc = OPc,
			subscriberID = SubscriberID},
	add_user(User).

%% @spec (User) -> ok
%% 	User = user()
%% @doc Create a new user entry in the database.
%%
add_user(#user{privateUserID = PrivateUserID} = User) ->
	case mnesia:transaction(fun() ->
			[S] = mnesia:read(subscriber, User#user.subscriberID, write),
			case lists:member(PrivateUserID, S#subscriber.privateUserIDs) of
				true ->
					ok;
				false ->
					PrivateUserIDs = S#subscriber.privateUserIDs ++ [PrivateUserID],
					mnesia:write(subscriber,
							S#subscriber{privateUserIDs = PrivateUserIDs}, write)
			end,
			mnesia:write(user, User, write) end) of
		{atomic, _Result} ->
			ok;
		{aborted, Reason} = Error ->
			error_logger:warning_msg(mnesia:error_description(Error)),
			exit(Reason)
	end.
			
%% @spec (Address) -> ok
%% 	Address = address()
%% @doc Create a new address entry in the database.
%%
add_address(#address{publicUserID = "sip:" ++ _,
		subscriberID = SubscriberID,
		serviceProfileID = ServiceProfileID} = Address) ->
	add_address(Address, SubscriberID, ServiceProfileID);
add_address(#address{publicUserID = "tel:" ++ _,
		subscriberID = SubscriberID,
		serviceProfileID = ServiceProfileID} = Address) ->
	add_address(Address, SubscriberID, ServiceProfileID).

%% @hidden
add_address(Address, SubscriberID, ServiceProfileID) 
		when is_record(Address, address),
		is_reference(SubscriberID), is_reference(ServiceProfileID) ->
	case mnesia:transaction(fun() ->
			[_Subscriber] = mnesia:read(subscriber, SubscriberID, read),
			[_Profile] = mnesia:read(profile, ServiceProfileID, read),
			mnesia:write(address, Address, write) end) of
		{atomic, _Result} ->
			ok;
		{aborted, Reason} = Error ->
			error_logger:warning_msg(mnesia:error_description(Error)),
			exit(Reason)
	end.

%% @spec (SubscriberID, PublicUserID, ServiceProfileID,
%% 		AuthVisitedNetworkIDs, BarringIndicator, ServicesUnregState) -> ok
%% 	SubscriberID = subscriberID()
%% 	PublicUserID = publicUserID()
%% 	ServiceProfileID = serviceProfileID()
%% 	AuthVisitedNetworkIDs = authVisitedNetworkIDs()
%% 	BarringIndicator = barringIndicator()
%% 	ServicesUnregState = servicesUnregState()
%%
%% @doc Create a new address entry in the database.
%%
add_address(SubscriberID, PublicUserID, ServiceProfileID,
		AuthVisitedNetworkIDs, BarringIndicator, ServicesUnregState)
		when is_reference(SubscriberID), is_list(PublicUserID),
		is_reference(ServiceProfileID), is_list(AuthVisitedNetworkIDs),
		(BarringIndicator =:= true orelse BarringIndicator =:= false),
		(ServicesUnregState =:= true orelse ServicesUnregState =:= false) ->
	Address = #address{publicUserID = PublicUserID,
			subscriberID = SubscriberID,
			serviceProfileID = ServiceProfileID,
			authVisitedNetworkIDs = AuthVisitedNetworkIDs,
			barringIndicator = BarringIndicator,
			servicesUnregState = ServicesUnregState},
	add_address(Address, SubscriberID, ServiceProfileID).

%% @spec (Priority, ApplicationServerName) -> initialFilterCriteriaID()
%% 	Priority = integer()
%% 	ApplicationServerName = sipURL()
%%
%% @doc Create a new initial filter criteria (iFC) in the database.
%%
add_filter(Priority, ApplicationServerName) ->
	InitialFilterCriteriaID = make_ref(),
	Filter = #filter{initialFilterCriteriaID = InitialFilterCriteriaID,
			priority = Priority,
			applicationServerName = ApplicationServerName},
	case mnesia:transaction(fun() -> mnesia:write(Filter) end) of
		{atomic, _Result} ->
			InitialFilterCriteriaID;
		Error ->
			error_logger:error_msg(mnesia:error_description(Error)),
			exit(Error)
	end.

%% @spec (Priority, ApplicationServerName, ProfilePartIndicator,
%% 		DefaultHandling, ServiceInformation, ConditionTypeCNF,
%% 		ServicePointTriggerIDs) -> initialFilterCriteriaID()
%% 	Priority = integer()
%% 	ApplicationServerName = sipURL()
%% 	ProfilePartIndicator = undefined | profilePartIndicator()
%% 	DefaultHandling = undefined | continued | terminated
%% 	ServiceInformation = undefined | string()
%% 	ConditionTypeCNF = boolean()
%% 	ServicePointTriggerIDs = undefined | [servicePointTriggerID()]
%%
%% @doc Create a new initial filter criteria (iFC) in the database.
%%
add_filter(Priority, ApplicationServerName, ProfilePartIndicator,
		DefaultHandling, ServiceInformation, ConditionTypeCNF,
		ServicePointTriggerIDs) when is_integer(Priority), Priority >= 0,
		is_list(ApplicationServerName), (ProfilePartIndicator == undefined
		orelse ProfilePartIndicator == registered orelse
		ProfilePartIndicator == unregistered),
		(DefaultHandling == undefined orelse DefaultHandling == continued
		orelse DefaultHandling == terminated),
		(ServiceInformation == undefined orelse is_list(ServiceInformation)),
		(is_list(ServicePointTriggerIDs) andalso (ConditionTypeCNF == true
		orelse ConditionTypeCNF == false))
		orelse (ServicePointTriggerIDs == undefined
		andalso ConditionTypeCNF == undefined) ->
	InitialFilterCriteriaID = make_ref(),
	Filter = #filter{initialFilterCriteriaID = InitialFilterCriteriaID,
			priority = Priority,
			profilePartIndicator = ProfilePartIndicator,
			applicationServerName = ApplicationServerName,
			defaultHandling = DefaultHandling,
			serviceInformation = ServiceInformation,
			conditionTypeCNF = ConditionTypeCNF,
			servicePointTriggerIDs = ServicePointTriggerIDs},
	case mnesia:transaction(fun() -> mnesia:write(Filter) end) of
		{atomic, _Result} ->
			InitialFilterCriteriaID;
		Error ->
			error_logger:error_msg(mnesia:error_description(Error)),
			exit(Error)
	end.
	
%% @spec (ConditionNegated, Group, Condition, RegistrationType) ->
%% 		servicePointTriggerID()
%% 	ConditionNegated = bool()
%% 	Group = [integer]
%% 	Condition = condition()
%% 	RegistrationType = registrationType()
%%
%% @doc Create a new service point trigger (SPT) in the database.
%% 	An SPT for the `RegistrationType' is created.  Condition must
%% 	be `{method, "REGISTER"}'.
%%
add_trigger(ConditionNegated, Group, Condition, RegistrationType)
		when (ConditionNegated == true orelse ConditionNegated == false),
		is_list(Group), Condition == {method, "REGISTER"},
		(RegistrationType == registration orelse
		RegistrationType == reregistration orelse RegistrationType
		== deregistration) ->
	Fint = fun(N) when is_integer(N), N >= 0 ->
				true;
			(_) ->
				false
	end,
	case lists:all(Fint, Group) of
		true ->
			ServicePointTriggerID = make_ref(),
			Trigger = #trigger{servicePointTriggerID = ServicePointTriggerID,
					conditionNegated = ConditionNegated, group = Group,
					condition = {method, "REGISTER"},
					registrationType = RegistrationType},
			case mnesia:transaction(fun() -> mnesia:write(Trigger) end) of
				{atomic, _Result} ->
					ServicePointTriggerID;
				Error ->
					error_logger:error_msg(mnesia:error_description(Error)),
					exit(Error)
			end;
		false ->
			exit(badarg)
	end.

%%
%% @spec (ConditionNegated, Group, Condition) ->
%% 		servicePointTriggerID()
%% 	ConditionNegated = bool()
%% 	Group = [integer]
%% 	Condition = {requestURI, string()} | {method, string()}
%% 			| {sipHeader, Header} | {sipHeader, Header, Content}
%% 			| {sessioncase, SessionCase}
%% 			| {sessionDescription, Line} | {sessionDescription, Line, Content}
%% 	Header = string()
%% 	Content = string()
%% 	SessionCase = originating_session | terminating_registered
%% 			| terminating_unregistered
%% 	Line = string()
%% 	Content = string()
%%
%% @doc Create a new service point trigger (SPT) in the database.
%%
add_trigger(ConditionNegated, Group, Condition)
		when (ConditionNegated == true orelse ConditionNegated == false),
		is_list(Group), is_tuple(Condition) ->
	case Condition of
		{requestURI, RequestURI} when is_list(RequestURI) -> ok;
		{method, Method} when is_list(Method) -> ok;
		{sipHeader, Header} when is_list(Header) -> ok;
		{sipHeader, Header, Content}
				when is_list(Header), is_list(Content) -> ok;
		{sessionCase, SessionCase} when SessionCase == originating;
				SessionCase == terminating_registered;
				SessionCase == terminating_unregistered -> ok;
		{sessionDescription, Line, Content}
				when is_list(Line), is_list(Content) -> ok;
		_ ->
			exit(badarg)
	end,
	Fint = fun(N) when is_integer(N), N >= 0 ->
				true;
			(_) ->
				false
	end,
	case lists:all(Fint, Group) of
		true ->
			ServicePointTriggerID = make_ref(),
			Trigger = #trigger{servicePointTriggerID = ServicePointTriggerID,
					conditionNegated = ConditionNegated, group = Group,
					condition = Condition},
			case mnesia:transaction(fun() -> mnesia:write(Trigger) end) of
				{atomic, _Result} ->
					ServicePointTriggerID;
				Error ->
					error_logger:error_msg(mnesia:error_description(Error)),
					exit(Error)
			end;
		false ->
			exit(badarg)
	end.

%% @spec (ServiceProfileID) -> ok
%% 	ServiceProfileID = serviceProfileID()
%%
%% @doc Remove a service profile from the database.
%%
remove_profile(ServiceProfileID) when is_reference(ServiceProfileID) -> 
	case mnesia:transaction(fun() ->
			mnesia:delete(profile, ServiceProfileID, write) end) of
		{atomic, _Result} ->
			ok;
		{aborted, Reason} = Error ->
			error_logger:warning_msg(mnesia:error_description(Error)),
			exit(Reason)
	end.

%% @spec (Filter) -> ok
%% 	Filter = initialFilterCriteriaID()
%%
%% @doc Remove a filter from the database.
%%
remove_filter(Filter) when is_reference(Filter) -> 
	case mnesia:transaction(fun() ->
			mnesia:delete(filter, Filter, write) end) of
		{atomic, _Result} ->
			ok;
		{aborted, Reason} = Error ->
			error_logger:warning_msg(mnesia:error_description(Error)),
			exit(Reason)
	end.

%% @spec (Trigger) -> ok
%% 	Trigger = serviceTriggerPointID()
%%
%% @doc Remove a trigger from the database.
remove_trigger(Trigger) when is_reference(Trigger) -> 
	case mnesia:transaction(fun() ->
			mnesia:delete(trigger, Trigger, write) end) of
		{atomic, _Result} ->
			ok;
		{aborted, Reason} = Error ->
			error_logger:warning_msg(mnesia:error_description(Error)),
			exit(Reason)
	end.

%% @spec (PrivateUserID) -> ok
%% 	PrivateUserID = privateUserID()
%%
%% @doc Remove a user from the database.
%%
remove_user(PrivateUserID) when is_reference(PrivateUserID) -> 
	case mnesia:transaction(fun() ->
			[User] = mnesia:read(user, PrivateUserID, write),
			[Subscriber] = mnesia:read(subscriber,
					User#user.subscriberID, write),
			PrivateUserIDs = lists:delete(PrivateUserID,
					Subscriber#subscriber.privateUserIDs),
			mnesia:write(subscriber,
					Subscriber#subscriber{privateUserIDs = PrivateUserIDs}, write),
			mnesia:delete(user, PrivateUserID, write) end) of
		{atomic, _Result} ->
			ok;
		{aborted, Reason} = Error ->
			error_logger:warning_msg(mnesia:error_description(Error)),
			exit(Reason)
	end.

%% @spec (PublicUserID) -> ok
%% 	PublicUserID = publicUserID()
%%
%% @doc Remove an address from the database.
%%
remove_address(PublicUserID) when is_reference(PublicUserID) -> 
	case mnesia:transaction(fun() ->
			mnesia:delete(address, PublicUserID, write) end) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} = Error ->
			error_logger:warning_msg(mnesia:error_description(Error)),
			exit(Reason)
	end.

%% @spec (SubscriberID) -> ok
%% 	SubscriberID = subscriberID()
%%
%% @doc Remove subscriber from the database.
%%
remove_subscriber(SubscriberID) when is_reference(SubscriberID) -> 
	case mnesia:transaction(fun() ->
			Faddr = fun(PublicUserID) ->
						mnesia:delete(address, PublicUserID, write) end,
			Fuser = fun(PrivateUserID) ->
						[User] = mnesia:read(user, PrivateUserID, write),
						lists:foreach(Faddr, User#user.publicUserIDs),
						mnesia:delete(user, PrivateUserID, write)
			end,
			[Subscriber] = mnesia:read(subscriber, SubscriberID, write),
			lists:foreach(Fuser, Subscriber#subscriber.privateUserIDs),
			mnesia:delete(subscriber, SubscriberID, write) end) of
		{atomic, _Result} ->
			ok;
		{aborted, Reason} = Error ->
			error_logger:warning_msg(mnesia:error_description(Error)),
			exit(Reason)
	end.

%% @spec () -> subscriberID()
%%
%% @doc Retrieves the first {@link subscriberID()} key from the database.
%%
get_subscriber() ->
	get_first(subscriber).

%% @spec (SubscriberID) -> subscriber()
%% 	SubscriberID = subscriberID()
%%
%% @doc Retrieves the {@link subscriber()} entry from the database.
%%
get_subscriber(SubscriberID) when is_reference(SubscriberID) ->
	get_record(subscriber, SubscriberID).

%% @spec (UserIdentity) -> subscriberID()
%% 	UserIdentity = publicUserID() | privateUserID()
%%
%% @doc Retrieves the index into the subscriber table for
%% 	a Private User Identity or a Public User Identity.
%%
get_subscriber_id("sip:" ++ _ = PublicUserID) ->
	#address{subscriberID = SubscriberID} = get_record(address, PublicUserID),
	SubscriberID;
get_subscriber_id("tel:" ++ _ = PublicUserID) ->
	#address{subscriberID = SubscriberID} = get_record(address, PublicUserID),
	SubscriberID;
get_subscriber_id(PrivateUserID) ->
	#user{subscriberID = SubscriberID} = get_record(user, PrivateUserID),
	SubscriberID.
		
%% @spec () -> privateUserID()
%%
%% @doc Retrieves the first {@link privateUserID()} key entry from the database.
%%
get_user() ->
	get_first(user).

%% @spec (PrivateUserID) -> user()
%% 	PrivateUserID = privateUserID()
%%
%% @doc Retrieves the user entry from the database.
%%
get_user(PrivateUserID) when is_list(PrivateUserID) ->
	get_record(user, PrivateUserID).

%% @spec () -> serviceProfileID()
%%
%% @doc Retrieves the first {@link serviceProfileID()} key from the database.
%%
get_profile() ->
	get_first(profile).

%% @spec (ProfileID) -> profile()
%% 	ProfileID = profileID()
%%
%% @doc Retrieves the profile entry from the database.
%%
get_profile(ProfileID) when is_reference(ProfileID) ->
	get_record(profile, ProfileID).

%% @spec () -> publicUserID()
%%
%% @doc Retrieves the first {@link publicUserID()} key from the database.
%%
get_address() ->
	get_first(address).

%% @spec (PublicUserID) -> address()
%% 	PublicUserID = publicUserID()
%%
%% @doc Retrieves the address entry from the database.
%%
get_address(PublicUserID) when is_list(PublicUserID) ->
	get_record(address, PublicUserID).

%% @spec (PublicUserIDs) -> [address()]
%% 	PublicUserIDs = [publicUserID()]
%%
%% @doc Retrieves the address entries from the database.
%%
get_addresses(PublicUserIDs) when is_list(PublicUserIDs) ->
	get_records(address, PublicUserIDs).

%% @spec () -> initialFilterCriteriaID()
%%
%% @doc Retrieves the first {@link initialFilterCriteriaID()} key 
%% 	from the database.
%%
get_filter() ->
	get_first(filter).

%% @spec (InitialFilterCriteriaID) -> filter()
%% 	InitialFilterCriteriaID = initialFilterCriteriaID()
%%
%% @doc Retrieves the filter entry from the database.
%%
get_filter(InitialFilterCriteriaID) when is_reference(InitialFilterCriteriaID) ->
	get_record(filter, InitialFilterCriteriaID).

%% @spec () -> servicePointTriggerID()
%%
%% @doc Retrieves the first {@link servicePointTriggerID()} key
%% 	from the database.
%%
get_trigger() ->
	get_first(trigger).

%% @spec (ServicePointTriggerID) -> trigger()
%% 	ServicePointTriggerID = servicePointTriggerID()
%%
%% @doc Retrieves the triger entry from the database.
%%
get_trigger(ServicePointTriggerID) when is_reference(ServicePointTriggerID) ->
	get_record(trigger, ServicePointTriggerID).

%% @spec (ServiceProfileID) -> serviceProfile()
%% 	ServiceProfileID = serviceProfileID()
%%
%% @doc Retrieves the complete Service Profile.
%%
get_service(ServiceProfileID) when is_reference(ServiceProfileID) ->
	Fun = fun(Fun, [H | T], Acc) ->
				Triggers = lists:flatten([mnesia:read(trigger, S, read)
						|| S <- H#filter.servicePointTriggerIDs]),
				Fun(Fun, T,
						[H#filter{servicePointTriggerIDs = Triggers} | Acc]);
			(_, [], Acc) ->
				lists:reverse(Acc)
	end,
	case mnesia:transaction(fun() ->
			[Profile] = mnesia:read(profile, ServiceProfileID, read),
			FilterIDs = Profile#profile.initialFilterCriteriaIDs,
			Filters = lists:flatten([mnesia:read(filter, ID, read)
					|| ID <- FilterIDs]),
			Profile#profile{initialFilterCriteriaIDs
					= Fun(Fun, Filters, [])} end) of
		{atomic, Result} ->
			Result;
		{aborted, Reason} ->
			exit(Reason)
	end.

%% @spec () -> implicitRegistrationSets()
%% 
%% @doc Returns an empty set of Implicit Registration Sets.
%% 
new_implicit_registration_sets() ->
	ordsets:new().

%% @spec (ImplicitRegistrationSet, ImplicitRegistrationSets) -> 
%% 			NewImplicitRegistrationSets
%% 		ImplicitRegistrationSet = [publicUserID()]
%% 		ImplicitRegistrationSets = implicitRegistrationSets()
%% 		NewImplicitRegistrationSets = implicitRegistrationSets()
%%
%% @doc Create a new Implicit Registration Set.
%% 	The list of Public User Identifiers in 
%% 	<tt>ImplicitRegistrationSet</tt> is added as a new
%% 	Implicit Registration Set in the set of sets.
%% 	Returns a new set of sets.  Fails if any Public
%% 	User Identifier is an element of an existing set.
%%
add_implicit_registration_set(ImplicitRegistrationSet,
		ImplicitRegistrationSets) when is_list(ImplicitRegistrationSet) ->
	case is_element_in_sets(ImplicitRegistrationSet,
			ImplicitRegistrationSets) of
		false ->
			Elem = ordsets:from_list(ImplicitRegistrationSet),
			ordsets:add_element(Elem, ImplicitRegistrationSets);
		true ->
			exit(badarg)
	end.

%% @spec (PublicUserID, ImplicitRegistrationSets) -> 
%% 			ImplicitRegistrationSet
%% 		PublicUserID = publicUserID()
%% 		ImplicitRegistrationSets = implicitRegistrationSets()
%% 		ImplicitRegistrationSet = [publicUserID()]
%% 		NewImplicitRegistrationSets = implicitRegistrationSets()
%%
%% @doc Retrieves the Implicit Registration Set.
%% 	Returns the list of Public User Identities which make
%% 	up the Implicit Registration Set containing
%% 	<tt>PublicUserID</tt>.  Fails if <tt>PublicUserID</tt>
%% 	does not exist in any Implicit Registration Set.
%% 
get_implicit_registration_set(PublicUserID, [H|ImplicitRegistrationSets])
		when is_list(PublicUserID) ->
	case ordsets:is_element(PublicUserID, H) of
		true ->
			ordsets:to_list(H);
		false ->
			get_implicit_registration_set(PublicUserID, ImplicitRegistrationSets)
	end.

%% @spec (ImplicitRegistrationSet, ImplicitRegistrationSets) -> 
%% 			NewImplicitRegistrationSets
%% 		ImplicitRegistrationSet = [publicUserID()]
%% 		ImplicitRegistrationSets = implicitRegistrationSets()
%% 		NewImplicitRegistrationSets = implicitRegistrationSets()
%%
%% @doc Remove the Implicit Registration Set.
%% 	Returns a new set of sets.  Fails if the set does not exist.
%%
remove_implicit_registration_set(ImplicitRegistrationSet,
			ImplicitRegistrationSets)
		when is_list(ImplicitRegistrationSet) ->
	Set = ordsets:from_list(ImplicitRegistrationSet),
	ordsets:add_element(Set, ImplicitRegistrationSets).

%%----------------------------------------------------------------------
%%  The hss procedure API
%%----------------------------------------------------------------------

%% @headerfile "procedures.hrl"

%% @spec (PublicUserID, VisitedNetworkIdentifier, TypeOfAuthorization,
%% 		PrivateUserID, RoutingInformation) -> Result
%% 	PublicUserID = publicUserID()
%% 	VisitedNetworkIdentifier = string()
%% 	TypeOfAuthorization = registration | deregistration
%% 			| registration_and_capabilities
%% 	PrivateUserID = privateUserID()
%% 	RoutingInformation = sipURL()
%% 	Result = user_registration_status_response()
%%
%% @doc This procedure is used between the I-CSCF and the HSS during
%% 	SIP registrations. The procedure is invoked by the I-CSCF to
%% 	authorize the registration of the Public User Identity and
%% 	obtain either the S-CSCF where the Public User Identity is 
%% 	registered or the list of capabilities that the S-CSCF has
%% 	to support. 
%%
user_registration_status_query(PublicUserID, VisitedNetworkIdentifier,
		TypeOfAuthorization, PrivateUserID, RoutingInformation)
		when is_list(PublicUserID), is_list(VisitedNetworkIdentifier),
		(TypeOfAuthorization =:= registration
				orelse TypeOfAuthorization =:= deregistration
				orelse TypeOfAuthorization =:= registration_and_capabilities),
		is_list(PrivateUserID),
		(is_list(RoutingInformation) orelse is_pid(RoutingInformation)) ->
	URSQ = #user_registration_status_query{publicUserID = PublicUserID,
			visitedNetworkIdentifier = VisitedNetworkIdentifier,
			typeOfAuthorization = TypeOfAuthorization, 
			privateUserID = PrivateUserID},
	case locate_hss(RoutingInformation) of
		Server when is_pid(Server) ->
			gen_server:call(Server, URSQ);
		undefined ->
			exit(badarg)
	end.

%% @spec (PublicUserID, SCSCFName, PrivateUserID,
%% 		ServerAssignmentType, UserDataAlreadyAvailable,
%% 		RoutingInformation) -> Result
%% 	PublicUserID = publicUserID() | [publicUserID()]
%% 	PrivateUserID = privateUserID()
%% 	ServerAssignmentType = serverAssignmentType()
%% 	UserDataAlreadyAvailable = boolean()
%% 	RoutingInformation = sipURL()
%% 	Result = scscf_registration_notification_response()
%%
%% @doc This procedure is used between the S-CSCF and the HSS. The
%% 	procedure is invoked by the S-CSCF to assign an S-CSCF to a
%% 	Public Identity, or to clear the name of the S-CSCF assigned
%% 	to one or more Public Identities and to download from the
%% 	HSS the relevant user information.
%%
scscf_registration_notification(PublicUserID, SCSCFName, PrivateUserID,
		ServerAssignmentType, UserDataAlreadyAvailable, 
		RoutingInformation) when is_list(PublicUserID),
		is_list(SCSCFName), is_list(PrivateUserID), 
		(ServerAssignmentType =:= no_assignment
		orelse ServerAssignmentType =:= no_assignment
		orelse ServerAssignmentType =:= registration
		orelse ServerAssignmentType =:= reregistration
		orelse ServerAssignmentType =:= unregistered_user
		orelse ServerAssignmentType =:= timeout_deregistration
		orelse ServerAssignmentType =:= user_deregistration
		orelse ServerAssignmentType =:= timeout_deregistration_store_server_name
		orelse ServerAssignmentType =:= user_deregistration_store_server_name
		orelse ServerAssignmentType =:= administrative_deregistration
		orelse ServerAssignmentType =:= authentication_failure
		orelse ServerAssignmentType =:= authentication_timeout
		orelse ServerAssignmentType =:= deregistration_too_much_data),
		(UserDataAlreadyAvailable =:= true
		orelse UserDataAlreadyAvailable =:= false) ->
	SRN = #scscf_registration_notification{publicUserID = PublicUserID,
			sCSCFName = SCSCFName, privateUserID = PrivateUserID,
			serverAssignmentType = ServerAssignmentType,
			userDataAlreadyAvailable = UserDataAlreadyAvailable},
	case locate_hss(RoutingInformation) of
		Server when is_pid(Server) ->
			gen_server:call(Server, SRN);
		undefined ->
			exit(badarg)
	end.

%% @spec (PublicUserID, RoutingInformation) -> Result
%% 	PublicUserID = publicUserID()
%% 	RoutingInformation = sipURL()
%% 	Result = user_location_response()
%%
%% @doc This procedure is used between the I-CSCF and the HSS.
%% 	This procedure is invoked to obtain the name of the S-CSCF
%% 	assigned to a Public Identity.
%%
user_location_query(PublicUserID, RoutingInformation)
			when is_list(PublicUserID) ->
	ULQ = #user_location_query{publicUserID = PublicUserID},
	case locate_hss(RoutingInformation) of
		Server when is_pid(Server) ->
			gen_server:call(Server, ULQ);
		undefined ->
			exit(badarg)
	end.

%% @spec (PublicUserID, PrivateUserID, NumberAuthenticationItems,
%% 		AuthenticationData, SCSCFName, RoutingInformation) -> Result
%% 	PublicUserID = publicUserID()
%% 	PrivateUserID = privateUserID()
%% 	NumberAuthenticationItems = integer()
%% 	AuthenticationData = authenticationData()
%% 	SCSCFName = sipURL(),
%% 	RoutingInformation = sipURL()
%% 	Result = authentication_request_response()
%%
%% @doc This procedure is used between the S-CSCF and the HSS.
%% 	Used to exchange information to support the authentication between 
%% 	the end user and the home IMS network. The procedure is
%% 	invoked by the S-CSCF to retrieve authentication vectors 
%% 	from the HSS or to resolve synchronization failures between
%% 	the sequence numbers in the UE and the HSS.
%%
authentication_request(PublicUserID, PrivateUserID,
			NumberAuthenticationItems, AuthenticationData, SCSCFName,
			RoutingInformation) when is_list(PublicUserID),
			is_list(PrivateUserID),
			is_integer(NumberAuthenticationItems) ->
	ARQ = #authentication_request{publicUserID = PublicUserID,
			privateUserID = PrivateUserID,
			numberAuthenticationItems = NumberAuthenticationItems,
			authenticationData = AuthenticationData,
			sCSCFName = SCSCFName},
	case locate_hss(RoutingInformation) of
		Server when is_pid(Server) ->
			gen_server:call(Server, ARQ);
		undefined ->
			exit(badarg)
	end.

%%----------------------------------------------------------------------
%%  The private hss API
%%----------------------------------------------------------------------

%% @spec (Nodes) -> {ok, Tables}
%% 	Nodes = [node()]
%% 	Tables = [atom()]
%%
%% @doc Initialize a new installation.
%% 	<p><tt>Nodes</tt> is a list of the nodes where the application 
%% 	will run.</p>
%% 	<p>An mnesia schema should be created and mnesia started on
%% 	all nodes before running this function. e.g.&#058;</p>
%% 	<tt>
%% 		1> mnesia:create_schema([node()]).<br />
%% 		ok<br />
%% 		2> mnesia:start().<br />
%% 		ok<br />
%% 		3> hss:install([node()]).<br />
%% 		{ok,[subscriber,user,profile,address,filter,trigger,location]}<br />
%% 		ok<br />
%% 	</tt>
%%
%% @private
%%
install(Nodes) when is_list(Nodes) ->
	try
		case mnesia:wait_for_tables([schema], ?WAITFORSCHEMA) of
			ok ->
				ok;
			SchemaResult ->
				throw(SchemaResult)
		end,
		case mnesia:create_table(subscriber, [{disc_copies, Nodes},
				{attributes, record_info(fields, subscriber)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new subscriber table.~n");
			{aborted, {already_exists, subscriber}} ->
				error_logger:warning_msg("Found existing subscriber table.~n");
			T1Result ->
				throw(T1Result)
		end,
		case mnesia:create_table(user, [{disc_copies, Nodes},
				{attributes, record_info(fields, user)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new user table.~n");
			{aborted, {already_exists, user}} ->
				error_logger:warning_msg("Found existing user table.~n");
			T2Result ->
				throw(T2Result)
		end,
		case mnesia:create_table(profile, [{disc_copies, Nodes},
				{attributes, record_info(fields, profile)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new profile table.~n");
			{aborted, {already_exists, profile}} ->
				error_logger:warning_msg("Found existing profile table.~n");
			T3Result ->
				throw(T3Result)
		end,
		case mnesia:create_table(address, [{disc_copies, Nodes},
				{attributes, record_info(fields, address)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new address table.~n");
			{aborted, {already_exists, address}} ->
				error_logger:warning_msg("Found existing address table.~n");
			T4Result ->
				throw(T4Result)
		end,
		case mnesia:create_table(filter, [{disc_copies, Nodes},
				{attributes, record_info(fields, filter)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new filter table.~n");
			{aborted, {already_exists, filter}} ->
				error_logger:warning_msg("Found existing filter table.~n");
			T5Result ->
				throw(T5Result)
		end,
		case mnesia:create_table(trigger, [{disc_copies, Nodes},
				{attributes, record_info(fields, trigger)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new trigger table.~n");
			{aborted, {already_exists, trigger}} ->
				error_logger:warning_msg("Found existing trigger table.~n");
			T6Result ->
				throw(T6Result)
		end,
		case mnesia:create_table(location, [{type, bag}, {ram_copies, Nodes},
				{attributes, record_info(fields, location)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new location table.~n");
			{aborted, {already_exists, trigger}} ->
				error_logger:warning_msg("Found existing location table.~n");
			T7Result ->
				throw(T7Result)
		end,
		Tables = [subscriber, user, profile, address, filter, trigger, location],
		case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
			ok ->
				Tables;
			TablesResult ->
				throw(TablesResult)
		end
	of
		Result -> {ok, Result}
	catch
		throw:Error ->
			mnesia:error_description(Error)
	end.

%% @spec (Args) -> Result
%% 	Result = {ok, Pid} | {error, Error}
%% @doc Starts an instance of the procedures server.
%% @private
start_link(Args) ->
	supervisor:start_child(hss_sup, [Args, []]).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @spec (RoutingInformation::string()) -> pid()
%% @hidden
%% @TODO:  something a bit more sophisticated?
locate_hss(RoutingInformation) when is_pid(RoutingInformation) ->
	RoutingInformation;
locate_hss(RoutingInformation) when is_list(RoutingInformation) ->
	whereis(hss).

%% @spec (Table, Key) -> Record
%% @doc Retrieves <tt>Key</tt> from <tt>Table</tt>.
%% @hidden
get_record(Table, Key) ->
	case mnesia:transaction(fun() ->
			mnesia:read(Table, Key, read) end) of
		{atomic, [Result]} ->
			Result;
		{atomic, []} ->
			exit(not_found);
		{aborted, Reason} ->
			exit(Reason)
	end.

%% @spec (Table, Keys) -> Record
%% @doc Retrieves <tt>Keys</tt> from <tt>Table</tt>.
%% @hidden
get_records(Table, Keys) ->
	case mnesia:transaction(fun() ->
			[mnesia:read(Table, Key, read) || Key <- Keys] end) of
		{atomic, [Result]} ->
			Result;
		{atomic, []} ->
			exit(not_found);
		{aborted, Reason} ->
			exit(Reason)
	end.

%% @spec (Table) -> Record
%% @doc Retrieves first key from <tt>Table</tt>.
%% @hidden
get_first(Table) ->
	case mnesia:transaction(fun() ->
			mnesia:first(Table) end) of
		{atomic, '$end_of_table'} ->
			exit(empty_table);
		{atomic, Result} ->
			Result;
		{aborted, Reason} ->
			exit(Reason)
	end.

%% @spec (Identifiers, Sets) -> NewSet
%% 	List = [publicUserID()]
%% 	Set = implicitRegistrationSets()
%% @doc Returns true if any Identifier is an element in any set,
%% 	within the set of sets, otherwise false.
%% @hidden
is_element_in_sets([], _) ->
	false;
is_element_in_sets([H|T], Set) when is_list(H) ->
	F = fun(E, AccIn) ->
			case ordsets:is_element(H, E) of
			  	true -> 
					true;
			  	false -> 
					AccIn
			end
	end,
	case ordsets:fold(F, false, Set) of
		true ->
			true;
		false ->
			is_element_in_sets(T, Set)
	end;
is_element_in_sets(_, _) ->
	exit(badarg).

