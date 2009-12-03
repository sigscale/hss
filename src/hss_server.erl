%%% $Id: hss_server.erl,v 1.30 2008/02/18 05:34:31 vances Exp $
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
%%% @doc A {@link //stdlib/gen_server. <tt>gen_server</tt>} module
%%% 		for the hss application.
%%% @TODO:  implement Public Service Identities
%%% @TODO:  implement HSS initiated procedures
%%% @TODO:  improve visited networks authorization
%%%
-module(hss_server).
-copyright('Copyright (c) 2008 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.30 $').

-behaviour(gen_server).

%% export the call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).
%% optional call back for gen_server status
-export([format_status/2]).

%% @type state() = #state{}.
-record(state, {}).

-include("subscriber_data.hrl").
-include("procedures.hrl").

%%----------------------------------------------------------------------
%%  The hss_server API
%%----------------------------------------------------------------------

%% @type k() = binary().
%% 	A 128 bit subscriber authentication key (K) known only to the
%% 	HSS and the ISIM/USIM application on the UICC.
%%
%% @type opc() = binary().
%% 	A 128 bit key (OPc) derived from the Operator Variant Algorithm
%% 	Configuration Field (OP) and {@link k(). K} known only to the
%% 	HSS and the ISIM/USIM application on the UICC.
%%
%% @type rand() = binary().
%% 	A 128 bit random challenge (RAND).
%%
%% @type sqn() = binary().
%% 	A 48 bit sequence number (SQN).  The management of sequence 
%% 	numbers is specified in
%% 	<a href="http://www.3gpp.org/ftp/Specs/html-info/33102.htm">
%% 	3GPP TS 33.102</a> Annex C.  The current implementation of
%% 	this module uses sequence numbers which are not time-based as
%% 	described in C.1.1.2 and C.3.2.  The sequence number
%% 	`SQN = <<SQE:43, IND:5>>' where SQE is incremented to generate
%% 	a fresh sequence number and IND is incremented each time an 
%% 	authentication vector is generated.
%%
%% @type amf() = binary().
%% 	A 16 bit authentication management field (AMF).
%%
%% @type mac() = binary().
%% 	A 64 bit message authentication code (MAC).
%%
%% @type res() = binary().
%% 	A 64 bit challenge response (RES).
%%
%% @type ak() = binary().
%% 	A 48 bit anonymity key (AK).
%%
%% @type ck() = binary().
%% 	A 128 bit confidentiality key (AK).
%%
%% @type ik() = binary().
%% 	A 128 bit integrity key (AK).
%%

%%----------------------------------------------------------------------
%%  The hss_server gen_server call backs
%%----------------------------------------------------------------------

%% @spec (Args) -> Result
%% 	Args = []
%% 	Result = {ok,State} | {ok,State,Timeout} | {stop,Reason} | ignore
%% 	State = state()
%% 	Timeout = integer() | infinity
%% 	Reason = term()
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_server:init/1
%% @private
%%
init(_Args) ->
	process_flag(trap_exit, true),
	{ok, #state{}}.

%% @spec (Request, From, State) -> Result
%% 	Request = term()
%% 	From = {pid(), Tag}
%% 	Tag = any()
%% 	State = state()
%% 	Result = {reply, Reply, NewState}
%% 	         | {reply, Reply, NewState, Timeout}
%% 	         | {noreply, NewState}
%% 	         | {noreply, NewState, Timeout}
%% 	         | {stop, Reason, Reply, NewState}
%% 	         | {stop, Reason, NewState}
%% 	Reply = term()
%% 	NewState = state()
%% 	Timeout = integer() | infinity
%% 	Reason = term()
%% @doc Handle a request sent using {@link call/2. call/2,3} or
%% 		{@link multi_call/2. multi_call/2,3,4}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
%%
handle_call(#user_registration_status_query{} = URSQ, From, State) ->
	user_registration_status_query(URSQ, From, State);
handle_call(#scscf_registration_notification{} = SRN, From, State) ->
	scscf_registration_notification(SRN, From, State);
handle_call(#user_location_query{} = ULQ, From, State) ->
	user_location_query(ULQ, From, State);
handle_call(#authentication_request{} = ARQ, From, State) ->
	authentication_request(ARQ, From, State);
handle_call(_Request, {Pid, _}, State) ->
	exit(Pid, badarg),
	{noreply, State}.

%% @spec (Request::term(), State::state()) -> Result
%% 	Request = term()
%% 	State = state()
%% 	Result = {noreply, NewState} | {noreply, NewState, Timeout}
%% 	         | {stop, Reason, NewState}
%% 	NewState = state()
%% 	Timeout = integer() | infinity
%% 	Reason = term()
%% @doc Handle a request sent using {@link cast/2} or
%% 		{@link abcast/2. abcast/2,3}.
%% @see //stdlib/gen_server:handle_cast/2
%% @private
%%
handle_cast(_Request, State) ->
	{noreply, State}.

%% @spec (Info, State) -> Result
%% 	Info = timeout | term()
%% 	State = state()
%% 	Result = {noreply, NewState} | {noreply, NewState, Timeout}
%% 	         | {stop, Reason, NewState}
%% 	NewState = state()
%% 	Timeout = integer() | infinity
%% 	Reason = normal | term()
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info(_Info, State) ->
	{noreply, State}.

%% @spec (Reason, State) -> any()
%% 	Reason = normal | shutdown | term()
%% 	State = state()
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(_Reason, _State) ->
	ok.

%% @spec (OldVsn, State, Extra) -> Result
%% 	OldVsn = Vsn | {down, Vsn}
%% 	State = state()
%% 	Extra = uterm()
%% 	Vsn = term()
%% 	Result = {ok, NewState}
%% 	NewState = state()
%% @doc Update internal state data during a release upgrade/downgrade.
%% @see //stdlib/gen_server:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% @spec (Opt, StatusData) -> FormattedStatusData
%% 	Opt = term()
%% 	StatusData = list()
%% 	FormattedStatusData = list()
%% @doc Format internal state and status information.
%% 	This callback is called when {@link sys:get_status/1}
%% 	is used to inspect the status of a `gen_server'
%% 	process.  It should format the internal state data for
%% 	display in the result.
%% @see //stdlib/sys:get_status/1
%% @private
%%
format_status(_Opt, [_PDict, State]) ->
	[{data, [{"State", State}]}].

%%----------------------------------------------------------------------
%%  The hss_server procedures
%%----------------------------------------------------------------------

%% @headerfile "procedures.hrl"

%% @spec (URSQ, From, State) -> Result
%% 	URSQ = user_registration_status_query()
%% 	From = {pid(), Tag}
%% 	Tag = any()
%% 	State = state()
%% 	Result = {reply, Reply, NewState}
%% 	         | {reply, Reply, NewState, Timeout}
%% 	         | {noreply, NewState}
%% 	         | {noreply, NewState, Timeout}
%% 	         | {stop, Reason, Reply, NewState}
%% 	         | {stop, Reason, NewState}
%% 	Reply = term()
%% 	NewState = state()
%% 	Timeout = integer() | infinity
%% 	Reason = term()
%% @doc Handle a user registration status query (URSQ).
%% 	Procedure described in 3GPP TS 29.228
%% 	6.1.1 User registration status query.
%% @see //stdlib/gen_server:handle_call/3
%% 
user_registration_status_query(#user_registration_status_query{
		publicUserID = PublicUserID, privateUserID = PrivateUserID,
		typeOfAuthorization = TypeOfAuthorization,
		visitedNetworkIdentifier = VisitedNetworkIdentifier} = URSQ,
		From, State) ->
	Ferr = fun(Reason) ->
			error_logger:error_report(["URSQ",
					{reply, Reason},
					{typeOfAuthorization, TypeOfAuthorization},
					{visitedNetworkIdentifier, VisitedNetworkIdentifier},
					{privateUserID, PrivateUserID},
					{publicUserID, PublicUserID}])
	end,
	case mnesia:activity(transaction, fun() ->
			user_registration_status_query1(URSQ, From, State) end) of
		{atomic, Reply} ->
			{reply, Reply, State};
		{aborted, Reason} when Reason == user_unknown; 
				Reason == identities_dont_match;
				Reason == identity_not_registered;
				Reason == roaming_not_allowed ->
			Ferr(Reason),
			Reply = #user_registration_status_response{
					experimentalResult = Reason},
			{reply, Reply, State};
		{aborted, Reason} when Reason == authorization_rejected ->
			Ferr(Reason),
			Reply = #user_registration_status_response{
					resultCode = Reason},
			{reply, Reply, State};
		{aborted, _Reason} ->
			Ferr(unable_to_comply),
			Reply = #user_registration_status_response{
					resultCode = unable_to_comply},
			{reply, Reply, State}
	end.

%% @spec (URSQ::user_registration_status_query(), From::tuple(),
%% 		State::state()) -> Result
%% @doc Check that the Private User Identity and the Public User
%%		Identity exists in the HSS.
%% 	3GPP TS 29.228 6.1.1 1.
%% @hidden
user_registration_status_query1(#user_registration_status_query{
		privateUserID = PrivateUserID, publicUserID = PublicUserID} = URSQ,
		From, State) ->
	try
		[User] = mnesia:read(user, PrivateUserID, read),
		[Address] = mnesia:read(address, PublicUserID, read),
		[Subscriber] = mnesia:read(subscriber, User#user.subscriberID, read),
		{User, Address, Subscriber}
	of
		{U, A, S} ->
			user_registration_status_query2(URSQ, From, State, U, A, S)
	catch
		error:{badmatch, []} ->
			mnesia:abort(user_unknown)
	end.

%% @spec (URSQ::user_registration_status_query(), From::tuple(), State::state(),
%% 		User::user(), Address::address(), Subscriber::subscriber()) -> Result
%% @doc Check that the Public User Identity received in the request is 
%% 	associated with the Private User Identity received in the request.
%% 	3GPP TS 29.228 6.1.1 2.
%% @hidden
user_registration_status_query2(#user_registration_status_query{
		publicUserID = PublicUserID} = URSQ,
		From, State, #user{subscriberID = SubscriberID,
		publicUserIDs = UserPublicUserIDs} = U,
		#address{subscriberID = SubscriberID} = A, S) ->
	case lists:member(PublicUserID, UserPublicUserIDs) of
		true ->
			user_registration_status_query3(URSQ, From, State, U, A, S);
		false ->
			mnesia:abort(identities_dont_match)
	end;
user_registration_status_query2(_ULQ, _From, _State, _U, _A, _S) ->
	%% user and address have different subscriberID
	mnesia:abort(identities_dont_match).

%% @spec (URSQ::user_registration_status_query(), From::tuple(), State::state(),
%% 		User::user(), Address::address(), Subscriber::subscriber()) -> Result
%% @doc Check whether the Public User Identity received in the request is
%% 	barred for the establishment of multimedia sessions. 
%% 	3GPP TS 29.228 6.1.1 3.
%% @hidden
user_registration_status_query3(URSQ, From, State, U, A, S) ->
	case A#address.barringIndicator of
		true ->
			user_registration_status_query3a(URSQ, From, State, U, A, S);
		_ ->
			user_registration_status_query4(URSQ, From, State, U, A, S)
	end.
%% @hidden
user_registration_status_query3a(#user_registration_status_query{
		publicUserID = PublicUserID} = URSQ, From, State, U, A,
		#subscriber{implicitRegistrationSets = ImplicitRegistrationSets} = S) ->
	% Check whether there are other non-barred Public User 
	% Identities to be implicitly registered.
	case catch hss:get_implicit_registration_set(PublicUserID,
			ImplicitRegistrationSets) of
		ImplicitRegistrationSet when is_list(ImplicitRegistrationSet) ->
			user_registration_status_query3b(URSQ, From, State, U, A, S,
					ImplicitRegistrationSet);
		_NotFound  ->
			mnesia:abort(authorization_rejected)
end.
%% @hidden
user_registration_status_query3b(#user_registration_status_query{
		publicUserID = PublicUserID} = URSQ,
		From, State, U, A, S, ImplicitRegistrationSet) ->
	Addresses = [mnesia:read(address, P, read)
			|| P <- ImplicitRegistrationSet -- [PublicUserID]],
	Fbar = fun(F, [[#address{barringIndicator = true}] | T]) ->
			F(F, T);
		(_, [[#address{}] | _]) ->
			true;
		(_, _) ->
			false
	end,							
	case Fbar(Fbar, Addresses) of
		true ->
			user_registration_status_query4(URSQ, From, State, U, A, S);
		false ->
			mnesia:abort(authorization_rejected)
	end.

%% @spec (URSQ::user_registration_status_query(), From::tuple(), State::state(),
%% 		User::user(), Address::address(), Subscriber::subscriber()) -> Result
%% @doc Check the User-Authorization-Type received in the request.
%% 	3GPP TS 29.228 6.1.1 4.
%% @hidden
user_registration_status_query4(#user_registration_status_query{
		typeOfAuthorization = TypeOfAuthorization,
		visitedNetworkIdentifier = VisitedNetworkIdentifier} = URSQ,
		From, State, U,
		#address{authVisitedNetworkIDs = AuthVisitedNetworkIDs},
		#subscriber{serverCapabilities = ServerCapabilities} = S) ->
	MyRealms = case application:get_env(realms) of
		{ok, Realms} ->
			Realms;
		undefined ->
			[]
	end,
	case TypeOfAuthorization of
		registration ->
			case lists:member(VisitedNetworkIdentifier,
					MyRealms ++ AuthVisitedNetworkIDs) of
				true ->
					% @TODO: authorized to register?
					user_registration_status_query5(URSQ, From, State, U, S);
				false ->
					mnesia:abort(roaming_not_allowed)
			end;
		deregistration ->
			user_registration_status_query5(URSQ, From, State, U, S);
		registration_and_capabilities ->
			case lists:member(VisitedNetworkIdentifier,
					MyRealms ++ AuthVisitedNetworkIDs) of
				true ->
					% @TODO: authorized to register?
					#user_registration_status_response{resultCode = success,
							sCSCFCapabilities = ServerCapabilities};
				false ->
					mnesia:abort(roaming_not_allowed)
			end
	end.

%% @spec (URSQ::user_registration_status_query(), From::tuple(), State::state(),
%% 		User::user(), Subscriber::subscriber()) -> Result
%% @doc Check the state of the Public User Identity received in the request
%% 	3GPP TS 29.228 6.1.1 5.
%% @hidden
user_registration_status_query5(#user_registration_status_query{
		publicUserID = PublicUserID} = URSQ,
		From, State, U, S) ->
	L = mnesia:read(location, PublicUserID, read),
	user_registration_status_query5(URSQ, From, State, U, S, L).
%% @hidden
user_registration_status_query5(#user_registration_status_query{
		typeOfAuthorization = registration}, _From, _State, _U, _S,
		[#location{state = registered, sCSCFName = SCSCFName} | _]) ->
	#user_registration_status_response{
			experimentalResult = subsequent_reqistration,
			sCSCFName = SCSCFName};
user_registration_status_query5(#user_registration_status_query{
		typeOfAuthorization = deregistration}, _From, _State, _U, _S,
		[#location{state = registered, sCSCFName = SCSCFName} | _]) ->
	#user_registration_status_response{
			experimentalResult = subsequent_reqistration,
			sCSCFName = SCSCFName};
user_registration_status_query5(#user_registration_status_query{
		typeOfAuthorization = deregistration}, _From, _State, _U, _S,
		[#location{state = unregistered, sCSCFName = SCSCFName} | _]) ->
	#user_registration_status_response{resultCode = success,
			sCSCFName = SCSCFName};
user_registration_status_query5(#user_registration_status_query{
		typeOfAuthorization = registration}, _From, _State, _U,
		#subscriber{serverCapabilities = ServerCapabilities},
		[#location{state = unregistered, sCSCFName = SCSCFName} | _]) ->
	% selection of a new S-CSCF necessary?
	case application:get_env(keep_unregistered_scscf) of
		{ok, false} ->
			#user_registration_status_response{
					experimentalResult = server_selection,
					sCSCFCapabilities = ServerCapabilities,
					sCSCFName = SCSCFName};
		_ ->
			#user_registration_status_response{
					experimentalResult = subsequent_registration,
					sCSCFName = SCSCFName}
	end;
user_registration_status_query5(#user_registration_status_query{
		typeOfAuthorization = deregistration}, _From, _State, _U, _S, L)
		when L == [#location{state = not_registered}] orelse L == [] ->
	mnesia:abort(identity_not_registered);
user_registration_status_query5(#user_registration_status_query{
		publicUserID = PublicUserID, privateUserID = PrivateUserID,
		typeOfAuthorization = TypeOfAuthorization}, _From, _State,
		#user{publicUserIDs = PublicUserIDs} = U, S, L)
		when (L == [#location{state = not_registered}] orelse L == []),
		(TypeOfAuthorization =:= registration
		orelse TypeOfAuthorization =:= undefined) ->
	case get_registered_address(U, S, PublicUserID) of
		 #location{sCSCFName = SCSCFName} ->
			% If there is at least one Public User Identity
			% within the IMS Subscription that is registered.
			#user_registration_status_response{
					experimentalResult = subsequent_registration,
					sCSCFName = SCSCFName};
		[#location{sCSCFName = SCSCFName} | _] ->
			% If there is at least one Public User Identity
			% within the IMS Subscription that is unregistered.
			% Is selection of a new S-CSCF necessary?
			case application:get_env(keep_unregistered_scscf) of
				{ok, false} ->
					#user_registration_status_response{
					experimentalResult = server_selection,
					sCSCFCapabilities =
							S#subscriber.serverCapabilities,
					sCSCFName = SCSCFName};
				_ ->
					#user_registration_status_response{
					experimentalResult = subsequent_registration,
					sCSCFName = SCSCFName}
			end;
		_ ->
			% If there is no identity of the user within the
			% same Subscription that is registered or unregistered.
			% Check if an S-CSCF is stored for the user (auth pending).
			Fpend = fun(_, P, [#location{privateUserID = P,
						state = notregistered, authenticationPending = true,
						sCSCFName = UserSCSCFName} | _], _) ->
					UserSCSCFName;
				(Fpend, P, [_ | T], Rest) ->
					Fpend(Fpend, P, T, Rest);
				(Fpend, P, [], [H | T]) ->
					Fpend(Fpend, P, mnesia:read(location, H, read), T);
				(_, _, [], []) ->
					undefined
			end,
			case Fpend(Fpend, PrivateUserID, [], PublicUserIDs) of
				UserSCSCFName when is_list(UserSCSCFName) ->
					#user_registration_status_response{
							experimentalResult = subsequent_registration,
							sCSCFName = UserSCSCFName};
				undefined  ->
					#user_registration_status_response{
							experimentalResult = first_registration,
							sCSCFCapabilities = S#subscriber.serverCapabilities}
			end
	end.

%% @spec (SRN, From, State) -> Result
%% 	SRN = scscf_registration_notification()
%% 	From = {pid(), Tag}
%% 	Tag = any()
%% 	State = state()
%% 	Result = {reply, Reply, NewState}
%% 	         | {reply, Reply, NewState, Timeout}
%% 	         | {noreply, NewState}
%% 	         | {noreply, NewState, Timeout}
%% 	         | {stop, Reason, Reply, NewState}
%% 	         | {stop, Reason, NewState}
%% 	Reply = term()
%% 	NewState = state()
%% 	Timeout = integer() | infinity
%% 	Reason = term()
%% @doc Handle an S-CSCF registration/deregistration notification (SRN).
%% 	Procedure described in 3GPP TS 29.228
%% 	6.1.2 S-CSCF registration/deregistration notification.
%% @see //stdlib/gen_server:handle_call/3
%% 
scscf_registration_notification(#scscf_registration_notification{
		publicUserID = PublicUserID, privateUserID = PrivateUserID,
		sCSCFName = SCSCFName, serverAssignmentType = Type} = SRN,
		From, State) ->
	Ferr = fun(Reason) ->
			error_logger:error_report(["SRN",
					{reply, Reason},
					{serverAssignmentType, Type},
					{sCSCFName, SCSCFName},
					{privateUserID, PrivateUserID},
					{publicUserID, PublicUserID}])
	end,
	case mnesia:activity(transaction, fun() ->
			scscf_registration_notification1(SRN, From, State) end) of
		{atomic, Reply} ->
			{reply, Reply, State};
		{aborted, Reason} when Reason == user_unknown; 
				Reason == identities_dont_match ->
			Ferr(Reason),
			Reply = #scscf_registration_notification_response{
					experimentalResult = Reason},
			{reply, Reply, State};
		{aborted, Reason} when Reason == avp_occurs_too_many_times ->
			Ferr(Reason),
			Reply = #scscf_registration_notification_response{
					resultCode = Reason},
			{reply, Reply, State};
		{aborted, _Reason} ->
			Ferr(unable_to_comply),
			Reply = #scscf_registration_notification_response{
					resultCode = unable_to_comply},
			{reply, Reply, State}
	end.

%% @spec (SRN::scscf_registration_notification(), From::tuple(),
%% 		State::state()) -> Result
%% @doc 3GPP TS 29.228 6.1.2.1 1.
%% @hidden
scscf_registration_notification1(#scscf_registration_notification{
		publicUserID = PublicUserIDs, privateUserID = undefined} = SRN,
		From, State) when is_list(hd(PublicUserIDs)) ->
	% 6.1.2.1 1. Check that the Public User Identities exist in the HSS.
	Faddress = fun(Faddress, [PublicUserID | T], Acc) ->
			[Address] = mnesia:read(address, PublicUserID, read),
			Faddress(Faddress, T, [[Address] | Acc]);
		(_, [], Acc) ->
			Addresses = lists:reverse(Acc),
			[Subscriber] = mnesia:read(subscriber, hd(Addresses), read),
			[User] = mnesia:read(user,
					hd(Subscriber#subscriber.privateUserIDs), read),
			{User, Addresses, Subscriber}
	end,
	try Faddress(Faddress, PublicUserIDs, []) of
		{U, A, S} ->
			scscf_registration_notification2(SRN, From, State, U, A, S)
	catch
		error:{badmatch, []} ->
			mnesia:abort(user_unknown)
	end;
scscf_registration_notification1(#scscf_registration_notification{
		publicUserID = PublicUserID, privateUserID = undefined} = SRN,
		From, State) when is_list(PublicUserID) ->
	% 6.1.2.1 1. Check that the Public User Identity exists in the HSS.
	try
		[Address] = mnesia:read(address, PublicUserID, read),
		[Subscriber] = mnesia:read(subscriber,
				Address#address.subscriberID, read),
		[User] = mnesia:read(user,
				hd(Subscriber#subscriber.privateUserIDs), read),
		{User, Address, Subscriber}
	of
		{U, A, S} ->
			scscf_registration_notification3(SRN, From, State, U, A, S)
	catch
		error:{badmatch, []} ->
			mnesia:abort(user_unknown)
	end;
scscf_registration_notification1(#scscf_registration_notification{
		publicUserID = undefined, privateUserID = PrivateUserID} = SRN,
		From, State) when is_list(PrivateUserID) ->
	% 6.1.2.1 1. Check that the Private User Identity exists in the HSS.
	try
		[User] = mnesia:read(user, PrivateUserID, read),
		[Subscriber] = mnesia:read(subscriber, User#user.subscriberID, read),
		{User, Subscriber}
	of
		{U, S} ->
			scscf_registration_notification3(SRN, From, State, U, undefined, S)
	catch
		error:{badmatch, []} ->
			mnesia:abort(user_unknown)
	end;
scscf_registration_notification1(#scscf_registration_notification{
		publicUserID = PublicUserIDs, privateUserID = PrivateUserID} = SRN,
		From, State) when is_list(hd(PublicUserIDs)) ->
	% 6.1.2.1 1. Check that the Public User Identities and the
	%            Private User Identity exist in the HSS.
	Faddress = fun(Faddress, [PublicUserID | T], Acc) ->
			[Address] = mnesia:read(address, PublicUserID, read),
			Faddress(Faddress, T, [[Address] | Acc]);
		(_, [], Acc) ->
			case mnesia:read(user, PrivateUserID, read) of
				[User] ->
					[Subscriber] = mnesia:read(subscriber,
							User#user.subscriberID, read),
					{User, lists:reverse(Acc), Subscriber};
				_ ->
					mnesia:abort(user_unknown)
			end
	end,
	try Faddress(Faddress, PublicUserIDs, []) of
		{U, A, S} ->
			scscf_registration_notification2(SRN, From, State, U, A, S)
	catch
		error:{badmatch, []} ->
			mnesia:abort(user_unknown)
	end;
scscf_registration_notification1(#scscf_registration_notification{
		publicUserID = PublicUserID, privateUserID = PrivateUserID} = SRN,
		From, State) ->
	% 6.1.2.1 1. Check that the Private User Identity and the
	%            Public User Identity exists in the HSS.
	try
		[User] = mnesia:read(user, PrivateUserID, read),
		[Address] = mnesia:read(address, PublicUserID, read),
		[Subscriber] = mnesia:read(subscriber, User#user.subscriberID, read),
		{User, Address, Subscriber}
	of
		{U, A, S} ->
			scscf_registration_notification2(SRN, From, State, U, A, S)
	catch
		error:{badmatch, []} ->
			mnesia:abort(user_unknown)
	end.

%% @spec (SRN::scscf_registration_notification(), From::tuple(), State::state(),
%% 		User::user(), Address::address(), Subscriber::subscriber()) -> Result
%% @doc Check that the Public User Identity received in the request is 
%% 	associated with the Private User Identity received in the request.
%% 	3GPP TS 29.228 6.1.2.1 2.
%% @hidden
scscf_registration_notification2(#scscf_registration_notification{
		publicUserID = PublicUserIDs, privateUserID = PrivateUserID} = SRN,
		From, State,
		#user{subscriberID = SubscriberID, publicUserIDs = UserPublicUserIDs} = U,
		#address{subscriberID = SubscriberID} = A, S)
		when is_list(hd(PublicUserIDs)), is_list(PrivateUserID) ->
	% Check if Public Identities are associated with Private Identity
	case PublicUserIDs -- UserPublicUserIDs of
		[] ->
			scscf_registration_notification3(SRN, From, State, U, A, S);
		_ ->
			mnesia:abort(identities_dont_match)
	end;
scscf_registration_notification2(#scscf_registration_notification{
		publicUserID = PublicUserID, privateUserID = PrivateUserID} = SRN,
		From, State,
		#user{subscriberID = SubscriberID, publicUserIDs = PublicUserIDs} = U,
		#address{subscriberID = SubscriberID} = A, S)
		when is_list(PublicUserID), is_list(PrivateUserID) ->
	% Check if Public Identity is associated with Private Identity
	case lists:member(PublicUserID, PublicUserIDs) of
		true ->
			scscf_registration_notification3(SRN, From, State, U, A, S);
		false ->
			mnesia:abort(identities_dont_match)
	end;
scscf_registration_notification2(#scscf_registration_notification{
		publicUserID = PublicUserIDs, privateUserID = undefined} = SRN,
		From, State,
		#user{subscriberID = SubscriberID, publicUserIDs = PublicUserIDs} = U,
		Addresses, #subscriber{privateUserIDs = PrivateUserIDs} = S)
		when is_list(hd(PublicUserIDs)), is_list(Addresses) ->
	% Check if Public Identities are associated
	Fsub = fun(Fsub, ID, [#address{subscriberID = ID} | T])  ->
			Fsub(Fsub, ID, T) ;
		(_, _, []) ->
			true;
		(_, _, _) ->
			false
	end,
	Fuser = fun(Fuser, [#user{publicUserIDs = IDs} = User | T]) ->
			case PublicUserIDs -- IDs of
				[] ->
					User;
				_ ->
					Fuser(Fuser, T)
			end;
		(Fuser, [PrivateUserID | T]) ->
			[User] = mnesia:read(user, PrivateUserID, read),
			Fuser(Fuser, [User | T]);
		(_, []) ->
			mnesia:abort(identities_dont_match)
	end,
	% Check that all identities are associated with the same subscriber
	case Fsub(Fsub, SubscriberID, Addresses) of
		true ->
			% Check that all identities are all associated with one user
			U = Fuser(Fuser, [U | PrivateUserIDs]),
			scscf_registration_notification3(SRN, From, State, U, Addresses, S);
		false ->
			mnesia:abort(identities_dont_match)
	end.

%% @spec (SRN::scscf_registration_notification(), From::tuple(), State::state(),
%% 		User::user(), Address::address(), Subscriber::subscriber()) -> Result
%% @doc Check for too many Public User Identities.
%% 	3GPP TS 29.228 6.1.2.1 3.
%% @hidden
scscf_registration_notification3(#scscf_registration_notification{
		publicUserID = PublicUserID, serverAssignmentType = Type},
		_From, _State, _U, _A, _S) when is_list(hd(PublicUserID)),
		(Type =/= timeout_deregistration
		andalso Type =/= user_deregistration
		andalso Type =/= deregistration_too_much_data
		andalso Type =/= timeout_deregistration_store_server_name
		andalso Type =/= user_deregistration_store_server_name
		andalso Type =/= administrative_deregistration) ->
	% 6.1.2.1 3. Check for too many Public User Identities.
	mnesia:abort(avp_occurs_too_many_times);
scscf_registration_notification3(SRN, From, State, U, A, S) ->
	scscf_registration_notification4(SRN, From, State, U, A, S).

%% @spec (SRN::scscf_registration_notification(), From::tuple(), State::state(),
%% 		User::user(), Address::address(), Subscriber::subscriber()) -> Result
%% @doc Check if the identity in the request is a Public Service Identity.
%% 	3GPP TS 29.228 6.1.2.1 4.
%% @TODO:  implement Public Service Identities
%% @hidden
scscf_registration_notification4(SRN, From, State, U, A, S) ->
	scscf_registration_notification5(SRN, From, State, U, A, S).

%% @spec (SRN::scscf_registration_notification(), From::tuple(), State::state(),
%% 		User::user(), Address::address(), Subscriber::subscriber()) -> Result
%% @doc Check the Server Assignment Type received in the request.
%% 	3GPP TS 29.228 6.1.2.1 5.
%% @hidden
scscf_registration_notification5(#scscf_registration_notification{
		publicUserID = ExplicitPublicUserID, privateUserID = PrivateUserID,
		sCSCFName = SCSCFName, serverAssignmentType = Type},
		_From, _State, _U, _A,
		#subscriber{privateUserIDs = PrivateUserIDs,
		implicitRegistrationSets = ImplicitRegistrationSets})
		when Type =:= registration orelse Type =:= reregistration ->
	% clear the authentication pending flag for the private id if set
	% set the registration state as registered (if not already)
	Fpub = fun(Fpub, [PublicUserID | T]) ->
			case mnesia:read(location, PublicUserID, write) of
				[] ->
					L = #location{publicUserID = PublicUserID,
							privateUserID = PrivateUserID,
							state = registered, sCSCFName = SCSCFName},
					mnesia:write(location, L, write);
				Locations ->
					UpdateFlag = [L || L <- Locations,
							L#location.privateUserID =:= PrivateUserID,
							L#location.authenticationPending =:= true,
							L#location.state =:= registered],
					lists:foreach(fun(L) ->
							La = L#location{authenticationPending = false,
							sCSCFName = SCSCFName},
							mnesia:delete_object(location, L, write),
							mnesia:write(location, La, write) end, UpdateFlag),
					UpdateState = [L || L <- Locations,
							L#location.state =/= registered],
					lists:foreach(fun(L) ->
							La = L#location{state = registered, sCSCFName = SCSCFName},
							mnesia:delete_object(location, L, write),
							mnesia:write(location, La, write) end, UpdateState),
					case [L || L <- Locations,
							L#location.privateUserID =:= PrivateUserID] of
						[] ->
							L = #location{publicUserID = PublicUserID,
									privateUserID = PrivateUserID,
									state = registered, sCSCFName = SCSCFName},
							mnesia:write(location, L, write);
						_ ->
							ok
					end
			end,
			Fpub(Fpub, T);
		(_, []) ->
			ok
	end,
	PublicUserIDs = case hss:get_implicit_registration_set(ExplicitPublicUserID,
			ImplicitRegistrationSets) of
		[] ->
			[ExplicitPublicUserID];
		ImplicitRegistrationSet ->
			ImplicitRegistrationSet
	end,
	Fpub(Fpub, PublicUserIDs),
	AssociatedPrivateIDs = case PrivateUserIDs -- [PrivateUserID] of
		[] ->
			undefined;
		IDs ->
			IDs
	end,
	UserProfile = hss_xml:subscription(PrivateUserID, PublicUserIDs),
	#scscf_registration_notification_response{
			resultCode = success,
			userProfile = UserProfile,
			associatedPrivateIDs = AssociatedPrivateIDs};
scscf_registration_notification5(#scscf_registration_notification{
		publicUserID = ExplicitPublicUserID, sCSCFName = SCSCFName,
		serverAssignmentType = unregistered_user}, _From, _State, _U, _A,
		#subscriber{privateUserIDs = [PrivateUserID | RestIDs],
		implicitRegistrationSets = ImplicitRegistrationSets} = S) ->
	% store the S-CSCF name
	% set the registration state as unregistered (if not already)
	Fpub = fun(Fpub, [PublicUserID | T]) ->
			case mnesia:read(location, PublicUserID, write) of
				[#location{state = unregistered, privateUserID = undefined,
						sCSCFName = S}] when S =:= SCSCFName ->
					ok;
				Remove ->
					lists:foreach(fun(L) ->
							mnesia:delete_object(location, L, write) end, Remove),
					mnesia:write(location,
							#location{publicUserID = PublicUserID,
							state = unregistered, sCSCFName = SCSCFName}, write)
			end,
			Fpub(Fpub, T);
		(_, []) ->
			ok
	end,
	PublicUserIDs = case hss:get_implicit_registration_set(ExplicitPublicUserID,
			ImplicitRegistrationSets) of
		[] ->
			[ExplicitPublicUserID];
		ImplicitRegistrationSet ->
			ImplicitRegistrationSet
	end,
	Fpub(Fpub, PublicUserIDs),
	AssociatedPrivateIDs = case RestIDs of
		[] ->
			undefined;
		RestIDs ->
			RestIDs
	end,
	UserProfile = hss_xml:subscription(PrivateUserID, PublicUserIDs),
	#scscf_registration_notification_response{
			privateUserID = PrivateUserID,
			resultCode = success,
			userProfile = UserProfile,
			associatedPrivateIDs = AssociatedPrivateIDs};
scscf_registration_notification5(#scscf_registration_notification{
		publicUserID = undefined, privateUserID = PrivateUserID,
		serverAssignmentType = Type} = SRN, From, State,
		#user{publicUserIDs = PublicUserIDs} = U, A, S)
		when (Type =:= timeout_deregistration
		orelse Type =:= user_deregistration
		orelse Type =:= deregistration_too_much_data
		orelse Type =:= administrative_deregistration
		orelse Type =:= timeout_deregistration_store_server_name
		orelse Type =:= user_deregistration_store_server_name),
		is_list(PrivateUserID) ->
	scscf_registration_notification5(SRN#scscf_registration_notification{
			publicUserID = PublicUserIDs}, From, State, U, A, S);
scscf_registration_notification5(#scscf_registration_notification{
		publicUserID = ExplicitPublicUserIDs, serverAssignmentType = Type},
		_From, _State, _U, _A,
		#subscriber{implicitRegistrationSets = ImplicitRegistrationSets})
		when (Type =:= timeout_deregistration
		orelse Type =:= user_deregistration
		orelse Type =:= deregistration_too_much_data
		orelse Type =:= administrative_deregistration),
		is_list(hd(ExplicitPublicUserIDs)) ->
	Freg = fun(Freg, [[#location{state = registered} = L] | T]) ->
			% if registered with only one user set to not registered
			mnesia:delete_object(location, L, write),
			Freg(Freg, T);
		(Freg, [[#location{state = registered} | _] | T]) ->
			% keep if registered with more than one user 
			Freg(Freg, T);
		(Freg, [[#location{state = unregistered} = L] | T]) ->
			% if unregistered set to not registered
			mnesia:delete_object(location, L, write),
			Freg(Freg, T);
		(_, []) ->
			ok
	end,
	Fpub = fun(Fpub, [ExplicitPublicUserID | T], Acc) ->
			case hss:get_implicit_registration_set(ExplicitPublicUserID,
					ImplicitRegistrationSets) of
				[] ->
					Fpub(Fpub, T, [ExplicitPublicUserID | Acc]);
				ImplicitRegistrationSet ->
					Fpub(Fpub, T, [ImplicitRegistrationSet | Acc])
			end;
		(_, [], Acc) ->
			lists:flatten(lists:reverse(Acc))
	end,
	PublicUserIDs = Fpub(Fpub, ExplicitPublicUserIDs, []),
	Locations = [mnesia:read(location, P, write) || P <- PublicUserIDs],
	Freg(Freg, Locations),
	#scscf_registration_notification_response{resultCode = success};
scscf_registration_notification5(#scscf_registration_notification{
		publicUserID = PublicUserID, serverAssignmentType = Type} = SRN,
		From, State, U, A, S)
		when (Type =:= timeout_deregistration
		orelse Type =:= user_deregistration
		orelse Type =:= deregistration_too_much_data
		orelse Type =:= administrative_deregistration
		orelse Type =:= timeout_deregistration_store_server_name
		orelse Type =:= user_deregistration_store_server_name),
		is_list(PublicUserID) ->
	scscf_registration_notification5(SRN#scscf_registration_notification{
			publicUserID = [PublicUserID]}, From, State, U, A, S);
scscf_registration_notification5(#scscf_registration_notification{
		publicUserID = ExplicitPublicUserIDs, serverAssignmentType = Type},
		_From, _State, _U, _A,
		#subscriber{implicitRegistrationSets = ImplicitRegistrationSets})
		when (Type =:= timeout_deregistration_store_server_name
		orelse Type =:= user_deregistration_store_server_name),
		is_list(hd(ExplicitPublicUserIDs)) ->
	case application:get_env(keep_unregistered_scscf) of
		{ok, true} ->
			Fkeep = fun(L) ->
					mnesia:delete_object(location, L, write),
					mnesia:write(location,
							L#location{state = unregistered}, write) end,
			Success = #scscf_registration_notification_response{
					resultCode = success};
		_ ->
			Fkeep = fun(L) ->
					mnesia:delete_object(location, L, write) end,
			Success = #scscf_registration_notification_response{
					experimentalResult = success_server_name_not_stored}
	end,
	Freg = fun(Freg, [[#location{state = registered} = L] | T]) ->
			% if registered with only one user
			Fkeep(L),
			Freg(Freg, T);
		(Freg, [[#location{state = registered} | _] | T]) ->
			% keep if registered with more than one user 
			Freg(Freg, T);
		(_, []) ->
			ok
	end,
	Fpub = fun(Fpub, [ExplicitPublicUserID | T], Acc) ->
			case hss:get_implicit_registration_set(ExplicitPublicUserID,
					ImplicitRegistrationSets) of
				[] ->
					Fpub(Fpub, T, [ExplicitPublicUserID | Acc]);
				ImplicitRegistrationSet ->
					Fpub(Fpub, T, [ImplicitRegistrationSet | Acc])
			end;
		(_, [], Acc) ->
			lists:flatten(lists:reverse(Acc))
	end,
	PublicUserIDs = Fpub(Fpub, ExplicitPublicUserIDs, []),
	Locations = [mnesia:read(location, P, write) || P <- PublicUserIDs],
	Freg(Freg, Locations),
	Success;
scscf_registration_notification5(#scscf_registration_notification{
		publicUserID = ExplicitPublicUserID, privateUserID = PrivateUserID,
		sCSCFName = SCSCFName, serverAssignmentType = no_assignment},
		_From, _State, _U, _A,
		#subscriber{privateUserIDs = PrivateUserIDs,
		implicitRegistrationSets = ImplicitRegistrationSets}) ->
	% Check if the requesting S-CSCF is the one assigned
	[#location{sCSCFName = SCSCFName} | _] = mnesia:read(location,
			ExplicitPublicUserID, write),
	AssociatedPrivateIDs = case PrivateUserIDs -- [PrivateUserID] of
		[] ->
			undefined;
		RestIDs ->
			RestIDs
	end,
	PublicUserIDs = case hss:get_implicit_registration_set(ExplicitPublicUserID,
			ImplicitRegistrationSets) of
		[] ->
			[ExplicitPublicUserID];
		ImplicitRegistrationSet ->
			ImplicitRegistrationSet
	end,
	UserProfile = hss_xml:subscription(PrivateUserID, PublicUserIDs),
	#scscf_registration_notification_response{
			resultCode = success,
			userProfile = UserProfile,
			associatedPrivateIDs = AssociatedPrivateIDs};
scscf_registration_notification5(#scscf_registration_notification{
		publicUserID = ExplicitPublicUserID, serverAssignmentType = Type},
		_From, _State, _U, _A,
		#subscriber{implicitRegistrationSets = ImplicitRegistrationSets})
		when (Type =:= authentication_failure
		orelse Type =:= authentication_timeout) ->
	Fclear = fun(#location{authenticationPending = true} = L) ->
			mnesia:delete_object(location, L, write),
			mnesia:write(location,
					L#location{authenticationPending = false}, write),
			ok;
		(_) ->
			ok
	end,
	Freg = fun(Freg, [[#location{state = registered} = L] | T]) ->
			% if registered with only one user set to not registered
			mnesia:delete_object(location, L, write),
			Freg(Freg, T);
		(Freg, [[#location{state = registered} | _] = L | T]) ->
			% keep if registered with more than one user 
			% clear authentication pending flag
			lists:foreach(Fclear, L),
			Freg(Freg, T);
		(Freg, [[#location{state = unregistered} = L] | T]) ->
			% if unregistered set to not registered
			mnesia:delete_object(location, L, write),
			Freg(Freg, T);
		(_, []) ->
			ok
	end,
	PublicUserIDs = case hss:get_implicit_registration_set(ExplicitPublicUserID,
			ImplicitRegistrationSets) of
		[] ->
			[ExplicitPublicUserID];
		ImplicitRegistrationSet ->
			ImplicitRegistrationSet
	end,
	Locations = [mnesia:read(location, P, write) || P <- PublicUserIDs],
	Freg(Freg, Locations),
	#scscf_registration_notification_response{resultCode = success}.

%% @spec (ULQ, From, State) -> Result
%% 	ULQ = user_location_query()
%% 	From = {pid(), Tag}
%% 	Tag = any()
%% 	State = state()
%% 	Result = {reply, Reply, NewState}
%% 	         | {reply, Reply, NewState, Timeout}
%% 	         | {noreply, NewState}
%% 	         | {noreply, NewState, Timeout}
%% 	         | {stop, Reason, Reply, NewState}
%% 	         | {stop, Reason, NewState}
%% 	Reply = term()
%% 	NewState = state()
%% 	Timeout = integer() | infinity
%% 	Reason = term()
%% @doc Handle a user location query (ULQ) from an I-CSCF.
%% 	Procedure described in 3GPP TS 29.228
%% 	6.1.4 User location query.
%% @see //stdlib/gen_server:handle_call/3
%% @TODO: implement Public Service Identities
%% 
user_location_query(#user_location_query{publicUserID = PublicUserID} = ULQ,
		From, State) ->
	Ferr = fun(Reason) ->
		error_logger:error_report(["ULQ",
				{reply, Reason},
				{publicUserID, PublicUserID}])
	end,
	case mnesia:activity(transaction, fun() ->
			user_location_query1(ULQ, From, State) end) of
		{atomic, Reply} ->
			{reply, Reply, State};
		{aborted, Reason} when Reason == user_unknown; 
				Reason == unregistered_service;
				Reason == identity_not_registered ->
			Ferr(Reason),
			Reply = #user_location_response{
					experimentalResult = Reason},
			{reply, Reply, State};
		{aborted, _Reason} ->
			Ferr(unable_to_comply),
			Reply = #user_location_response{
					resultCode = unable_to_comply},
			{reply, Reply, State}
	end.

%% @spec (ULQ::user_location_query(), From::tuple(), State::state()) -> Result
%% @doc 3GPP TS 29.228 6.1.4.1 1. Check that the Public Identity is known.
%% @TODO: 6.1.4.1 2. Check the type of the Public Identity
%% @hidden
user_location_query1(#user_location_query{publicUserID = PublicUserID} = ULQ,
		From, State) ->
	case mnesia:read(address, PublicUserID, read) of
		[Address] ->
			Location = mnesia:read(location, PublicUserID, read),
			user_location_query3(ULQ, From, State, Address, Location);
		[] ->
			mnesia:abort(user_unknown)
	end.

%% @spec (ULQ::user_location_query(), From::tuple(), State::state(),
%% 		Address::address(), Location::list()) -> Result
%% @doc 3GPP TS 29.228 6.1.4.1 3. Check the state of the Public Identity.
%% @hidden
user_location_query3(_ULQ, _From, _State, _Address,
		[#location{state = registered, sCSCFName = SCSCFName} | _]) ->
	#user_location_response{resultCode = success, sCSCFName = SCSCFName};
user_location_query3(#user_location_query{
		originatingRequest = OriginatingRequest}, _From, _State,
		#address{servicesUnregState = ServicesUnregState},
		[#location{state = unregistered, sCSCFName = SCSCFName}])
		when is_list(OriginatingRequest) orelse ServicesUnregState == true ->
	#user_location_response{resultCode = success, sCSCFName = SCSCFName};
user_location_query3(#user_location_query{
		originatingRequest = OriginatingRequest}, _From, _State,
		#address{servicesUnregState = ServicesUnregState},
		[#location{state = notregistered, sCSCFName = SCSCFName} | _])
		when is_list(OriginatingRequest) orelse ServicesUnregState == true ->
	#user_location_response{resultCode = success, sCSCFName = SCSCFName};
user_location_query3(#user_location_query{publicUserID = PublicUserID,
		originatingRequest = OriginatingRequest}, _From, _State,
		#address{servicesUnregState = ServicesUnregState,
		subscriberID = SubscriberID}, [])
		when is_list(OriginatingRequest) orelse ServicesUnregState == true ->
	Fuser = fun(Fuser, [H | T], PrivateUserIDs) ->
			case mnesia:read(location, H, read) of
				[] ->
					Fuser(Fuser, T, PrivateUserIDs);
				[#location{sCSCFName = SCSCFName} | _] ->
					SCSCFName
			end;
		(Fuser, [], [H | T]) ->
			[User] = mnesia:read(user, H, read),
			PublicUserIDs = User#user.publicUserIDs -- PublicUserID,
			Fuser(Fuser, PublicUserIDs, T);
		(_, [], []) ->
			not_found
	end,
	[Subscriber] = mnesia:read(subscriber, SubscriberID, read),
	case Fuser(Fuser, [], Subscriber#subscriber.privateUserIDs) of
		not_found ->
			Subscriber#subscriber.serverCapabilities,
			#user_location_response{experimentalResult = unregistered_service,
					sCSCFCapabilities = Subscriber#subscriber.serverCapabilities};
		SCSCFName ->
			#user_location_response{resultCode = success, sCSCFName = SCSCFName}
	end;
user_location_query3(#user_location_query{originatingRequest = undefined},
		_From, _State, #address{servicesUnregState = false}, []) ->
	mnesia:abort(identity_not_registered);
user_location_query3(#user_location_query{originatingRequest = undefined},
		_From, _State, #address{servicesUnregState = false},
		[#location{state = notregistered} | _]) ->
	mnesia:abort(identity_not_registered).
	
%% @spec (ARQ, From, State) -> Result
%% 	ARQ = authentication_request()
%% 	From = {pid(), Tag}
%% 	Tag = any()
%% 	State = state()
%% 	Result = {reply, Reply, NewState}
%% 	         | {reply, Reply, NewState, Timeout}
%% 	         | {noreply, NewState}
%% 	         | {noreply, NewState, Timeout}
%% 	         | {stop, Reason, Reply, NewState}
%% 	         | {stop, Reason, NewState}
%% 	Reply = term()
%% 	NewState = state()
%% 	Timeout = integer() | infinity
%% 	Reason = term()
%% @doc Handle an authentication request (ARQ) from an S-CSCF.
%% 	Procedure described in 3GPP TS 29.228
%% 	6.3.1 Authentication procedures.
%% @see //stdlib/gen_server:handle_call/3
%%
authentication_request(#authentication_request{
		publicUserID = PublicUserID, privateUserID = PrivateUserID,
		numberAuthenticationItems = NumItems, sCSCFName= SCSCFName} = ARQ,
		From, State) ->
	Ferr = fun(Reason) ->
		error_logger:error_report(["ARQ",
				{reply, Reason},
				{numberAuthenticationItems, NumItems},
				{sCSCFName, SCSCFName},
				{privateUserID, PrivateUserID},
				{publicUserID, PublicUserID}])
	end,
	case mnesia:activity(transaction, fun() ->
			authentication_request1(ARQ, From, State) end) of
		{atomic, Reply} ->
			{reply, Reply, State};
		{aborted, Reason} when Reason == user_unknown; 
				Reason == identities_dont_match;
				Reason == auth_scheme_unsupported ->
			Ferr(Reason),
			Reply = #authentication_request_response{
					experimentalResult = Reason},
			{reply, Reply, State};
		{aborted, Reason} ->
io:fwrite("Reason = ~p~n", [Reason]),
			Ferr(unable_to_comply),
			Reply = #authentication_request_response{
					resultCode = unable_to_comply},
			{reply, Reply, State}
	end.

%% @spec (ARQ::authentication_request(), From::tuple(), State::state())
%% 		-> Result
%% @doc Check that the Private User Identity and the Public User
%% 	Identity exists in the HSS.
%% 	3GPP TS 29.228 6.3.1 1.
%% @hidden
authentication_request1(#authentication_request{
		publicUserID = PublicUserID, privateUserID = PrivateUserID} = ARQ,
		From, State) when is_list(PublicUserID), is_list(PrivateUserID) ->
	% 6.3.1 1. Check that the Private User Identity and the
	%          Public User Identity exists in the HSS.
	try
		[User] = mnesia:read(user, PrivateUserID, read),
		[Address] = mnesia:read(address, PublicUserID, read),
		[Subscriber] = mnesia:read(subscriber, User#user.subscriberID, read),
		{User, Address, Subscriber}
	of
		{U, A, S} ->
			authentication_request2(ARQ, From, State, U, A, S)
	catch
		error:{badmatch, []} ->
			mnesia:abort(user_unknown)
	end.

%% @spec (ARQ::authentication_request(), From::tuple(), State::state(),
%% 		User::user(), Address::address(), Subscriber::subscriber()) -> Result
%% @doc Check whether the Private and Public User Identities in the
%% 	request are associated in the HSS.
%% 	3GPP TS 29.228 6.3.1 2.
%% @hidden
authentication_request2(#authentication_request{
		publicUserID = PublicUserID} = ARQ, From, State,
		#user{subscriberID = SubscriberID, publicUserIDs = PublicUserIDs} = U,
		#address{subscriberID = SubscriberID} = A, S) ->
	case lists:member(PublicUserID, PublicUserIDs) of
		true ->
			authentication_request3(ARQ, From, State, U, A, S);
		false ->
			mnesia:abort(identities_dont_match)
	end;
authentication_request2(_ARQ, _From, _State, _U, _A, _S) ->
	mnesia:abort(identities_dont_match).

%% @spec (ARQ::authentication_request(), From::tuple(), State::state(),
%% 		User::user(), Address::address(), Subscriber::subscriber()) -> Result
%% @doc Check that the authentication scheme indicated in the request
%% 	is supported.
%% 	3GPP TS 29.228 6.3.1 3.
%% @hidden
authentication_request3(#authentication_request{
		authenticationData = #authenticationData{
		authenticationScheme = "Digest-AKAv1-MD5"},
		publicUserID = ExplicitPublicUserID} = ARQ, From, State, U, A,
		#subscriber{implicitRegistrationSets = ImplicitRegistrationSets} = S) ->
	[H | T] = case hss:get_implicit_registration_set(ExplicitPublicUserID,
			ImplicitRegistrationSets) of
		[] ->
			[ExplicitPublicUserID];
		ImplicitRegistrationSet ->
			ImplicitRegistrationSet
	end,
	L = mnesia:read(location, H, read),
	authentication_request4(ARQ, From, State, U, A, S, L, H, T);
authentication_request3(_ARQ, _From, _State, _U, _A, _S) ->
	mnesia:abort(auth_scheme_unsupported).

%% @spec (ARQ::authentication_request(), From::tuple(), State::state(),
%% 		User::user(), Address::address(), Subscriber::subscriber(),
%% 		Location::location(), PublicUserID::publicUserID(),
%% 		Rest::list()) -> Result
%% @doc Check if the request indicates there is a synchronization failure.
%% 	3GPP TS 29.228 6.3.1 4.
%% @hidden
authentication_request4(#authentication_request{
		authenticationData = #authenticationData{
		authorizationInformation = undefined}} = ARQ,
		From, State, U, A, S, L, P, R) ->
	authentication_request5(ARQ, From, State, U, A, S, L, P, R);
authentication_request4(#authentication_request{
		numberAuthenticationItems = NumItems,
		authenticationData = #authenticationData{
		authorizationInformation = <<RAND:16/binary,
		ConcSQNms:6/binary, MAC_S:8/binary>>},
		publicUserID = PublicUserID, privateUserID = PrivateUserID,
		sCSCFName = SCSCFName}, _From, _State,
		#user{k = K, opc = OPc} = U, _A, _S,
		[#location{sCSCFName = SCSCFName} | _], _P, _R) ->
	% process AUTS as described in 3GPP TS 33.203 (33.102 6.3.5)
	AK = milenage:f5star(OPc, K, RAND),
	SQNms = crypto:exor(ConcSQNms, AK),
	<<SEQms:43, _INDms:5>>  = SQNms,
	NewSEQ = (SEQms + 1) rem 16#7FFFFFFFFFF,
	MAC_S = milenage:f1star(OPc, K, RAND, SQNms, <<0:16>>),
	mnesia:write(user, U#user{sequence = NewSEQ}, write),
	AuthenticationData = authentication_data(OPc, K, NewSEQ, NumItems),
	#authentication_request_response{
			publicUserID = PublicUserID,
			privateUserID = PrivateUserID,
			numberAuthenticationItems = NumItems,
			authenticationData = AuthenticationData,
			resultCode = success}.

%% @spec (ARQ::authentication_request(), From::tuple(), State::state(),
%% 		User::user(), Address::address(), Subscriber::subscriber(),
%% 		Location::location(), PublicUserID::publicUserID(),
%% 		Rest::list()) -> Result
%% @doc Check the registration status of the Public User Identity
%% 	received in the request.
%% 	3GPP TS 29.228 6.3.1 5.
%% @hidden
authentication_request5(#authentication_request{
		sCSCFName = SCSCFName} = ARQ, From, State, U, A, S,
		[#location{state = registered, sCSCFName = SCSCFName} | _], _P, R) ->
	case R of
		[H | T] ->
			L = mnesia:read(location, H, write),
			authentication_request5(ARQ, From, State, U, A, S, L, H, T);
		[] ->
			authentication_request5(ARQ, From, State, U, A, S, [], [], [])
	end;
authentication_request5(#authentication_request{
		numberAuthenticationItems = NumItems,
		publicUserID = PublicUserID, privateUserID = PrivateUserID},
		_From, _State,
		#user{k = K, opc = OPc, sequence = SEQ} = U, _A, _S, [], [], []) ->
	NewSEQ = (SEQ + 1) rem 16#7FFFFFFFFFF,
	mnesia:write(user, U#user{sequence = NewSEQ}, write),
	AuthenticationData = authentication_data(OPc, K, NewSEQ, NumItems),
	#authentication_request_response{
			publicUserID = PublicUserID, privateUserID = PrivateUserID,
			numberAuthenticationItems = NumItems,
			authenticationData = AuthenticationData,
			resultCode = success};
authentication_request5(#authentication_request{ privateUserID = PrivateUserID,
		sCSCFName = SCSCFName} = ARQ, From, State, U, A, S,
		[#location{state = RegState, sCSCFName = SCSCFName} | _] = L, P, R) ->
	Fexists = fun(#location{privateUserID = IMPI})
				when IMPI == PrivateUserID ->
			true;
		(_) ->
			false
	end,
	case lists:any(Fexists, L) of
		true ->
			Floc = fun(#location{privateUserID = IMPI} = Loc)
						when IMPI == PrivateUserID ->
					mnesia:delete_object(location, Loc, write),
					mnesia:write(location,
							Loc#location{authenticationPending = true}, write);
				(_) ->
					ok
			end,
			lists:foreach(Floc, L);
		false ->
			mnesia:write(location, #location{publicUserID = P,
					privateUserID = PrivateUserID, state = RegState,
					sCSCFName = SCSCFName, authenticationPending = true}, write)
	end,
	case R of
		[H | T] ->
			Lnext = mnesia:read(location, H, write),
			authentication_request5(ARQ, From, State, U, A, S, Lnext, H, T);
		[] ->
			authentication_request5(ARQ, From, State, U, A, S, [], [], [])
	end;
authentication_request5(#authentication_request{ privateUserID = PrivateUserID,
		sCSCFName = SCSCFName} = ARQ, From, State, U, A, S, L, P, R) ->
	Floc = fun(#location{privateUserID = IMPI} = Loc)
				when IMPI == PrivateUserID ->
			mnesia:delete_object(location, Loc, write),
			mnesia:write(location, Loc#location{sCSCFName = SCSCFName,
					authenticationPending = true}, write);
		(Loc) ->
			mnesia:delete_object(location, Loc, write),
			mnesia:write(location, Loc#location{sCSCFName = SCSCFName}, write)
	end,
	lists:foreach(Floc, L),
	Fexists = fun(#location{privateUserID = IMPI})
				when IMPI == PrivateUserID ->
			true;
		(_) ->
			false
	end,
	case lists:any(Fexists, L) of
		false ->
			RegState = case L of
				[#location{state = RS} | _] ->
					RS;
				[] ->
					notregistered
			end,
			mnesia:write(location, #location{publicUserID = P,
					privateUserID = PrivateUserID, state = RegState,
					sCSCFName = SCSCFName, authenticationPending = true}, write);
		true ->
			ok
	end,
	case R of
		[H | T] ->
			Lnext = mnesia:read(location, H, write),
			authentication_request5(ARQ, From, State, U, A, S, Lnext, H, T);
		[] ->
			authentication_request5(ARQ, From, State, U, A, S, [], [], [])
	end.

%%----------------------------------------------------------------------
%% The hss_server internal functions
%%----------------------------------------------------------------------

%% @spec (User, Subscriber, PublicUserID) -> Result
%% 	User = user()
%% 	Subscriber = subscriber()
%% 	PublicUserID = publicUserID()
%% 	Result = Registered | Unregistered
%% 	Registered = location()
%% 	Unregistered = [location()]
%% @doc Find a PublicUserID in the registered state within the 
%% 	same subscription as <tt>User</tt>.
%% 	A lazy search is done in the following order; first the other 
%% 	PublicUserIDs in <tt>User</tt>, then shared PublicUserIDs in
%% 	<tt>Subscriber</tt> and lastly the PublicUserIDs for all other 
%% 	PrivateUserIDs in <tt>Subscriber</tt>.
%% 	Returns a {@link location()} list of the first registered
%% 	PublicUserID found.  If none is found a list of all
%% 	unregistered (not not registered) PublicUserIDs is returned.
%% @private
get_registered_address(#user{privateUserID = PrivateUserID,
		publicUserIDs = PublicUserIDs, subscriberID = SubscriberID},
		#subscriber{subscriberID = SubscriberID,
		sharedPublicUserIDs = SharedPublicUserIDs,
		privateUserIDs = PrivateUserIDs}, PublicUserID)
		when is_list(PublicUserID) ->
	Ffilter = fun(_, _, [#location{state = registered} = Location | _], _, _) ->
			Location;
		(_, Flocation, [#location{state = unregistered} = Location | _],
				T, Acc) ->
			Flocation(Flocation, T, [Location | Acc]);
		(_, Flocation, _, T, Acc) ->
			Flocation(Flocation, T, Acc)
	end,
	Flocation = fun(Flocation, [H | T], Acc) ->
			Ffilter(Ffilter, Flocation,
					mnesia:read(location, H, read), T, Acc);
		(_, [], Acc) ->
			lists:reverse(Acc)
	end,
	% Check other PublicUserIDs in user
	% Check shared PublicUserIDs in subscriber
	OtherPublicUserIDs = PublicUserIDs ++ SharedPublicUserIDs
			-- PublicUserIDs -- [PublicUserID],
	OtherPrivateUserIDs = PrivateUserIDs -- [PrivateUserID],
	case Flocation(Flocation, OtherPublicUserIDs, []) of
		#location{} = Registered ->
			Registered;
		Unregistered when length(OtherPrivateUserIDs) > 0 ->
			% Check other PublicUserIDs in subscriber
			Fuser = fun(Fuser, [H | T], Acc) ->
					[User] = mnesia:read(user, H, read),
					case Flocation(Flocation, User#user.publicUserIDs, Acc) of
						#location{} = R ->
							R;
						UR ->
							Fuser(Fuser, T, [UR | Acc])
					end;
				(_, [], Acc) ->
					lists:reverse(Acc)
			end,
			Fuser(Fuser, OtherPrivateUserIDs, Unregistered);
		Unregistered ->
			Unregistered
	end.

%% @spec (OPc, K, SEQ, N) -> authenticationData()
%% 	OPc = opc()
%% 	K = k()
%% 	SEQ = integer()
%% 	N = integer()
%% @doc Generate authentication data for response.
%% @private
authentication_data(OPc, K, SEQ, N) ->
	authentication_data(OPc, K, SEQ, N, []).
%% @hidden
authentication_data(_OPc, _K, _SEQ, 0, Acc) ->
	Acc;
authentication_data(OPc, K, SEQ, N, Acc) ->
	RAND = crypto:rand_bytes(16),
	{RES, CK, IK, AK} = milenage:f2345(OPc, K, RAND),
	IND = N - 1,
	SQN = <<SEQ:43, IND:5>>,
	AMF = <<0:16>>,
	MAC = milenage:f1(OPc, K, RAND, SQN, AMF),
	ConcSQN = crypto:exor(SQN, AK),
	AUTN = <<ConcSQN/binary, AMF/binary, MAC/binary>>,
	AD = #authenticationData{itemNumber = N,
			authenticationScheme = "Digest-AKAv1-MD5",
			authenticationInformation = <<RAND/binary, AUTN/binary>>,
			authorizationInformation = RES,
			confidentialityKey = CK,
			integrityKey = IK},
	authentication_data(OPc, K, SEQ, N - 1, [AD | Acc]).

