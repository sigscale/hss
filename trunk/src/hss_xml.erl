%%% $Id: hss_xml.erl,v 1.3 2008/02/18 05:34:31 vances Exp $
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
%%% @doc This module implements user profile XML construction.
%%%
%%% @TODO implement shared iFC sets
%%%
-module(hss_xml).
-copyright('Copyright (c) 2008 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.3 $').

%% export the API
-export([subscription/2]).

-include("subscriber_data.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%----------------------------------------------------------------------
%%  The hss_profile API
%%----------------------------------------------------------------------

%% @spec (PrivateUserID, PublicUserIDs) -> XML
%% 	PrivateUserID = privateUserID()
%% 	PublicUserID = [publicUserID()]
%% 	XML = string()
%% @doc Create a user profile in XML format for download to an S-CSCF.
%%
subscription(PrivateUserID, PublicUserIDs) ->
	case mnesia:transaction(fun() -> profiles(PublicUserIDs) end) of
		{atomic, Profiles} ->
			Root = root(),
			PrivateID = #xmlElement{name = 'PrivateID',
					content = [#xmlText{value = PrivateUserID}]},
			XML = Root#xmlElement{content = [PrivateID | Profiles]},
			lists:flatten(xmerl:export([XML], xmerl_xml));
		{aborted, Reason} ->
			exit(Reason)
	end.
	
%%----------------------------------------------------------------------
%% The hss_profile internal functions
%%----------------------------------------------------------------------

%% @spec () -> XML
%% 	XML = #xmlElement{}
%% @doc Returns the root element of a user profile XML document.
%% @private
root() ->
	TopNode = {"xsi",'http://www.w3.org/2001/XMLSchema-instance'},
	TopNameSpace = #xmlNamespace{nodes = [TopNode]},
	Attribute1 = #xmlAttribute{name = 'xmlns:xsi',
			namespace = {"xmlns", "xsi"},
			value = "http://www.w3.org/2001/XMLSchema-instance"},
	Attribute2 = #xmlAttribute{name = 'xsi:noNamespaceSchemaLocation',
			namespace = {"xsi", "noNamespaceSchemaLocation"},
			value = "CxDataType.xsd"},
	#xmlElement{name = 'IMSSubscription',
			namespace = TopNameSpace,
			attributes = [Attribute1, Attribute2]}.

%% @spec (PublicUserIDs) -> XML
%% 	PublicUserID = [publicUserID()]
%% 	XML = #xmlElement{}
%% @doc Form the service profile elements for the XML document.
%% @private
profiles(PublicUserIDs) ->
	profiles(get_profiles(PublicUserIDs), []).
%% @hidden
profiles([{ProfileID, PublicUserIDs} | T], Acc) ->
	[P] = mnesia:read(profile, ProfileID, read),
	Fimpu = fun({PublicUserID, false}) ->
			IMPU = #xmlElement{name = 'Identity', 
					content = [#xmlText{value = PublicUserID}]},
			#xmlElement{name = 'PublicIdentity', content= [IMPU]};
		({PublicUserID, true}) ->
			BI = #xmlElement{name = 'BarringIndication',
					content= [#xmlText{value = "1"}]},
			IMPU = #xmlElement{name = 'Identity', 
					content = [#xmlText{value = PublicUserID}]},
			#xmlElement{name = 'PublicIdentity', content= [BI, IMPU]}
	end,
	PublicIdentity = [Fimpu(X) || X <- PublicUserIDs],
	InitialFilterCriteria = case P#profile.initialFilterCriteriaIDs of
		IFCs when IFCs =:= []; IFCs =:= undefined ->
			[];
		IFCs ->
			filters(IFCs)
	end,
	CoreNetworkServicesAuthorization = case P#profile.subscribedMediaProfileID of
		undefined ->
			[];
		SMPI ->
			#xmlElement{name = 'CoreNetworkServicesAuthorization',
					content = [#xmlElement{name = 'SubscribedMediaProfileId',
					content = [#xmlText{value = integer_to_list(SMPI)}]}]}
	end,
	ServiceProfile = #xmlElement{name = 'ServiceProfile',
			content = lists:flatten([PublicIdentity, InitialFilterCriteria,
			CoreNetworkServicesAuthorization])},
	profiles(T, [ServiceProfile | Acc]);
profiles([], Acc) ->
	lists:reverse(Acc).

%% @spec (PublicUserIDs) -> Profiles
%% 	Profiles = [{ProfileID, PublicIDs}]
%% 	ProfileID = serviceProfileID()
%% 	PublicIDs = [{PublicUserID, BarringIndicator}]
%% 	PublicUserID = publicUserID()
%% 	BarringIndicator = boolean()
%% @doc Forms the list of profiles and public identities.
%% @private
get_profiles(PublicUserIDs) ->
	get_profiles(PublicUserIDs, []).
%% @hidden
get_profiles([PublicUserID | T], Acc) ->
	[A] = mnesia:read(address, PublicUserID, read),
	ProfileID = A#address.serviceProfileID,
	BI = A#address.barringIndicator,
	case lists:keysearch(ProfileID, 1, Acc) of
		false ->
			get_profiles(T, [{ProfileID, [{PublicUserID, BI}]} | Acc]);
		{value, {ProfileID, L}} ->
			New = {ProfileID, L ++ [{PublicUserID, BI}]},
			get_profiles(T, lists:keyreplace(ProfileID, 1, Acc, New))
	end;
get_profiles([], Acc) ->
	lists:reverse(Acc).
	
%% @spec (InitialFilterCriteriaIDs) -> Filters
%% 	InitialFilterCriteriaIDs = [initialFilterCriteriaID()]
%% 	Filters = [#xmlElement{}]
%% @doc Form initial filter criteria elements for the XML document.
%% @private
filters(InitialFilterCriteriaIDs) -> 
	filters(InitialFilterCriteriaIDs, []).
%% @hidden
filters([InitialFilterCriteriaID | T], Acc) ->
	[F] = mnesia:read(filter, InitialFilterCriteriaID, read),
	Priority = #xmlElement{name = 'Priority',
			content = [#xmlText{value = integer_to_list(F#filter.priority)}]},
	TriggerPoint = case F#filter.servicePointTriggerIDs of
		Triggers when Triggers =:= []; Triggers =:= undefined  ->
			[];
		SPTIDs ->
			ConditionTypeCNF = #xmlElement{name = 'ConditionTypeCNF',
					content = case F#filter.conditionTypeCNF of
						false ->
							[#xmlText{value = "0"}];
						true ->
							[#xmlText{value = "1"}]
					end},
			#xmlElement{name = 'TriggerPoint',
					content = [ConditionTypeCNF | triggers(SPTIDs)]}
	end,
	ServerName = #xmlElement{name = 'ServerName',
			content =  [#xmlText{value = F#filter.applicationServerName}]},
	DefaultHandling = case F#filter.defaultHandling of
		undefined ->
			[];
		continued ->
			#xmlElement{name = 'DefaultHandling',
					 content = [#xmlText{value = "0"}]};
		terminated ->
			#xmlElement{name = 'DefaultHandling',
					 content = [#xmlText{value = "1"}]}
	end,
	ServiceInfo = case F#filter.serviceInformation of
		SI when SI =:= []; SI =:= undefined  ->
			[];
		SI ->
			#xmlElement{name = 'ServiceInfo', content = [#xmlText{value = SI}]}
	end,
	ApplicationServer = #xmlElement{name = 'ApplicationServer',
			content =  lists:flatten([ServerName, DefaultHandling, ServiceInfo])},
	ProfilePartIndicator = case F#filter.profilePartIndicator of
		undefined ->
			[];
		PPI ->
			#xmlElement{name = 'ProfilePartIndicator',
					content = case PPI of
						registered ->
							[#xmlText{value = "0"}];
						unregistered ->
							[#xmlText{value = "1"}]
					end}
	end,
	Content = lists:flatten([Priority, TriggerPoint, ApplicationServer,
			ProfilePartIndicator]),
	IFC = #xmlElement{name = 'InitialFilterCriteria', content = Content},
	filters(T, [IFC | Acc]);
filters([], Acc) ->
	lists:reverse(Acc).

%% @spec (ServicePointTriggerIDs) -> Triggers
%% 	ServicePointTriggerIDs = [servicePointTriggerIDs()]
%% 	Triggers = [#xmlElement{}]
%% @doc Form service point trigger elements for the XML document.
%% @private
triggers(ServicePointTriggerIDs) ->
	triggers(ServicePointTriggerIDs, []).
%% @hidden
triggers([ServicePointTriggerID | T], Acc) ->
	[S] = mnesia:read(trigger, ServicePointTriggerID, read),
	ConditionNegated = case S#trigger.conditionNegated of
		false ->
			[];
		true ->
			#xmlElement{name = 'ConditionNegated',
					content = [#xmlText{value = "1"}]}
	end,
	Fgroup = fun(N) -> #xmlElement{name = 'Group',
			content = [#xmlText{value = integer_to_list(N)}]} end,
	Group = [Fgroup(N) || N <- S#trigger.group],
	Condition = case S#trigger.condition of
		{requestURI, RequestURI} ->
			#xmlElement{name = 'RequestURI',
					content = [#xmlText{value = RequestURI}]};
		{method, Method} ->
			#xmlElement{name = 'Method',
					content = [#xmlText{value = Method}]};
		{sipHeader, SIPHeader} ->
			Header = #xmlElement{name = 'Header',
					content = [#xmlText{value = SIPHeader}]},
			#xmlElement{name = 'SIPHeader', content = [Header]};
		{sipHeader, SIPHeader, SIPContent} ->
			Header = #xmlElement{name = 'Header',
					content = [#xmlText{value = SIPHeader}]},
			Content = #xmlElement{name = 'Content',
					content = [#xmlText{value = SIPContent}]},
			#xmlElement{name = 'SIPHeader', content = [Header, Content]};
		{sessionCase, SessionCase} ->
			#xmlElement{name = 'SessionCase',
					content = case SessionCase of
						originating_session ->
							[#xmlText{value = "0"}];
						terminating_registered ->
							[#xmlText{value = "1"}];
						terminating_unregistered ->
							[#xmlText{value = "2"}];
						originating_unregistered ->
							[#xmlText{value = "3"}]
					end};
		{sessionDescription, SDPLine} ->
			Line = #xmlElement{name = 'Line',
					content = [#xmlText{value = SDPLine}]},
			#xmlElement{name = 'SessionDescription', content = [Line]};
		{sessionDescription, SDPLine, SDPContent} ->
			Line = #xmlElement{name = 'Line',
					content = [#xmlText{value = SDPLine}]},
			Content = #xmlElement{name = 'Content',
					content = [#xmlText{value = SDPContent}]},
			#xmlElement{name = 'SessionDescription', content = [Line, Content]}
	end,
	Extension = case S#trigger.registrationType of
		undefined ->
			[];
		RegType ->
			Freg = fun(registration) ->
					#xmlElement{name = 'RegistrationType',
							content = [#xmlText{value = "0"}]};
				(reregistration) ->
					#xmlElement{name = 'RegistrationType',
							content = [#xmlText{value = "1"}]};
				(deregistration) ->
					#xmlElement{name = 'RegistrationType',
							content = [#xmlText{value = "2"}]}
			end,
			#xmlElement{name = 'Extension',
					content = [Freg(R) || R <- RegType]}
	end,
	ServicePointTrigger = #xmlElement{name = 'SPT',
			content = lists:flatten([ConditionNegated, Group,
			Condition, Extension])},
	triggers(T, [ServicePointTrigger | Acc]);
triggers([], Acc) ->
	lists:reverse(Acc).

