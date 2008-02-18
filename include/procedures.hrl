%%% $Id: procedures.hrl,v 1.12 2008/02/18 05:34:31 vances Exp $
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
%%% This header file defines the format of the procedural requests
%%% between a CSCF (client) and the HSS (server).
%%%

%% @type user_registration_status_query() =
%% 	#user_registration_status_query{
%% 		publicUserID = publicUserID(),
%% 		typeOfAuthorization = registration | deregistration
%% 				| registration_and_capabilities,
%% 		visitedNetworkIdentifier = string(),
%% 		privateUserID = privateUserID(),
%% 		routingInformation = string()}.
%%
%%		User registration status query.
%%
-record(user_registration_status_query,
		{publicUserID,
		visitedNetworkIdentifier,
		typeOfAuthorization,
		privateUserID,
		routingInformation}).

%% @type experimentalResult() = user_unknown | identities_dont_match
%% 				| roaming_not_allowed | subsequent_reqistration
%% 				| server_selection | identity_not_registered
%% 				| first_registration | success_server_name_not_stored.
%% 	Experimental-Result AVP shall be used for Cx/Dx errors.

%% @type resultCode() = success | authorization_rejected
%% 				| unable_to_reply | unable_to_comply.
%%		Result-Code AVP shall be used for errors defined in the
%% 	Diameter Base Protocol.

%% @type user_registration_status_response() =
%% 	#user_registration_status_response{
%% 		resultCode = resultCode(),
%% 		experimentalResult = experimentalResult(),
%% 		sCSCFCapabilities = serverCapabilities(),
%% 		sCSCFName = sipURL()}.
%%
%%		User registration status response .
%%
-record(user_registration_status_response,
		{resultCode,
		experimentalResult,
		sCSCFCapabilities,
		sCSCFName}).

%% @type serverAssignmentType() = no_assignment | registration
%% 				| reregistration | unregistered_user 
%% 				| timeout_deregistration | user_deregistration
%% 				| timeout_deregistration_store_server_name
%% 				| user_deregistration_store_server_name
%% 				| administrative_deregistration
%% 				| authentication_failure
%% 				| authentication_timeout
%% 				| deregistration_too_much_data.
%% 	Type of update that the S-CSCF requests in the HSS.

%% @type scscf_registration_notification() =
%% 	#scscf_registration_notification{
%% 		publicUserID = publicUserID(),
%% 		sCSCFName = sipURL(),
%% 		privateUserID = privateUserID(),
%% 		serverAssignmentType = serverAssignmentType(),
%% 		userDataAlreadyAvailable = user_data_not_available
%% 				| user_data_already_available,
%% 		routingInformation = string()}.
%%
%%		S-CSCF registration/deregistration notification.
%%
-record(scscf_registration_notification,
		{publicUserID,
		sCSCFName,
		privateUserID,
		serverAssignmentType,
		userDataAlreadyAvailable,
		routingInformation}).

%% @type chargingInformation() = #chargingInformation{
%%			primary_event_charging_function_name = diameterURI(),
%%			secondary_event_charging_function_name = diameterURI(),
%%			primary_charging_collection_function_name = diameterURI(),
%%			secondary_charging_collection_function_name = diameterURI()}.
%%
%% 	RFC 3588 Addresses of the charging functions.
%% 	(e.g. "aaa://tabulator.acme.net")
%%
-record(chargingInformation,
		{primary_event_charging_function_name,
		secondary_event_charging_function_name,
		primary_charging_collection_function_name,
		secondary_charging_collection_function_name}).

%% @type scscf_registration_notification_response() =
%% 	#scscf_registration_notification_response{
%% 		privateUserID = privateUserID(),
%% 		resultCode = resultCode(),
%% 		experimentalResult = experimentalResult(),
%% 		userProfile = userProfile(),
%% 		chargingInformation = chargingInformation(),
%% 		associatedPrivateIDs = [privateUserID()]}.
%%
%%		S-CSCF registration/deregistration notification response.
%%
-record(scscf_registration_notification_response,
		{privateUserID,
		resultCode,
		experimentalResult,
		chargingInformation,
		userProfile,
		associatedPrivateIDs}).

%% @type reasonDeregistration() = #reasonDeregistration{
%% 		reasonCode = permanent_termination | new_server_assigned
%% 				| server_change | remove_scscf,
%% 		reasonInfo = string()}.
%% 	The HSS shall send to the S-CSCF a reason for the de-registration.
%% 	The <tt>reasonCode</tt> is intended for the S-CSCF while the
%% 	<tt>reasonInfo</tt> is intended to be forwarded to the user.

%% @type network_initiated_deregistration_by_hss() =
%% 	#scscf_registration_notification{
%% 		publicUserID = publicUserID(),
%% 		privateUserID = privateUserID(),
%% 		reasonDeregistration = reasonDeregistration(),
%% 		routingInformation = string()}.
%%
%%		Network initiated de-registration by the HSS, administrative
%% 	procedure.
%%
-record(network_initiated_deregistration_by_hss,
		{publicUserID,
		privateUserID,
		reasonDeregistration,
		routingInformation,
		associatedPrivateIDs}).

%% @type network_initiated_deregistration_by_hss_response() =
%% 	#network_initiated_deregistration_by_hss_response{
%% 		resultCode = resultCode(),
%% 		experimentalResult = experimentalResult(),
%% 		associatedPrivateIDs = [privateUserID()]}.
%%
%%		Network initiated de-registration by the HSS response.
%%
-record(network_initiated_deregistration_by_hss_response,
		{resultCode,
		experimentalResult,
		associatedPrivateIDs}).

%% @type user_location_query() =
%% 	#user_location_query{
%% 		publicUserID = publicUserID(),
%% 		routingInformation = string(),
%% 		originatingRequest = boolean()}.
%%
%%		User location query.
%%
-record(user_location_query,
		{publicUserID,
		routingInformation,
		originatingRequest}).

%% @type user_location_response() =
%% 	#user_location_response{
%% 		resultCode = resultCode(),
%% 		experimentalResult = experimentalResult(),
%% 		sCSCFCapabilities = serverCapabilities(),
%% 		sCSCFName = sipURL()}.
%%
%% 	User location response.
%%
-record(user_location_response,
		{resultCode,
		experimentalResult,
		sCSCFName,
		sCSCFCapabilities}).

%% @type userProfile() = binary().
%%		This information element contains the user profile in XML format.

%% @type hss_initiated_update_of_user_profile() =
%% 	#hss_initiated_update_of_user_profile{
%% 		privateUserID = privateUserID(),
%% 		chargingInformation = chargingInformation(),
%% 		userProfile = userProfile(),
%% 		routingInformation = string()}.
%%
%%		HSS initiated update of user profile.
%%
-record(hss_initiated_update_of_user_profile,
		{privateUserID,
		chargingInformation,
		userProfile,
		routingInformation}).

%% @type user_profile_update_response() =
%% 	#user_profile_update_response{
%% 		resultCode = resultCode(),
%% 		experimentalResult = experimentalResult()}.
%%
%%		User profile update response.
%%
-record(user_profile_update_response,
		{resultCode,
		experimentalResult}).

%% @type authenticationData() =
%% 	#authenticationData{
%% 		itemNumber = integer(),
%% 		authenticationScheme = string(),
%% 		authenticationInformation = string(),
%% 		authorizationInformation = string(),
%% 		confidentialityKey = string(),
%% 		integrityKey = string()}.
%%
%% 	Information to support the authentication between 
%% 	the end user and the home IMS network.
%%
-record(authenticationData,
		{itemNumber,
		authenticationScheme = "Digest-AKAv1-MD5",
		authenticationInformation,
		authorizationInformation,
		confidentialityKey,
		integrityKey}).

%% @type authentication_request() =
%% 	#authentication_request{
%% 		publicUserID = publicUserID(),
%% 		privateUserID = privateUserID(),
%% 		numberAuthenticationItems = integer(),
%% 		authenticationData = authenticationData(),
%% 		sCSCFName = sipURL(),
%% 		routingInformation = string()}.
%%
%%		Authentication request.
%%
-record(authentication_request,
		{publicUserID,
		privateUserID,
		numberAuthenticationItems,
		authenticationData,
		sCSCFName,
		routingInformation}).

%% @type authentication_request_response() =
%% 	#authentication_request_response{
%% 		resultCode = resultCode(),
%% 		experimentalResult = experimentalResult(),
%% 		publicUserID = publicUserID(),
%% 		privateUserID = privateUserID(),
%% 		numberAuthenticationItems = integer(),
%% 		authenticationData = [authenticationData()]}.
%%
%%		Authentication request response.
%%
-record(authentication_request_response,
		{resultCode,
		experimentalResult,
		publicUserID,
		privateUserID,
		numberAuthenticationItems,
		authenticationData}).

