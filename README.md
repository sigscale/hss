![logo](doc/3Gdb.50.png?raw=true)

# The 3Gdb Home Subscriber Server (HSS) #

This application implements the **HSS** network element within an IP Multimedia Subsystem (**IMS**) Core Network (CN).

## IP Multimedia Subsystem (IMS) ##

The IMS is defined by the [Third Generation Partnership Project (3GPP)](http://www.3gpp.org) as part of it's architecture for an *all-IP* 3G mobile phone network.  [IETF](http://www.ietf.org) protocols were chosen wherever possible, in particular [SIP](http://www.ietf.org/rfc/rfc3261.txt) and [DIAMETER](http://www.ietf.org/rfc/rfc3588.txt).  An IMS Core Network (CN) can be seen mainly as a collection of SIP proxies, registrars, servers and media gateways along with the Home Subscriber Server (HSS).  While the IMS is based on SIP it is done so in a highly structured way which may be quite different from practices used in other contexts.  It should be remembered that the IMS was designed by and for *service providers* and as such it has a centralized control model.

[3GPP TS 23.002 Network Architecture](http://www.3gpp.org/ftp/Specs/html-info/23002.htm) describes an **HSS**:
> The HSS is the master database for a given user. It is the entity containing the subscription-related information to support the network entities actually handling calls/sessions.
>     A Home Network may contain one or several HSSs: it depends on the number of mobile subscribers, on the capacity of the equipment and on the organisation of the network.
> As an example, the HSS provides support to the call control servers in order to complete the routing/roaming procedures by solving authentication, authorisation, naming/addressing resolution, location dependencies, etc."

The HSS *stores* the following types of information:
	- subscription, identification and numbering
	- registration
	- authentication and ciphering
	- service profile

The HSS *generates* security information for:
	- mutual authentication
	- communication integrity check
	- ciphering.

## Network Architecture ##

Within an IMS CN the Call Session Control Functions (CSCF) provide the SIP proxy and registrar functionality. There are three distinct types; Proxy-CSCF, Interrogating-CSCF and Serving-CSCF. All communications with IMS clients transit a P-CSCF, possibly in a visited IMS. An example signaling flow can be seen in [Figure-1.1](doc/registration-path.png) where the P-CSCF forwards an initial `REGISTER` to an I-CSCF in the home IMS CN. The I-CSCF iqueries the HSS to help select an appropriate S-CSCF to forward the request to. The S-CSCF acts as the registrar sending an authentication challenge to the user with authentication vectors supplied by the HSS.

Figure-1.1 ![figure-1.1](doc/registration-path.png?raw=true)

Once the user has successfully authenticated the S-CSCF will complete the registration and notify the HSS.  While it is the S-CSCF which acts as the SIP registrar the HSS must track which S-CSCF a user is registered at.  An HSS may be associated with many S-CSCFs.  An IMS CN may contain more than one HSS.

## Implementation ##

This project implements the (IMS) database schema described in [23.008 Subscriber Data](http://www.3gpp.org/ftp/Specs/html-info/23008.htm), the procedures in [29.228 Signaling](http://www.3gpp.org/ftp/Specs/html-info/29228.htm) and the algorithms in [35.206 MILENAGE](http://www.3gpp.org/ftp/Specs/html-info/35206.htm).  It is based upon Release 7.

The system was developed entirely in [Erlang/OTP](http://www.erlang.org) using the [Mnesia](http://www.erlang.org/doc/apps/mnesia/) distributed database application.  The procedures are executed in mnesia transaction contexts and many servers may be run on many nodes for a very high capacity system.  The [MILENAGE](http://www.3gpp.org/ftp/Specs/html-info/35206.htm) algorithms utilize the [crypto](http://www.erlang.org/doc/apps/crypto/) application.  

A number of test suites are included which use the [common_test](http://www.erlang.org/doc/apps/common_test/) application.  All of the conformance test data in [35.208](http://www.3gpp.org/ftp/Specs/html-info/35208.htm) is included in one test suite.  The [xmerl](http://www.erlang.org/doc/apps/xmerl/) application is used in test cases to validate the XML user profile downloaded during registration.

## Database Schema ##

The schema in [Figure 1.2](doc/permanent-subscriber-data.png) shows the organization of the permanent subscriber data. The HSS maintains the data related to **_Subscribers_** of IMS services.  Each subscription may include a number of **_Users_** of the subscribed services.  Users are identified within the IMS by a **_Private User Identity_** in the ([NAI](http://www.ietf.org/rfc/rfc4282.txt)) form "user@realm".  All communications with a user involves a **_Public User Identity_** in the form of either a [SIP URI](http://www.ietf.org/rfc/rfc3261) or a [TEL URL](http://www.ietf.org/rfc/rfc2806).  Users can, and usually are, associated with multiple Public User Identities which may also be shared among users of a common subscription.  The subscribed services are associated with *Public User Identities*.  *Subscriptions* are represented by records in the `subscriber` table, *Users* in the `user` table, *Public User Identities* in the `address` table and *Service Profiles* is the `profile` table.

Figure 1.2 ![figure-1.2](doc/permanent-subscriber-data.png?raw=true)


Registrations are temporary subscriber data describing the registration status of *Public User Identities* including the name of the S-CSCF where the registration is held. A *Public User Identity* may be registered by several users at the same time and so multiple entries with registered state may appear in the table for the same identity. An S-CSCF may keep a user profile stored without any registrations in some cases so one entry with unregistered state may remain in the table. An unregistered *Public User Identity* will generally not have a record in the table. One or more entries with notregistered state may exist while authentication is pending. The schema in [Figure 1.3](doc/temporary-subscriber-data.png) shows the organization of the temporary subscriber data.

Figure 1.3 ![figure-1.3](doc/temporary-subscriber-data.png?raw=true)


## Provisioning ##

In a conventional operator network a large number of *Universal Integrated Circuit Cards* (**UICC**) would be manufactured and put into the the distribution channel. The UICCs may contain an *IP Multimedia Services Identity Module* (**ISIM**) application or a *Universal Subscriber Identity Module* (**USIM**) application or both. The manufacturer would provide a file listing the ICCIDs and the corresponding authentication keys (**K** and **OPc**) as well as IMS *Private User Identity* (**IMPI**) and IMS *Public User Identity* (**IMPU**) in the case of an ISIM, or *International Mobile Subscriber Identity* (**IMSI**) in the case of a USIM. This file is used to manage the card inventory.

Figure 1.4 ![figure-1.4](doc/card-flow.png?raw=true)


To provision a new subscriber a card is taken from inventory and it's ICCID is used to determine the identities and keys stored on the UICC. Appropriate *Public User Identities* are assigned and a *Service Profile* is selected for each one. A new `subscriber` and `user` table entry are created along with `address` table entries for each *Public User Identity*.

Figure 1.5 ![figure-1.5](doc/provisioning.png?raw=true)


It is mandatory that at least one *Public User Identity* record is contained in the ISIM application however it is usually not practical to assign appropriate *Public User Identities* at the time of manufacture. Assigning *Public User Identities* is typically done after a card is drawn from inventory and a subscriber identifies which city a telephone number should be assigned from or what name to use in a SIP URI. This could be managed by administratively writing to the card before providing it to the subscriber but it is more convenient to manage address assignment entirely within the HSS. This can be accomplished by manufacturing the UICCs with a default IMPU, usually formed from the IMPI, and provisioning it as *barred*. When the UICC is inserted in the *User Equipment* (**UE**) it will read the ISIM and attempt to register the default IMPU. For example:

```html
   REGISTER sip:323.248.imsi.3gppnetwork.org SIP/2.0
   From: <sip:2483235551234@323.248.imsi.3gppnetwork.org>;tag=4fa3
   To: <sip:2483235551234@323.248.imsi.3gppnetwork.org>
   Authorization: Digest username="2483235551234@323.248.imsi.3gppnetwork.org",
                  realm="323.248.imsi.3gppnetwork.org", nonce="",
                  uri="sip:323.248.imsi.3gppnetwork.org", response=""
```

The same situation arises when the UICC contains only a USIM application. The IMS user equipment will read the USIM application and form a *Temporary Private User Identity* and *Temporary Public User Identity* derived from the IMSI. The entire IMSI is used as the username part of the *Private User Identity* and the realm is constructed from the *Mobile Network Code* (**MNC**) and *Mobile Country Code* (**MCC**) and follows the form `MNC.MCC.imsi.3gppnetwork.org`. The *Public User Identity* is formed by prepending "`sip:`" to the *Private User Identity*.

In the final response to a successful registration the `P-Associated-URI` header will contain the list of all non-barred *Public User Identities* associated with the registered identity. The UE will understand that it may not use the default temporary identity for communications and will recognize the assigned identities.

```html
   SIP/2.0 200 OK
   From: <sip:2483235551234@323.248.imsi.3gppnetwork.org>;tag=4fa3
   To: <sip:2483235551234@323.248.imsi.3gppnetwork.org>;tag=409sp3
   P-Associated-URI: <sip:alex.harvey@vibrania.com>,
                     <sip:+16475551234@acme.net;user=phone>
```

### Security #)#

Security in the IMS is built on UMTS *Authentication and Key Agreement* ([AKA](http://www.3gpp.org/ftp/Specs/html-info/33102.htm).  A long term secret (**K**) is shared between the USIM/ISIM and the HSS only.  The AKA algorithms are executed on the UICC which is tamper resistant so even physical access to it is unlikely to expose K.  AKA accomplishes mutual authentication, the home network authenticates the USIM/ISIM which in turn authenticates the network.  It also establishes a pair of cipher and integrity keys which can be used to secure subsequent communications (i.e. **IPsec**).

AKA defines seven security functions; `f1`, `f1*`, `f2`, `f3`, `f4`, `f5` and `f5*` used for authentication and key generation. The operation of these functions is to be specified by each operator and as such is not fully standardised. The algorithms implemented here follow the examples produced on request from 3GPP by ETSI SAGE Task Force ([MILENAGE](http://www.3gpp.org/ftp/Specs/html-info/35206.htm)) and are based on the block cipher *Rinjindael* now known as *Advanced Encryption Standard* (**AES**). 

### User Profile ###

In response to a registration notification from an S-CSCF the HSS will download the user's *Service Profile* in the form of an XML document.  The document is formed on the fly from permanent subscriber data stored in the HSS.  An example is shown here:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<IMSSubscription xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="CxDataType.xsd">
     <PrivateID>2483235551234@323.248.imsi.3gppnetwork.org</PrivateID>
     <ServiceProfile>
          <PublicIdentity>
               <BarringIndication>1</BarringIndication>
               <Identity>sip:2483235551234@323.248.imsi.3gppnetwork.org</Identity>
          </PublicIdentity>
          <PublicIdentity>
               <Identity>sip:alex@vibrania.com</Identity>
          </PublicIdentity>
          <PublicIdentity>
               <Identity>tel:+16475551234</Identity>
          </PublicIdentity>
          <InitialFilterCriteria>
               <Priority>0</Priority>
               <ApplicationServer>
                    <ServerName>sip:as.acme.net</ServerName>
               </ApplicationServer>
          </InitialFilterCriteria>
     </ServiceProfile>
</IMSSubscription>
```

The S-CSCF uses this profile to determine how to involve *Application Servers* in the routing of SIP messages.

### Limitations ###

The current implementation does not include a DIAMETER protocol stack implementation.

### TODO ###

	- Implement alternative algorithms.
	- Implement *HSS initiated procedures*.
	- Implement Yaws based administration.
	- Implement *Public Service Identifiers*.
	- Improve visited network authorization.
	- Implement `Sh` reference point interface to *Application Servers*.

----
References:

[Network Architecture (3GPP TS  23.002)](http://www.3gpp.org/ftp/Specs/html-info/23002.htm)
[Organization of Subscriber Data (3GPP TS 23.008)](http://www.3gpp.org/ftp/Specs/html-info/23008.htm)
[Security Architecture (3GPP TS  33.102)](http://www.3gpp.org/ftp/Specs/html-info/33102.htm)
[IP Multimedia Subsystem (IMS); Stage 2 (3GPP TS 23.228)](http://www.3gpp.org/ftp/Specs/html-info/23228.htm)
[IP Multimedia (IM) Subsystem Cx and Dx Interfaces; Signalling flows and message contents (3GPP TS  29.228)](http://www.3gpp.org/ftp/Specs/html-info/29228.htm)
[Specification of the MILENAGE algorithm set: An example algorithm set for the 3GPP authentication and key generation functions f1, f1*, f2, f3, f4, f5 and f5*; Document 2: Algorithm specification (3GPP TS 35.206)](http://www.3gpp.org/ftp/Specs/html-info/35206.htm)
[Specification of the MILENAGE algorithm set: An example algorithm set for the 3GPP authentication and key generation functions f1, f1*, f2, f3, f4, f5 and f5*; Document 4: Design conformance test data (3GPP TS 35.208)](http://www.3gpp.org/ftp/Specs/html-info/35208.htm)
![3gdb logo](doc/3Gdb.50.png?raw=true) ![erlang logo](http://www.erlang.org/images/logo.gif?raw=true)

