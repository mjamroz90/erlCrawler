%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: RemoteManager_propList
%% Source: /home/michal/nauka/erlang/erlCrawler/remote_crawl_manager/src/idl/RemoteManager.idl
%% IC vsn: 4.2.30
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('RemoteManager_propList').
-ic_compiled("4_2_30").


-include("RemoteManager.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_sequence,{tk_array,{tk_string,0},2},0}.

%% returns id
id() -> "IDL:RemoteManager/propList:1.0".

%% returns name
name() -> "RemoteManager_propList".



