%%%----------------------------------------------------------------------
%%% File    : mod_muc_log.erl
%%% Author  : Badlop@process-one.net
%%% Purpose : MUC room logging
%%% Created : 12 Mar 2006 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_log).

-behaviour(gen_mod).

-export([start/2,
    stop/1,
    check_access_log/2,
    add_to_log/5]).

-include("ejabberd.hrl").
-include_lib("exmpp/include/exmpp.hrl").

-record(message, {
    host,
    room,
    nick,
    action,
    data,
    timestamp}).

start(_Host, _Opts) ->
  ok.

stop(_Host) ->
  ok.

add_to_log(_Host, Type, Data, Room, Opts) ->
  ?DEBUG("Calling add_to_log", []),
  case catch add_to_log2(Type, Data, Room, Opts) of
    {'EXIT', Reason} ->
      ?ERROR_MSG("~p", [Reason]);
    _ ->
      ok
  end.

add_to_log2(text, {Nick, Packet}, Room, Opts) ->
  case {exmpp_xml:get_element(Packet, 'subject'),
      exmpp_xml:get_element(Packet, 'body')} of
    {'undefined', 'undefined'} ->
      ok;
    {'undefined', SubEl} ->
      Message = {body, exmpp_xml:get_cdata_as_list(SubEl)},
      add_message_to_log(binary_to_list(Nick), Message, Room, Opts);
    {SubEl, _} ->
      Message = {subject, exmpp_xml:get_cdata_as_list(SubEl)},
      add_message_to_log(binary_to_list(Nick), Message, Room, Opts)
  end;

add_to_log2(roomconfig_change, _Occupants, Room, Opts) ->
  add_message_to_log("", roomconfig_change, Room, Opts);

add_to_log2(roomconfig_change_enabledlogging, Occupants, Room, Opts) ->
  add_message_to_log("", {roomconfig_change, Occupants}, Room, Opts);

add_to_log2(room_existence, NewStatus, Room, Opts) ->
  add_message_to_log("", {room_existence, NewStatus}, Room, Opts);

add_to_log2(nickchange, {OldNick, NewNick}, Room, Opts) ->
  add_message_to_log(binary_to_list(NewNick), {nickchange, binary_to_list(OldNick)}, Room, Opts);

add_to_log2(join, Nick, Room, Opts) ->
  add_message_to_log(binary_to_list(Nick), join, Room, Opts);

add_to_log2(leave, {Nick, Reason}, Room, Opts) ->
  case binary_to_list(Reason) of
    "" -> add_message_to_log(binary_to_list(Nick), leave, Room, Opts);
    R -> add_message_to_log(binary_to_list(Nick), {leave, R}, Room, Opts)
  end;

add_to_log2(kickban, {Nick, Reason, Code}, Room, Opts) ->
  add_message_to_log(binary_to_list(Nick), {kickban, Code, binary_to_list(Reason)}, Room, Opts).


add_message_to_log(Nick, Message, RoomJID, _Opts) ->
  {Action, Data} = case Message of
    join ->
      {join, ""};
    leave ->
      {leave, ""};
    {leave, Reason} ->
      {leave, Reason};
    {kickban, "301", ""} ->
      {ban, ""};
    {kickban, "301", Reason} ->
      {ban, Reason};
    {kickban, "307", ""} ->
      {kick, ""};
    {kickban, "307", Reason} ->
      {kick, Reason};
    {kickban, "321", ""} ->
      {kick, "affiliation change"};
    {kickban, "322", ""} ->
      {kick, "members-only"};
    {kickban, "332", ""} ->
      {kick, "shutdown"};
    {nickchange, OldNick} ->
      {nickchange, OldNick};
    {subject, T} ->
      {subject, T};
    {body, T} ->
      {body, T};
    {room_existence, RoomExistence} ->
      {room_existence, RoomExistence}
  end,
  MessageToLog = #message{host = exmpp_jid:domain_as_list(RoomJID),
    room = exmpp_jid:node_as_list(RoomJID),
    nick = Nick,
    action = Action,
    data = Data,
    timestamp = now()
  },

  ?DEBUG("Writing to storage: ~p ~n", [MessageToLog]),
  ok.

%% TODO: Factor out calling code.
check_access_log(_, _) ->
  allow.
