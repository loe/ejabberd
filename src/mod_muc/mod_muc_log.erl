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
-author('badlop@process-one.net').

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2,
    start/2,
    stop/1,
    add_to_log/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_muc_room.hrl").

%% Copied from mod_muc/mod_muc.erl
-record(muc_online_room, {name_host, pid}).

-define(T(Text), translate:translate(Lang, Text)).
-define(PROCNAME, ejabberd_mod_muc_log).
-record(room, {jid, title, subject, subject_author, config}).


-record(logstate, {host}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
  Proc = get_proc_name(Host),
  gen_server:start_link(Proc, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
  Proc = get_proc_name(Host),
  ChildSpec =
  {Proc,
    {?MODULE, start_link, [Host, Opts]},
    temporary,
    1000,
    worker,
    [?MODULE]},
  supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
  Proc = get_proc_name(Host),
  gen_server:call(Proc, stop),
  supervisor:delete_child(ejabberd_sup, Proc).

add_to_log(Host, Type, Data, Room, Opts) ->
  gen_server:cast(get_proc_name(Host),
    {add_to_log, Type, Data, Room, Opts}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Opts]) ->
  {ok, #logstate{host = Host}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add_to_log, Type, Data, Room, Opts}, State) ->
  case catch add_to_log2(Type, Data, Room, Opts, State) of
    {'EXIT', Reason} ->
      ?ERROR_MSG("~p", [Reason]);
    _ ->
      ok
  end,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
add_to_log2(text, {Nick, Packet}, Room, Opts, State) ->
  case {exmpp_xml:get_element(Packet, 'subject'),
      exmpp_xml:get_element(Packet, 'body')} of
    {'undefined', 'undefined'} ->
      ok;
    {'undefined', SubEl} ->
      Message = {body, exmpp_xml:get_cdata_as_list(SubEl)},
      add_message_to_log(binary_to_list(Nick), Message, Room, Opts, State);
    {SubEl, _} ->
      Message = {subject, exmpp_xml:get_cdata_as_list(SubEl)},
      add_message_to_log(binary_to_list(Nick), Message, Room, Opts, State)
  end;

add_to_log2(roomconfig_change, _Occupants, Room, Opts, State) ->
  add_message_to_log("", roomconfig_change, Room, Opts, State);

add_to_log2(roomconfig_change_enabledlogging, Occupants, Room, Opts, State) ->
  add_message_to_log("", {roomconfig_change, Occupants}, Room, Opts, State);

add_to_log2(room_existence, NewStatus, Room, Opts, State) ->
  add_message_to_log("", {room_existence, NewStatus}, Room, Opts, State);

add_to_log2(nickchange, {OldNick, NewNick}, Room, Opts, State) ->
  add_message_to_log(binary_to_list(NewNick), {nickchange, binary_to_list(OldNick)}, Room, Opts, State);

add_to_log2(join, Nick, Room, Opts, State) ->
  add_message_to_log(binary_to_list(Nick), join, Room, Opts, State);

add_to_log2(leave, {Nick, Reason}, Room, Opts, State) ->
  case binary_to_list(Reason) of
    "" -> add_message_to_log(binary_to_list(Nick), leave, Room, Opts, State);
    R -> add_message_to_log(binary_to_list(Nick), {leave, R}, Room, Opts, State)
  end;

add_to_log2(kickban, {Nick, Reason, Code}, Room, Opts, State) ->
  add_message_to_log(binary_to_list(Nick), {kickban, Code, binary_to_list(Reason)}, Room, Opts, State).


%%----------------------------------------------------------------------
%% Core

add_message_to_log(Nick1, Message, RoomJID, Opts, State) ->
  Room = get_room_info(RoomJID, Opts),
  Now = now(),

  %% Build message
  Text = case Message of
    join ->
      io_lib:format("~s ~s", [Nick, ?T("joins the room")]);
    leave ->
      io_lib:format("~s ~s", [Nick, ?T("leaves the room")]);
    {leave, Reason} ->
      io_lib:format("~s ~s: ~s", [Nick, ?T("leaves the room"), Reason, NoFollow]);
    {kickban, "301", ""} ->
      io_lib:format("~s ~s", [Nick, ?T("has been banned")]);
    {kickban, "301", Reason} ->
      io_lib:format("~s ~s: ~s", [Nick, ?T("has been banned"), Reason]);
    {kickban, "307", ""} ->
      io_lib:format("~s ~s", [Nick, ?T("has been kicked")]);
    {kickban, "307", Reason} ->
      io_lib:format("~s ~s: ~s", [Nick, ?T("has been kicked"), Reason]);
    {kickban, "321", ""} ->
      io_lib:format("~s ~s", [Nick, ?T("has been kicked because of an affiliation change")]);
    {kickban, "322", ""} ->
      io_lib:format("~s ~s", [Nick, ?T("has been kicked because the room has been changed to members-only")]);
    {kickban, "332", ""} ->
      io_lib:format("~s ~s", [Nick, ?T("has been kicked because of a system shutdown")]);
    {nickchange, OldNick} ->
      io_lib:format("~s ~s ~s", OldNick, ?T("is now known as"), Nick]);
  {subject, T} ->
    io_lib:format("~s~s~s",
      [Nick, ?T(" has set the subject to: "), T]);
  {body, T} ->
    case {re:run(T, "^/me\s", [{capture, none}]), Nick} of
      {_, ""} ->
        io_lib:format("~s",[T]);
      {match, _} ->
        io_lib:format("~s ~s", [Nick, string:substr(T, 5)]);
      {nomatch, _} ->
        io_lib:format("~s ~s", [Nick2, T])
    end;
  {room_existence, RoomNewExistence} ->
    io_lib:format("~s", [get_room_existence_string(RoomNewExistence)])
end,

%% Write message

ok.


%%----------------------------------------------------------------------
%% Utilities

get_room_existence_string(created) -> ?T("Room is created");
get_room_existence_string(destroyed) -> ?T("Room is destroyed");
get_room_existence_string(started) -> ?T("Room is started");
get_room_existence_string(stopped) -> ?T("Room is stopped").

get_room_name(RoomJID) ->
  JID = exmpp_jid:parse(RoomJID),
  exmpp_jid:node_as_list(JID).

get_room_info(RoomJID, Opts) ->
  Title =
  case lists:keysearch(title, 1, Opts) of
    {value, {_, T}} -> T;
    false -> ""
  end,
  Subject =
  case lists:keysearch(subject, 1, Opts) of
    {value, {_, S}} -> S;
    false -> ""
  end,
  SubjectAuthor =
  case lists:keysearch(subject_author, 1, Opts) of
    {value, {_, SA}} -> SA;
    false -> ""
  end,
  #room{jid = exmpp_jid:to_list(RoomJID),
    title = Title,
    subject = Subject,
    subject_author = SubjectAuthor,
    config = Opts
  }.


get_roomconfig_text(title) -> "Room title";
get_roomconfig_text(persistent) -> "Make room persistent";
get_roomconfig_text(public) -> "Make room public searchable";
get_roomconfig_text(public_list) -> "Make participants list public";
get_roomconfig_text(password_protected) -> "Make room password protected";
get_roomconfig_text(password) -> "Password";
get_roomconfig_text(anonymous) -> "This room is not anonymous";
get_roomconfig_text(members_only) -> "Make room members-only";
get_roomconfig_text(moderated) -> "Make room moderated";
get_roomconfig_text(members_by_default) -> "Default users as participants";
get_roomconfig_text(allow_change_subj) -> "Allow users to change the subject";
get_roomconfig_text(allow_private_messages) -> "Allow users to send private messages";
get_roomconfig_text(allow_query_users) -> "Allow users to query other users";
get_roomconfig_text(allow_user_invites) -> "Allow users to send invites";
get_roomconfig_text(logging) ->  "Enable logging";
get_roomconfig_text(allow_visitor_nickchange) ->  "Allow visitors to change nickname";
get_roomconfig_text(allow_visitor_status) ->  "Allow visitors to send status text in presence updates";
get_roomconfig_text(captcha_protected) ->  "Make room captcha protected";
get_roomconfig_text(description) ->  "Room description";
%% get_roomconfig_text(subject) ->  "Subject";
%% get_roomconfig_text(subject_author) ->  "Subject author";
get_roomconfig_text(max_users) -> "Maximum Number of Occupants";
get_roomconfig_text(_) -> undefined.

get_proc_name(Host) ->
  {global, gen_mod:get_module_proc(Host, ?PROCNAME)}.
