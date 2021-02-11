%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 玩家进程
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(player_svr).
-author("dominic").

-behaviour(plm_svr).

-include("plm_lib.hrl").
-include("game.hrl").
-include("game_11_pb.hrl").
-include("player.hrl").
-include("gateway.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([start_link/1, get_pid/1]).

start_link([Id, Gateway]) ->
    plm_svr:start_link({local, name(Id)}, ?MODULE, [Id, Gateway], []).

%% @doc 根据玩家id获取pid
-spec get_pid(non_neg_integer()) -> undefined|pid().
get_pid(Id) ->
    whereis(name(Id)).

name(Id) ->
    list_to_atom("player_" ++ integer_to_list(Id)).

init([Id, Gateway]) ->
    ?LOG_ERROR("~w ~w", [Id, Gateway]),
    plm_sql:load(player, [Id]),
    {ok, #player_state{id = Id, gateway = Gateway}}.

handle_call(?MSG13_GATEWAY_RECONNECT1(Gateway), _From, State) ->
    {reply, ok, State#player_state{gateway = Gateway}};

handle_call(Request, From, State) ->
    ?LOG_ERROR("~w ~w", [Request, From]),
    {reply, error, State}.

handle_cast(?MSG13_GATEWAY_PROTO1(Msg), #player_state{gateway = Gateway} = State) ->
    Rollback = plm_svr:hold(State),
    try proto_mapping:route(Msg, State) of
        #player_state{} = State1 ->
            {noreply, State1};
        UnKnow ->%% ????
            ?LOG_ERROR("unknown return ~w", [UnKnow]),
            Proto = proto_mapping:proto(Msg),
            plm_svr:cast(Gateway, ?MSG12_SEND_MSG1(#game_s_error{code = ?M11_EC_ERROR, proto = Proto})),
            State1 = plm_svr:rollback(Rollback),
            {noreply, State1}
    catch
        throw:?M11_EC1(ErrorCode) ->
            Proto = proto_mapping:proto(Msg),
            plm_svr:cast(Gateway, ?MSG12_SEND_MSG1(#game_s_error{code = ErrorCode, proto = Proto})),
            State1 = plm_svr:rollback(Rollback),
            {noreply, State1};
        C:E:S ->
            erlang:raise(C, E, S)
    end;

handle_cast(?MSG13_GATEWAY_DISCONNECT, State) ->
    ?LOG_NOTICE("gateway disconnect"),
    {stop, normal, State};
handle_cast(Request, State) ->
    ?LOG_ERROR("~w", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG_ERROR("~w", [Info]),
    {noreply, State}.