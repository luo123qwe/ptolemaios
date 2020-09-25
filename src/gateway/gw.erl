%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc gateway api
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gw).
-author("dominic").

-include("util.hrl").
-include("gw.hrl").

%% API
-export([
    pack/1,
    unpack/1
]).

-type proto() :: tuple().

%% @doc 打包一条Protobuff信息
-spec pack(proto()) -> any().
pack(Msg) ->
    case proto_mapping:encode(Msg) of
        Bin when is_binary(Bin) ->
            Proto = proto_mapping:proto(Msg),
            Len = byte_size(Bin),
            <<Len:?GATEWAY_LEN, Proto:?GATEWAY_PROTO, Bin/binary>>;
        Error ->
            throw(Error)
    end.

%% @doc 解包一条Protobuff信息, 成功返回{ok, 协议, 剩余数据}或者返回more(需要更多数据)
-spec unpack(binary()) -> more|{ok, Proto :: proto(), Remain :: binary()}|{error, Error :: term()}.
unpack(Bin) ->
    case Bin of
        <<Len:?GATEWAY_LEN, Proto:?GATEWAY_PROTO, ProtoBin:Len/binary, Remain/binary>> ->
            try
                case proto_mapping:decode(Proto, ProtoBin) of
                    {error, Error} ->
                        ?LOG_ERROR("~w", [Error]),
                        {error, Error};
                    Msg ->
                        {ok, Msg, Remain}
                end
            catch
                C:E:S ->
                    ?LOG_ERROR("~w, ~w~n~w", [C, E, S]),
                    {error, E}
            end;
        Bin ->
            more
    end.