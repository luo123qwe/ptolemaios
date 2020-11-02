%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 一个帧战斗框架
%%%
%%% 效果 = 这次操作要做什么
%%%
%%% 技能 = N个效果
%%%
%%% 战斗单位 = 执行效果的单位
%%%
%%% 每帧执行一个列表: [#dynames_frame{}]
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dynames).
-author("dominic").

-behaviour(exia).

-include("dynames.hrl").
-include("util.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

init(_Args) ->
    {ok, #dynames{}}.

handle_call(Request, From, State) ->
    ?LOG_ERROR("unknow call from ~p~n~p", [Request, From]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOG_ERROR("unknow cast~n~p", [Request]),
    {noreply, State}.

handle_info(loop, State) ->
    State1 = execute_frame(State),
    {noreply, State1#dynames{frame = State#dynames.frame + 1}};
handle_info(Info, State) ->
    ?LOG_ERROR("unknow info~n~p", [Info]),
    {noreply, State}.

%% 执行一帧
execute_frame(#dynames{frame = Frame, frame_list = FrameList} = State) ->
    case FrameList of
        [#dynames_effect{frame = Frame} = H | T] ->
            State1 = State#dynames{frame_list = T},
            %% 释放对象
            State2 = execute_effect(H, State1),
            %% 执行效果
            execute_frame(State2);
        _ ->
            ok
    end.

%% 执行一个效果
execute_effect(#dynames_effect{user = User} = Effect, State) ->
    Unit = find_unit(User),
    case can_effect() of
        true ->
            %% 过滤目标
            TargetList = filter_target(Unit, Effect, State),
            %% 执行效果
            execute_effect(TargetList, Unit, Effect, State);
        false ->
            State
    end.