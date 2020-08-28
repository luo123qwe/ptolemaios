%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(enif_example).
-author("dominic").

%% API
-export([past_ptr/1]).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      BeamDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(BeamDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    Threads = erlang:system_info(schedulers),
    ok = erlang:load_nif(filename:join(PrivDir, ?MODULE_STRING), Threads).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

past_ptr(_Int) ->
    ?NOT_LOADED.