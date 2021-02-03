%%% @private
%%%-------------------------------------------------------------------
%% @doc ptolemaios top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ptolemaios_sup).

-behaviour(supervisor).

-include("ptolemaios_lib.hrl").

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    
    plm_ll:init_ets(),
    
    proto_mapping:load(),
    
    %% 子进程
    ChildSpecs = virture:get_sup_spec() ++
        [
            #{id => gateway_sup, start => {gateway_sup, start_link, []}, type => supervisor},
            #{id => player_sup, start => {player_sup, start_link, []}, type => supervisor},
            #{id => ds_sup, start => {ds_sup, start_link, []}, type => supervisor}
        ],
    
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions