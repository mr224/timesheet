-module(st_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

% %% Helper macro for declaring children of supervisor
% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_link(Arg) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Arg]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

% init([]) ->
%     {ok, { {one_for_one, 5, 10},
% 		 [
% 			?CHILD(st_serv,worker)
% 			]} }.


init([Children]) ->
  lager:debug("Children: ~p", [Children]),
  {ok, { {one_for_one, 5, 10},
    Children} }.


% init([]) ->
%     {ok, { {one_for_one, 5, 10},
% 		 []} }.
