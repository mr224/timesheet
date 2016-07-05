-module(st_data_util).
-include("../include/st_types.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([generate_key/0]).
-export([now/0,convert_datetime/1]).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

generate_key()->
  R = crypto:hash(sha,term_to_binary({erlang:timestamp(),make_ref()})),
  <<I:160/integer>> = R,
  integer_to_binary(I,36).

now()->
  DT = calendar:now_to_datetime(erlang:timestamp()),
  convert_datetime(DT).

convert_datetime({{Y,M,D},{H,Min,S}})->
  list_to_binary(date_to_list(Y) ++ date_to_list(M) ++ date_to_list(D) ++ "T" ++
  date_to_list(H) ++ date_to_list(Min) ++ date_to_list(S)).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

date_to_list(V) when V >=0 andalso V =<9->
  "0" ++ integer_to_list(V);
date_to_list(V)->
  integer_to_list(V).