-module(st_data_util).
-include("../include/st_types.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([generate_key/0]).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

generate_key()->
  R = crypto:hash(sha,term_to_binary({erlang:timestamp(),make_ref()})),
  <<I:160/integer>> = R,
  integer_to_binary(I,36).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
