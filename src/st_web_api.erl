-module(st_web_api).
-include("../include/st_types.hrl").
-include("../deps/yaws/include/yaws_api.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([out/1]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

out(Arg) ->
  lager:info("arg = ~p",[Arg]),
  lager:info("state = ~p",[Arg#arg.state  ]),
  case get_session_data(Arg) of
    {ok,Data}->
      lager:debug("session found: ~p",[Data]),
      handle(Arg#arg.req#http_request.method,Arg);
    {error,not_found}->
      %%todo redirect to login page
      lager:debug("data not found"),
      [{redirect, "http://localhost:8080/login.yaws"}]
  end.

  %%todo serve web requests
%%  {ehtml, io_lib:format(
%%    "This is st_web_api.~n
%%   A#arg.appmoddata = ~p~n
%%   A#arg.appmod_prepath = ~p~n
%%   A#arg.querydata = ~p~n
%%    A#arg.req = ~p~n
%%    Req#http_request.method = ~p~n",
%%
%%   [A#arg.appmoddata,
%%    A#arg.appmod_prepath,
%%    A#arg.querydata,
%%     A#arg.req,
%%     A#arg.req#http_request.method
%%     ])}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle('GET', Arg)->
  Q = yaws_api:parse_query(Arg),
  Res = get_answer(Q),
  {ehtml, io_lib:format("~p",[Res])};
%%    "This is st_web_api.~n
%%   A#arg.appmoddata = ~p~n
%%   A#arg.appmod_prepath = ~p~n
%%   A#arg.querydata = ~p~n
%%    A#arg.req = ~p~n
%%    Req#http_request.method = ~p~n,Res = ~p~n
%%    Q = ~p~n",
%%    [Arg#arg.appmoddata,
%%      Arg#arg.appmod_prepath,
%%      Arg#arg.querydata,
%%      Arg#arg.req,
%%      Arg#arg.req#http_request.method
%%      , Res, Q]
%%   [Arg#arg.appmoddata,
%%    Arg#arg.appmod_prepath,
%%    Arg#arg.querydata,
%%     Arg#arg.req,
%%     Arg#arg.req#http_request.method
%%  , Res, Q]
handle('POST', Arg)->
  %%todo implement post request
  {ehtml, io_lib:format("Res = ~p~n", ["not implemented"])};
handle('PUT', Arg)->
  %%todo implement put request
  {ehtml, io_lib:format("Res = ~p~n", ["not implemented"])};
handle('DELETE', Arg)->
  %%todo implement delete request
  {ehtml, io_lib:format("Res = ~p~n", ["not implemented"])}.

get_answer([])->
  empty_answer;
get_answer([{"all_from_table",TableName}]) when is_list(TableName) ->
  st_api:select_all_from_table(list_to_atom(TableName));
get_answer([{"user_timestamp",UserName} | Rest]=Params) when is_list(UserName)->
  DateFrom = case proplists:get_value("from",Rest) of
    undefined ->
      <<"19100101">>;
    V when is_list(V)->
      list_to_binary(V)
  end,
  DateTo = case proplists:get_value("to",Rest) of
               undefined ->
                 <<"19100101">>;
               V1 when is_list(V1)->
                 list_to_binary(V1)
             end,
  UserName1 = list_to_binary(UserName),
  st_api:get_user_timesheet_for_period(UserName1,DateFrom,DateTo).

get_session_data(Arg)->
  lager:debug("arg: ~p",[Arg]),
  case get_cookie_val(Arg) of
    []->
      lager:debug("no cookie found in query"),
      {error, not_found};
    Cookie->
%%      lager:debug("cookie found! ~p", [Cookie]),
%%      SessionID = yaws_api:cookieval_to_opaque(Cookie),
      SessionID = Cookie,
      case SessionID of
        {error,no_session}->
          yaws_api:delete_cookie_session(Cookie),
          lager:debug("no cookie found in server"),
          {error,not_found};
        ID->
          lager:debug("cookie data to opaque! ~p", [ID]),
          st_session_handler:get_session_data(ID)
      end
  end.


get_cookie_val(Arg)->
  H = Arg#arg.headers,
  yaws_api:find_cookie_val(?ST_SESSION_NAME,H#headers.cookie).