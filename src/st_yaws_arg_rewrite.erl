%%%-------------------------------------------------------------------
%%% @author mr224
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2016 10:06 PM
%%%-------------------------------------------------------------------
-module(st_yaws_arg_rewrite).
-author("mr224").
-include("../include/st_types.hrl").
-include("../deps/yaws/include/yaws_api.hrl").

%% API
-export([arg_rewrite/1, check_cookie/2]).

arg_rewrite(Arg) ->
  lager:debug("request = ~p",[Arg#arg.req]),
  lager:debug("check_cookie = ~p",[check_cookie(Arg, ?ST_SESSION_NAME)]),
  case check_cookie(Arg, ?ST_SESSION_NAME) of
    {error, _} ->
      do_rewrite(Arg);
    {ok, _Session} ->
      Arg
end.

login_pages() ->
  ["/login.yaws", "/login_post.yaws", "/sign_up.yaws", "/sign_up_post.yaws"].

do_rewrite(Arg) ->
  Req = Arg#arg.req,
  {abs_path, Path} = Req#http_request.path,
  case lists:member(Path, login_pages()) of
    true ->
      lager:debug("rewrite is_not_needed, path = ~p",[Path]),
      Arg;
    false ->
      lager:debug("rewrite arg, path = ~p",[Path]),
      Arg#arg{req = Req#http_request{path = {abs_path, "/login.yaws"}},
        state = Path}
  end.

get_cookie_val(CookieName, Arg) ->
  H = Arg#arg.headers,
  yaws_api:find_cookie_val(CookieName, H#headers.cookie).
check_cookie(A, CookieName) ->
  case get_cookie_val(CookieName, A) of
    [] ->
      {error, "not logged in"};
    Cookie ->
      st_session_handler:get_session_data(Cookie)
  end.