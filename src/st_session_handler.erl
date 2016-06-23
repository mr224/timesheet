-module(st_session_handler).
-include("../include/st_types.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([auth/2]).
-export([create_new_session/2]).
-export([get_session_data/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

auth(Login,Pwd)->
  lager:info("auth start, login: ~p; password: ~p~n",[Login,Pwd]),
  PassHash = crypto:hash(sha224,Pwd),
  st_db_helper:check_user_pwd(Login,PassHash).

create_new_session(Login,UserID)->
  lager:info("login: ~p; id: ~p",[Login,UserID]),
  case st_db_helper:get_user_company(UserID) of
    {ok,CompanyID}->
      lager:debug("company found: ~p",[CompanyID]),
      SessionID0 = st_data_util:generate_key(),
      SessionID = binary_to_list(SessionID0),
      lager:debug("session id created: ~p",[SessionID]),
      Obj = st_obj:new(SessionID,[
        {?ST_SESSION_USER_ID,UserID},{?ST_SESSION_COMPANY_ID,CompanyID},{?ST_SESSION_LOGIN,Login}
      ]),
      lager:debug("session object created: ~p",[Obj]),
      case st_db_helper:save_new_session(Obj) of
        {ok,done} ->
          lager:info("new session created, id: ~p",[SessionID]),
          {ok,SessionID};
        {error,Reason}->
          lager:error("session created failed with reason: ~p",[Reason]),
          {error,Reason}
      end;
    {error,Err}->
      Err
  end.

get_session_data(SessionID)->
  case st_db_helper:get_session_data(SessionID) of
    {ok,V}->
      lager:debug("get session data success: ~p",[V]),
      {ok,st_obj:session_to_obj(V)};
    {error,Reason}->
      lager:debug("get session error: ~p",[Reason]),
      {error,Reason}
  end.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
