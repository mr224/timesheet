-module(st_member_manager).
-include("../include/st_types.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([add_new_user/5]).
-export([add_user/5,add_user/6,add_user/7]).
-export([delete_user/1,update_user/2]).
-export([add_project/5]).
-export([delete_project/1,update_project/2]).
-export([add_company/1]).
-export([delete_company/1,update_company/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

add_new_user(Login,Password,Name,Email,Phone)->
  case st_verification:verificate_user(Login,Password,Name,Email,Phone) of
    {ok,true} ->
      add_new_user_1(Login,Password,Name,Email,Phone);
    {false,Reason} ->
      {error,Reason}
  end.

-spec add_user(id(),binary(),binary(),binary(),binary()) ->{ok,id()} | {error,any()}.
add_user(Company,Login,Password,Name,Email)->
  add_user(Company,Login,Password,Name,Email,undefined,undefined).

-spec add_user(id(),binary(),binary(),binary(),binary(),binary()) ->{ok,id()} | {error,any()}.
add_user(Company,Login,Password,Name,Email,Phone)->
  add_user(Company,Login,Password,Name,Email,Phone,undefined).

-spec add_user(id(),binary(),binary(),binary(),binary(),binary(),binary()) ->{ok,id()} | {error,any()}.
add_user(Company,Login,Password,Name,Email,Phone,Department)->
  case st_verification:verificate_user(Login,Password,Name,Email,Phone) of
    {ok,true} ->
      add_user_1(Company,Login,Password,Name,Email,Phone,Department);
    {false,Reason} ->
      {error,Reason}
  end.


-spec delete_user(binary()) ->{ok,done} | {error,any()}.
delete_user(UniqueLogin)->
  st_db_helper:delete_user(UniqueLogin).

-spec update_user(id(),data()) ->{ok,id()} | {error,any()}.
update_user(ID,UpdatesList) when is_list(UpdatesList)->
  case st_verification:verificate_user_updates(UpdatesList) of
    {ok,true}->
      update_user_1(ID,UpdatesList);
    {false,Reason}->
      {error,Reason}
  end.

-spec add_project(id(),name(),binary(),binary(),binary()) ->{ok,id()} | {error,any()}.
add_project(Company, Name,Type,Description,Priority)->
  ID = st_data_util:generate_key(),
  Status = ?ST_PROJECT_STATUS_NEW,
  Obj = st_obj:new(ID, [{?ST_PROJECT_TYPE,Type},{?ST_PROJECT_COMPANY,Company},{?ST_PROJECT_NAME,Name},
    {?ST_PROJECT_STATUS,Status},{?ST_PROJECT_DESCRIPTION,Description},{?ST_PROJECT_PRIORITY,Priority}]),
  st_db_helper:save_new_project(Obj).

-spec delete_project(id()) ->{ok,done} | {error,any()}.
delete_project(ID)->
  st_db_helper:delete_project(ID).

-spec update_project(id(),data()) ->{ok,id()} | {error,any()}.
update_project(ID,UpdatesList) when is_list(UpdatesList)->
  st_db_helper:update_project(ID,UpdatesList).

-spec add_company(name()) ->{ok,id()} | {error,any()}.
add_company(Name)->
  ID = st_data_util:generate_key(),
  Obj = st_obj:new(ID, [{?ST_COMPANY_NAME, Name}]),
  st_db_helper:save_new_company(Obj).

-spec delete_company(name()) ->{ok,done} | {error,any()}.
delete_company(ID)->
  st_db_helper:delete_company(ID).

-spec update_company(id(),data()) ->{ok,id()} | {error,any()}.
update_company(ID,UpdatesList) when is_list(UpdatesList)->
  st_db_helper:update_company(ID,UpdatesList).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

add_user_1(Company,Login,Password,Name,Email,Phone,Department)->
  ID = st_data_util:generate_key(),
  PassHash = crypto:hash(sha224,Password),
  Obj = st_obj:new(ID,[{?ST_USER_PASSWORD_HASH,PassHash}, {?ST_USER_COMPANY,Company}, {?ST_USER_LOGIN,Login}, {?ST_USER_NAME, Name},
                        {?ST_USER_EMAIL,Email}, {?ST_USER_PHONE,Phone}, {?ST_USER_DEPARTMENT,Department}]),
  st_db_helper:save_new_user(Obj).

update_user_1(Login,UpdatesList)->
  st_db_helper:update_user(Login,UpdatesList).

add_new_user_1(Login,Password,Name,Email,Phone)->
  ID = st_data_util:generate_key(),
  PassHash = crypto:hash(sha224,Password),
  {ok,Company} = create_company_for_new_user(),
  Obj = st_obj:new(ID,[{?ST_USER_PASSWORD_HASH,PassHash}, {?ST_USER_COMPANY,Company}, {?ST_USER_LOGIN,Login}, {?ST_USER_NAME, Name},
    {?ST_USER_EMAIL,Email}, {?ST_USER_PHONE,Phone}]),
  st_db_helper:save_new_user(Obj).

create_company_for_new_user()->
  Name = ?ST_COMPANY_DEFAULT_NAME,
  add_company(Name).

