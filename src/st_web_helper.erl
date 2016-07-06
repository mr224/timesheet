-module(st_web_helper).
-include("../include/st_types.hrl").
-include("../deps/yaws/include/yaws_api.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([projects_view/1]).
-export([timestamps_view/1]).
-export([gen_select/2]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

projects_view(List)->
  lager:debug("start projects view: ~p",[List]),
  projects_view(List,[]).

timestamps_view(List)->
  lager:debug("start timestamps view: ~p",[List]),
  timestamps_view(List,[]).

gen_select(ValNames,Selected)->
  gen_select(ValNames,Selected,[]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

projects_view([],Acc)->
  ["Name  Type  Priority Status",{br} | Acc];
projects_view([K|Rest],Acc)->
  projects_view(Rest,[projects_view_1(K)|Acc]).

projects_view_1(K)->
  {ok,ProjectObj} = st_api:get_project_obj(K),
  Name = st_obj:get(?ST_PROJECT_NAME,ProjectObj),
  Type = st_obj:get(?ST_PROJECT_TYPE,ProjectObj),
  Priority = st_obj:get(?ST_PROJECT_PRIORITY,ProjectObj),
  Status = st_obj:get(?ST_PROJECT_STATUS,ProjectObj),
  {'div',[{id,K}],
    [
      io_lib:format("~s  ~s     ~s  ~s",[Name,Type,Priority,Status]),
      {a,[{href,io_lib:format("/show_project.yaws?key=~s",[binary_to_list(K)])}],"Show Log"},
      "    ",
      {a,[{href,io_lib:format("/edit_project.yaws?key=~s",[binary_to_list(K)])}],"Edit Project"}
    ]
  }.


timestamps_view([],Acc)->
  ["Name   Date   TimeSpent  DateTime Comment",{br} |Acc];
timestamps_view([K|Rest],Acc)->
  timestamps_view(Rest,[timestamps_view_1(K)|Acc]).

timestamps_view_1(K)->
  {ok,TSObj} = st_api:get_timestamp_obj(K),
  UserID = st_obj:get(?ST_TIMESTAMP_USER_ID,TSObj),
  Date = st_obj:get(?ST_TIMESTAMP_DATE, TSObj),
  SpentTime = st_obj:get(?ST_TIMESTAMP_TIME_SPENT,TSObj),
  DateTime = st_obj:get(?ST_TIMESTAMP_DATETIME,TSObj),
  Comment = st_obj:get(?ST_TIMESTAMP_COMMENT,TSObj),
  {ok,UserObj} = st_api:get_user_obj(UserID),
  UserName = st_obj:get(?ST_USER_NAME,UserObj),
  {'div',[{id,K}],
    [
      io_lib:format("~s   ~s   ~s   ~s   ~s",[UserName,Date,SpentTime,DateTime,Comment]),
      {a,[{href,io_lib:format("/edit_timestamp.yaws?key=~s",[binary_to_list(K)])}],"Edit"},
      "    ",
      {a,[{href,io_lib:format("/delete_timestamp.yaws?key=~s",[binary_to_list(K)])}],"Delete"}
    ]
  }.

gen_select([],_,Acc)->
  Acc;
gen_select([{Val,Name}|Rest],Val,Acc)->
  gen_select(Rest,Val,[select_view(Val,Name,true)|Acc]);
gen_select([{Val,Name}|Rest],Selected,Acc)->
  gen_select(Rest,Selected,[select_view(Val,Name,false)|Acc]).

select_view(Val,Name,true)->
  {option, [{value,Val},selected],Name};
select_view(Val,Name,_)->
  {option, [{value,Val}],Name}.

