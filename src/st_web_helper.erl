-module(st_web_helper).
-include("../include/st_types.hrl").
-include("../deps/yaws/include/yaws_api.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([projects_view/1]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

projects_view(List)->
  lager:debug("start project view: ~p",[List]),
  projects_view(List,[]).

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
      {a,[{href,io_lib:format("/delete_project.yaws?key=~s",[binary_to_list(K)])}],"Delete Project"}
    ]
  }.