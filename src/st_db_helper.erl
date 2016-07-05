-module(st_db_helper).

-include("../include/st_types.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([first_start/0]).
-export([save_new_user/1,delete_user/1,update_user/2]).
-export([save_new_project/1,delete_project/1,update_project/2]).
-export([save_new_company/1,delete_company/1,update_company/2]).
-export([save_new_timestamp/1,delete_timestamp/1,update_timestamp/2]).
-export([save_new_session/1]).
-export([get_session_data/1]).
%%% queries
-export([get_obj/2]).
-export([check_user_pwd/2, get_user_company/1]).
-export([select_all_timestamps_for_user/1]).

-export([add_row/6]).
-export([select_all/1, select_all_data_for_period/3, select_project_id_for_period/4]).
-export([select_all_user_id_data_for_period/4, select_user_id_project_id_for_period/5, delete/6]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

first_start()->
  mnesia:stop(),
  create_timesheet_table(),
  mnesia:start().

save_new_user(Obj)->
  Employee = st_obj:obj_to_employee(Obj),
  Member = st_obj:obj_to_member(Obj),
  case save_new_user_to_mnesia(Employee,Member) of
    {atomic,ok}->
      {ok,st_obj:id(Obj)};
    {aborted,Reason}->
      {error,Reason}
  end.

delete_user(Login)->
  case delete_user_from_mnesia(Login) of
    {atomic,ok}->
      {ok,done};
    {aborted,Reason}->
      {error,Reason}
  end.

update_user(ID,UpdatesList)->
  case update_user_in_mnesia(ID,UpdatesList) of
    {atomic,ok}->
      {ok,ID};
    {aborted,Reason}->
      {error,Reason}
  end.

save_new_project(Obj)->
  Project = st_obj:obj_to_project(Obj),
  case save_new_project_to_mnesia(Project) of
    {atomic,ok}->
      {ok,st_obj:id(Obj)};
    {aborted,Reason}->
      {error,Reason}
  end.

delete_project(ID)->
  case delete_project_from_mnesia(ID) of
    ok->
      {ok,done};
    {aborted,Reason}->
      {error,Reason}
  end.

update_project(ID,UpdatesList)->
  case update_project_in_mnesia(ID,UpdatesList) of
    {atomic,ok}->
      {ok,ID};
    {aborted,Reason}->
      {error,Reason}
  end.

save_new_company(Obj)->
  Company = st_obj:obj_to_company(Obj),
  case save_new_company_to_mnesia(Company) of
    ok->
      {ok,st_obj:id(Obj)};
    {aborted,Reason}->
      {error,Reason}
  end.

delete_company(ID)->
  case delete_company_from_mnesia(ID) of
    ok->
      {ok,done};
    {aborted,Reason}->
      {error,Reason}
  end.

update_company(ID,UpdatesList)->
  case update_company_in_mnesia(ID,UpdatesList) of
    {atomic,ok}->
      {ok,ID};
    {aborted,Reason}->
      {error,Reason}
  end.

save_new_timestamp(Obj)->
  Timestamp = st_obj:obj_to_timestamp(Obj),
  case save_new_timestamp_to_mnesia(Timestamp) of
    {atomic,ok}->
      {ok,st_obj:id(Obj)};
    {aborted,Reason}->
      {error,Reason}
  end.


add_row(TableName, User, Date, Job, SpentTime, Comment) ->
  Row = #timesheet{user_id = User, date = Date, project_id = Job,
									 spent_time = SpentTime, comment = Comment},
  F = fun() ->
    mnesia:write(TableName, Row, write)
  end,
  mnesia:transaction(F).


delete_timestamp(ID)->
  case delete_timestamp_from_mnesia(ID) of
    ok->
      {ok,done};
    {aborted,Reason}->
      {error,Reason}
  end.

update_timestamp(ID,UpdatesList)->
  case update_timestamp_in_mnesia(ID,UpdatesList) of
    {atomic,ok}->
      {ok,ID};
    {aborted,Reason}->
      {error,Reason}
  end.

check_user_pwd(Login,Pwd)->
  case check_user_pwd_1(Login,Pwd) of
    {atomic,ID}->
      {ok,ID};
    {aborted,Reason}->
      Reason
  end.

get_user_company(ID)->
  case mnesia:dirty_read({?TABLE_EMPLOYEE,ID}) of
    [Employee]->
      {ok,Employee#employee.company_id};
    Err->
      {error,Err}
  end.

get_obj(Table,ID)->
  case mnesia:dirty_read({Table,ID}) of
    [Record]->
      {ok,st_obj:record_to_obj(Table,Record)};
    Err->
      {error,Err}
  end.

%%% queries
select_all_timestamps_for_user(ID)->
  do(qlc:q([X || X <-mnesia:table(?TABLE_TIMESHEET), X#timesheet.user_id =:= ID])).

%%todo next block from release

%%--------------------------
select_all(TableName) ->
  do(qlc:q([X || X <- mnesia:table(TableName)])).

select_all_data_for_period(TableName, From, To) ->
  do(qlc:q([X || X <- mnesia:table(TableName),
    X#timesheet.date =< To,
    X#timesheet.date >= From])).

select_all_user_id_data_for_period(TableName, User, From, To) ->
  do(qlc:q([X || X <- mnesia:table(TableName),
    X#timesheet.date =< To,
    X#timesheet.date >= From,
		X#timesheet.user_id =:= User])).

select_project_id_for_period(TableName, Job, From, To) ->
  do(qlc:q([X || X <- mnesia:table(TableName),
    X#timesheet.date =< To,
    X#timesheet.date >= From,
    X#timesheet.project_id == Job])).

select_user_id_project_id_for_period(TableName, User, Job, From, To) ->
  do(qlc:q([X || X <- mnesia:table(TableName),
    X#timesheet.date =< To,
    X#timesheet.date >= From,
    X#timesheet.project_id =:= Job,
		X#timesheet.user_id =:= User])).

delete(_TableName, User, Date, Job, SpentTime, Comment)->
  Object = #timesheet{user_id = User, date = Date, project_id = Job,
                   spent_time = SpentTime, comment = Comment},
  mnesia:transaction(fun() -> mnesia:delete_object(Object) end).

save_new_session(Obj)->
  Session = st_obj:obj_to_session(Obj),
  lager:debug("save_new session, Session = ~p",[Session]),
  case save_new_session_to_mnesia(Session) of
    ok->
      {ok,done};
    {aborted,Reason}->
      {error,Reason}
  end.

get_session_data(SessionID)->
  case mnesia:dirty_read({?TABLE_SESSION,SessionID}) of
    [V]->
      {ok,V};
    []->
      {error,not_found}
  end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%todo move first startup function to individual module
create_timesheet_table() ->
  case mnesia:create_schema([node()]) of
		ok ->
      mnesia:start(),
			lager:info("create table ~p", [?TABLE_TIMESHEET]),
			mnesia:create_table(timesheet, [{attributes, record_info(fields,timesheet)},
				{disc_copies, [node()]},
				{type, bag}]),
      lager:info("create table ~p", [?TABLE_PROJECT]),
      mnesia:create_table(?TABLE_PROJECT, [{attributes, record_info(fields,?TABLE_PROJECT)},
  			{disc_copies, [node()]},
  			{type, set}]),
      lager:info("create table ~p", [?TABLE_EMPLOYEE]),
      mnesia:create_table(?TABLE_EMPLOYEE, [{attributes, record_info(fields,?TABLE_EMPLOYEE)},
        {disc_copies, [node()]},
        {type, set}]),
      lager:info("create table ~p", [?TABLE_COMPANY]),
      mnesia:create_table(?TABLE_COMPANY, [{attributes, record_info(fields,?TABLE_COMPANY)},
        {disc_copies, [node()]},
        {type, set}]),
      lager:info("create table ~p", [?TABLE_MEMBERS]),
      mnesia:create_table(?TABLE_MEMBERS, [{attributes, record_info(fields,?TABLE_MEMBERS)},
        {disc_copies, [node()]},
        {type, set}]),
      lager:info("create table ~p", [?TABLE_SESSION]),
      mnesia:create_table(?TABLE_SESSION, [{attributes, record_info(fields,?TABLE_SESSION)},
        {type, set}]),
  		mnesia:stop();
		{error, Reason} ->
			case Reason of
				{_,{already_exists,_}} ->
					lager:error("table ~p already exists", [Reason]);
				_ ->
					lager:error("error when creating schema with reason ~p", [Reason])
			end
	end.

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

save_new_user_to_mnesia(Employee, Member)->
  Key = Member#members.login,
  F = fun() ->
    case mnesia:wread({?TABLE_MEMBERS,Key}) of
      [] ->
        CompanyID = Employee#employee.company_id,
        case update_company_in_mnesia(CompanyID, [{?ST_COMPANY_EMPLOYEES,Employee#employee.id}]) of
          {atomic,ok}->
            mnesia:write(Member),
            mnesia:write(Employee);
          {aborted, Reason}->
            mnesia:abort(Reason)
        end;
      _ ->
        mnesia:abort("User already exists")
    end
  end,
  mnesia:transaction(F).

delete_user_from_mnesia(Login)->
  F = fun()->
    case mnesia:wread({?TABLE_MEMBERS,Login}) of
      [] ->
        mnesia:abort("User doesn't exist in table members");
      [Member] ->
        EmployeeID = Member#members.id,
        case mnesia:wread({?TABLE_EMPLOYEE,EmployeeID}) of
          []->
            mnesia:abort("User doesn't exist in table employee");
          [Employee]->
            CompanyID = Employee#employee.company_id,
            delete_from_company_in_mnesia(CompanyID,[{?ST_USER, EmployeeID}]),
            ProjectsAssignID = Employee#employee.projects_assign,
            ProjectsManagerID = Employee#employee.projects_manager,
            delete_from_projects(EmployeeID,ProjectsAssignID,ProjectsManagerID),
            mnesia:delete({employee,EmployeeID}),
            mnesia:delete({members,Login})
        end
    end
  end,
  mnesia:transaction(F).

update_user_in_mnesia(Login,UpdatesList)->
  case mnesia:dirty_read({?TABLE_MEMBERS,Login}) of
    []->
      {aborted,"user doesn't exist in table members"};
    [Member]->
      ID = Member#members.id,
      update_user_in_mnesia_1(ID,UpdatesList)
  end.

update_user_in_mnesia_1(ID,UpdatesList)->
  F = fun()->
    case mnesia:wread({?TABLE_EMPLOYEE,ID}) of
      []->
        lager:error("User ~p doesn't exist in table employee", [ID]),
        mnesia:abort("User doesn't exist in table employee");
      [Employee]->
        Emp1 = st_obj:employee_to_obj(Employee),
        Emp2 = update_employee_obj(Emp1,UpdatesList),
        lager:debug("Employee obj before update = ~p", [Emp1]),
        lager:debug("Employee obj after update = ~p", [Emp2]),
        mnesia:write(st_obj:obj_to_employee(Emp2))
    end
  end,
  mnesia:transaction(F).

update_employee_obj(Obj,[])->
  Obj;
update_employee_obj(Obj,[{_K,undefined} | Rest])->
  update_employee_obj(Obj,Rest);
update_employee_obj(Obj,[{?ST_USER_PROJECTS_MANAGER = K,V} | Rest])->
  case st_obj:get(K,Obj) of
    L when is_list(L) ->
      update_employee_obj(st_obj:put(K,[V|L],Obj),Rest);
    _ ->
      update_employee_obj(st_obj:put(K,[V],Obj),Rest)
  end;
update_employee_obj(Obj,[{?ST_USER_PROJECTS_ASSIGN = K,V} | Rest])->
  case st_obj:get(K,Obj) of
    L when is_list(L) ->
      update_employee_obj(st_obj:put(K,[V|L],Obj),Rest);
    _ ->
      update_employee_obj(st_obj:put(K,[V],Obj),Rest)
  end;
update_employee_obj(Obj,[{K,V} | Rest])->
  update_employee_obj(st_obj:put(K,V,Obj),Rest).

save_new_project_to_mnesia(Project)->
  F = fun()->
    CompanyID = Project#project.company_id,
    ManagerID = Project#project.manager,
    AssignID = Project#project.assignee,
    lager:debug("CompanyID = ~p", [CompanyID]),
    lager:debug("ManagerID = ~p", [ManagerID]),
    lager:debug("AssignID = ~p", [AssignID]),
    case update_company_in_mnesia(CompanyID, [{?ST_COMPANY_PROJECTS,Project#project.id}]) of
      {atomic,ok}->
        lager:debug("Company ~p was successfully updated",[CompanyID]);
      Err->
        mnesia:abort(Err)
    end,
    case update_user_in_mnesia_1(ManagerID, [{?ST_USER_PROJECTS_MANAGER,Project#project.id}]) of
      {atomic,ok}->
        ok;
      Err1->
        mnesia:abort(Err1)
    end,
    case update_user_in_mnesia_1(AssignID, [{?ST_USER_PROJECTS_ASSIGN,Project#project.id}]) of
      {atomic,ok}->
        ok;
      Err2->
        mnesia:abort(Err2)
    end,
    mnesia:write(Project)
  end,
  mnesia:transaction(F).

delete_project_from_mnesia(ID)->
  F = fun()->
    case mnesia:wread({?TABLE_PROJECT, ID}) of
      []->
        mnesia:abort("Project doesn't exist");
      [Project]->
        CompanyID = Project#project.company_id,
        delete_from_company_in_mnesia(CompanyID,[{?ST_PROJECT, Project#project.id}]),
        ManagerID = Project#project.manager,
        AssignID = Project#project.assignee,
        delete_from_employees(ID,[{?ST_PROJECT_ASSIGNEE,AssignID},{?ST_PROJECT_MANAGER,ManagerID}]),
        mnesia:delete(Project)
    end
  end,
  mnesia:transaction(F).

update_project_in_mnesia(ID,UpdatesList)->
  lager:debug("Update project in mnesia: K = ~p, UpdatesList = ~p",[ID, UpdatesList]),
  F = fun()->
    case mnesia:wread({?TABLE_PROJECT,ID}) of
      []->
        mnesia:abort("Project doesn't exist");
      [Project]->
        P1 = st_obj:project_to_obj(Project),
        P2 = update_project_obj(P1,UpdatesList),
        mnesia:write(st_obj:obj_to_project(P2))
    end
      end,
  mnesia:transaction(F).

update_project_obj(Obj,[])->
  Obj;
update_project_obj(Obj,[{?ST_PROJECT_TIMESTAMPS = K,V} | Rest])->
  case st_obj:get(K,Obj) of
    L when is_list(L) ->
      update_project_obj(st_obj:put(K,[V|L],Obj),Rest);
    _ ->
      update_project_obj(st_obj:put(K,[V],Obj),Rest)
  end;
update_project_obj(Obj,[{K,V} | Rest])->
  update_project_obj(st_obj:put(K,V,Obj),Rest).

save_new_company_to_mnesia(Company)->
  mnesia:dirty_write(Company).

delete_company_from_mnesia(ID)->
  mnesia:dirty_delete({?TABLE_COMPANY,ID}).


update_company_in_mnesia(ID,UpdatesList)->
  F = fun()->
    case mnesia:wread({?TABLE_COMPANY,ID}) of
      []->
        mnesia:abort("Company doesn't exist");
      [Company]->
        C1 = st_obj:company_to_obj(Company),
        C2 = update_company_obj(C1,UpdatesList),
        lager:debug("Update list: ~p",[UpdatesList]),
        lager:debug("Company before change: ~p",[C1]),
        lager:debug("Company after change: ~p",[C2]),
        mnesia:write(st_obj:obj_to_company(C2))
    end
      end,
  mnesia:transaction(F).

delete_from_company_in_mnesia(ID, RemoveList)->
  F = fun()->
    case mnesia:wread({?TABLE_COMPANY,ID}) of
      []->
        mnesia:abort("Company doesn't exist");
      [Company]->
        C1 = st_obj:company_to_obj(Company),
        C2 = remove_from_company_obj(C1,RemoveList),
        mnesia:write(st_obj:obj_to_company(C2))
    end
      end,
  mnesia:transaction(F).

remove_from_company_obj(Obj,[])->
  Obj;
remove_from_company_obj(Obj,[{?ST_PROJECT,V}|Rest])->
  Companies = st_obj:get(?ST_COMPANY_PROJECTS,Obj),
  remove_from_company_obj(Obj#company{projects = lists:delete(V,Companies)},Rest);
remove_from_company_obj(Obj,[{?ST_USER,V}|Rest])->
  Employees = st_obj:get(?ST_COMPANY_EMPLOYEES,Obj),
  remove_from_company_obj(Obj#company{employees = lists:delete(V,Employees)},Rest).


update_company_obj(Obj,[])->
  Obj;
update_company_obj(Obj,[{?ST_COMPANY_PROJECTS = K,V} | Rest])->
  case st_obj:get(K,Obj) of
    L when is_list(L) ->
      update_company_obj(st_obj:put(K,[V|L],Obj),Rest);
    _ ->
      update_company_obj(st_obj:put(K,[V],Obj),Rest)
  end;
update_company_obj(Obj,[{?ST_COMPANY_EMPLOYEES = K,V} | Rest])->
  case st_obj:get(K,Obj) of
    L when is_list(L) ->
      update_company_obj(st_obj:put(K,[V|L],Obj),Rest);
    _ ->
      update_company_obj(st_obj:put(K,[V],Obj),Rest)
  end;
update_company_obj(Obj,[{K,V} | Rest])->
  update_company_obj(st_obj:put(K,V,Obj),Rest).

delete_from_employees(ID,List)->
  delete_from_employees(ID,List, []).

delete_from_employees(_,[],Acc)->
  Acc;
delete_from_employees(ID,[E|Rest],Acc)->
  delete_from_employees(ID,Rest,[delete_from_employee(ID,E)|Acc]).

delete_from_employee(ID,{K,E})->
  F = fun()->
    EmployeeID = E#employee.id,
    case mnesia:wread({?TABLE_EMPLOYEE,EmployeeID}) of
      []->
        mnesia:abort("Employee doesn't exist");
      [Employee]->
        E1 = st_obj:employee_to_obj(Employee),
        E2 = remove_from_employee_obj(E1,K,ID),
        mnesia:write(st_obj:obj_to_employee(E2))
    end
      end,
  mnesia:transaction(F).

remove_from_employee_obj(Obj,K,ID) when is_binary(ID)->
  case st_obj:get(K,Obj) of
    L when is_list(L) ->
      st_obj:put(K,lists:delete(ID,L),Obj);
    _ ->
      Obj
  end.

delete_from_projects(EmployeeID,AssignList,ManagerList)->
  AssignList1 = [{?ST_PROJECT_ASSIGNEE,E} || E <- AssignList],
  ManagerList1 = [{?ST_PROJECT_MANAGER,E} || E <- ManagerList],
  delete_from_projects_1(EmployeeID,AssignList1++ManagerList1).

delete_from_projects_1(EmployeeID,Projects)->
  delete_from_projects_1(EmployeeID,Projects,[]).

delete_from_projects_1(_,[],Acc)->
  Acc;
delete_from_projects_1(EmployeeID,[E|Rest],Acc)->
  delete_from_projects_1(EmployeeID,Rest,[delete_from_project(EmployeeID,E)|Acc]).

delete_from_project(EmployeeID,{Type,ProjectID})->
  F = fun()->
    case mnesia:wread({?TABLE_PROJECT,ProjectID}) of
      []->
        mnesia:abort("Project doesn't exist");
      [Project]->
        P1 = st_obj:project_to_obj(Project),
        P2 = remove_from_project_obj(P1,Type,EmployeeID),
        mnesia:write(st_obj:obj_to_project(P2))
    end
      end,
  mnesia:transaction(F).

remove_from_project_obj(Obj,Type,EmployeeID)->
  case st_obj:get(Type,Obj) of
    EmployeeID->
      st_obj:remove(Type,Obj);
    _ ->
      Obj
  end.

save_new_timestamp_to_mnesia(Timestamp)->
  lager:debug("save timestamp to mnesia: ~p",[Timestamp]),
  [Project] = mnesia:dirty_read(?TABLE_PROJECT,Timestamp#timesheet.project_id),
  CompanyID = Project#project.company_id,
  Timestamp1 = Timestamp#timesheet{company_id = CompanyID},
  save_new_timestamp_to_mnesia_1(Timestamp1,Project).
%%  mnesia:dirty_write({?TABLE_TIMESHEET,Timestamp1}).

save_new_timestamp_to_mnesia_1(Timestamp,Project)->
  F = fun()->
    case update_project(Project#project.id,[{?ST_PROJECT_TIMESTAMPS,Timestamp#timesheet.id}]) of
      {ok,_}->
        mnesia:write(Timestamp);
      _->
        mnesia:abort("Writing timestamp to project failed")
    end
      end,
  mnesia:transaction(F).


delete_timestamp_from_mnesia(ID)->
  mnesia:dirty_delete({?TABLE_TIMESHEET,ID}).

update_timestamp_in_mnesia(ID,UpdatesList)->
  case mnesia:dirty_read(?TABLE_TIMESHEET,ID) of
    []->
      {aborted,"timestamp doesn't exist"};
    [Timestamp]->
      Obj1 = st_obj:timestamp_to_obj(Timestamp),
      Obj2 = st_obj:put(UpdatesList,Obj1),
      mnesia:dirty_write(st_obj:obj_to_timestamp(Obj2))
  end.

check_user_pwd_1(Login,Pwd)->
  F = fun()->
    case mnesia:read({?TABLE_MEMBERS,Login}) of
      []->
        mnesia:abort("User doesn't exist in table members");
      [Member]->
        if
          Member#members.password_hash =:= Pwd ->
            Member#members.id;
%%            ID = Member#members.id,
%%            case mnesia:read({?TABLE_EMPLOYEE,ID}) of
%%              []->
%%                mnesia:abort("User doesn't exist in table employee");
%%              [Employee]->
%%                Company = Employee#employee.company_id,
%%
%%            end
          true ->
            mnesia:abort("Incorrect password")
        end
    end
      end,
  mnesia:transaction(F).

save_new_session_to_mnesia(Session)->
  mnesia:dirty_write(Session).
