-module(st_obj).
-include("../include/st_types.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/1,new/2,put/3,put/2,get/2,remove/2]).
-export([id/1,data/1]).
-export([record_to_obj/2]).
-export([obj_to_member/1]).
-export([obj_to_employee/1,employee_to_obj/1]).
-export([obj_to_company/1,company_to_obj/1]).
-export([obj_to_project/1,project_to_obj/1]).
-export([obj_to_timestamp/1, timestamp_to_obj/1]).
-export([obj_to_session/1,session_to_obj/1]).
-export([is_st_obj/1]).

-record(st_obj, {
              id :: id(),
              data :: data()
}).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

record_to_obj(?TABLE_COMPANY,ID)->
  company_to_obj(ID);
record_to_obj(?TABLE_EMPLOYEE,ID)->
  employee_to_obj(ID);
record_to_obj(?TABLE_PROJECT,ID)->
  project_to_obj(ID);
record_to_obj(?TABLE_TIMESHEET,ID)->
  timestamp_to_obj(ID);
record_to_obj(?TABLE_SESSION,ID)->
  session_to_obj(ID);
record_to_obj(Table,_ID)->
  {no_such_table,Table}.


new(ID)->
  #st_obj{id=ID, data=orddict:new()}.
new(ID,Data) when is_list(Data)->
  #st_obj{id=ID, data=orddict:from_list(Data)}.

put([],StObj)->
  StObj;
put([{K,V}|R],StObj)->
  ?MODULE:put(R, put(K,V,StObj)).

put(?ST_ID,_V,StObj)->
  StObj;
put(K,V,StObj)->
  StObj#st_obj{data=orddict:store(K,V,StObj#st_obj.data)}.

get(K,Obj)->
  get(K,Obj,undefined).

get(?ST_ID,Obj,Default)->
  Obj#st_obj.id;
get(K,Obj,Default) when is_binary(K)->
  case orddict:find(K,Obj#st_obj.data) of
    {ok,Val}->
      Val;
    _ ->
      Default
  end.

remove(K,Obj)->
  Obj#st_obj{data=orddict:erase(K,Obj#st_obj.data)}.

id(Obj)->
  Obj#st_obj.id.

data(Obj)->
  Obj#st_obj.data.

obj_to_member(Obj)->
  case is_st_obj(Obj) of
    true->
      ID = get(?ST_ID,Obj),
      PHash = get(?ST_USER_PASSWORD_HASH,Obj),
      Login = get(?ST_USER_LOGIN,Obj),
      #members{id=ID,password_hash = PHash,login = Login};
    _ ->
      error
  end.

obj_to_employee(Obj)->
  case is_st_obj(Obj) of
    true->
      ID = get(?ST_ID,Obj),
      Company = get(?ST_COMPANY,Obj),
      Name = get(?ST_USER_NAME,Obj),
      Email = get(?ST_USER_EMAIL,Obj),
      Phone = get(?ST_USER_PHONE,Obj),
      Department = get(?ST_USER_DEPARTMENT,Obj),
      PEmp = get(?ST_USER_PROJECTS_ASSIGN,Obj),
      PMan = get(?ST_USER_PROJECTS_MANAGER,Obj),
      #employee{id=ID,company_id = Company,name = Name,email = Email,phone = Phone,department = Department,
        projects_assign = PEmp, projects_manager = PMan};
    _ ->
      error
  end.

obj_to_project(Obj)->
  case is_st_obj(Obj) of
    true->
      ID = get(?ST_ID,Obj),
      Type = get(?ST_PROJECT_TYPE,Obj),
      Company = get(?ST_PROJECT_COMPANY,Obj),
      Name = get(?ST_PROJECT_NAME,Obj),
      Status = get(?ST_PROJECT_STATUS,Obj),
      Description = get(?ST_PROJECT_DESCRIPTION,Obj),
      Manager = get(?ST_PROJECT_MANAGER,Obj),
      TSpent = get(?ST_PROJECT_TIME_SPENT,Obj),
      Assignee = get(?ST_PROJECT_ASSIGNEE,Obj),
      DateCreated = get(?ST_PROJECT_DATE_CREATED,Obj),
      TRemaining = get(?ST_PROJECT_TIME_REMAINING,Obj),
      TEstimate = get(?ST_PROJECT_TIME_ESTIMATE,Obj),
      Priority = get(?ST_PROJECT_PRIORITY,Obj),
      Author = get(?ST_PROJECT_AUTHOR,Obj),
      Timestamps = get(?ST_PROJECT_TIMESTAMPS,Obj),
      #project{id = ID, type = Type, company_id = Company, name = Name,
        status = Status, description = Description, manager = Manager, time_spent = TSpent,
        assignee = Assignee, date_created = DateCreated, time_remaining = TRemaining, time_estimate = TEstimate,
        priority = Priority, author = Author, timestamps = Timestamps};
    _ ->
      error
  end.

obj_to_company(Obj)->
  case is_st_obj(Obj) of
    true ->
      ID = get(?ST_ID,Obj),
      Name = get(?ST_COMPANY_NAME,Obj),
      Employees = get(?ST_COMPANY_EMPLOYEES,Obj),
      Projects = get(?ST_COMPANY_PROJECTS,Obj),
      #company{id = ID, name = Name, employees = Employees, projects = Projects};
    _ ->
      error
  end.

obj_to_timestamp(Obj)->
  case is_st_obj(Obj) of
    true->
      ID = get(?ST_ID,Obj),
      Project = get(?ST_TIMESTAMP_PROJECT_ID,Obj),
      Company = get(?ST_TIMESTAMP_COMPANY_ID,Obj),
      User = get(?ST_TIMESTAMP_USER_ID,Obj),
      Date = get(?ST_TIMESTAMP_DATE,Obj),
      TSpent = get(?ST_TIMESTAMP_TIME_SPENT,Obj),
      Comment = get(?ST_TIMESTAMP_COMMENT,Obj),
      DateTime = get(?ST_TIMESTAMP_DATETIME,Obj),
      #timesheet{id = ID, project_id = Project, company_id = Company, user_id = User, date = Date,
        spent_time = TSpent, comment = Comment, date_time = DateTime};
    _ ->
      error
  end.

obj_to_session(Obj)->
  case is_st_obj(Obj) of
    true->
      ID = get(?ST_SESSION_ID,Obj),
      CompanyID = get(?ST_SESSION_COMPANY_ID,Obj),
      UserID = get(?ST_SESSION_USER_ID,Obj),
      Login = get(?ST_SESSION_LOGIN,Obj),
      #session{id = ID, company = CompanyID, user = UserID, login = Login};
    _->
      error
  end.


is_st_obj(Obj) when is_record(Obj,st_obj)->
  true;
is_st_obj(_Obj)->
  false.

employee_to_obj(#employee{id = ID, name = Name,company_id = Company,email = Email,phone = Phone,
                                projects_assign = PEmp, projects_manager = PMan, department = Department})->
  new(ID, [
    {?ST_USER_COMPANY, Company},{?ST_USER_NAME, Name},
    {?ST_USER_EMAIL, Email},{?ST_USER_PHONE,Phone},{?ST_USER_DEPARTMENT,Department},
    {?ST_USER_PROJECTS_ASSIGN,PEmp},{?ST_USER_PROJECTS_MANAGER,PMan}
  ]).

project_to_obj(#project{id = ID, type = Type, company_id = Company, name = Name,
  status = Status, description = Description, manager = Manager, time_spent = TSpent,
  assignee = Assignee, date_created = DateCreated, time_remaining = TRemaining, time_estimate = TEstimate,
  priority = Priority, author = Author, timestamps = Timestamps})->
  new(ID, [
    {?ST_PROJECT_TYPE, Type}, {?ST_PROJECT_COMPANY, Company}, {?ST_PROJECT_NAME, Name},
    {?ST_PROJECT_STATUS,Status}, {?ST_PROJECT_DESCRIPTION,Description}, {?ST_PROJECT_MANAGER,Manager},
    {?ST_PROJECT_ASSIGNEE,Assignee}, {?ST_PROJECT_DATE_CREATED, DateCreated}, {?ST_PROJECT_TIME_REMAINING, TRemaining},
    {?ST_PROJECT_TIME_ESTIMATE,TEstimate}, {?ST_PROJECT_PRIORITY,Priority}, {?ST_PROJECT_AUTHOR,Author},
    {?ST_PROJECT_TIME_SPENT, TSpent}, {?ST_PROJECT_TIMESTAMPS,Timestamps}
  ]).

company_to_obj(#company{id = ID, name = Name, employees = Employees, projects = Projects})->
  new(ID, [
    {?ST_COMPANY_NAME, Name}, {?ST_COMPANY_EMPLOYEES, Employees}, {?ST_COMPANY_PROJECTS, Projects}
  ]).

timestamp_to_obj(#timesheet{id = ID, project_id = ProjectID, company_id = Company, user_id = UserID,
  date = Date, spent_time = TSpent, comment = Comment, date_time = DateTime})->
  st_obj:new(ID, [
    {?ST_TIMESTAMP_USER_ID,UserID},{?ST_TIMESTAMP_PROJECT_ID,ProjectID},{?ST_TIMESTAMP_COMPANY_ID, Company},
    {?ST_TIMESTAMP_DATE,Date},{?ST_TIMESTAMP_TIME_SPENT,TSpent},{?ST_TIMESTAMP_COMMENT,Comment},
    {?ST_TIMESTAMP_DATETIME,DateTime}
  ]).

session_to_obj(#session{id = ID, company = Company, user = User, login = Login, udata = Data})->
  st_obj:new(ID, [
    {?ST_SESSION_ID,ID},{?ST_SESSION_COMPANY_ID,Company},{?ST_SESSION_LOGIN,Login},
    {?ST_SESSION_USER_ID,User},{?ST_SESSION_DATA,Data}
  ]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
