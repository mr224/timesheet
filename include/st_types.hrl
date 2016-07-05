-define(TABLE_TIMESHEET, timesheet).
-define(TABLE_PROJECT, project).
-define(TABLE_EMPLOYEE, employee).
-define(TABLE_COMPANY, company).
-define(TABLE_MEMBERS, members).
-define(TABLE_SESSION, session).

-define(ST_ID,<<"id">>).
-define(ST_COMPANY, <<"company">>).
-define(ST_PROJECT, <<"project">>).
-define(ST_USER, <<"user">>).
-define(ST_TIMESTAMP, <<"timestamp">>).


-define(ST_USER_PASSWORD_HASH, <<"password_hash">>).
-define(ST_USER_COMPANY,?ST_COMPANY).
-define(ST_USER_LOGIN,<<"login">>).
-define(ST_USER_NAME,<<"name">>).
-define(ST_USER_EMAIL,<<"email">>).
-define(ST_USER_PHONE,<<"phone">>).
-define(ST_USER_DEPARTMENT,<<"department">>).
-define(ST_USER_PROJECTS_ASSIGN,<<"projects_assign">>).
-define(ST_USER_PROJECTS_MANAGER,<<"projects_manager">>).


-define(ST_PROJECT_STATUS_NEW, <<"new">>).
-define(ST_PROJECT_STATUS_CLOSED, <<"closed">>).

-define(ST_PROJECT_PRIORITY_LOW, <<"low">>).
-define(ST_PROJECT_PRIORITY_MEDIUM, <<"medium">>).
-define(ST_PROJECT_PRIORITY_HIGH, <<"high">>).

-define(ST_PROJECT_ID, ?ST_ID).
-define(ST_PROJECT_TYPE, <<"type">>).
-define(ST_PROJECT_COMPANY, ?ST_COMPANY).
-define(ST_PROJECT_NAME, <<"name">>).
-define(ST_PROJECT_STATUS, <<"status">>).
-define(ST_PROJECT_DESCRIPTION, <<"description">>).
-define(ST_PROJECT_MANAGER, <<"manager">>).
-define(ST_PROJECT_TIME_SPENT, <<"time_spent">>).
-define(ST_PROJECT_ASSIGNEE, <<"assignee">>).
-define(ST_PROJECT_DATE_CREATED, <<"date_created">>).
-define(ST_PROJECT_TIME_REMAINING, <<"time_remaining">>).
-define(ST_PROJECT_TIME_ESTIMATE, <<"time_estimate">>).
-define(ST_PROJECT_PRIORITY, <<"priority">>).
-define(ST_PROJECT_AUTHOR, <<"author">>).
-define(ST_PROJECT_TIMESTAMPS, <<"timestamps">>).

-define(ST_COMPANY_NAME, <<"name">>).
-define(ST_COMPANY_ID, ?ST_ID).
-define(ST_COMPANY_EMPLOYEES, <<"employees">>).
-define(ST_COMPANY_PROJECTS, <<"projects">>).
-define(ST_COMPANY_DEFAULT_NAME, <<"my own projects">>).

-define(ST_TIMESTAMP_ID, ?ST_ID).
-define(ST_TIMESTAMP_COMPANY_ID, ?ST_COMPANY).
-define(ST_TIMESTAMP_USER_ID, ?ST_USER).
-define(ST_TIMESTAMP_PROJECT_ID, ?ST_PROJECT).
-define(ST_TIMESTAMP_DATE, <<"date">>).
-define(ST_TIMESTAMP_TIME_SPENT, <<"time_spent">>).
-define(ST_TIMESTAMP_COMMENT, <<"comment">>).
-define(ST_TIMESTAMP_DATETIME, <<"datetime">>).

-define(ST_SESSION_NAME, "st_session").
-define(ST_SESSION_ID, ?ST_ID).
-define(ST_SESSION_COMPANY_ID, <<"company_id">>).
-define(ST_SESSION_USER_ID, <<"user_id">>).
-define(ST_SESSION_LOGIN, <<"login">>).
-define(ST_SESSION_DATA, <<"data">>).

-type id() :: binary().
-type name() :: binary().
-type date() :: binary().
-type datetime() :: binary().
-type data() :: [{binary(),any()}].

-record(timesheet,
{
  id :: id(),
  company_id :: id(),
  user_id :: id(),
  project_id :: id(),
  date :: date(),
  spent_time = 0 :: integer(),
  comment :: binary(),
  date_time :: datetime()
}).

-record(project,
{
  id :: id(),
  type :: binary(),
  company_id :: id(),
  name :: name(),
  status :: binary(),
  description :: binary(),
  manager :: id(),
  time_spent :: number(),
  assignee :: id(),
  date_created :: date(),
  time_remaining :: number(),
  time_estimate :: number(),
  timestamps :: [id()],
  priority :: binary(),
  author :: id()
}).

-record(employee,
{
  id :: id(),
  name :: name(),
  company_id :: id(),
  email :: binary(),
  phone :: binary(),
  projects_assign = [] :: [id()],
  projects_manager = [] :: [id()],
  department :: name()
}).

-record(company,
{
  id :: id(),
  name :: name(),
  employees = [] :: [id()],
  projects = [] :: [id()]
}).

-record(members,
{
  login :: binary(),
  id :: id(),
  password_hash :: binary()
}).

-record(session,
{
  id :: id(),
  user :: id(),
  company :: id(),
  login :: binary(),
  udata = [] :: [{binary(),binary()}]
}).