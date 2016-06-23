-module(st_api).
-include("../include/st_types.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

%% Working with user
-export([get_user_obj/1]).

-export([log_work/5,delete_timestamp/1,update_timestamp/2, get_user_timesheet_for_period/3]).
%%% queries
-export([select_all_from_table/1, select_all_timestamps_for_user/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_user_obj(UserID)->
  st_db_helper:get_user_data(UserID).

log_work(UserID,ProjectID,Date,TSpent,Comment)->
  ID = st_data_util:generate_key(),
  Timestamp = st_obj:new(ID, [
    {?ST_TIMESTAMP_USER_ID,UserID},{?ST_TIMESTAMP_PROJECT_ID,ProjectID},{?ST_TIMESTAMP_DATE,Date},
    {?ST_TIMESTAMP_TIME_SPENT,TSpent},{?ST_TIMESTAMP_COMMENT,Comment}
  ]),
  st_db_helper:save_new_timestamp(Timestamp).

delete_timestamp(ID)->
  st_db_helper:delete_timestamp(ID).

update_timestamp(ID,UpdatesList) when is_list(UpdatesList)->
  st_db_helper:update_timestamp(ID,UpdatesList).

select_all_from_table(TableName)->
	st_db_helper:select_all(TableName).

select_all_timestamps_for_user(UserID)->
  st_db_helper:select_all_timestamps_for_user(UserID).

get_user_timesheet_for_period(User,From,To)->
	st_db_helper:select_all_user_data_for_period(?TABLE_TIMESHEET,User,From,To).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
