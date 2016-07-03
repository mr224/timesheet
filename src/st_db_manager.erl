%%%-------------------------------------------------------------------
%%% @author mr224
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(st_db_manager).
-author("mr224").
-include("../include/st_types.hrl").


%% API
-export([transform_table_project/0]).

transform_table_project()->
  F = fun({project,Id,Type,Company_id,Name,
    Status,Description,Manager,Time_spent,Assignee,Date_created,
    Time_remaining,Time_estimate,Priority,Author})->
    #project{id = Id,type = Type,company_id = Company_id,name = Name,status = Status,
      description = Description,manager = Manager,time_spent = Time_spent,assignee = Assignee,
      date_created = Date_created,time_remaining = Time_remaining,time_estimate = Time_estimate,
      priority = Priority,author = Author, timestamps = []} end,
  NewFields = record_info(fields,project),
  mnesia:transform_table(project,F,NewFields).
