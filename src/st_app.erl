-module(st_app).

-behaviour(application).

-define(YAWS_ROOT_PATH, "priv/yaws/www").
-define(YAWS_LOG_DIR, "log/yaws").
-define(YAWS_PORT,8080).
-define(YAWS_LISTEN,{127,0,0,1}).
-define(YAWS_APPMODE_CONFIG, [{"/appmode", st_web_api, [["login.yaws"],["login_post.yaws"],
																									["main.html"]]}]).
-define(YAWS_ARG_REWRITE_MOD,st_yaws_arg_rewrite).


%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs)->
  lager:info("starting application"),
	lager:debug("testing debug level"),
	{ok, SCList, GC, ChildSpecs} = configure_yaws(),
	lager:info("starting simple_timesheet application"),
	case application:get_env(simple_timesheet, tables) of
		{ok,L} when is_list(L) ->
			R = mnesia:wait_for_tables(L,5000),
			case R of
				ok ->
					lager:info("all tables are ready: ~p", [L]);
				Err ->
					lager:error("wait for tables failed with reason: ~p", [Err])
			end;
		_ ->
			lager:error("where is no tables to be accessed\n")
	end,
	st_sup:start_link(ChildSpecs),
	yaws_api:setconf(GC, SCList),
	{ok,self()}.

stop(_State) ->
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

configure_yaws()->
	lager:debug("configure yaws as embedded app"),
	YawsGlobalConfList = [
	{logdir, ?YAWS_LOG_DIR}
	% {id, "my_server"}
	],
	YawsDocRoot = ?YAWS_ROOT_PATH,
	YawsServerConfList = [
		{docroot,YawsDocRoot},
    {port, ?YAWS_PORT},
    {listen, ?YAWS_LISTEN},
		{appmods,?YAWS_APPMODE_CONFIG},
		{arg_rewrite_mod, ?YAWS_ARG_REWRITE_MOD}
	],
	yaws_api:embedded_start_conf(YawsDocRoot,YawsServerConfList,YawsGlobalConfList).
