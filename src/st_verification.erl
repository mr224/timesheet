-module(st_verification).
-include("../include/st_types.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([verificate_user/5, verificate_user_updates/1]).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

verificate_user(Login,Password,Name,Email,Phone)->
  PL = new_user_verification(Login,Password,Name,Email,Phone),
  verificate(PL).

verificate_user_updates(UpdatesList)->
  PL = verification_list(UpdatesList),
  verificate(PL).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
verificate(PL)->
  verificate(PL,[]).

verificate([],[])->
  {ok,true};
verificate([],Acc)->
  {false, Acc};
verificate([{_Name,Value,F}|Rest],Acc)->
  case F(Value) of
    {ok,true}->
      verificate(Rest,Acc);
    {false,Reason}->
      verificate(Rest,[Reason|Acc])
  end.

new_user_verification(Login,Password,Name,Email,Phone)->
  PL = [
    {login,Login},
    {password,Password},
    {name,Name},
    {email,Email},
    {phone,Phone}
  ],
  verification_list(PL).

verification_list(UpdatesList)->
  Fields = verification_fields(),
  lists:foldl(fun({K,V},Acc)->
    case proplists:get_value(K,Fields) of
      undefined ->
        Acc;
      F ->
        [{K,V,F}|Acc]
    end
  end,[],UpdatesList).

verificate_login(Login)->
  {ok,true}.

verificate_password(Login)->
  {ok,true}.

verificate_name(Login)->
  {ok,true}.

verificate_email(Login)->
  {ok,true}.

verificate_phone(Login)->
  {ok,true}.


verification_fields()->
  [
    {login,fun(E) -> verificate_login(E) end},
    {password,fun(E) -> verificate_password(E) end},
    {name,fun(E) -> verificate_name(E) end},
    {email,fun(E) -> verificate_email(E) end},
    {phone,fun(E) -> verificate_phone(E) end}
  ].