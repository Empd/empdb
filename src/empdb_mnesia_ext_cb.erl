%% EMPDB_Mnesia_Ext_CB.erl 
%%
%% @copyright 2011-2012 The EMP Group <http://www.emp-d.com/>
%% @end
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either 
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.
%% 
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library.  If not, see <http://www.gnu.org/licenses/>.
%% ----------------------------------------------------------------------------
%% @see empdb. <em>EMP DB Module</em>
%%
%% @doc 
%%  The Mnesia callbacks for Extension (plugins and interfaces) state 
%%  persistence from the EMP Database.  
%% @end
%% ----------------------------------------------------------------------------
-module(empdb_mnesia_ext_cb).

-include("empdb.hrl").
-include("empdb_mnesia_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include_lib("$EMP_ROOT/lib/empinternal.hrl").

-export([
    emp_get_var/1,              % Internal State Persistence
    emp_get_vars/0, 
    emp_set_var/2,
    
    plugin_get_var/3,           % Plugin State Persistence
    plugin_set_var/4,        
    plugin_get_config/3, 
    plugin_state_saver/3, 
    plugin_load_prev_state/2,
    
    get_plugin_defs/1,           % Extensions Management
    get_target/2,
    get_commands/1,
    get_events/1
]). 


%%%
%%% ======================================================================
%%% Internal State Callbacks
%%% ======================================================================
%%%

%% @doc Grab a particular system variable from the state store. 
-spec emp_get_var( atom() ) -> possible_failure( term() ).
emp_get_var(VarName) ->
    F = fun() ->
            Q = qlc:q([V#emp_vars.val || V <- mnesia:table(emp_vars),
                    V#emp_vars.var =:= VarName]),
            qlc:e(Q)
    end,
    empdb_mnesia:run_tran(F).

%% @doc Get all system variables from the state store.
-spec emp_get_vars() -> possible_failure( [ {atom(), term()} ] ).
emp_get_vars() ->
    F = fun() ->
            Q = qlc:q([{V#emp_vars.var, 
                        V#emp_vars.val} || V <- mnesia:table(emp_vars)]),
            qlc:e(Q)
    end,
    empdb_mnesia:run_tran(F).

%% @doc Set a particular system variable in the state store.
-spec emp_set_var( atom(), term() ) -> possible_failure( term() ).
emp_set_var(VarName, NewVal) ->
    F = fun() ->
            mnesia:write(emp_vars,#emp_vars{var=VarName, val=NewVal},write)
    end,
    empdb_mnesia:run_tran(F, NewVal).


%%%
%%% ======================================================================
%%% Plugin State Callbacks
%%% ======================================================================
%%%

%% @doc Get a particular plugin variable for a given plugin and user.
-spec plugin_get_var( 'EMPPLUGINDEF'(), 'EMPUSER'(), binary() ) -> 
            possible_failure( term() ).
plugin_get_var(Plugin, User, VarName) -> 
    Res = empdb:run_tran( fun() ->
                  Q = qlc:q([ Ps#target_table.state || 
                              Ps<-mnesia:table(target_table),
                              Ps#target_table.user_id =:= User#user.uid,
                              Ps#target_table.plug_id =:= Plugin#plugindef.id
                            ]),
                  qlc:e(Q)
          end),
    case catch lists:keytake(VarName,1,Res) of
        {value, {_,Val}} -> Val;
        _ -> throw(badarg)
    end.
    
%% @doc Set a particular plugin variable for a given plugin and user.
-spec plugin_set_var( 'EMPPLUGINDEF'(), 'EMPUSER'(), binary(), term() ) ->
            possible_failure( ok ).
plugin_set_var(Plugin, User, VarName, NewVal) ->
    case 
        empdb_mnesia:run_tran( fun() ->
            Q = qlc:q([ Ps || 
                        Ps<-mnesia:table(target_table),
                        Ps#target_table.user_id =:= User#user.uid,
                        Ps#target_table.plug_id =:= Plugin#plugindef.id
                      ]),
            qlc:e(Q)
        end)
    of 
        {error, Res} -> {error, Res};
        Row ->
            NewRow = Row#target_table{
                            state = lists:keyreplace( VarName, 1, 
                                                      Row#target_table.state,
                                                      {VarName, NewVal} )
                        },
            empdb_mnesia:run_tran( fun()-> mnesia:write(NewRow) end )
    end.

%% @doc Reset a particular plugin variable for a given plugin and user.
-spec plugin_get_config( 'EMPPLUGINDEF'(), 'EMPUSER'(), binary() ) -> 
            possible_failure( term() ).
plugin_get_config(Plugin, User, VarName) ->
    Res = empdb:run_tran( fun() ->
                  Q = qlc:q([ Ps#target_table.cfg_vals || 
                              Ps<-mnesia:table(target_table),
                              Ps#target_table.user_id =:= User#user.uid,
                              Ps#target_table.plug_id =:= Plugin#plugindef.id
                            ]),
                  qlc:e(Q)
          end),
    case catch lists:keytake(VarName,1,Res) of
        {value, {_,Val}} -> Val;
        _ -> throw(badarg)
    end.
    

%% @doc Update all plugin variables for a given plugin and user.
-spec plugin_state_saver( 'EMPPLUGINDEF'(), 
                          'EMPUSER'(), 
                          [{binary(),term()}] ) ->
            possible_failure( ok ).
plugin_state_saver(Plugin, User, State) ->
    case 
        empdb_mnesia:run_tran( fun() ->
            Q = qlc:q([ Ps || 
                        Ps<-mnesia:table(target_table),
                        Ps#target_table.user_id =:= User#user.uid,
                        Ps#target_table.plug_id =:= Plugin#plugindef.id
                      ]),
            qlc:e(Q)
        end)
    of 
        {error, Res} -> {error, Res};
        Row ->
            empdb_mnesia:run_tran( 
              fun()-> 
                mnesia:write(Row#target_table{ state = State }) 
              end )
    end.

%% @doc Grab all plugin variables for a given plugin and user.
-spec plugin_load_prev_state('EMPPLUGINDEF'(), 'EMPUSER'() ) ->
            possible_failure( [{binary(),term()}] ).
plugin_load_prev_state(Plugin, User)->
    empdb_mnesia:run_tran( fun() ->
            Q = qlc:q([ Ps#target_table.state || 
                        Ps<-mnesia:table(target_table),
                        Ps#target_table.user_id =:= User#user.uid,
                        Ps#target_table.plug_id =:= Plugin#plugindef.id
                      ]),
            qlc:e(Q)
        end).

%% @doc Get all the active plugin definitions for a given user.
-spec get_plugin_defs( 'EMPUSER'() ) -> possible_failure( ['EMPPLUGINDEF'()] ).
get_plugin_defs( User ) -> 
    case get_targets( User ) of
        {error, Reason} -> {error, Reason};
        Targets -> lists:foldl(fun build_plugdefs/2, [], Targets)
    end.

%% @doc Grab the target string for a particular user and plugin.
-spec get_target( 'EMPUSER'(), 'EMPPLUGINDEF'() ) ->
          possible_failure( binary() ).
get_target( User, PluginDef ) ->
    strip_target(PluginDef, get_targets(User)).

%% @doc Gets the list of EMPCOMMAND objects for a given Plugin ID.
-spec get_commands( 'UUID'() ) -> possible_failure( [ 'EMPCOMMAND'() ] ).
get_commands( PID ) ->
    case
        empdb_mnesia:run_tran( fun() ->
            Q = qlc:q([ Cmd || 
                        Cmd<-mnesia:table(command_table),
                        Cmd#command_table.plug_id =:= PID
                      ]),
            qlc:e(Q)
        end)
    of
        {error, _Reason} -> [];
        Cmds -> lists:foldl(fun cmd_builder/2, [], Cmds) 
    end.

%% @doc Gets the list of EMPEVENT objects for a givent Plugin ID.
-spec get_events( 'UUID'() ) -> possible_failure( [ 'EMPEVENT'() ] ).
get_events( PID ) ->
    case 
        empdb_mnesia:run_tran( fun() ->
            Q = qlc:q([ Evnt ||
                        Evnt<-mnesia:table(event_table),
                        Evnt#event_table.plug_id =:= PID
                      ]),
            qlc:e(Q)
        end)
    of
        {error, _Reason} -> [];
        Events -> lists:foldl(fun event_builder/2, [], Events)
    end.

%%
%% ===========================================================================
%% Private functions
%% ===========================================================================
%%

%% @hidden Pulls all plugins for a particular user.
get_targets( User ) ->
    empdb_mnesia:run_tran( 
        fun() ->
            Q = qlc:q([ Ps || 
                        Ps<-mnesia:table(target_table),
                        Ps#target_table.user_id == User#user.uid
                      ]),
            qlc:e(Q)
        end).

%% @hidden 
%% Takes a target_table record and produces a plugindef record and pushes it 
%% onto the Acc. Called from get_plugin_defs/2.
build_plugdefs( Target, Acc ) ->
    Pid = Target#target_table.plug_id,
    [?EMPPLUGINDEF(Pid,
                   get_module_name(Pid),
                   Target#target_table.state,
                   get_commands(Pid))
     | Acc].
            
%% @hidden Gets the module name for a given plugin id.
get_module_name( PID ) ->
    case
        empdb_mnesia:run_tran( fun() ->
            Q = qlc:q([ Pt#plugin_table.module_name || 
                        Pt<-mnesia:table(plugin_table),
                        Pt#plugin_table.plug_id =:= PID
                      ]),
            qlc:e(Q)
        end)
    of
        {error, _Reason} -> throw(badarg);
        Module -> Module
    end.

%% @hidden 
%% Takes a command_table record and produces a command record and pushes it
%% onto the Acc. Called from get_commands/1.
cmd_builder( Cmd, Acc ) ->
    [ ?EMPCOMMAND(
        Cmd#command_table.name,
        Cmd#command_table.function,
        Cmd#command_table.params,
        Cmd#command_table.returns )
     | Acc ].

%% @hidden
%% Takes an event_table record and produces a command record and pushes it
%% onto the Acc. Called from get_events/1.
event_builder( Event, Acc ) ->
    [
     ?EMPEVENT(
        Event#event_table.event_name,
        Event#event_table.params
     )
    | Acc].

%% @hidden From a list of target_table records, pulls just the target string.
strip_target(_, []) ->
    {error, badarg};
strip_target(PlugDef, [T|R]) ->
    if PlugDef#plugindef.id == T#target_table.plug_id -> 
           T#target_table.target;
       true -> 
           strip_target(PlugDef, R)
    end.
