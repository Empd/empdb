%% EMPDB_Mnesia_Install.erl
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
%% @doc 
%%  This module is called from EMPD.erl, and provides the callbacks devoted
%%  to database verification and installation. It also makes sure all defaults
%%  are inserted into the database too.
%% @end
%% ----------------------------------------------------------------------------
-module(empdb_mnesia_install).

% pull in standard includes
-include("empdb.hrl").
-include("empdb_mnesia_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("$EMP_ROOT/lib/empinternal.hrl").

% export our only public function.
-export([
    install/2,         % System Setup
    verify/2,
    register_plugin/4, % Extension Management
    install_plugin/2,
    uninstall_plugin/1
]).


%%%
%%% ===================================================================
%%% EMPDB Callbacks
%%% ===================================================================
%%%

%% @doc
%%  Installs the EMP Mnesia backend. Uses the options provided by empdb 
%%  application. 
%% @end
-spec install( nonempty_nodelist(), [tuple()] ) ->  possible_failure( ok ).
install( Nodes, Options ) ->
    Opts = case lists:keytake(mnesia_dir, 1, Options) of
               {value,{_,Path},O}-> 
                   Ppath = filename:join([os:getenv("HOME"), ".emp", Path]),
                   filelib:ensure_dir(Ppath),
                   application:set_env(mnesia, dir, Ppath), 
                   O;
               false -> Options
           end,
    create_schema( Nodes ),
    rpc:multicall(Nodes, application, start, [mnesia]),
    make_tables( Nodes, Opts ).


%% @doc
%%  Verify that your database is installed on at least one of the nodes
%%  and can be communicated to by all of them. This verification can take
%%  some time, as this is only called once on EMPDB startup.
%% @end
-spec verify( nonempty_nodelist(), [tuple()] ) -> 
            ok |
            {install, nonempty_nodelist()} | 
            {error, continue | shutdown, Reason :: any()}.
verify( Nodes, _Options ) ->
    %No need to verify, just run installation over all nodes.
    {install, Nodes}.


%% @doc create tables for plugin and interface 
-spec register_plugin( atom(), [tuple()], ['EMPCOMMAND'()], ['EMPEVENT'()] ) ->
            possible_failure( {ok, 'UUID'()} ).
register_plugin( ModuleName, Info, Cmds, Events ) ->
    Pid = empdb:gen_uuid(),
    CmdIds   = gen_uuidlist( length(Cmds) ),
    EventIds = gen_uuidlist( length(Events) ),
    Uids = get_user_ids(),
    {IsSysPlug, Defaults, Target} = parseInfo( Info ),
    InsCmds   = command_writer( Pid ),
    InsEvents = event_writer( Pid ),
    InsTarget = target_writer( Pid, Target, Defaults ),
    F = fun() ->
            % Insert plugin
            mnesia:write(#plugin_table{
                                plug_id=Pid,
                                module_name=ModuleName,
                                sysplug=IsSysPlug,
                                commands=CmdIds,
                                events=EventIds}),
            % Add events and commands
            lists:foldl(InsCmds, CmdIds, Cmds),
            lists:foldl(InsEvents, EventIds, Events),
            % Insert target and user unique state
            lists:foreach(InsTarget, Uids)
        end,
    empdb_mnesia:run_tran(F),
    {ok, Pid}.

%% @doc Upload the binaries of everything to db.
-spec install_plugin( atom(), [binary()] ) -> possible_failure( ok ).
install_plugin(_ModuleName, _Files) -> ok. %TODO: Implement

%% @doc Remove all binaries for a particular plugin from db.
-spec uninstall_plugin( atom() ) -> possible_failure( ok ).
uninstall_plugin(_ModuleName)-> ok. %TODO: Implement


%%%
%%% ===================================================================
%%% Private Functionality
%%% ===================================================================
%%%

%%@hidden
%% Loop over nodes and create the schema, it may fail for a single node
%% But as long as a couple of them succeed; creation could fail becaus
%% the schema already exists on the node. It doesn't matter.
create_schema( Nodes ) ->
    F = fun( N ) ->
            try mnesia:create_schema([N])
            catch 
                exit:{N,{already_exists,N}} -> ok;
                exit:Reason -> 
                    emplog:warn("Mneisa schema creation failed; node(~p)=~p",
                                 [N, Reason]);
                Type:Reason -> 
                    emplog:warn(
                      "Unknown exit type during Mneisa schema creation ~p:~p",
                       [Type, Reason])
            end
    end,
    lists:foreach(F, Nodes).


%% @hidden
%% Creates the default tables in the database, and fills them with 
%% default data.
make_tables( Nodes, Options )->
    emplog:debug("Options for empdb_mnesia_install: ~p",[Options]),
    PersistType = case lists:keysearch(persist, 1, Options) of
                      {value,{persist,P}} -> P;
                      false -> disc_copies
                  end,
    Tabs = [
        {user_table,      record_info(fields, user_table),     [] },
        {plugin_table,    record_info(fields, plugin_table),   [] },
        {command_table,   record_info(fields, command_table),  [] },
        {event_table,     record_info(fields, event_table),    [] },
        {target_table,    record_info(fields, target_table),   [plug_id] },
        {subscription_table, 
                      record_info(fields, subscription_table), [user_id] },
        {emp_vars,        record_info(fields, emp_vars),       [] },
        {active_sessions, record_info(fields, active_sessions),[] },
        {prev_sessions,   record_info(fields, prev_sessions),  [] }
           ],
    F = fun(Tab = {TabName, _, _}, Acc) ->
            I = try mnesia:table_info(TabName, all) catch _:_ -> [] end,
            case length(I) of
                0 -> 
                    create_table(Tab, PersistType, Nodes ),
                    [Tab|Acc];
                _ -> 
                    Acc
            end
    end,
    NewlyMadeTables = lists:foldl(F, [], Tabs),
    if length(NewlyMadeTables) > 0 -> 
           emplog:debug("Finished making tables for the first time.");
       true -> 
           ok
    end.

%% @hidden
%%  Create a table according to spec.
create_table( {TabName, Info, Index}, PType, Nodes ) ->
    emplog:debug("Building table ~p in mnesia.",[TabName]),
    DefaultOpts = [{attributes, Info}, {type, ordered_set}, {PType, Nodes}],
    CrOption = if 
        length(Index) > 0 -> [{index, Index} | DefaultOpts ];
        true -> DefaultOpts 
    end,
    case mnesia:create_table( TabName, CrOption ) of
        {atomic, ok} ->
            fill_table_defaults( TabName );
        {aborted,Reason} ->
            emplog:debug("Failed to create table ~p because: ~p",
                         [TabName, Reason])
    end.


%% @hidden
%% Goes about filling a particular table with its default information.
fill_table_defaults( user_table ) ->
    UserId = ?ROOT_USER_ID,
    F = fun() ->
            mnesia:write(#user_table{
                    user_id=UserId,
                    username=?ROOT_USER_NAME,
                    password_hash=?PASS("toor"),
                    public_key=null}) end,
    empdb_mnesia:run_tran(F,UserId);

fill_table_defaults( _ ) -> ok.

%% @hidden
%% Grab the list of all the user IDs.
get_user_ids() ->
    F = fun() ->
            Q = qlc:q([ U#user_table.user_id || 
                        U <- mnesia:table(user_table)
                      ]),
            qlc:e(Q)
    end,
    empdb_mnesia:run_tran(F).

%% @hidden
%% Generates a list of UUIDs of the size specified.
gen_uuidlist( Size ) -> gen_uuidlist( Size, [] ).
gen_uuidlist( 0, List ) -> List;
gen_uuidlist( Size, List ) when Size > 0 ->
    gen_uuidlist( Size-1, [empdb:gen_uuid()|List] ).


%% @hidden
%% Returns a function that can be used to loop over commands and their IDs to 
%% insert them into the database. Used in register_plugin/4.
command_writer( Pid) ->
    fun( C, [Cid|Rest] ) ->
        mnesia:write(#command_table{
            cmd_id=Cid,
            plug_id=Pid,
            name=C#command.name,
            function=C#command.func,
            params=C#command.dparams,
            returns=C#command.rettype
        }),
        Rest
    end.

%% @hidden
%% Returns a function that can be used to loop over events and their IDs to 
%% insert them into the database. Used in register_plugin/4.
event_writer( Pid ) ->
    fun( E, [Eid|Rest] ) ->
        mnesia:write(#event_table{
                event_id=Eid,
                plug_id=Pid,
                event_name=E#event.name,
                params=E#event.dvalues
            }),
        Rest
    end.

%% @hidden
%% Returns a function that can be used to loop over user ids to insert user 
%% unique state into the database.
target_writer( Pid, Target, Defaults ) ->
    fun( Uid ) -> 
        mnesia:write(#target_table{ 
            user_id=Uid, 
            plug_id=Pid, 
            target=Target, 
            cfg_vals=Defaults 
        })
    end.

%% @hidden
%% Parses the return value from pluginmodule's registration_info/0.
parseInfo( Info ) ->
    {value, {_, Target}} = lists:keytake(default_target, 1, Info ),
    IsSysPlug = case lists:keytake(sys_plugin, 1, Info ) of
                    {value,{_,Val}} -> Val; false -> false
                end,
    Defaults = case lists:keytake(configs, 1, Info ) of
                    {value,{_,Val2}} -> Val2; false -> []
                end,
    {IsSysPlug, Defaults, Target}.
