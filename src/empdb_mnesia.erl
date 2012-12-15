%% EMPDB_Mnesia.erl
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
%%  This is the entry point for the Mnesia backend for the EMP Database. All
%%  this process does is monitor the database and make sure it starts and stops
%%  when it does.
%% @end
%% ----------------------------------------------------------------------------
-module( empdb_mnesia ).
-behavior( gen_server ).
-include( "empdb.hrl" ).

%
% Primary module for starting and stopping the Mnesia database on startup.
%

% TODO: Expand to handle stateful transactions?

%% gen_server API
-export([start_link/2]).

%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Private exports
-export([ run_tran/1, % Run a transaction function with a userid 
          run_tran/2  %  over the database. 
        ]). 

%%%
%%% ======================================================================
%%% Server API
%%% ======================================================================
%%%

%% @doc Starts and links the EMP Mnesia backend monitor.
-spec start_link( [atom(),...], term() ) -> {ok, pid()} | {error, any()}.
start_link( Nodes, Args ) ->
    emplog:debug("Starting EMP_MNESIA on the following nodes: ~p",[Nodes]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{nodes, Nodes}|Args], []).


%% @private
%% @doc Verifies the tables are existant or throws an exception.
init( State ) -> 
    start_mnesia( State ),
    {ok, State, hibernate}.

%% @private
%% @doc `gen_server' callback. Unused.
handle_call(_Request, _From, State) ->  {noreply, State}.

%% @private
%% @doc `gen_server' callback. Unused.
handle_cast(_Request, State ) -> {noreply, State}. 

%% @private
%% @doc `gen_server' callback. Unused.
handle_info(_Info, State) -> {noreply, State}.

%% @private
%% @doc Shuts down Mnesia.
terminate(_Reason, State)-> 
    case lists:keysearch(nodes, 1, State) of
        {value, {nodes,Nodes}} ->
            rpc:multicall(Nodes,application,stop,[mnesia]);
        _ -> ok
    end,
    shutdown.

%% @private
%% @doc `gen_server' callback. Unused.
code_change(_OldVsn, State, _Extra)-> {ok, State}.


%%%
%%% ======================================================================
%%% Module Functions
%%% ======================================================================
%%%
 
%% @doc Run a transaction on the mnesia database and expect 'ok' back.
-spec run_tran( fun() ) -> possible_failure( ok ).
run_tran(F) ->
    run_tran(F,ok).

%% @doc 
%%  Run a transaction on the mnesia database and set what the default 
%%  return value is.
%% @end
-spec run_tran( fun(), T ) -> possible_failure( T ) when T :: any().
run_tran(F, Ret) ->
    try mnesia:transaction(F) of
        {aborted, Reason} ->
            {error, Reason};
        {atomic, [Res]} ->
            Res;
        {atomic, ok} ->
            Ret;
        {atomic, Drop} ->
            Drop
    catch
        error: Reason ->
                {error, Reason}
    end.

%% @hidden Start and wait for the nodes to set up their db.
-spec start_mnesia( [{atom(),any()}] ) -> ok.
start_mnesia( State ) ->
    case lists:keyfind( nodes, 1, State ) of 
        {value,{nodes,Nodes}} ->
            rpc:multicall(Nodes, application, start, [mnesia]);
        _ -> ok
    end,
    mnesia:wait_for_tables([emp_vars,
                            user_table,
                            plugin_table,
                            target_table,
                            command_table,
                            event_table,
                            subscription_table,
                            active_sessions,
                            prev_sessions], 5000),
    ok.