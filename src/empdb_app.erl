%% EMPDB_App.erl 
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
%% @see empdb_mnesia. <em>EMP's Mnesia Backend</em> 
%%
%% @doc 
%%  The front end OTP Application for the EMP Database application. EMPDB is
%%  the state store and distributed cache for the Extensible Monitoring 
%%  Platform.
%%  <br/><br/>
%%  EMPDB is completely extensible and can be swapped out for other storage
%%  options. However, the default uses MNesia, the erlang native database
%%  solution and provides fault-tolerance through replication. See the EMPDB
%%  module for more information on how to swap out other backends. 
%% @end
%% ----------------------------------------------------------------------------
-module(empdb_app).
-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc Starts the EMP database application after verifying its installation.
start(_StartType, StartArgs) ->
    {Args, Nodes, Options} = clean( StartArgs ),
    verify_install( Nodes, Options ),
    empdb_sup:start_link( Nodes, Args ).

%% @doc Prepares to shutdown.
prep_stop( _State ) ->
    emplog:debug("EmpDB is shutting down.").

%% @doc Stops the EMPDB application
stop(_State) ->
    ok.


%% ===================================================================
%% Private functions
%% ===================================================================

%% @hidden
%% Runs the empdb:install/2 function after verifying that its not installed.
verify_install( Nodes, Options ) -> 
    case empdb:verify( Nodes, Options ) of
        ok -> ok;
        {install, EmptyNodes} ->
            empdb:install( EmptyNodes, Options );
        {error, continue, Reason} ->
            emplog:warn("EMPDB could not be verified: ~p",[Reason]);
        {error, shutdown, Reason} ->
            emplog:error("EMPDB verification error", [Reason]),
            error(Reason)
    end.
    
%% @hidden
%% Cleans the starting arguments for the EMP DB application. 
%% See empdb.app.src.
clean( StartingArgs ) ->
    Nodes   = case lists:keysearch( db_nodes, 1, StartingArgs ) of
                  {value, {db_nodes,N}} -> N; false -> [] end,
    Options = case lists:keysearch( db_init_opts, 1, StartingArgs ) of
                  {value, {db_init_opts,O}} -> O; false -> [] end,
    Args    = case lists:keysearch( db_args, 1, StartingArgs ) of
                  {value, {db_args,A}} -> A; false -> [] end,
    {Args, [node()|Nodes], Options}.
