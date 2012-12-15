%% EMPDB_Sup.erl 
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
%%  The top level EMP Database supervisor. It just verifies that the EMPDB 
%%  process lanched via empdb:start_link/2 is stays running.
%% @end
%% ----------------------------------------------------------------------------
-module(empdb_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Starts the EMPDB worker processes, and monitors them.
start_link( Nodes, Args ) ->
    process_flag(trap_exit, true),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Nodes, Args]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @private
%% @doc `gen_server' callback for start_link.
init(Args) ->
    {ok, { {one_for_one, 5, 10}, 
           [ ?CHILD( empdb, worker, Args ) ]} }.

