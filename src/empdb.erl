%% EMPDB.erl 
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
%% @see empdb_mnesia. <em>EMP's Mnesia Backend</em>
%%
%% @doc 
%%  This is the primary entry point for the EMP Database server. All callbacks 
%%  are handled here and in subsequent backends.
%%  <br/><br/>
%%  EMPDB is merely a wrapper module that calls through to the actual
%%  implementation of the database. This makes it fairly simple to swap out
%%  your own solution for EMP's database. The way this works is through macros
%%  that the preprocessor with swap out with the currently used database 
%%  modules. 
%%  <br/><br/>
%%  If you want to implement your own backend, just swap out the macros at the 
%%  of empdb.erl with your own packages and recompile empdb. Hotswaping the 
%%  databases is as easy as reloading the new version of empdb, it will 
%%  automatically redirect call throughs. 
%%  <br/><br/>
%%  You do not have to hotswap all of empdb if you need to fix a particular
%%  function in your backend, just hotswap the individual module. Call
%%  throughs will happen like normal.
%% @end
%% ----------------------------------------------------------------------------
-module(empdb).

-include("empdb.hrl").
-include_lib("empd/include/empinternal.hrl").

%%% ======================================================================
%%% Edit these definitions if Syou would like to utilize a different  
%%% backend for EMP. Make sure all type specs are correct or you could
%%% cause errors in the EMP server!
%%% ======================================================================

% This module will start a monitoring process for starting/stopping your db.
-define( EMPDB_LOADER,    empdb_mnesia ).
% This is where all the required callbacks are.
-define( EMPDB_USER_CALLBACKS, empdb_mnesia_user_cb ).
-define( EMPDB_EXTENSION_CALLBACKS, empdb_mnesia_ext_cb ).
-define( EMPDB_SUBSCRIPTION_CALLBACKS, empdb_mnesia_sub_cb ).
-define( EMPDB_INSTALLATION_CALLBACKS, empdb_mnesia_install ).
% This is where the installation specific callbacks are.
-define( EMPDB_INSTALLER, empdb_mnesia_install ).


% Installation & Startup verification Callbacks 
-export( [ verify/2, install/2 ] ).

% EMPDB Public Functions
-export( [ start/0, stop/0, gen_uuid/0, gen_uuid2/0 ]).

% EMPDB OTP Callbacks
-export( [ start_link/2 ] ).

% EMPDB API Callbacks
-export([ 
    get_root_user/0,            % User Management
    add_user/2, 
    rm_user/1, 
    get_users/0,
    user_login/1, user_login/2, % User Session Control 
    user_logout/1,
    verify_session/1,
    add_subscription/4,         % Subscription Management
    rm_subscription/1, 
    get_subscriptions/2,
    register_plugin/4,          % Extension Management
    get_plugin_defs/1,
    get_target/2,
    get_commands/1,
    get_events/1,
    install_plugin/2,
    uninstall_plugin/1,
    emp_get_var/1,              % Internal State Persistence
    emp_get_vars/0, 
    emp_set_var/2,
    plugin_get_var/3,           % Plugin State Persistence
    plugin_set_var/4,        
    plugin_get_config/3, 
    plugin_state_saver/3, 
    plugin_load_prev_state/2
]). 

%%%
%%% ======================================================================
%%% Public Server Functions
%%% ======================================================================
%%%

%% @doc Utility function for calling application:start/1 on empdb.
-spec start() -> ok.
start()->
    application:start(empdb).

%% @doc Utility function for calling application:stop/1 on empdb.
-spec stop() -> ok.
stop()->
    application:stop(empdb).

%% @doc Follows RFC4122 for generating UUIDs version 4 via Random Numbers. 
%% This function will perform fairly poorly on a 32 bit vm as the random
%% number generation range is larger than the 28 bit word space on 32 bit
%% machines. www.erlang.org/doc/efficiency_guide/advanced.html
%% @end
-spec gen_uuid() -> binary().
gen_uuid()->
    % 1.) set the two most significant bits (bits 6 and 7) of the 
    % clock_seq_hi_and_reserved to zero and 1, respectively.
    % 2.) Set the four most significant bits (bits 12 through 15) of the
    % time_hi_and_version field to the 4-bit version number from
    % Section 4.1.3.
    % 3.) Set all the other bits to randomly (or pseudo-randomly) chosen
    % values.
    Time_Low_Mid = random:uniform(140737488355327)-1, %48
    Time_Hi = random:uniform(2047)-1, %12
    Nodept1 = random:uniform(2147483647)-1, %32
    Nodept2 = random:uniform(536870912)-1, %30
    <<
      Time_Low_Mid:48, % Time_Low (32 bit) & Time_Mid(16 bit)
      4:4, Time_Hi:12, % Time_hi_and_version(16 bit)
      2:2, Nodept1:32, Nodept2:30 % Clock_seq_hi_and_reserved(8 bit) & Clock_seq_low(8 bit) &  Node(48 bit)
    >>.

%% @doc Just like gen_uuid/0 it generates UUIDs using version 4. However,
%% the difference here is that it uses erlang's crypto module instead of
%% the random module. It is a 'bit more random', but it takes twice as  
%% long on average.
%% @end
-spec gen_uuid2() -> binary().
gen_uuid2()->
    <<A:48,B:12,C:62,_:6>>=crypto:rand_bytes(16),
    <<A:48,4:4,B:12,2:2,C:62>>.


%%%
%%% ======================================================================
%%% EMPDB Callthrough Functions
%%% ======================================================================
%%%

%% @doc 
%%  Starts the EMP DB system by calling the start_link/2 function on the
%%  specified backend module. This should return a PID of the running
%%  db server so that the application knows what to monitor.
%% @end
-spec start_link( nonempty_nodelist(), [tuple()] ) -> 
            possible_failure({ok, pid()}).
start_link( Nodes, Args ) -> 
    ?EMPDB_LOADER:start_link(Nodes,Args).

%% @doc
%%  Verify that your database is installed on at least one of the nodes
%%  and can be communicated to by all of them. This verification can take
%%  some time, as this is only called once on EMPDB startup.
%% @end
-spec verify( nonempty_nodelist(), [tuple()] ) -> 
            ok |
            {install, nonempty_nodelist()} | 
            {error, continue | shutdown, Reason :: any()}.
verify( Nodes, Options ) -> ?EMPDB_INSTALLER:verify(Nodes,Options).

%% @doc
%%  Install the database on a set of nodes with the set of options.
%%  This runs the install/2 call-through on the backend module. We
%%  separate this from verify/2 in case this needs to be run by hand.
%% @end
-spec install( nonempty_nodelist(), [tuple()] ) ->  possible_failure( ok ).
install( Nodes, Options ) -> ?EMPDB_INSTALLER:install(Nodes,Options).

%% @doc
%%  Return the Root user for the emp system.
%% @end
-spec get_root_user() -> possible_failure( 'EMPUSER'() ).
get_root_user() ->  %TODO: push to database call.
    ?EMPUSER( ?ROOT_USER_ID, ?ROOT_USER_NAME ).   

%% @doc 
%%  Add a user to the database, this user will have access to
%%  all plugins and will have their own uman tree.
%% @end
-spec add_user( binary(), useroptions() ) -> possible_failure( ok ).
add_user( Name, InfoOptions ) -> 
    ?EMPDB_USER_CALLBACKS:add_user( Name, InfoOptions ).

%% @doc
%%  Remove a user and all state for them (this includes plugin
%%  and system state). This function can not be reversed. 
%% @end
-spec rm_user( binary() ) -> possible_failure( ok ).
rm_user( Name ) -> ?EMPDB_USER_CALLBACKS:rm_user(Name).

%% @doc 
%%  Gets the list of user objects stored in the database. This does
%%  NOT include the root user. So it is possible for an empty list
%%  to be returned if there are no other users in the system.
%% @end
-spec get_users() -> possible_failure( ['EMPUSER'()] ).
get_users() -> ?EMPDB_USER_CALLBACKS:get_users().

%% @doc
%%  Attempt to log a user in, using their public key. This should return
%%  a user whos public key has been registered with EMP.
%% @end
-spec user_login( binary() ) -> 
          possible_failure( {ok, 'EMPUSER'(), 'UUID'()} ).
user_login( PUBKEY ) -> ?EMPDB_USER_CALLBACKS:user_login(PUBKEY).

%% @doc Standard login function using a username and a password hash.
-spec user_login( binary(), binary() ) -> 
          possible_failure( {ok, 'EMPUSER'(), 'UUID'()} ).
user_login( UName, PHash ) -> ?EMPDB_USER_CALLBACKS:user_login( UName, PHash ).

%% @doc Log a user out using their session ID.
-spec user_logout( 'UUID'() ) -> possible_failure( ok ).
user_logout( SessionID ) -> ?EMPDB_USER_CALLBACKS:user_logout(SessionID).

%% @doc Verify that a session is actually active and not fake.
-spec verify_session( 'UUID'() ) -> possible_failure( ok ).
verify_session(SessionId)->?EMPDB_USER_CALLBACKS:verify_session(SessionId).

%% @doc 
%%  Add a subscription to be stored in the database for persistence.
%%  This function does not need to communicate with EMP.
%% @end
-spec add_subscription( 'EMPUSER'(), 'EMPPLUGINDEF'(), 
                        'EMPPLUGINDEF'(), 'EMPSUBSCRIPTION'() ) ->
            possible_failure( {ok, 'UUID'()} ).
add_subscription(User, TriggerPlug, HandlePlug, Subscription) -> 
    ?EMPDB_SUBSCRIPTION_CALLBACKS:add_subscription( User, TriggerPlug, 
                                                    HandlePlug, Subscription ).

%% @doc Remove a particular subscription by its ID.
-spec rm_subscription( 'UUID'() ) -> possible_failure( ok ).
rm_subscription(SubscriptionId) -> 
    ?EMPDB_SUBSCRIPTION_CALLBACKS:rm_subscription(SubscriptionId).

%% @doc 
%%  Get all the subscriptions that a particular plugin needs to handle 
%%  for a given user.
%% @end
-spec get_subscriptions( 'EMPUSER'(), 'EMPPLUGINDEF'() ) ->
            possible_failure( ['EMPSUBSCRIPTION'()] ).
get_subscriptions(User, HandlePlug) -> 
    ?EMPDB_SUBSCRIPTION_CALLBACKS:get_subscriptions(User, HandlePlug).

%% @doc
%%  Register a plugin so that it gets an ID in the EMP system.
%% @end
-spec register_plugin( atom(), [tuple()], ['EMPCOMMAND'()], ['EMPEVENT'()] ) ->
            possible_failure( {ok, 'UUID'()} ).
register_plugin( ModuleName, Info, Cmds, Events ) -> 
    ?EMPDB_INSTALLATION_CALLBACKS:register_plugin(ModuleName,Info,Cmds,Events).

%% @doc
%%  Get all plugins for a particular user. Used at startup to initialize the
%%  system, so it can take longer.
%% @end
-spec get_plugin_defs( 'EMPUSER'() ) -> possible_failure( ['EMPPLUGINDEF'()] ).
get_plugin_defs( User ) -> ?EMPDB_EXTENSION_CALLBACKS:get_plugin_defs(User).

%% @doc
%%  All users can specify unique targets for their plugins. This is the name
%%  by which they can communicate with the plugin.
%% @end
-spec get_target( 'EMPUSER'(), 'EMPPLUGINDEF'() ) -> 
          possible_failure( binary() ).
get_target( User, Plugin ) -> 
    ?EMPDB_EXTENSION_CALLBACKS:get_target( User, Plugin ).

%% @doc
%%  Get all of the commands for a particular plugin using its ID.
%% @end
-spec get_commands( 'UUID'() ) -> possible_failure( [ 'EMPCOMMAND'() ] ).
get_commands( PlugID ) ->
    ?EMPDB_EXTENSION_CALLBACKS:get_commands( PlugID ).

%% @doc
%%  Get all of the events for a particular plugin using its ID.
%% @end
-spec get_events( 'UUID'() ) -> possible_failure( [ 'EMPEVENT'() ] ).
get_events( PlugID ) ->
    ?EMPDB_EXTENSION_CALLBACKS:get_events( PlugID ).

%% @doc 
%%  Installs a plugin by copying all files into the database to be replicated 
%%  on all nodes attached to the EMP node. This is so that any drivers or 
%%  runtime binaries can be transfered along with the Erlang modules when 
%%  distributing EMP's Plugin computation.
%%  <br/><br/>
%%  <em>WARNING:</em> Please note that this functionality is alpha and 
%%  still definitely in testing, do not rely on this.
%% @end
-spec install_plugin( atom(), [binary()] ) -> possible_failure( ok ).
install_plugin(ModuleName, Files) -> 
    ?EMPDB_INSTALLATION_CALLBACKS:install_plugin(ModuleName, Files).

%% @doc
%%  Removes the files from the database for an installed plugin. This does
%%  not unregister the plugin. 
%% @end
-spec uninstall_plugin( atom() ) -> possible_failure( ok ).
uninstall_plugin(ModuleName)-> 
    ?EMPDB_INSTALLATION_CALLBACKS:uninstall_plugin(ModuleName).

%% @doc 
%%  Grab a system configuration from storage. This is called fairly frequently
%%  and not just at start up. 
%% @end
-spec emp_get_var( atom() ) -> possible_failure( term() ).
emp_get_var(VarName) -> ?EMPDB_EXTENSION_CALLBACKS:emp_get_var(VarName).

%% @doc
%%  Get all system settings from the database. Only called when the system is
%%  queried to do so by an admin.
%% @end
-spec emp_get_vars() -> possible_failure( [ {atom(), term()} ] ).
emp_get_vars() -> ?EMPDB_EXTENSION_CALLBACKS:emp_get_vars().

%% @doc
%%  Set a system setting in the database, will add it if its not there. 
%%  Infrequently called, normally only if a change is made by the admin.
%% @end
-spec emp_set_var( atom(), term() ) -> possible_failure( term() ).
emp_set_var(VarName, NewVal) ->?EMPDB_EXTENSION_CALLBACKS:emp_set_var(VarName, NewVal).

%% @doc
%%  Very frequently called, it returns a saved value for a plugin. 
%% @end
-spec plugin_get_var( 'EMPPLUGINDEF'(), 'EMPUSER'(), binary() ) -> 
            possible_failure( term() ).
plugin_get_var(Plugin, User, VarName) ->
    ?EMPDB_EXTENSION_CALLBACKS:plugin_get_var(Plugin,User,VarName).

%% @doc
%%  Very frequently called, it saved the state of a single value into the
%%  database.
%% @end
-spec plugin_set_var( 'EMPPLUGINDEF'(), 'EMPUSER'(), binary(), term() ) ->
            possible_failure( ok ).
plugin_set_var(Plugin, User, VarName, NewVal) ->
    ?EMPDB_EXTENSION_CALLBACKS:plugin_set_var(Plugin,User,VarName,NewVal).

%% @doc
%%  Plugin Configuration access, these are variables that have been set on 
%%  build and on start-up and can-not be changed during runtime.
%% @end
-spec plugin_get_config( 'EMPPLUGINDEF'(), 'EMPUSER'(), binary() ) -> 
            possible_failure( term() ).
plugin_get_config(Plugin, User, VarName) ->
    ?EMPDB_EXTENSION_CALLBACKS:plugin_get_config(Plugin,User,VarName).

%% @doc
%%  Upon crash or on an interval, EMP will automatically save the running 
%%  state of the plugin in case of crash. The state is always saved when the
%%  plugin is shutdown as well. This is essentially like calling 
%%  plugin_set_var/4 repeatedly.
%% @end
-spec plugin_state_saver( 'EMPPLUGINDEF'(), 'EMPUSER'(), [{binary(),term()}] ) ->
            possible_failure( ok ).
plugin_state_saver(Plugin, User, State) -> 
    ?EMPDB_EXTENSION_CALLBACKS:plugin_state_saver(Plugin,User,State).

%% @doc
%%  Upon plugin startup EMP will resume the old state of the plugin.
%% @end
-spec plugin_load_prev_state( 'EMPPLUGINDEF'(), 'EMPUSER'() ) ->
            possible_failure( [{binary(),term()}] ).
plugin_load_prev_state(Plugin, User)-> 
    ?EMPDB_EXTENSION_CALLBACKS:plugin_load_prev_state(Plugin,User).
