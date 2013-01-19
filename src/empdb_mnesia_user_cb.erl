%% EMPDB_Mnesia_User_CB.erl
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
%%  User based callbacks for the Mnesia backend of EMP Database. This module 
%%  does all the reading and writing to the User and Session tables. 
%% @end
%% ----------------------------------------------------------------------------
-module(empdb_mnesia_user_cb).

-include("empdb.hrl").
-include("empdb_mnesia_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include_lib("empd/include/empinternal.hrl").

% EMPDB API Callbacks
-export([ 
    get_root_user/0,            % User Management
    add_user/2, 
    rm_user/1, 
    get_users/0,
    user_login/1, user_login/2, % User Session Control 
    user_logout/1,
    verify_session/1
]).


%%%
%%% ======================================================================
%%% EMPDB Callback Functions
%%% ======================================================================
%%%

%% @doc Return the root user for the emp system.
-spec get_root_user() -> possible_failure( 'EMPUSER'() ).
get_root_user() ->
    ?EMPUSER( ?ROOT_USER_ID, ?ROOT_USER_NAME ).

%% @doc Create entry in users table.
-spec add_user( binary(), useroptions() ) -> possible_failure( ok ).
add_user( Name, InfoOptions ) ->
    UserId = empdb:gen_uuid(),
    case gen_password( InfoOptions ) of
        {ok, Pass} -> ok;
        {error, _} -> Pass=null
    end, 
    case gen_pubkey( InfoOptions ) of
        {ok, Key} -> ok;
        {error, _} -> Key=null
    end,
    if %TODO: Check that Username does not already exist?
        Pass =:= null andalso Key =:= null -> 
            {error, <<"Missing password">>};
        true ->
            empdb_mnesia:run_tran( fun() -> 
                mnesia:write(#user_table{
                            user_id=UserId,
                            username=Name,
                            password_hash=Pass,
                            public_key=Key}) 
            end)
    end.

%% @doc Remove entry in users table.
-spec rm_user( binary() ) -> possible_failure( ok ).
rm_user( Name ) -> 
    empdb_mnesia:run_tran(fun() ->
            List = mnesia:match_object(#user_table{username = Name, _ = '_'}),
            lists:foreach(fun(X) -> mnesia:delete_object(X) end, List)
    end).
    

%% @doc Get all user objects in the database.
-spec get_users() -> possible_failure( ['EMPUSER'()] ).
get_users() ->
    F = fun() ->
            Q = qlc:q([ ?EMPUSER(U#user_table.user_id, 
                                 U#user_table.username) 
                        || U <- mnesia:table(user_table), 
                           U#user_table.user_id =/= ?ROOT_USER_ID]),
            qlc:e(Q)
    end,
    empdb_mnesia:run_tran(F).


%%%
%%% USER SESSION CONTROL
%%%

%% @doc Log a user in and return their user object and the new session id.
-spec user_login( binary() ) -> 
            possible_failure( {ok, 'EMPUSER'(), 'UUID'()} ).
user_login( PUBKEY ) ->
    F = fun() ->
            Q = qlc:q([
                    ?EMPUSER(U#user_table.user_id,
                                 U#user_table.username)
                    || U <- mnesia:table(user_table),
                    U#user_table.public_key =:= PUBKEY]),
            qlc:e(Q)
    end,
    case mnesia:transaction(F) of 
        {aborted, Reason} ->
            {error, Reason};
        {atomic, Res} ->
            case length(Res) of
                1 -> % insert the session since user exists.
                    SessionId = create_new_session(Res#user_table.user_id),
                    {ok, Res#user_table.username, SessionId};
                _ ->
                    {error, badarg} %TODO: is there a better error?
            end
    end.

%% @doc Log-in a user and start their session using username and password.
-spec user_login( binary(), binary() ) -> 
          possible_failure( {ok, 'EMPUSER'(), 'UUID'()} ).
user_login( UName, PHash ) -> 
    F = fun() ->
            Q = qlc:q([ ?EMPUSER(U#user_table.user_id,
                                 U#user_table.username)
                    || 
                    U <- mnesia:table(user_table),
                    U#user_table.username =:= UName, 
                    U#user_table.password_hash =:= PHash]),
            qlc:e(Q)
    end,
    case mnesia:transaction(F) of 
        {aborted, Reason} ->
            {error, Reason};
        {atomic, Res} ->
            case length(Res) of
                1 -> %insert the session since user exists
                    SessionId = create_new_session(Res),
                    {ok, hd(Res), SessionId};
                _ -> 
                    {error,badarg} %TODO: is there a better error?
            end
    end.

%% @doc Ends a valid session.
-spec user_logout( 'UUID'() ) -> possible_failure( ok ).
user_logout( SessionID ) -> 
    F = fun() ->
            Q = qlc:q([U || U <- mnesia:table(active_sessions),
                    U#active_sessions.session_id =:= SessionID]),
            Old = qlc:e(Q),
            Prev = #prev_sessions{
                user_id = Old#active_sessions.user_id,
                session_id = Old#active_sessions.session_id,
                session_start = Old#active_sessions.session_start,
                session_length = Old#active_sessions.session_length},
            mnesia:write(Prev),
            mnesia:delete_object(Old#active_sessions.user_id)
    end,
    empdb_mnesia:run_tran(F).

%% @doc 
%%  Verify that a session is actually in the database and within the time 
%%  limit.
%% @end
-spec verify_session( 'UUID'() ) -> possible_failure( ok ).
verify_session(SessionId)->
    F = fun() -> %TODO: check start time is within bounds and not old.
            Q = qlc:q([
                    {U#user_table.username,
                     S} 
                    ||  S <- mnesia:table(active_sessions),
                        U <- mnesia:table(user_table),
                    S#active_sessions.session_id =:= SessionId,
                    U#user_table.user_id =:= S#active_sessions.user_id]),
            qlc:e(Q)
    end,
    case mnesia:transaction(F) of 
        {aborted, Reason} ->
            {error, Reason};
        {atomic, Res} ->
            %only want one result
            case length(Res) of 
                1 -> ok;
                _ -> {error, badarg}
            end
    end.

%%%
%%% ======================================================================
%%% Private Functions
%%% ======================================================================
%%%

%% @private
%% @doc Create a new session paired with a user id.
create_new_session(UserId) ->
    SessionId = empdb:gen_uuid(),
    SessionStart = erlang:now(),
    F = fun() ->
            mnesia:write(#active_sessions{
                    session_id= SessionId, 
                    user_id=UserId, 
                    session_start=SessionStart, 
                    session_length=0})
    end,
    empdb_mnesia:run_tran(F,SessionId).

%% @hidden Grab the password from the user option list, see add_user/2.
gen_password( Options ) ->
    case lists:keytake(password, 1, Options) of
        {value, {_,Pass}} -> {ok, ?PASS( Pass )};
        false -> 
            case lists:keytake(passhash, 1, Options ) of
                {value, {_, Hash}} -> {ok, Hash};
                false -> {error, <<"No password given.">>}
            end
    end.

%% @hidden Grab the public key from the user option list, see add_user/2.
gen_pubkey( Options ) ->
    case lists:keytake(pubkey, 1, Options) of
        {value, {_,Key}} -> {ok, Key};
        false -> {error, badarg}
    end.