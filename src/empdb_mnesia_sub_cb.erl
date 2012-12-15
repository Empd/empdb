%% EMPDB_Mnesia_Sub_CB.erl
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
%%  This module contains all of the callbacks for the subscription management
%%  portion of the EMP Database callback list. 
%% @end
%% ----------------------------------------------------------------------------
-module(empdb_mnesia_sub_cb).

-include("empdb.hrl").
-include("empdb_mnesia_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include_lib("$EMP_ROOT/lib/empinternal.hrl").

-export([
    add_subscription/4,         % Subscription Management
    rm_subscription/1, 
    get_subscriptions/2
]).


%% @doc Add a subscription to the database.
-spec add_subscription( 'EMPUSER'(), 'EMPPLUGINDEF'(), 
                        'EMPPLUGINDEF'(), 'EMPSUBSCRIPTION'() ) ->
            possible_failure( {ok, 'UUID'()} ).
add_subscription(User, TriggerPlug, HandlePlug, Subscription) ->
    F = fun() -> 
            mnesia:write(#subscription_table{
                    user_id=User,
                    sub_id=empdb:gen_uuid(),
                    cmd_id=TriggerPlug,
                    event_id=HandlePlug,
                    pred_fun=null,
                    pred_map=[],
                    e2cmap=Subscription})
    end,
    empdb_mnesia:run_tran(F).

%% @doc Remove a subscription by ID from the database.
-spec rm_subscription( 'UUID'() ) -> possible_failure( ok ).
rm_subscription(SubscriptionId) ->
    Delete = #subscription_table{sub_id=SubscriptionId, _ ='_'},
    F = fun() ->
            List = mnesia:match_object(Delete),
            lists:foreach(fun(X) -> mnesia:delete_object(X) end, List)
    end,
    empdb_mnesia:run_tran(F).
    
%% @doc Get all the subscriptions for a particular plugin/user combination.
-spec get_subscriptions( 'EMPUSER'(), 'EMPPLUGINDEF'() ) ->
            possible_failure( ['EMPSUBSCRIPTION'()] ).
get_subscriptions(User, HandlePlug) ->
    F = fun() ->
            Q = qlc:q([S || S <- mnesia:table(subscription_table),
                    S#subscription_table.user_id =:= User, 
                    S#subscription_table.event_id =:= HandlePlug]),
            qlc:e(Q)
    end,
    empdb_mnesia:run_tran(F).
