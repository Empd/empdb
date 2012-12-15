
%%
%% Default values for root and base password
%%

-define(ROOT_USER_ID, <<"00000000-0000-0000-0000-000000000000">>).
-define(ROOT_USER_NAME, <<"root">>).
-define(PASS(WORD), bin_to_hex:bin_to_hex(crypto:sha256(WORD))).


-type nonempty_nodelist() :: [atom(), ...].

-type useroptions() :: [ user_option(), ... ].
-type user_option() :: {password, RawPass :: binary()}
                         | {passhash, HashedPass :: binary()}
                         | {override_uuid, UUID :: binary()}.
                         %TODO: user level? more information about user (e.g. email, phonenumber, etc)?
                       

-type possible_failure(Success) :: Success | {error, Reason :: any()}. %TODO: each function has a different list of possible reasons
