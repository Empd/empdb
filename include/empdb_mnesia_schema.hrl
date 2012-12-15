%%
%% The Mnesia records that are used as the table schema's. Please
%% see empdb, for more information regarding table construction.
%%

% Table: emp_vars => emp_util:var_val
% Stores configuration values, can be overridden by config files.
-record(emp_vars,
           {var,     % Term()
            val      % Term()
           }).

% Table: user_table
% User records, used for launching umans and tying plugins to users.
-record(user_table,
        {user_id,          % UUID
         username,         % String
         password_hash,    % String
         public_key        % null/String
        }).


% Table: plugin_table
% Plugin registration information. Entries are added when plugins are
% installed.
-record(plugin_table,
        {plug_id,          % UUID
         module_name,      % atom()
         sysplug = false,  % boolean()
         commands = [],    % [ UUID ]
         events = []       % [ UUID ]
        }).

% Table: target_table
% Users can link target names to plugin definitions.
-record(target_table,
        {user_id,          % UUID
         plug_id,          % UUID
         target,           % String
         enabled = false,  % boolean()
         cfg_vals = [],    % [ tuple() ]
         state = []        % [ tuple() ]
        }).


% Table: command_table
% Command registration, entries are added when plugins are installed.
% It is also modified when emp gets more commands too.
-record(command_table,
        {cmd_id,           % UUID
         plug_id,          % UUID
         name,             % String
         function,         % Function
         params,           % [ EMPPARM() ]
         returns           % [ int() ] <- TypeDef
        }).


% Table: event_table
% Event Registration, entries are added when plugins are installed.
-record(event_table,
        {event_id,         % UUID
         plug_id,          % UUID
         event_name,       % String
         params            % [ EMPPARAM() ]
        }).


% Table: subscription_table
% Emp's saved record for subscriptions. These map events to commands,
% and are subject to a predicate.
-record(subscription_table,
        {sub_id,           % UUID
         user_id,          % UUID
         cmd_id,           % UUID
         event_id,         % UUID
         pred_fun=fun()->true end,  % Function
         pred_map=[],      % [ EventValueNames ] 
         e2cmap            % [ tuple() ]
        }).


% Table: active_sessions
% Current interface sessions of users. 
-record(active_sessions,
        {session_id,       % UUID
         user_id,          % UUID
         session_start,    % Date/Time
         session_length    % Date/Time
        }).


% Table: prev_sessions
% Old sessions are saved to this table as a log.
-record(prev_sessions,
        {session_id,       % UUID
         user_id,          % UUID
         session_start,    % Date/Time
         session_length    % Date/Time
        }).
