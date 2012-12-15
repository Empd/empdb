## EMP DB ##

The primary repo for the EMP Database program. The current implementation uses
Erlang's built in MNesia database for storage and replication. However, this
is merely for conveinence and you can use any database you want.

See the documentation for more information on how to extend EMP's Database.

NOTE: There is no need to clone this repo directly, Empd/emp will pull this 
repo as a dependency using rebar.


### Dev Notes ###

Make sure when compiling, set the environment variable EMP_ROOT equal to the
path to the primary emp repo clone on the local machine. This is so EMPDB can
reference the lib/ directory for include files.