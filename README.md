## EMP DB ##

This is the primary repository for the EMP Database application. 

The current implementation uses Erlang's built in MNesia database for storage 
and fault-tolerance. However, this is merely for convenience and you can use any 
database you want as empdb.erl provides hooks to the actual implementation.

See the [wiki][1] for more information on how to extend or change EMP's 
Database. If there is something missing from the documentation, please be so 
kind as to post an issue. We are still in alpha and will update as soon as 
possible.

NOTE: There is no need to clone this repo directly, [Empd/emp][2] will pull this 
repo as a dependency using rebar.


[1]: https://github.com/Empd/empdb/wiki
[2]: https://github.com/Empd/emp
