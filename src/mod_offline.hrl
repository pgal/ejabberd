-include("ejabberd.hrl").
-include("jlib.hrl").

-record(offline_msg, {us, timestamp, expire, from, to, packet}).

%% CAUTION: using this macro is valid only after ejabberd_backend:init
%% was called!
-define(DISPATCH(Function, Args),
    ejabberd_backend:dispatch(?MODULE, mod_offline_backend:backend(),
        Function, Args)).
