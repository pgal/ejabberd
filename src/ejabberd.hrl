%%%----------------------------------------------------------------------
%%% File    : ejabberd.hrl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 17 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

%-define(ejabberd_debug, true).
%-define(DBGFSM, true).

-ifdef(ejabberd_debug).
-define(DEBUG(Format, Args), io:format("D(~p:~p:~p) : "++Format++"~n",
                                       [self(),?MODULE,?LINE]++Args)).
-else.
-define(DEBUG(F,A),[]).
-endif.


%-define(MYNAME,"e.localhost").
-define(MYNAME, ejabberd_config:get_option(host)).

-define(MSGS_DIR, "msgs").
-define(CONFIG_PATH, "ejabberd.cfg").
-define(ERROR_LOG_PATH, "error.log").

