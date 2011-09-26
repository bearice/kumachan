-module(http_dispatcher).
-author("Bearice Ren <bearice@gmail.com>").
-export([behaviour_info/1]).
behaviour_info(callbacks) ->
    [{dispatch,1}];
behaviour_info(_) ->
    undefined.
