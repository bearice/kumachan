-module(websocket).
-author("Bearice Ren <bearice@gmail.com>").
-include_lib("http_server.hrl").

-export([handle/2,send/4]).

handle(Request,Ownner)->
    Socket = Request#request.socket,
    inet:setopts(Socket,[{packet,raw},{active,false}]),
    Path = Request#request.path,
    Headers = Request#request.headers,
    Host    = proplists:get_value('Host',Headers),
    Version = proplists:get_value(<<"Sec-WebSocket-Version">>,Headers),
    Key     = proplists:get_value(<<"Sec-Websocket-Key">>,Headers),
    <<"8">> = Version,
    BaseString = [Key,"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"],
    Hash = base64:encode(crypto:sha(BaseString)),
    http_server:send_response_header(Socket,
        101,"WebSocket Protocol Handshake",Request#request.version,
    [
        {'Upgrade','WebSocket'},
        {'Connection','Upgrade'},
        {'Sec-WebSocket-Accept',Hash}
    ]),
    loop(Socket,Ownner).

loop(Socket,Ownner) ->
    {Op,Data} = recv(Socket),
    Ownner ! {ws_data,{Op,Data}},
    loop(Socket,Ownner).

recv(Socket) -> recv(header,Socket).

recv(header,Socket) ->
    {ok,<<
        Fin:1/integer,
        _Rsv:3/integer,
        Op:4/integer,
        Mask:1/integer,
        Len1:7/integer
    >>=Header} = gen_tcp:recv(Socket,2),
%    error_logger:info_report([
%        {fin,Fin},
%        {op,Op},
%        {mask,Mask}
%    ]),
    recv({len,Fin,Op,Mask,Len1},Socket);
 
recv({len,Fin,Op,Mask,Len1},Socket) ->
    Len = case Len1 of
        N when N < 126 ->
            Len1;
        N when N == 126 ->
            {ok,<<X:16/integer>>=H} = gen_tcp:recv(Socket,2),
            X;
        N when N == 127 ->
            {ok,<<X:64/integer>>=H} = gen_tcp:recv(Socket,8),
            X
    end,
%    error_logger:info_report([
%        {len,Len}
%    ]),
    case Mask of
        0 -> recv({payload,Fin,Op,Len,undefined},Socket);
        1 -> recv({mask,Fin,Op,Len},Socket)
    end;

recv({mask,Fin,Op,Len},Socket) ->
    {ok,Key} = gen_tcp:recv(Socket,4),
%    error_logger:info_report([
%        {mask_key,Key}
%    ]),
    recv({payload,Fin,Op,Len,Key},Socket);

recv({payload,Fin,Op,Len,Key},Socket) ->
    {ok,Payload} = gen_tcp:recv(Socket,Len),
    Data = mask(Key,Payload),
%    error_logger:info_report([
%        {data,Data}
%    ]),
    {Op,Data}.

mask(undefined,Data) -> Data;
mask(K,Data) -> mask(K,Data,<<>>).

mask(<<Key:32/integer>>=K,<<Val:32/integer,Left/binary>>,Acc) ->
    Mask = Val bxor Key,
    mask(K,Left,<<Acc/binary,Mask:32/integer>>);

mask(<<Key:24/integer,_/binary>>=K,<<Val:24/integer,Left/binary>>,Acc) ->
    Mask = Val bxor Key,
    mask(K,Left,<<Acc/binary,Mask:24/integer>>);

mask(<<Key:16/integer,_/binary>>=K,<<Val:16/integer,Left/binary>>,Acc) ->
    Mask = Val bxor Key,
    mask(K,Left,<<Acc/binary,Mask:16/integer>>);

mask(<<Key:8/integer,_/binary>>=K,<<Val:8/integer,Left/binary>>,Acc) ->
    Mask = Val bxor Key,
    mask(K,Left,<<Acc/binary,Mask:8/integer>>);

mask(_,<<>>,Acc) ->
    Acc.

len_bytes(Mask,Len) when Len < 16#7E     -> <<Mask:1/integer,Len:7/integer>>;
len_bytes(Mask,Len) when Len < 16#FFFF   -> <<Mask:1/integer,16#7E:7/integer,Len:16/integer>>;
len_bytes(Mask,Len)                      -> <<Mask:1/integer,16#7F:7/integer,Len:64/integer>>.

send(Fin,Op,Data,Socket) ->
    Len = iolist_size(Data),
    LenBytes = len_bytes(0,Len),
    Head = <<
        Fin:1/integer,
        0:3/integer,
        Op:4/integer,
        LenBytes/binary
    >>,
%    error_logger:info_report([
%        {client,Socket},
%        {head,Head},
%        {data,Data}
%    ]),
    gen_tcp:send(Socket,[Head,Data]).
