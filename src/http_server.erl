-module(http_server).
-author("Bearice Ren <bearice@gmail.com>").
-behaviour(http_dispatcher).
-export([start/1,stop/1]).
-export([dispatch/1]).
-export([send_chunk/2,send_response/6,send_response_header/5,close/1]).
-export([parse_query/1,quote_plus/1,urlencode/1]).

-define(SERVER_VERSION,"KumaChan/1.0").
-include("http_server.hrl").


start(Args)->
    Port = proplists:get_value(port,Args,8080),
    Module = proplists:get_value(module,Args,?MODULE),
    Pid = spawn(?MODULE,init,[Port,Module]),
    {ok,Pid}.

start_link(Args) ->
    Port = proplists:get_value(port,Args,8080),
    Module = proplists:get_value(module,Args,?MODULE),
    Pid = spawn_link(?MODULE,init,[Port,Module]),
    {ok,Pid}.

stop(_) -> 
    ok.

init(Port,Mod) ->
    {ok,Listen} = gen_tcp:listen(Port,[binary,{packet,http_bin},{reuseaddr,true},{active,false},{backlog, 500}]),
    accept(Listen,Mod).

accept(Listen,Mod)->
    {ok,Socket} = gen_tcp:accept(Listen),
    Pid = spawn(?MODULE,recv,[Socket,#request{socket=Socket},Mod]),
    ok  = gen_tcp:controlling_process(Socket,Pid),
    inet:setopts(Socket,[{active,true}]),
    accept(Listen,Mod).

recv(Socket,Request,Mod)->
    receive
        {http, Socket, http_eoh} ->
            error_logger:info_report([
                {request,Request}
            ]),
            try Mod:dispatch(Request) 
            catch
                C:E ->
                    send_response(Socket,500,"Server Error",Request#request.version,[{'Connection','close'}],
                        <<"<h1>&lt;_&gt; Something went wrong!</h1>">>
                    ),
                    error_logger:error_report([
                        {request,Request},
                        {error,{C,E}},
                        {trace,erlang:get_stacktrace()}
                    ])
            end,
            close(Socket);

        {http, Socket, Data} ->
            %%io:format("Http: ~p~n",[Data]),
            case Data of 
                {http_request,Method,{abs_path,Uri},Version} ->
                    {Path,Query} = split_path(Uri),
                    NewRequest = Request#request{method=Method,path=Path,q=Query,uri=Uri,version=Version},
                    recv(Socket,NewRequest,Mod);

                {http_request,_,_,Version} ->
                    send_response(Socket,417,"Expectation Failed",Version,
                        [{'Connection','close'}],
                        <<"<h1>O_O Hah?</h1>">>
                    ),
                    close(Socket);

                {http_header,_,Name,_,Value} ->
                    NewRequest = Request#request{headers=[{Name,Value}|Request#request.headers]},
                    recv(Socket,NewRequest,Mod)
            end;

        {tcp_closed,Socket} ->
            close(Socket);

        _ ->
            %%io:format("Unexpected: ~p~n",[O]),
            send_response(Socket,400,"Bad Request",{1,1},[{'Connection','close'}],
                <<"<h1>x_x I don't understand your speaking.</h1>">>
            ),
            close(Socket)
    end.

dispatch(Req) ->
    Socket = Req#request.socket,
    Version = Req#request.version,
    case Req#request.path of
        <<"/">> ->
            send_response(Socket,200,"OK",Version,[{'Connection','close'}],
                <<"<h1>^_^ Hello world!</h1>">>
            );
        _ ->
            send_response(Socket,404,"Not Found",Version,[{'Connection','close'}],
                <<"<h1>?_? It doesn't exists!</h1>">>
            )
    end.

close(Socket) ->
    gen_tcp:close(Socket).

send_chunk(Socket,Body) ->
    Len  = iolist_size(Body),
    Resp = [io_lib:format("~.16B\r\n",[Len]),Body,"\r\n"],
    gen_tcp:send(Socket,Resp).

send_response(Socket,Code,Message,Ver,Headers,Body) ->
    Len = iolist_size(Body),
    send_response_header(Socket,Code,Message,Ver,[{'Content-Length',Len}|Headers]),
    gen_tcp:send(Socket,Body).

send_response_header(Socket,Code,Message,{V1,V2},Headers) ->
    gen_tcp:send(Socket,[
        io_lib:format("HTTP/~p.~p ~p ~s\r\n",[V1,V2,Code,Message]),
        "Server: " ?SERVER_VERSION "\r\n"|
        make_headers(Headers,[])
    ]).

make_headers([],Acc) ->
    lists:reverse(["\r\n"|Acc]);

make_headers([{Name,Value}|Tail],Acc) ->
    make_headers(Tail,[[to_iolist(Name),": ",to_iolist(Value),"\r\n"]|Acc]).

to_iolist(String) when is_list(String) ->
    String;

to_iolist(Binary) when is_binary(Binary) ->
    Binary;

to_iolist(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);

to_iolist(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);

to_iolist(Other)->
    io_lib:format("~p",[Other]).

split_path(URI) ->
    case binary:match(URI,<<"?">>) of
        nomatch -> {URI,<<"">>};
        {P,L} ->
            <<Path:P/binary,$?,Query/binary>> = URI,
            {Path,Query}
    end.

%% code below were copied from mochiweb written by Bob Ippolito <bob@mochimedia.com>
-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.

%% @spec parse_query(string() | binary()) -> [{Key, Value}]
%% @doc Parse a query string or application/x-www-form-urlencoded.
parse_query(Binary) when is_binary(Binary) ->
    parse_query(binary_to_list(Binary));
parse_query([$?|String]) ->
    parse_query(String, []);
parse_query(String) ->
    parse_query(String, []).

parse_query([], Acc) ->
    lists:reverse(Acc);
parse_query(String, Acc) ->
    {Key, Rest} = parse_qs_key(String),
    {Value, Rest1} = parse_qs_value(Rest),
    parse_query(Rest1, [{Key, Value} | Acc]).

parse_qs_key(String) ->
    parse_qs_key(String, []).

parse_qs_key([], Acc) ->
    {qs_revdecode(Acc), ""};
parse_qs_key([$= | Rest], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_key(Rest=[$; | _], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_key(Rest=[$& | _], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_key([C | Rest], Acc) ->
    parse_qs_key(Rest, [C | Acc]).

parse_qs_value(String) ->
    parse_qs_value(String, []).

parse_qs_value([], Acc) ->
    {qs_revdecode(Acc), ""};
parse_qs_value([$; | Rest], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_value([$& | Rest], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_value([C | Rest], Acc) ->
    parse_qs_value(Rest, [C | Acc]).

%% @spec unquote(string() | binary()) -> string()
%% @doc Unquote a URL encoded string.
unquote(Binary) when is_binary(Binary) ->
    unquote(binary_to_list(Binary));
unquote(String) ->
    qs_revdecode(lists:reverse(String)).

qs_revdecode(S) ->
    qs_revdecode(S, []).

qs_revdecode([], Acc) ->
    Acc;
qs_revdecode([$+ | Rest], Acc) ->
    qs_revdecode(Rest, [$\s | Acc]);
qs_revdecode([Lo, Hi, ?PERCENT | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
    qs_revdecode(Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc]);
qs_revdecode([C | Rest], Acc) ->
    qs_revdecode(Rest, [C | Acc]).

%% @spec quote_plus(atom() | integer() | float() | string() | binary()) -> string()
%% @doc URL safe encoding of the given term.
quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(Binary) when is_binary(Binary) ->
    quote_plus(binary_to_list(Binary));
quote_plus(Float) when is_float(Float) ->
    quote_plus(mochinum:digits(Float));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

%% @spec urlencode([{Key, Value}]) -> string()
%% @doc URL encode the property list.
urlencode(Props) ->
    Pairs = lists:foldr(
              fun ({K, V}, Acc) ->
                      [quote_plus(K) ++ "=" ++ quote_plus(V) | Acc]
              end, [], Props),
    string:join(Pairs, "&").

