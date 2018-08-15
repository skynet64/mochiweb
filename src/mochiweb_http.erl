%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% @doc HTTP server.

-module(mochiweb_http).

-author('bob@mochimedia.com').

-export([start_link/2]).
-export([init/2, loop/2]).
-export([after_response/3, reentry/2]).
-export([parse_range_request/1, range_skip_length/2]).

-define(REQUEST_RECV_TIMEOUT, 300000).   %% timeout waiting for request line
-define(HEADERS_RECV_TIMEOUT, 30000).    %% timeout waiting for headers

-define(MAX_HEADERS, 1000).

-ifdef(gen_tcp_r15b_workaround).
r15b_workaround() -> true.
-else.
r15b_workaround() -> false.
-endif.

start_link(Conn, Callback) ->
    {ok, spawn_link(?MODULE, init, [Conn, Callback])}.

init({Conn,Args}, Callback) ->
    {ok, NewConn} = Conn:wait({Conn,Args}),
	loop(NewConn, Callback).

loop({Conn,Args}, Callback) ->
    ok = mochiweb_util:exit_if_closed(Conn:setopts([{packet, http}],{Conn,Args})),
    request({Conn,Args}, Callback).

request({Conn,Args}, Callback) ->
    ok = mochiweb_util:exit_if_closed(Conn:setopts([{active, once}],{Conn,Args})),
    receive
        {Protocol, _, {http_request, Method, Path, Version}} when Protocol == http orelse Protocol == ssl ->
            ok = mochiweb_util:exit_if_closed(Conn:setopts([{packet, httph}],{Conn,Args})),
            headers({Conn,Args}, {Method, Path, Version}, [], Callback, 0);
        {Protocol, _, {http_error, "\r\n"}} when Protocol == http orelse Protocol == ssl ->
            request({Conn,Args}, Callback);
        {Protocol, _, {http_error, "\n"}} when Protocol == http orelse Protocol == ssl ->
            request({Conn,Args}, Callback);
        {tcp_closed, _} ->
            Conn:close({Conn,Args}),
            exit(normal);
        {ssl_closed, _} ->
            Conn:close({Conn,Args}),
            exit(normal);
        Other ->
            handle_invalid_msg_request(Other, {Conn,Args})
    after ?REQUEST_RECV_TIMEOUT ->
        Conn:close({Conn,Args}),
        exit(normal)
    end.

reentry(Conn, Callback) ->
    fun (Req) ->
            ?MODULE:after_response(Conn, Callback, Req)
    end.

headers({Conn,Args}, Request, Headers, _Callback, ?MAX_HEADERS) ->
    %% Too many headers sent, bad request.
    ok = mochiweb_util:exit_if_closed(Conn:setopts([{packet, raw}],{Conn,Args})),
    handle_invalid_request(Conn, Request, Headers);

headers({Conn,Args}, Request, Headers, Callback, HeaderCount) ->
    ok = mochiweb_util:exit_if_closed(Conn:setopts([{active, once}],{Conn,Args})),
    receive
        {Protocol, _, http_eoh} when Protocol == http orelse Protocol == ssl ->
            Req = new_request({Conn,Args}, Request, Headers),
            callback(Callback, Req),
            ?MODULE:after_response({Conn,Args}, Callback, Req);
        {Protocol, _, {http_header, _, Name, _, Value}} when Protocol == http orelse Protocol == ssl ->
            headers({Conn,Args}, Request, [{Name, Value} | Headers], Callback, 1 + HeaderCount);
        {tcp_closed, _} ->
            Conn:close({Conn,Args}),
            exit(normal);
        Other ->
            handle_invalid_msg_request(Other, {Conn,Args}, Request, Headers)
    after ?HEADERS_RECV_TIMEOUT ->
        Conn:close({Conn,Args}),
        exit(normal)
    end.


-spec handle_invalid_msg_request(term(), esockd_connection:connection()) -> no_return().
handle_invalid_msg_request(Msg, Conn) ->
    handle_invalid_msg_request(Msg, Conn, {'GET', {abs_path, "/"}, {0,9}}, []).

-spec handle_invalid_msg_request(term(), esockd_connection:connection(), term(), term()) -> no_return().
handle_invalid_msg_request(Msg, {Conn,Args}, Request, RevHeaders) ->
    case {Msg, r15b_workaround()} of
        {{tcp_error,_,emsgsize}, true} ->
            %% R15B02 returns this then closes the socket, so close and exit
            Conn:close({Conn,Args}),
            exit(normal);
        _ ->
            handle_invalid_request({Conn,Args}, Request, RevHeaders)
    end.

-spec handle_invalid_request(esockd_connection:connection(), term(), term()) -> no_return().
handle_invalid_request({Conn,Args}, Request, RevHeaders) ->
    Req = new_request({Conn,Args}, Request, RevHeaders),
    Req:respond({400, [], []}),
    Conn:close({Conn,Args}),
    exit(normal).

new_request({Conn,Args}, Request, RevHeaders) ->
    ok = mochiweb_util:exit_if_closed(Conn:setopts([{packet, raw}],{Conn,Args})),
    mochiweb:new_request({{Conn,Args}, Request, lists:reverse(RevHeaders)}).

after_response({Conn,Args}, Callback, {Req,A}) ->
    case Req:should_close({Req,A}) of
        true ->
            Conn:close({Conn,Args}),
            exit(normal);
        false ->
            Req:cleanup({Req,A}),
            erlang:garbage_collect(),
            ?MODULE:loop({Conn,Args}, Callback)
    end.

parse_range_request(RawRange) when is_list(RawRange) ->
    try
        "bytes=" ++ RangeString = RawRange,
        RangeTokens = [string:strip(R) || R <- string:tokens(RangeString, ",")],
        Ranges = [R || R <- RangeTokens, string:len(R) > 0],
        lists:map(fun ("-" ++ V)  ->
                          {none, list_to_integer(V)};
                      (R) ->
                          case string:tokens(R, "-") of
                              [S1, S2] ->
                                  {list_to_integer(S1), list_to_integer(S2)};
                              [S] ->
                                  {list_to_integer(S), none}
                          end
                  end,
                  Ranges)
    catch
        _:_ ->
            fail
    end.

range_skip_length(Spec, Size) ->
    case Spec of
        {none, R} when R =< Size, R >= 0 ->
            {Size - R, R};
        {none, _OutOfRange} ->
            {0, Size};
        {R, none} when R >= 0, R < Size ->
            {R, Size - R};
        {_OutOfRange, none} ->
            invalid_range;
        {Start, End} when Start >= 0, Start < Size, Start =< End ->
            {Start, erlang:min(End + 1, Size) - Start};
        {_InvalidStart, _InvalidEnd} ->
            invalid_range
    end.

callback({M, F, A}, Req) ->
    erlang:apply(M, F, [Req | A]);
callback({M, F}, Req) ->
    M:F(Req);
callback(Callback, Req) when is_function(Callback) ->
    Callback(Req).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

range_test() ->
    %% valid, single ranges
    ?assertEqual([{20, 30}], parse_range_request("bytes=20-30")),
    ?assertEqual([{20, none}], parse_range_request("bytes=20-")),
    ?assertEqual([{none, 20}], parse_range_request("bytes=-20")),

    %% trivial single range
    ?assertEqual([{0, none}], parse_range_request("bytes=0-")),

    %% invalid, single ranges
    ?assertEqual(fail, parse_range_request("")),
    ?assertEqual(fail, parse_range_request("garbage")),
    ?assertEqual(fail, parse_range_request("bytes=-20-30")),

    %% valid, multiple range
    ?assertEqual(
       [{20, 30}, {50, 100}, {110, 200}],
       parse_range_request("bytes=20-30,50-100,110-200")),
    ?assertEqual(
       [{20, none}, {50, 100}, {none, 200}],
       parse_range_request("bytes=20-,50-100,-200")),

    %% valid, multiple range with whitespace
    ?assertEqual(
       [{20, 30}, {50, 100}, {110, 200}],
       parse_range_request("bytes=20-30, 50-100 , 110-200")),

    %% valid, multiple range with extra commas
    ?assertEqual(
       [{20, 30}, {50, 100}, {110, 200}],
       parse_range_request("bytes=20-30,,50-100,110-200")),
    ?assertEqual(
       [{20, 30}, {50, 100}, {110, 200}],
       parse_range_request("bytes=20-30, ,50-100,,,110-200")),

    %% no ranges
    ?assertEqual([], parse_range_request("bytes=")),
    ok.

range_skip_length_test() ->
    Body = <<"012345678901234567890123456789012345678901234567890123456789">>,
    BodySize = byte_size(Body), %% 60
    BodySize = 60,

    %% these values assume BodySize =:= 60
    ?assertEqual({1,9}, range_skip_length({1,9}, BodySize)), %% 1-9
    ?assertEqual({10,10}, range_skip_length({10,19}, BodySize)), %% 10-19
    ?assertEqual({40, 20}, range_skip_length({none, 20}, BodySize)), %% -20
    ?assertEqual({30, 30}, range_skip_length({30, none}, BodySize)), %% 30-

    %% valid edge cases for range_skip_length
    ?assertEqual({BodySize, 0}, range_skip_length({none, 0}, BodySize)),
    ?assertEqual({0, BodySize}, range_skip_length({none, BodySize}, BodySize)),
    ?assertEqual({0, BodySize}, range_skip_length({0, none}, BodySize)),
    ?assertEqual({0, BodySize}, range_skip_length({0, BodySize + 1}, BodySize)),
    BodySizeLess1 = BodySize - 1,
    ?assertEqual({BodySizeLess1, 1},
                 range_skip_length({BodySize - 1, none}, BodySize)),
    ?assertEqual({BodySizeLess1, 1},
                 range_skip_length({BodySize - 1, BodySize+5}, BodySize)),
    ?assertEqual({BodySizeLess1, 1},
                 range_skip_length({BodySize - 1, BodySize}, BodySize)),

    %% out of range, return whole thing
    ?assertEqual({0, BodySize},
                 range_skip_length({none, BodySize + 1}, BodySize)),
    ?assertEqual({0, BodySize},
                 range_skip_length({none, -1}, BodySize)),
    ?assertEqual({0, BodySize},
                 range_skip_length({0, BodySize + 1}, BodySize)),

    %% invalid ranges
    ?assertEqual(invalid_range,
                 range_skip_length({-1, 30}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({-1, BodySize + 1}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({BodySize, 40}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({-1, none}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({BodySize, none}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({BodySize + 1, BodySize + 5}, BodySize)),
    ok.

-endif.
