%%%
%%% erpher_rt_stat_web_handler: returns statistics for web queries
%%%
%%% Copyright (c) 2011 Megaplan Ltd. (Russia)
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom
%%% the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @author alx-xc <alx-xc@yandex.com>
%%% @since 2012-11-09 18:33
%%% @license MIT
%%% @doc returns statistics for web queries
%%%

-module(erpher_rt_stat_web_handler).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([init/3, handle/2, terminate/2]).

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    % @todo ip auth needed
    {Path, _} = cowboy_http_req:path(Req),
    {ok, Req2} = get_resource(Req, Path),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc
%%
-spec get_resource(any(), any()) -> any().

%% main page
get_resource(Req, []) ->
    Content = format_site_map( get_site_map(), html ),
    cowboy_http_req:reply(200, [{'Content-Type', "text/html"}], Content, Req),
    {ok, Req};

%% json main page
get_resource(Req, [<<"index.json">>]) ->
    Content = format_site_map( get_site_map(), json ),
    cowboy_http_req:reply(200, [{'Content-Type', "application/json"}], Content, Req),
    {ok, Req};

get_resource(Req, [H | T]) ->
    {Status, Content} = get_app_content(Req, H, T),
    case Status of
        ok ->
           cowboy_http_req:reply(200, [{'Content-Type', "text/html"}], Content, Req);
        error ->
            cowboy_http_req:reply(404, [], "404 Not Found\r\n", Req)
    end,
    {ok, Req};

%% others
get_resource(Req, _Path) ->
    cowboy_http_req:reply(404, [], "404 Not Found\r\n", Req),
    {ok, Req}.


get_site_map() ->
    [{"common", [{"stats", <<"all/stats">>}, {"ets", <<"all/ets">>}, {"versions", <<"all/versions">>}]},
     {"ecomet", [{"processes", <<"ec/prc">>}]},
     {"ejobman", [{"stat", <<"ej/stat">>}]}].


format_site_map( [], _Type ) ->
    [];
format_site_map( Data, json ) ->
    json_encode( Data );
format_site_map( [{Key, Items} | T], html = Type ) ->
    Key ++ "<br/>" ++ format_site_map_item( Items, Type ) ++ "<br/>" ++ format_site_map( T, Type ).

format_site_map_item( [], _Type ) ->
    [];
format_site_map_item( [{Name, Uri} | T], html = Type ) ->
    "<a href='" ++ mpln_misc_web:make_string( Uri ) ++ "'>" ++ Name ++ "</a><br/>" ++ format_site_map_item( T, Type ).


json_encode( Data ) ->
    io_lib:format("~ts", [lists:flatten(mochijson2:encode(Data))]).

%%
%% @doc gets status2 from  eworkman_worker and sends it as plain text response
%% to the client of the web server
%%
-spec get_app_content(any(), any(), any()) -> any().

%% common statistics
get_app_content(_Req, <<"all">>, Path) ->
    [Path2 | _Path2T] = Path,
    case Path2 of
        <<"versions">> ->
            Apps = application:loaded_applications(),
            Data = lists:map(fun({App, _Desc, Version}) -> {App, unicode:characters_to_binary(Version)} end, Apps);
        <<"ets">> ->
            Data = lists:map(fun(Tab) -> {ets:info(Tab, name), [{size, ets:info(Tab, size)}, {mem, ets:info(Tab, memory)}]} end, ets:all());
        _ ->
            Data = [
                {ts, erlang:list_to_binary(mpln_misc_time:get_time_str())},
                {proc, length(processes())},
                {ports, length(erlang:ports())},
                {mem, erlang:memory()}
            ]
    end,
    Text = json_encode(Data),
    {ok, Text};
%% ejobman stats
get_app_content(_Req, <<"ej">>, Path) ->
    [Path2 | _Path2T] = Path,
    Size = 1000,
    case Path2 of
        <<"log">> ->
            Data = ejobman_log(_Path2T),
            Text = json_encode(Data);
        <<"last">> ->
            Text = ejobman_log:get_last_jobs("html", Size);
        <<"last_text">> ->
            Text = ejobman_log:get_last_jobs("text", Size);
        <<"stat_t_text">> ->
            Text = ejobman_stat:make_stat_t_info_text();
        <<"stat_t">> ->
            Text = ejobman_stat_t_info_html();
        <<"qlen">> ->
            Text = ejobman_handler:stat_q();
        <<"rss">> ->
            Text = ejobman_handler:stat_rss(Size);
        _ ->
            Text = ejobman_stat_t_info_html()
    end,
    {ok, Text};
%% ecomet stats
get_app_content(_Req, <<"ec">>, Path) ->
    [Path2 | _Path2T] = Path,
    case Path2 of
        <<"rt">> ->
            Data = ecomet_server:get_stat_procs_mem();
        <<"prc">> ->
            Data = ecomet_server:get_stat_procs();
        _ ->
            Data = ecomet_server:get_stat_procs()
    end,
    Text = json_encode(Data),
    {ok, Text};
%% eworkman stats
get_app_content(_Req, <<"ew">>, _Path) ->
    Res = eworkman_handler:get_status2(),
    {_, Str} = eworkman_worker_web_page:create_html_status(Res),
    {ok, Str};
%% unknown queries
get_app_content(_Res, _App, _Path) ->
    {error, badPath}.

ejobman_log([]) ->
    {M, S, _} = now(),
    TS = M * 1000000 + S,
    ejobman_log([TS - 10000, TS + 3600]);

ejobman_log([Start, Stop]) ->
    erpher_rt_stat:get(Start, Stop).


ejobman_stat_t_info_html() ->
    List = ejobman_stat:stat_t(),

    L1 = proplists:get_value("minute data", List),
    L2 = proplists:get_value("hour data", List),
    L1j = lists:reverse(estat_misc:make_joined_list(L1)),
    L2j = lists:reverse(estat_misc:make_joined_list(L2)),
    make_one_stat_html([{"minute data", L1j}, {"hour data", L2j}]).

make_one_stat_html(List) ->
    F = fun({{Dt, Group}, {W_cur, W_max, W_finished, Q_cur, Q_max, _Q_finished}}) ->
        [
            "<tr>",
            "<td>", mpln_misc_time:make_str2_int(Dt), "</td>",
            "<td>", mpln_misc_web:make_string(Group), "</td>",
            "<td>", mpln_misc_web:make_string(W_finished), "</td>",
            "<td>", mpln_misc_web:make_string(W_cur), "</td>",
            "<td>", mpln_misc_web:make_string(W_max), "</td>",
            "<td>", mpln_misc_web:make_string(Q_cur), "</td>",
            "<td>", mpln_misc_web:make_string(Q_max), "</td>",
            "</tr>\n"
        ];
        ({K, V}) ->
            [
                "<tr>",
                "<td colspan=2>",
                mpln_misc_web:make_term_string(K),
                "</td>",
                "<td colspan=4>",
                mpln_misc_web:make_term_string(V),
                "</td>",
                "</tr>\n"
            ]
    end,
    F_big = fun({Tag, L}) ->
        [
            "<p>",
            "<table class='table table-condensed table-hover table-striped'><thead>",
            "<tr><th colspan=6>",
            mpln_misc_web:make_term_string(Tag),
            "</th></tr>\n",
            "<tr>",
            "<th>time</th>",
            "<th>group</th>",
            "<th>work finished</th>",
            "<th>work current</th>",
            "<th>work max</th>\n",
            "<th>queue current</th>",
            "<th>queue max</th>",
            "</tr></thead>\n<tbody>",
            lists:map(F, L),
            "</tbody></table>",
            "<p>"
        ]
    end,
    lists:flatten(lists:map(F_big, List)).