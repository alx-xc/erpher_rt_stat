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
%%% @author alx-xc <alx-xc@yandex.ru>
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

get_resource(Req, [H | T]) ->
    {Status, Content} = get_app_content(Req, H, T),
    case Status of
        ok ->
           cowboy_http_req:reply(200, [{'Content-Type', "text/html"}], Content, Req);
        error ->
            cowboy_http_req:reply(404, [], "404 Not Found\r\n", Req)
    end,
    {ok, Req};
%% main page
get_resource(Req, []) ->
    cowboy_http_req:reply(200, [{'Content-Type', "text/html"}], <<"common<br/><a href='/all/stats'>stats</a><br/><a href='/all/versions'>versions</a><br/><br/>ecomet<br/><a href='/ec/rt'>current</a><br/><br/>ejobman<br/><a href='/ej/stat_t'>stat</a>">>, Req),
    {ok, Req};
%% others
get_resource(Req, _Path) ->
    cowboy_http_req:reply(404, [], "404 Not Found\r\n", Req),
    {ok, Req}.

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
        _ ->
            Data = [
                {ts, erlang:list_to_binary(mpln_misc_time:get_time_str())},
                {proc, length(processes())},
                {mem, erlang:memory()}
            ]
    end,
    Text = io_lib:format("~ts", [lists:flatten(mochijson2:encode(Data))]),
    {ok, Text};
get_app_content(_Req, <<"ej">>, Path) ->
    [Path2 | _Path2T] = Path,
    Size = 1000,
    case Path2 of
        <<"last">> ->
            Text = ejobman_log:get_last_jobs("html", Size);
        <<"last_text">> ->
            Text = ejobman_log:get_last_jobs("text", Size);
        <<"stat_t_text">> ->
            Text = ejobman_stat:make_stat_t_info_text();
        <<"stat_t">> ->
            Text = ejobman_stat:make_stat_t_info_html();
        <<"qlen">> ->
            Text = ejobman_handler:stat_q();
        <<"rss">> ->
            Text = ejobman_handler:stat_rss(Size);
        _ ->
            Text = ejobman_stat:make_stat_t_info_html()
    end,
    {ok, Text};
get_app_content(_Req, <<"ec">>, Path) ->
    [Path2 | _Path2T] = Path,
    case Path2 of
        <<"rt">> ->
            Data = ecomet_server:get_stat_procs_mem();
        _ ->
            Data = ecomet_server:get_stat_procs_mem()
    end,
    Text = io_lib:format("~ts", [lists:flatten(mochijson2:encode(Data))]),
    {ok, Text};
get_app_content(_Req, <<"ew">>, _Path) ->
    Res = eworkman_handler:get_status2(),
    {_, Str} = eworkman_worker_web_page:create_html_status(Res),
    {ok, Str};
get_app_content(_Res, _App, _Path) ->
    {error, badPath}.