%% @author Driebit <tech@driebit.nl>
%% @copyright 2017

-module(mlc_parser).
-export([
    unescape/1,
    match_all_links/1,
    find_links/1,
    find_links/2,
    pre_crawl_status/2
]).

-include_lib("zotonic.hrl").

% @doc Run a regex globally over the input text
capture_global(Re, Text) ->
    re:run(Text, Re, [{capture, all, binary}, global, bsr_unicode]).

% @doc Unescape html encoding applied by tinymce
unescape(Str) when is_list(Str) ->
    z_convert:to_list(unescape(Str, []));
unescape(Bin) when is_binary(Bin)->
    unescape(z_convert:to_list(Bin), []).

unescape([], Acc) ->
    list_to_binary(lists:reverse(Acc));
unescape("&lt;" ++ Rest, Acc) ->
    unescape(Rest, lists:reverse("<", Acc));
unescape("&gt;" ++ Rest, Acc) ->
    unescape(Rest, lists:reverse(">", Acc));
unescape("&amp;" ++ Rest, Acc) ->
    unescape(Rest, lists:reverse("&", Acc));
unescape("&quot;" ++ Rest, Acc) ->
    unescape(Rest, lists:reverse("\"", Acc));
unescape([C | Rest], Acc) ->
    unescape(Rest, [C | Acc]).

% @doc Find all href and src links in the text
match_all_links(Text) when is_list(Text) ->
    match_all_links(z_convert:to_binary(Text));
match_all_links(Text) when is_binary(Text) ->
    case capture_global("(src|href)=\"([^\"]+)\"", Text) of
        nomatch -> [];
        {match, Matches} ->
            lists:map(
                fun([_, _, Url]) ->
                    mlc_parser:unescape(Url)
                end,
                Matches
            )
    end.

% @doc Find all links in the texts within the values of a proplist
find_links(Props) when is_list(Props) ->
    lists:flatmap(
        fun({_Key, Value}) when is_binary(Value) ->
                match_all_links(Value);
            ({_Key, Value}) when is_list(Value) ->
                case z_string:is_string(Value) of
                    true -> match_all_links(Value);
                    false -> []
                end;
            ({_Key, {trans, Trans}}) ->
                lists:flatmap(
                    fun({_LanCode, TransText}) ->
                        match_all_links(TransText)
                    end,
                    Trans
                );
            (_) ->
                []
        end,
        Props
    ).

% @doc Find all links in the content of RscId
find_links(RscId, Context) ->
    Props = m_rsc:get(RscId, Context),
    find_links(Props).

% @doc Determines the status of the found link before crawling
% Returns valid, invalid or ignore
% TODO: Support configuration of other protocols than http / https
% TODO: Compare against hostname or known routes to determine internal
pre_crawl_status(Link, IgnoreInternal) ->
    {Protocol, Host, Path, _, _} = mochiweb_util:urlsplit(z_convert:to_list(Link)),
    case http_uri:parse(z_convert:to_list(Link)) of
        {error, no_scheme} ->
            ProbablyInternal = case {Protocol, Host, Path} of
                {[], [], _} -> z_string:starts_with("/", Path);
                _ -> false
            end,
            case {ProbablyInternal, IgnoreInternal} of
                {true, true} -> ignore;
                {true, false} -> valid;
                {false, _} -> invalid
            end;
        {error, _} -> invalid;
        {ok, _} -> valid
    end.
