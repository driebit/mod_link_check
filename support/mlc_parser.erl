%% @author Driebit <tech@driebit.nl>
%% @copyright 2017

-module(mlc_parser).
-export([
    match_all_links/1,
    find_links/1,
    find_links/2,
    pre_crawl_status/2
]).

-include_lib("zotonic.hrl").

% @doc Run a regex globally over the input text
capture_global(Re, Text) ->
    re:run(Text, Re, [{capture, all, binary}, global, bsr_unicode]).

% @doc Find all href and src links in the text
match_all_links(Text) when is_list(Text) ->
    match_all_links(z_convert:to_binary(Text));
match_all_links(Text) when is_binary(Text) ->
    case capture_global("(src|href)=\"([^\"]+)\"", Text) of
        nomatch -> [];
        {match, Matches} ->
            lists:map(
                fun([_, _, Url]) -> Url end,
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
% Returns undefined, invalid or ignore
pre_crawl_status(Link, IgnoreInternal) ->
    SmellsLikeAbsolute = z_string:starts_with("http", Link),
    % TODO: Compare against hostname to determine internal
    SmellsLikeInternal = z_string:starts_with("/", Link),
    SmellsLikeMailto = z_string:starts_with("mailto:", Link),
    case {SmellsLikeAbsolute, SmellsLikeInternal, SmellsLikeMailto} of
        {true, _, _} -> undefined;
        {_, true, _} ->
            case IgnoreInternal of
                true -> ignore;
                false -> undefined
            end;
        {_, _, true} ->  ignore;
        {_, _, _} -> invalid
    end.
