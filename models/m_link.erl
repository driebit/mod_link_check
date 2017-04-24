%% @author Driebit <tech@driebit.nl>
%% @copyright 2017

-module(m_link).
-author("Driebit <tech@driebit.nl>").

-export([m_find_value/3, m_to_list/2, m_value/2]).
-include("zotonic.hrl").

-compile(export_all).

% Syntax: m.link.all
m_find_value(all, #m{value=undefined} = M, _Context) ->
    M#m{value=[all]};

% Syntax: m.link.problems
m_find_value(problems, #m{value=undefined} = M, _Context) ->
    M#m{value=[problems]}.

m_to_list(#m{value=[all]}, Context) ->
    mlc_data:get_urls_all(Context);

m_to_list(#m{value=[problems]}, Context) ->
    Interval = z_convert:to_integer(
        m_config:get_value(mod_link_check, check_interval, Context)
    ),
    IntervalSeconds = Interval / 1000,
    z_depcache:memo(
        fun() ->
            Urls = mlc_data:get_urls_problem(Context),
            friendly_statuses(Urls, Context)
        end,
        {m_link, problems},
        IntervalSeconds,
        Context
    ).

m_value(_M, _Context) ->
    undefined.

% Formats a friendly status description
friendly_status(Url, Context) ->
    IsInvalid = proplists:get_value(invalid, Url),
    ErrorReason = proplists:get_value(error_reason, Url),
    LastStatus = proplists:get_value(last_status, Url),
    FriendlyStatus = case {IsInvalid, ErrorReason, LastStatus} of
        {false, undefined, undefined} ->
            ?__(<<"Link has not been checked yet">>, Context);
        % TODO: Give more detail on why the link is invalid
        {true, _, _} ->
            ?__(<<"Link is invalid">>, Context);
        {false, undefined, LastStatus} ->
            case LastStatus < 300 of
                true -> ?__(<<"Link works correctly">>, Context);
                false ->
                    case LastStatus >= 400 of
                        true -> ?__(<<"Server responded with an error">>, Context);
                        false -> ?__(<<"Link has been redirected">>, Context)
                    end
             end;
        % TODO: Give more detail on what error has occured
        {_, ErrorReason, _} ->
            ?__(<<"A network error has occured">>, Context)
    end,
    StatusTooltip = string:join(
        [
            "Invalid: ", z_convert:to_list(IsInvalid), "\n",
            "Error Reason: ", z_convert:to_list(ErrorReason), "\n",
            "Http Status: ", z_convert:to_list(LastStatus)
        ],
        ""
    ),
    [{friendly_status, FriendlyStatus},{status_tooltip, StatusTooltip}|Url].

friendly_statuses(Urls, Context) ->
    lists:map(fun(Url) -> friendly_status(Url, Context) end, Urls).
