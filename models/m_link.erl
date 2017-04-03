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

m_find_value(problem, #m{value=undefined} = M, _Context) ->
    M#m{value=[problem]}.

m_to_list(#m{value=[all]}, Context) ->
    mlc_data:get_urls_all(Context);

m_to_list(#m{value=[problem]}, Context) ->
    mlc_data:get_urls_problem(Context).

m_value(_M, _Context) ->
    undefined.
