%% @author Driebit <tech@driebit.nl>
%% @copyright 2017

-module(mod_link_check).

-behaviour(gen_server).

-mod_title("Link Check").
-mod_description("External link checker").

-mod_prio(300).

-export([
    start_link/1,

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,

    find_pid/1,
    cast/2,

    observe_admin_menu/3,
    pid_observe_custom_pivot/3,
    pid_observe_rsc_delete/3,
    event/2
]).

-record(state, {context, timer}).
-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

% @doc Starts a timer that sends an interval messages to self()
set_timer(Time, #state{timer=CurrentTimer} = State) ->
    case CurrentTimer of
        undefined -> ok;
        _ -> timer:cancel(CurrentTimer)
    end,
    TimerRef = timer:send_interval(Time, interval),
    State#state{timer=TimerRef}.

% gen_server behaviour

init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    ok = mlc_data:init_link_table(Context),

    Config = [
        % How often initiate a check
        {mod_link_check, check_interval, timer:hours(24)},
        % How long ago was last check
        {mod_link_check, check_age, timer:hours(24 * 7)},
        % Ignore internal links
        {mod_link_check, ignore_internal, true}
    ],
    ok = mlc_data:install_config(Config, Context),

    InitState = #state{context=z_context:new(Context)},
    CheckInterval = z_convert:to_integer(
        m_config:get_value(mod_link_check, check_interval, Context)
    ),
    InitState2 = set_timer(CheckInterval, InitState),
    {ok, InitState2}.

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

% Force re-check of all known links
handle_cast({check, force}, #state{context=Context} = State) ->
    Urls = mlc_data:get_urls_distinct(Context),
    mlc_crawler:spawn_checks(Urls, self()),
    {noreply, State};

% Re-checks links that haven't been verified since max age
handle_cast({check, fresh}, #state{context=Context} = State) ->
    CheckAge = z_convert:to_integer(
        m_config:get_value(mod_link_check, check_age, Context)
    ),
    Urls = mlc_data:get_urls_aged(CheckAge, z_acl:sudo(Context)),
    mlc_crawler:spawn_checks(Urls, self()),
    {noreply, State};

% Re-checks links belonging to RscId
handle_cast({check, RscId}, #state{context=Context} = State) when is_integer(RscId) ->
    Urls = mlc_data:get_urls_byid(RscId, true, Context),
    mlc_crawler:spawn_checks(Urls, self()),
    {noreply, State};

% Re-checks links ..
% ... only useful if link is in link table and valid
handle_cast({check, Links}, #state{context=Context} = State) when is_list(Links) ->
    IgnoreInternal = z_convert:to_bool(
        m_config:get_value(mod_link_check, ignore_internal, Context)
    ),
    CheckLinks = lists:filter(
        fun(Link) ->
            case mlc_parser:pre_crawl_status(Link, IgnoreInternal) of
                valid -> true;
                _ -> false
            end
        end,
        Links
    ),
    mlc_crawler:spawn_checks(CheckLinks, self()),
    {noreply, State};

% Delete links belonging to RscId
handle_cast({delete, RscId}, #state{context=Context} = State) ->
    mlc_data:update_link_table(RscId, [], Context),
    {noreply, State};

% Updates the status of a link
handle_cast({update_status, [Url, Status]}, #state{context=Context} = State) ->
    mlc_data:update_status(Url, Status, Context),
    z_depcache:flush({m_link, problems}, Context),
    {noreply, State};

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(interval, State) ->
    gen_server:cast(self(), {check , fresh}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Utilities for casting messages to the module

% @doc Get module Pid
find_pid(Context) ->
    case z_module_manager:whereis(?MODULE, Context) of
        {ok, Pid} -> Pid;
        _ -> undefined
    end.

% @doc Cast a message to the module Pid
cast(Msg, Context) ->
    case find_pid(Context) of
        undefined -> undefined;
        Pid -> gen_server:cast(Pid, Msg)
    end.

%% Zotonic events and notifications

event(#postback{message={check, [{url, Url}]}}, Context) ->
    cast({check, [Url]}, Context),
    Context;
event(#postback{message=_Postback}, Context) ->
    Context.

pid_observe_custom_pivot(Pid, #custom_pivot{id=RscId}, Context) ->
    SudoContext = z_acl:sudo(Context),
    Links = mlc_parser:find_links(RscId, SudoContext),
    mlc_data:update_link_table(RscId, Links, SudoContext),
    case Links of
        [] -> undefined;
        _ -> gen_server:cast(Pid, {check, Links})
    end,
    none.

pid_observe_rsc_delete(Pid, #rsc_delete{id=RscId}, _Context) ->
    gen_server:cast(Pid, {delete, RscId}),
    undefined.

observe_admin_menu(admin_menu, Acc, Context) -> [
     #menu_item{id=link_check,
                parent=admin_modules,
                label=?__("External Link Checker", Context),
                url={admin_link_check},
                visiblecheck={acl, use, ?MODULE}}

     |Acc].
