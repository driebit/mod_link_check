%% @author Driebit <tech@driebit.nl>
%% @copyright 2017

-module(mlc_data).
-export([
    init_link_table/1,
    reset_link_table/1,

    get_urls_all/1,
    get_urls_distinct/1,
    get_urls_problem/1,
    get_urls_problem/2,
    get_urls_byid/2,
    get_urls_byid/3,
    get_urls_aged/2,

    update_link_table/3,
    update_status/3,

    install_config/2
]).

-include_lib("zotonic.hrl").

% @doc Creates the link table if not yet present
init_link_table(Context) ->
    case z_db:table_exists(external_links, Context) of
        true -> ok;
        false ->
            [] = z_db:q("
                create table external_links (
                    id SERIAL,
                    rsc_id int not null,
                    url text,
                    last_check timestamp with time zone,
                    last_success timestamp with time zone,
                    last_status int,
                    error_reason text,
                    invalid boolean,

                    constraint external_links_pkey primary key (id),
                    constraint fk_external_links_rsc_id foreign key (rsc_id)
                        references rsc(id) on delete cascade
                );
            ", Context),
            ok
    end.

% @doc Resets the link table, removes all known links and parse resource texts.
reset_link_table(Context) ->
    z_db:q("drop table external_links", Context),
    init_link_table(Context).

% @doc Unpack single elment result rows
rows_to_elements(Rows) ->
    lists:map(
        fun({Elm}) -> Elm end,
        Rows
    ).

% @doc Get all URLs in the links table
get_urls_all(Context) ->
    z_db:assoc("select * from external_links;", Context).

% @doc Get all valid and distinct URLS from the links table
get_urls_distinct(Context) ->
    Urls = z_db:q(
        "select distinct url from external_links where invalid = false;",
        Context
    ),
    rows_to_elements(Urls).

% @doc Get all URL's that need editorial attention
get_urls_problem(Context) ->
    z_db:assoc(
        "select * from external_links where invalid = true OR last_status >= 300 or last_status is null;",
        Context
    ).

% @doc Get all URL's that need editorial attention
get_urls_problem({Limit, Offset}, Context) ->
    z_db:assoc(
        "select * from external_links where invalid = true OR last_status >= 300 or last_status is null limit $1 offset $2;",
        [Limit, Offset],
        Context
    ).

% @doc Get all distinct URLS by resource id
get_urls_byid(RscId, Context) ->
    Urls = z_db:q(
        "select distinct url
         from external_links
         where rsc_id = $1 and invalid = false;",
        [RscId],
        Context
    ),
    rows_to_elements(Urls).

get_urls_byid(RscId, Valid, Context) ->
    Invalid = not Valid,
    Urls = z_db:q(
        "select distinct url
         from external_links
         where rsc_id = $1 and invalid = $2;",
        [RscId, Invalid],
        Context
    ),
    rows_to_elements(Urls).

% @doc Get all URLs that haven't been checked since milliseconds ago
get_urls_aged(Age, Context) ->
    % Note: regular z_db:q string interpolation fails here...
    % ... so therefore this hack
    AgeString = z_convert:to_list(Age),
    WherePart = string:join([
        "where last_check IS NULL and invalid = false ",
        "or last_check < (now() - interval '",
        AgeString,
        " milliseconds');"
    ], ""),
    Urls = z_db:q(
        "select distinct url from external_links " ++ WherePart,
        Context
    ),
    rows_to_elements(Urls).

% @doc Updates what links are known for a given resource
% Note: Links that are not passed in are removed for that resource
update_link_table(RscId, [], Context) ->
    z_db:q(
        "delete from external_links where rsc_id = $1;",
        [RscId],
        Context
    );
update_link_table(RscId, Links, Context) ->
    LinkIds = lists:filtermap(
        fun(Link) ->
            case z_db:q("select id from external_links where rsc_id = $1 and url = $2;",
                        [RscId, Link],
                        Context) of
                [] ->
                    IgnoreInternal = z_convert:to_bool(
                        m_config:get_value(mod_link_check, ignore_internal, Context)
                    ),
                    PreCrawlStatus = mlc_parser:pre_crawl_status(Link, IgnoreInternal),
                    case PreCrawlStatus of
                        ignore -> false;
                        _ ->
                            IsInvalid = (PreCrawlStatus == invalid),
                            Props = [{rsc_id, RscId},
                                     {url, Link},
                                     {invalid, IsInvalid}],
                            {ok, LinkId} = z_db:insert(external_links, Props, Context),
                            {true, LinkId}
                    end;
                [{LinkId}] ->
                    {true, LinkId}
            end
        end,
        Links
    ),
    % Delete links from table that are no longer in the resource
    case LinkIds of
        [] ->
            update_link_table(RscId, [], Context);
        _ ->
            IdArg = string:join(lists:map(fun z_convert:to_list/1, LinkIds), ","),
            z_db:q(
                "delete from external_links where rsc_id = $1 and id NOT IN (" ++ IdArg ++ ");",
                [RscId],
                Context
            )
    end.

% @doc Internal function that writes props to all links with URL
write_props_to_url(Url, Props, Context) ->
    LinkIds = z_db:q("select id from external_links where url = $1;",
                     [Url],
                     Context),
    lists:foreach(
        fun({LinkId}) ->
            z_db:update(external_links, LinkId, Props, Context)
        end,
        LinkIds
    ).

% @doc Updates the response status for a given URL in the database
update_status(Url, Status, Context) when is_integer(Status) ->
    Now = calendar:universal_time(),
    Props1 = case Status >= 300 of
        true -> [];
        false -> [{last_success, Now}]
    end,
    Props2 = [{last_check, Now},{last_status, Status},{error_reason, undefined}|Props1],
    write_props_to_url(Url, Props2, Context);
update_status(Url, Status, Context) when is_atom(Status); is_binary(Status) ->
    Now = calendar:universal_time(),
    Props = [{last_check, Now}, {error_reason, Status}, {last_status, undefined}],
    write_props_to_url(Url, Props, Context);
update_status(Url, {Reason, _}, Context) when is_atom(Reason) ->
    update_status(Url, Reason, Context);
update_status(Url, _Status, Context) ->
    ?zWarning("Unknown status update for: ", Context),
    ?zWarning(Url, Context),
    ok.

% @doc Installs the configuration options when not yet configured.
install_config(Config, Context) ->
    lists:foreach(
        fun({Mod, Key, Value}) ->
            m_config:get(Mod, Key, Context),
            case m_config:get(Mod, Key, Context) of
                undefined ->
                    m_config:set_value(Mod, Key, Value, Context);
                _ ->
                    ok
            end
        end,
        Config
    ),
    ok.
