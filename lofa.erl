-module(lofa).
-export([do_content_item/1]).

do_content_item( #{} = ContentItem) ->
    futil:pipe(
      [ ContentItem
      , fun resolve_content_item/1
      , fun save_links/1
      ]).

resolve_content_item(#{ sub_items := _ } = ContentItem) ->
    DoSubItems =
        (futil:curry(fun lists:map/2))
          (fun do_content_item/1),
        % OR
        % fun(SubItem) ->
        %     lists:map(fun do_content_item/1, SubItem)
        % end,

    maps:update_with
      ( sub_items
      , DoSubItems
      , ContentItem
      );

% Probably bad, but assuming that every query returns a list of articles, and that they all belong to the same publication.
resolve_content_item(#{ query := _Query } = QueryItem) ->
    % Result = [ Article ]
    % Article = #{ title => String
    %            , path  => Path
    %            , publication => String
    %            , issue => String (?)
    %            , ...
    %            }
    % Path = URL | FilesystemPath

    % (See note at "% QUERY" comment.)
    [ #{ publication := Publication } | _ ] =
    Result =
        % TODO
        % content_storage_api:query(Query),
        [ #{ title => a, path => a, publication => a }
        , #{ title => b, path => b, publication => a }
        , #{ title => c, path => c, publication => a }
        ],

    Articles =
        lists:map
          ( fun(Item) -> maps:put(type, article, Item) end
          , Result
          ),

    futil:pipe(
      [ QueryItem
      % Use the title of the `QueryItem` if none speficied in the publication guide
      , (futil:curry(fun maps:merge/2))(#{ title => Publication })
      , (futil:curry(fun maps:remove/2))(query)
      , ((futil:curry(fun maps:put/3))(sub_items))(Articles)
      ]);

resolve_content_item(#{ link_to := LinkID } = ContentItem) ->

    case get(LinkID) of
        undefined  ->
            ContentItem;
        LinkedItem ->
            LinkedItem
    end;

resolve_content_item(#{} = ContentItem) ->
    ContentItem.

save_links
( #{ link_id := LinkID } = ResolvedContentItem
)
->
    SanitizedItem =
        maps:remove(link_id, ResolvedContentItem),

    % logger:notice(#{ save_links => get(LinkID), sanitized_item => SanitizedItem, link_id => LinkID }),
    case get(LinkID) of
        undefined ->
            put(LinkID, SanitizedItem);
        _ ->
            error(duplicate_link, [ResolvedContentItem])
    end,
    SanitizedItem;

save_links(#{} = ContentItem) ->
    ContentItem.

linkto_count(increase)
