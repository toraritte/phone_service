-module(unfold_publication_guide).
-export([do_content_item/1]).

do(PublicationGuide) ->

do_content_item
( #{} = ContentItem
, #{} = Links
)
->
    futil:pipe(
      [ resolve_content_item(ContentItem, Links)
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

    case { get(LinkID)
         , ContentItem
         }
    of
        % This means that the provided `LinkID` to a `link_to`
        % map is  invalid as it  could not be resolved  in two
        % passes - meaning it does not exist.
        { undefined
        , #{ link_to_id := _ }
        }
        ->
            error(wrong_link_id, [ContentItem]);

        % The `link_to` map could not be resolve during first
        % pass; `LinkID` may show up later.
        { undefined
        , _
        }
        ->
            LinkToID = erlang:make_ref(),
            put({link_to, LinkToID}, whatever),
            ContentItem#{ link_to_id => LinkToID };

        % Link could only be resolved in the second pass.
        { LinkedItem
        , #{ link_to_id := LinkToID }
        }
        ->
            erase({link_to, LinkToID}),
            LinkedItem;

        % Link resolved on first pass.
        { LinkedItem
        , _
        }
        ->
            LinkedItem
    end;

resolve_content_item(#{} = ContentItem) ->
    ContentItem.

save_links
( #{ link_id := LinkID } = ResolvedContentItem
, #{} = Links
)
->
    SanitizedItem =
        maps:remove(link_id, ResolvedContentItem),

    NewLinks =
        case get(LinkID) of
            undefined ->
                maps:put(LinkID, SanitizedItem, Links);
            _ ->
                error(duplicate_link, [ResolvedContentItem])
        end,

    { SanitizedItem
    , Links
    };

save_links
( #{} = ContentItem
, #{} = Links
)
->
    { ContentItem
    , Links
    };
