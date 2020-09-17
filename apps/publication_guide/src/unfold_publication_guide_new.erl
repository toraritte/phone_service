-module(unfold_publication_guide_new).
-export([do_item/1, do/1]).

% TODO there is something monadic going on, but need to do more research
do(#{} = PublicationGuide) ->

    futil:pipe(
      [ { PublicationGuide
        , #{} % Links
        }
      % Two runs to make sure all links are resolved
      % , fun(X) -> logger:notice(#{ a => "run ONE", b => X }), X end
      , fun(X) -> logger:notice(#{ a => "run ONE"}), X end
      , fun do_item/1
      % , fun(X) -> logger:notice(#{ a => "run TWO", b => X }), X end
      , fun(X) -> logger:notice(#{ a => "run TWO"}), X end
      , fun do_item/1
      ])
.

do_item
( { #{} = ContentItem
  , #{} = Links
  }
)
->
    % The "vocabulary" of a publication guide
    KeyProcessOrder =
        [ sub_items % To go deep first
        , query     % To resolve queries before saving links
        , link_to   % Run TWO may still have queries; resolve them first
        , link_id   % Should only be eliminated in run ONE, but resolve all items first
        % order not important:
        , title
        , type
        , path
        , publication
        % internal keys:
        , link_to_id
        ]

,   validate_item(ContentItem, KeyProcessOrder)

% ,   logger:notice(#{ a => do_item, item => maps:remove(sub_items, ContentItem), valid => validate_item(ContentItem, KeyProcessOrder), links => Links})

,   do_item_row
      ( KeyProcessOrder
      , ContentItem
      , Links
      )
.

validate_item(ContentItem, KeyProcessOrder) -> % {{-

    ItemKeysSet =
        futil:pipe(
          [ ContentItem
          , fun maps:keys/1
          , fun sets:from_list/1
          ])

,   KeyProcessOrderSet =
        sets:from_list(KeyProcessOrder)

,   Diff =
        futil:pipe(
          [ sets:subtract(ItemKeysSet, KeyProcessOrderSet)
          , fun sets:to_list/1
          ])

,   case Diff of
        [] ->
            ok
    ;   Other ->
            error(invalid_item_key, Other)
    end
.
% }}-

do_item_row % {{-
( [ItemKey|Rest]
, #{} = ContentItem
, #{} = Links
)
->
    { NewContentItem
    , NewLinks
    } =
        futil:pipe
          ([ ItemKey
           , fun(Key) ->
                case maps:get(Key, ContentItem, not_present) of
                    not_present -> not_present
                ;   Value -> { ItemKey, Value }
                end
             end
           % , fun(X) -> logger:notice(#{ a => do_item_row, row => X}), X end
           % `Row` is a `{key, Value}` in `ContentItem`
           , fun(Row) -> resolve_item(Row, ContentItem, Links) end
           % , fun(X) -> logger:notice(#{ a => do_item_row, resolved_item => X}), X end
           ])

,   do_item_row
      ( Rest
      , NewContentItem
      , NewLinks
      )
;

% }}-
do_item_row % {{-
( []
, #{} = ContentItem
, #{} = Links
)
->
    { ContentItem, Links }
.
% }}-

resolve_item({ sub_items, SubItems }, ContentItem, Links) -> % {{-

    { NewSubItems
    , NewLinks
    } =
        do_subitems(SubItems, [], Links)

,   NewContentItem =
        ContentItem#{ sub_items => NewSubItems }

,    { NewContentItem, NewLinks }
;

% }}-
% Probably bad, but assuming that every query returns a list of articles, and that they all belong to the same publication.
resolve_item({ query, _Query }, ContentItem, Links) -> % {{-
    % Result = [ Article ]
    % Article = #{ title => String
    %            , path  => Path
    %            , publication => String
    %            , issue => String (?)
    %            , ...
    %            }
    % Path = URL | FilesystemPath

    % (See note at "% QUERY" comment in `publication_guide.erl`.)
    [ #{ publication := Publication } | _ ] =
    Result =
        % TODO
        % content_storage_api:query(Query),
        [ #{ title => a, path => a, publication => a }
        , #{ title => b, path => b, publication => a }
        , #{ title => c, path => c, publication => a }
        ]

,   Articles =
        lists:map
          ( fun(Item) -> maps:put(type, article, Item) end
          , Result
          )

,   NewContentItem =
        futil:pipe(
          [ ContentItem
          % Use the title of the `ContentItem` if none speficied in the publication guide
          , (futil:curry(fun maps:merge/2))(#{ title => Publication })
          , (futil:curry(fun maps:remove/2))(query)
          , ((futil:curry(fun maps:put/3))(sub_items))(Articles)
          ])

,    { NewContentItem, Links }
;

% }}-
resolve_item({ link_to, LinkID }, ContentItem, Links) -> % {{-

    { _NewContentItem
    , _NewLinks
    } =
        case { maps:get(LinkID, Links, undefined)
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
                error(wrong_link_id, [ContentItem])

            % The `link_to` map could not be resolve during first
            % pass; `LinkID` may show up later.
        ;   { undefined
            , _
            }
            ->
                LinkToID = erlang:make_ref()

                % The  dance  with  the  `link_to_id`  and  `{link_to,
                % LinkID}` reminders are necessary  because a link can
                % be used  multiple times,  and want to  track whether
                % all of them got resolved correctly.
            ,   { ContentItem#{ link_to_id => LinkToID }
                %          |-----key---------|  |value-|  |map|
                , maps:put({link_to, LinkToID}, whatever, Links)
                }

            % Links resolved on
            % FIRST pass  -> no `link_to_id` in `ContentItem`
            % SECOND pass -> `link_to_id` present
        ;   { LinkedItem
            , _
            }
            ->
                % Without going  through the  saved links  again, some
                % `{link_to, ...}` tuples would still remain
                { DefinitelyResolvedItem
                , NewLinks
                } =
                    do_item({ LinkedItem, Links })
                % If  link  resolved  in  FIRST pass,  there  will  be
                % no   `link_to_id`  (unlike   in  the   SECOND  pass)
                % but   handling  it   here   with  `maps:get/3`   and
                % `maps:remove/2` - if not there, noop.
            ,   RemoveLinkToID =
                    fun () ->
                        LinkToID = maps:get(link_to_id, ContentItem, undefined)
                    ,   maps:remove({link_to, LinkToID}, NewLinks)
                    end

                % TODO This will entirely replace the current item.
                %      Should this be merge instead?
            ,   { DefinitelyResolvedItem
                , RemoveLinkToID() % = Links
                }
        end
;

% }}-
resolve_item({ link_id, LinkID }, ContentItem, Links) -> % {{-

    SanitizedItem =
        maps:remove(link_id, ContentItem)

% ,   logger:notice(#{ t => T, item => ContentItem, links => Links})

,   NewLinks =
        case maps:get(LinkID, Links, undefined) of
            undefined ->
                maps:put(LinkID, SanitizedItem, Links)
        ;   _ ->
                error(duplicate_link_id, [LinkID])
        end

,   { SanitizedItem, NewLinks }
;

% }}-
resolve_item(not_present, ContentItem, Links) -> % {{-
    { ContentItem, Links }
;

% }}-
% `ContentItem` keys whose order  is not specified and
% do not need processing
resolve_item(_, ContentItem, Links) -> % {{-
    { ContentItem, Links }
.
% }}-

do_subitems([ContentItem|Rest], Acc, #{} = Links) ->

    { NewContentItem
    , NewLinks
    } =
        do_item({ ContentItem, Links })

,   do_subitems
      ( Rest
      , [NewContentItem|Acc]
      , NewLinks
      )
;

do_subitems([], Acc, #{} = Links) ->
    { lists:reverse(Acc)
    , Links
    }
.

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
