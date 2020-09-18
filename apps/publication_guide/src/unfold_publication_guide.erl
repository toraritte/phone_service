-module(unfold_publication_guide).
-export([do/1]).

-define
 % The "vocabulary" of a publication guide
 ( ITEM_KEY_PROCESS_ORDER
 , [ sub_items % To go deep first
   , query     % To resolve queries before saving links
   , link_to   % Run TWO may still have queries; resolve them first
   , link_id   % Should only be eliminated in run ONE, but resolve all items first
   % order not important:
   , title
   , type
   , path
   , issue
   , publication
   % internal keys:
   , link_to_id
   ]
 ).

-type query() :: id | issue | path_ref.
-type collection_type() :: category | publication | section.
-type article() :: #{ type        := article
                    , title       := string()
                    , path        := string()
                    , publication := string()
                    , issue => string()
                    % what else?
                    }.
-type regular_item() :: #{ type := collection_type()
                         , title     => string()
                         , link_id   => string()
                         % TODO Is this allowed?
                         , sub_items => list(content_item())
                         , query     => { query(), string() }
                         }.
-type resolved_item() :: #{ type  := collection_type()
                          , title := string()
                          % TODO Is this allowed?
                          , sub_items => list(content_item())
                          }.
% Overwriting linked attributes (e.g., title) is not allowed.
% TODO Is there a use case that would make enabling it worth it?
-type link_item() :: #{ link_to := string() }.
-type content_item() :: regular_item() | link_item() | article().
-type publication_guide() :: regular_item().

-type links() :: #{ string() => content_item()
                  , { link_to, reference() } => string()
                  }.

% TODO there is something monadic going on, but need to do more research
-spec do( publication_guide() ) -> resolved_item().
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
      , (futil:curry(fun erlang:element/2))(1)
      ])
.

% I wonder how this would have been done in Haskell or
% PureScript  because  there is  definitely  something
% monadic going on
-spec do_item({ content_item(), links() })
           -> { content_item(), links() }.
do_item
( { #{} = ContentItem
  , #{} = Links
  }
)
->
    logger:notice(#{ a => do_item, item => maps:remove(sub_items, ContentItem)})
,   futil:pipe(
      [ ContentItem
      , (futil:cflip(fun validate_item/2))
          (?ITEM_KEY_PROCESS_ORDER)
      , ((futil:curry(fun do_item_row/3))
          (?ITEM_KEY_PROCESS_ORDER))(Links)
      ])
.

% TODO
% Take  if   further  to  validate   specific  content
% item  types  (viz., `article`,  `collection_type()`,
% `regular_item()`, `resolve_item()`)?
-spec validate_item( content_item(), list(atom()) )
      -> content_item() | no_return().
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
            ContentItem
    ;   Other ->
            error(invalid_item_key, Other)
    end
.
% }}-

do_item_row % {{-
( [ItemKey|Rest] = R
, #{} = Links
, #{} = ContentItem
)
->
    logger:notice(#{ a => do_item_row, item => maps:remove(sub_items, ContentItem), rows => R})
,   { NewContentItem
    , NewLinks
    } =
        futil:pipe
          ([ ItemKey
           , (futil:cflip(fun proplists:lookup/2))
               (maps:to_list(ContentItem))
           % , fun(X) -> logger:notice(#{ a => do_item_row, row => X}), X end
           % `Row` is a `{key, Value}` in `ContentItem`
           , fun(Row) -> resolve_item(Row, ContentItem, Links) end
           % , fun(X) -> logger:notice(#{ a => do_item_row, resolved_item => X}), X end
           ])

,   do_item_row
      ( Rest
      , NewLinks
      , NewContentItem
      )
;

% }}-
do_item_row % {{-
( []
, #{} = Links
, #{} = ContentItem
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
    % logger:notice(#{ link_id => LinkID, links => Links}),

    { _NewContentItem
    , _NewLinks
    } =
        case { maps:get(LinkID, Links, undefined)
             , ContentItem
             }
        of
            % All items should be  resolved during the SECOND run,
            % so this  can only mean  that a `link_to`  item could
            % not be resolved on FIRST run (i.e., the `link_id` is
            % still further down  the line), but it  is located in
            % an  item  that  has  been  saved  in  `links()`  for
            % a  higher  level  `link_id`,  and when  there  is  a
            % `link_to` item  to this higher level  `link_id`, the
            % module will  try resolve that save  item (who knows,
            % maybe be all  the data is there by now),  so it will
            % do everything as if it  saw that item the first time
            % - including  trying to  add a `link_to_id`  which is
            % not necessary. So ignore  that if `link_to` item has
            % already been tagged.
            NOPE, this is wrong, endless looping ensues
            { undefined
            , #{ link_to_id := _ }
            }
            ->
                { ContentItem
                , Links
                }

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
                , maps:put({link_to, LinkToID}, LinkID, Links)
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
    logger:notice(#{ link_id => LinkID, links => Links}),

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
resolve_item(none, ContentItem, Links) -> % {{-
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
