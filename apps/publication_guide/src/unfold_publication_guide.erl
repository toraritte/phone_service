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

-type link_id() :: string().
% -type query() :: id | issue | path_ref.
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
                         , link_id   => link_id()
                         % TODO Is this allowed?
                         , sub_items => list(content_item())
                         % , query     => { query(), string() }
                         , query     => term()
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

% -type links() :: #{ string() => content_item()
%                   , { link_to, reference() } => string()
%                   }.

-type link_to_id() :: { link_to, reference() }.
% A  `link_id` can  be reused  multiple times,  and so
% `link_to`s need  to be  tracked somehow  whether the
% `publication_guide()`  got fully  resolved. (Queries
% on the other  hand are standalone, even  if the same
% query is used multiple times.)
-type link_tos() :: list({ link_to_id(), link_id() }).
-type links() :: list({ link_id(), content_item() }).
% -type queries() :: proplists:proplist().
% TODO Not sure how queries will look like
-type queries() :: list( any() )
-type acc() :: { links(), link_tos(), queries() }.

-type item_row() :: proplists:property().
-type row_callback() :: fun( ( item_row(), content_item(), acc() )
                          -> { content_item(), acc() }).

% TODO there is something monadic going on, but need to do more research
-spec do( publication_guide() ) -> resolved_item().
do(#{} = PublicationGuide) ->

    Collect =
        lists:map
          ( fun(Key) -> { Key, fun collect_links_and_queries/4 } end
          , [ query, link_id ]
          )

,   Expand =
        lists:map
          ( fun(Key) -> { Key, fun expand_links_and_queries/4 } end
          , [ query, link_to ]
          )

,   futil:pipe(
      [ { PublicationGuide
        , { [], [] } % = Acc
        }
      % , fun(X) -> logger:notice(#{ a => "run ONE", b => X }), X end
      , fun(X) -> logger:notice(#{ a => "run ONE"}), X end
      , (futil:curry(fun do_item/2))(Collect)
      % , fun(X) -> logger:notice(#{ a => "run TWO", b => X }), X end
      , fun(X) -> logger:notice(#{ a => "run TWO"}), X end
      , (futil:curry(fun do_item/2))(Expand)
      % , (futil:curry(fun erlang:element/2))(1)
      ])
.

% I wonder how this would have been done in Haskell or
% PureScript  because  there is  definitely  something
% monadic going on
% -spec do_item({ content_item(), links() })
%            -> { content_item(), links() }.
% do_item
% ( { #{} = ContentItem
%   , RowCallbacks
%   % , #{} = Links
%   }
% )
% ->
%     % logger:notice(#{ a => do_item, item => maps:remove(sub_items, ContentItem)})
%     iterate_item_rows(RowCallbacks, ContentItem)
% % ,   futil:pipe(
% %       [ ContentItem
% %       , ((futil:curry(fun iterate_item_rows/3))
% %           (RowProcessOrder))(RowCallbacks)
% %           % (?ITEM_KEY_PROCESS_ORDER))(Links)
% %       ])
% .

% TODO validate {{-
% Take  if   further  to  validate   specific  content
% item  types  (viz., `article`,  `collection_type()`,
% `regular_item()`, `resolve_item()`)?
% NOTE Decomissioning for now.
% Too restrictive;  queries may return maps  with keys
% not in the allowed list so this module would have to
% evolve  with  external  modules. Instead  it  should
% validate item types based on their restrictions (see
% todo note above.
% -spec validate_item( content_item(), list(atom()) )
%       -> content_item() | no_return().
% validate_item(ContentItem, KeyProcessOrder) -> % {{-

%     ItemKeysSet =
%         futil:pipe(
%           [ ContentItem
%           , fun maps:keys/1
%           , fun sets:from_list/1
%           ])

% ,   KeyProcessOrderSet =
%         sets:from_list(KeyProcessOrder)

% ,   Diff =
%         futil:pipe(
%           [ sets:subtract(ItemKeysSet, KeyProcessOrderSet)
%           , fun sets:to_list/1
%           ])

% ,   case Diff of
%         [] ->
%             ContentItem
%     ;   Other ->
%             error(invalid_item_key, Other)
%     end
% .
% }}-
% }}-

do_item
( RowCallbacks
, { #{} = ContentItem
  , Acc
  }
)
when is_list(RowCallbacks)
->
    iterate_item_rows
      ( [ { sub_items, fun sub_items/3 }
        | RowCallbacks
        ]
      , Acc
      , ContentItem
      )
.

% -spec iterate_item_rows
%       ( list( atom() )
%       , row_callback()
%       , acc()
%       , content_item()
%       )
%       -> { content_item()
%          , acc()
%          }.
iterate_item_rows % {{-
( [ { CallbackKey, RowFunction }
  | RestCallbacks
  ]
  = RowCallbacks
, { Links
  , Queries
  }
  = Acc
, #{} = ContentItem
)
->
    { NewContentItem
    , NewAcc
    } =
        case maps:get(CallbackKey, ContentItem, undefined) of

            RowValue ->

                RowFunction
                  ( { CallbackKey, RowValue } % = Row
                  , RowCallbacks
                  , ContentItem
                  , Acc
                  )

        % When key is not found, it means that
        % 1. the row doesn't need to be processed, or
        % 2. there is a typo in `CallbackKey` in `RowCallbacks`
        %    TODO Implement a validation function.
        ;   undefined ->
                { ContentItem, Acc }
        end

,   iterate_item_rows
      ( RestCallbacks
      , NewAcc
      , NewContentItem
      )
;

% }}-
iterate_item_rows % {{-
( []
, _RowCallbacks
, Acc
, #{} = ContentItem
)
->
    { ContentItem, Acc }
.
% }}-

% -spec collect_links_and_queries
%       ( { sub_items
%         , list( content_item() )
%         }
%       , content_item()
%       , acc()
%       )
%       ->
% All queries are article  queries, and these could be
% from the same publication  or by a certain criteria,
% such as  having the same tag(s).  (Requesting a list
% of  publications would  make their  order undefined,
% and  that is  something  listeners rely  on, but  it
% could not be guaranteed among restarts.)
collect_links_and_queries
( { query, Query }
, _RowCallbacks
, ContentItem
, { _Links, _LinkTos, Queries }
)
-> % {{-
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
collect_links_and_queries({ link_id, LinkID }, ContentItem, Links) -> % {{-
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

sub_items({ sub_items, SubItems }, RowCallbacks, ContentItem, Acc) -> % {{-

    { NewSubItems
    , NewAcc
    } =
        do_subitems(SubItems, RowCallbacks, [], Acc)

,   NewContentItem =
        ContentItem#{ sub_items => NewSubItems }

,   { NewContentItem, NewAcc }
.

do_subitems([ContentItem|RestItems], RowCallbacks, SubItemAcc, Acc) ->

    { NewContentItem
    , NewAcc
    } =
        do_item(RowCallbacks, { ContentItem, Acc })

,   do_subitems
      ( RestItems
      , RowCallbacks
      , [ NewContentItem|SubItemAcc ]
      , NewAcc
      )
;

do_subitems([], _RowCallback, SubItemAcc, #{} = Acc) ->
    { lists:reverse(SubItemAcc)
    , Acc
    }
.

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
