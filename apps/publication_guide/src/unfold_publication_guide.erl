-module(unfold_publication_guide).
-export([do/1, testguide/0]).

% -define
%  % The "vocabulary" of a publication guide
%  ( ITEM_KEY_PROCESS_ORDER
%  , [ sub_items % To go deep first
%    , query     % To resolve queries before saving links
%    , link_to   % Run TWO may still have queries; resolve them first
%    , link_id   % Should only be eliminated in run ONE, but resolve all items first
%    % order not important:
%    , title
%    , type
%    , path
%    , issue
%    , publication
%    % internal keys:
%    , link_to_id
%    ]
%  ).

% -type link_id() :: string().
% % -type query() :: id | issue | path_ref.
% -type collection_type() :: category | publication | section.
% -type article() :: #{ type        := article
%                     , title       := string()
%                     , path        := string()
%                     , publication := string()
%                     , issue => string()
%                     % what else?
%                     }.
% -type regular_item() :: #{ type := collection_type()
%                          , title     => string()
%                          , link_id   => link_id()
%                          % TODO Is this allowed?
%                          , sub_items => list(content_item())
%                          % , query     => { query(), string() }
%                          , query     => term()
%                          }.
% -type resolved_item() :: #{ type  := collection_type()
%                           , title := string()
%                           % TODO Is this allowed?
%                           , sub_items => list(content_item())
%                           }.
% % Overwriting linked attributes (e.g., title) is not allowed.
% % TODO Is there a use case that would make enabling it worth it?
% -type link_item() :: #{ link_to := string() }.
% -type content_item() :: regular_item() | link_item() | article().
% -type publication_guide() :: regular_item().

% % -type links() :: #{ string() => content_item()
% %                   , { link_to, reference() } => string()
% %                   }.

% -type link_to_id() :: { link_to, reference() }.
% % A  `link_id` can  be reused  multiple times,  and so
% % `link_to`s need  to be  tracked somehow  whether the
% % `publication_guide()`  got fully  resolved. (Queries
% % on the other  hand are standalone, even  if the same
% % query is used multiple times.)
% -type link_tos() :: list({ link_to_id(), link_id() }).
% -type links() :: list({ link_id(), content_item() }).
% % -type queries() :: proplists:proplist().
% % TODO Not sure how queries will look like
% -type queries() :: list( any() ).
% -type acc() :: { links(), link_tos(), queries() }. % TODO amend!

% -type item_row() :: proplists:property().
% -type row_callback() :: fun( ( item_row(), content_item(), acc() )
%                           -> { content_item(), acc() }).

% TODO there is something monadic going on, but need to do more research
% -spec do( publication_guide() ) -> resolved_item().
do(#{} = PublicationGuide) ->

    % `sub_items/4`  is  responsible to  drive  recursion,
    % and  decided to  always add  it explicitly  to avoid
    % surprises. It is depth first.

    PhaseOneCallbacks =
        [ { sub_items, fun sub_items/4 }
        , { query,   fun send_queries/4 }
        % These are mutually exclusive.
        , { link_id, fun collect_link_ids/4 }
        , { link_to, fun expand_link_tos/4 }
        ]

,    Acc =
        #{ links    => #{}
         , link_found => false
         , queries  => #{}
         }

,   SecondRun =
        [ { sub_items, fun sub_items/4 }
        , { link_to, fun expand/4 }
        , { query,   fun expand/4 }
        ]

,   futil:pipe(
      [ { PublicationGuide
        , Acc
        }
      , fun(X) -> logger:notice(#{ a => "run ONE"}), X end
      , (futil:curry(fun do_item/2))(PhaseOneCallbacks)
      , (futil:curry(fun do_item/2))(PhaseOneCallbacks)
      , (futil:curry(fun do_item/2))(PhaseOneCallbacks)
      , (futil:curry(fun do_item/2))(PhaseOneCallbacks)
      , fun({P,_}) ->
                R = io_lib:format("~p",[P]),
                S = lists:flatten(R),
                length(string:split(S, "link_to")) =:= 1
        end

      % , (futil:curry(fun do_item/2))(PhaseOneCallbacks)
      % , (futil:curry(fun do_item/2))(PhaseOneCallbacks)
      % , (futil:curry(fun clear_links/2))(PhaseOneCallbacks)
      % , fun(X) -> logger:notice(#{ a => "run TWO"}), X end
      % , (futil:curry(fun do_item/2))(SecondRun)
      % , fun(X) -> logger:notice(#{ a => "run THREE"}), X end
      % , (futil:curry(fun do_item/2))(SecondRun)
      ])
.

% c("apps/publication_guide/src/unfold_publication_guide.erl"), f(), A =  unfold_publication_guide:do(unfold_publication_guide:testguide()), A.

% clear_links
% ( PhaseOneCallbacks
% , { PublicationGuide
%   , Acc }
% )
% ->
%     clear_links(P, T, { link_found, )
% ;

% clear_links
% ( PhaseOneCallbacks
% , { PublicationGuide, Acc }
% )
% ->
%     NewLinks =
%         maps:map
%           ( fun( _CallbackKey, ContentItem ) ->
%                 futil:pipe(
%                   [ { ContentItem
%                     , Acc
%                     }
%                   , (futil:curry(fun do_item/2))(PhaseOneCallbacks)
%                   , (futil:curry(fun erlang:element/2))(1)
%                   ])
%             end
%           , Links
%           )

% ,   { PublicationGuide
%     , Acc#{ links => NewLinks }
%     }
% .
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
    logger:notice(#{ a => do_item, content_item => ContentItem })
,   iterate_item_rows
      ( RowCallbacks
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
( [ { CallbackKey, RowFunction } | RestCallbacks ] = RowCallbacks
, Acc
, #{} = ContentItem
)
->
    logger:notice(#{ a => iterate_item_rows, content_item => ContentItem, rowcallbacks => RowCallbacks })
,   { NewContentItem
    , NewAcc
    } =
        case maps:get(CallbackKey, ContentItem, undefined) of

        % When key is not found, it means that
        % 1. the row doesn't need to be processed, or
        % 2. there is a typo in `CallbackKey` in `RowCallbacks`
        %    TODO Implement a validation function.
            undefined ->
                { ContentItem, Acc }

        ;   RowValue ->

                RowFunction
                  ( { CallbackKey, RowValue } % = Row
                  , RowCallbacks
                  , ContentItem
                  , Acc
                  )
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
, Acc
, #{} = ContentItem
)
->
    logger:notice(#{ a => iterate_item_rows, content_item => ContentItem})
,   { ContentItem, Acc }
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
% NOTE
% All queries are article  queries, and these could be
% from the same publication  or by a certain criteria,
% such as  having the same tag(s).  (Requesting a list
% of  publications would  make their  order undefined,
% and  that is  something  listeners rely  on, but  it
% could not be guaranteed among restarts.)
send_queries
( { query, _ } = Query
, _RowCallbacks
, ContentItem
, #{ queries := Queries } = Acc
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

    % As established  in the  NOTE, the query  results are
    % always  articles,  hence  they  hava  an  associated
    % publication  (or should.  By an  arbitrary decision,
    % saving the  publication of the first  one, that will
    % be  used  as the  title  of  the content  item  that
    % has  the query  - unless  it provides  its own.  See
    % `NewContentItem` below.
    % ( If the query returns a collection of articles from
    % different publications, it is obviously more prudent
    % to  provide  the  title  in the  content  item  when
    % drawing up the publication guide. )
    % TODO not resolving queries here, just collecting
    % [ #{ publication := Publication } | _ ] =
    Result =
        % TODO
        % content_storage_api:query(Query),
        [ #{ title => a, path => a, publication => a }
        , #{ title => b, path => b, publication => a }
        , #{ title => c, path => c, publication => a }
        ]

% ,   Articles =
%         lists:map
%           ( fun(Item) -> maps:put(type, article, Item) end
%           , Result
%           )
,   QueryToResolve =
        % Queries#{ Query => "resolution_placeholder" }
        Queries#{ Query => Result }

,   { ContentItem
    , Acc#{ queries => QueryToResolve }
    }
.
% }}-

collect_link_ids
( { link_id, LinkID }
, _RowCallbacks
, ContentItem
, #{ links := Links } = Acc
)
-> % {{-
    logger:notice(#{ a => collect_link_ids, content_item => ContentItem, link_id => LinkID, links => Links })
,   SanitizedItem =
        maps:remove(link_id, ContentItem)

,   NewLinks =
        case maps:get(LinkID, Links, undefined) of
            undefined ->
                Links#{ LinkID => SanitizedItem }
        ;   _ ->
                error(duplicate_link_id, [LinkID])
        end

,   { SanitizedItem
    , Acc#{ links => NewLinks }
    }
.
% }}-

expand
( { query, _ } = Query
, _RowCallbacks
, ContentItem
, #{ queries := ResolvedQuery } = Acc
)
-> % {{-
    #{ Query :=
       [ #{ publication := Publication } | _ ] = QueryResult
     } = ResolvedQuery
    % As established  in the  NOTE, the query  results are
    % always  articles,  hence  they  hava  an  associated
    % publication  (or should.  By an  arbitrary decision,
    % saving the  publication of the first  one, that will
    % be  used  as the  title  of  the content  item  that
    % has  the query  - unless  it provides  its own.  See
    % `NewContentItem` below.
    % ( If the query returns a collection of articles from
    % different publications, it is obviously more prudent
    % to  provide  the  title  in the  content  item  when
    % drawing up the publication guide. )

,   Articles =
        lists:map
          ( fun(Item) -> Item#{ type => article } end
          , QueryResult
          )

,   NewContentItem =
        futil:pipe(
          [ ContentItem
          % Use the title of the `ContentItem` if none speficied
          % in the publication guide
          , (futil:curry(fun maps:merge/2))(#{ title => Publication })
          , (futil:curry(fun maps:remove/2))(query)
          , ((futil:curry(fun maps:put/3))(sub_items))(Articles)
          ])

,   { NewContentItem
    , Acc
    }
.
% }}-

% }}-
expand_link_tos
( { link_to, LinkID }
, _RowCallbacks
, ContentItem
, #{ links := Links } = Acc
)
-> % {{-

    logger:notice(#{ a => expand_link_tos, content_item => ContentItem, linked_item => maps:get(LinkID, Links, undefined) })

,   case maps:get(LinkID, Links, undefined) of

        undefined ->
            { ContentItem
            , Acc
            }

    ;   LinkedItem ->
            { LinkedItem
            , Acc
            }
    end
.
% }}-

sub_items({ sub_items, SubItems }, RowCallbacks, ContentItem, Acc) -> % {{-

    logger:notice(#{ a => sub_items, content_item => ContentItem })
,   { NewSubItems
    , NewAcc
    } =
        do_subitems(SubItems, RowCallbacks, [], Acc)

,   NewContentItem =
        ContentItem#{ sub_items => NewSubItems }

,   { NewContentItem, NewAcc }
.

do_subitems([ContentItem|RestItems], RowCallbacks, SubItemAcc, Acc) ->

    logger:notice(#{ a => do_subitems, content_item => ContentItem, acc => Acc, subitem_acc => SubItemAcc })
,   { NewContentItem
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
    logger:notice(#{ a => do_subitems, acc => Acc, subitem_acc => SubItemAcc })
,   { lists:reverse(SubItemAcc)
    , Acc
    }
.

testguide() ->
    #{ type => category
     , title => "Main menu"
     , sub_items =>
       [ #{ link_to => "week-29" }
     % , #{ link_to => "non-existent" } % WORKS
       , #{ link_to => "week-30" }
       , #{ link_to => "drugs" }
       , #{ link_to => "safeway" }
       , #{ link_to => "week-30" }
       , #{ link_to => "norcal" }
       , #{ type => category
          , title =>  "Northern California newspapers and magazines"
          , link_id => "norcal"
          , sub_items =>
            [ #{ type => category
               , title => "AAA"
               , sub_items =>
                 [ #{ link_to => "safeway"}
                 , #{ type => category
                    , title => "BBB"
                    , sub_items =>
                      [ #{ link_to => "raleys" }
                      , #{ type => category
                         , title => "CCC"
                         , sub_items =>
                           [ #{ link_to => "drugs" }
                           ]
                         }
                      ]
                    }
                 ]
               }
            ]
          }
       , #{ type => category
          , title =>  "Store sales advertising"
          , sub_items =>
            [ #{ type => category
               , title =>  "Grocery stores"
               , sub_items =>
                 [ #{ type => publication
                    , title =>  "Safeway"
                    , link_id => "safeway"
                    , sub_items =>
                      [ #{ type => section
                         , link_id => "week-29"
                         , title =>  "Week 7/15/2020 to 7/21/2020"
                         , query => #{ publication => safeway, issue => "week-29" }
                         }
                      , #{ type => section
                         , title =>  "Week 7/22/2020 to 7/28/2020"
                         , link_id => "week-30"
                         , query => #{ publication => safeway, issue => "week-30" }
                         }
                      , #{ link_to => "raleys" }
                      % Will never resolve; "drugs" has a link to "safeway" (i.e., this item), making them mutually recursive
                      % , #{ link_to => "drugs" }
                      ]
                    }
                 , #{ type => publication
                    , query => #{ publication => "raleys" }
                    , link_id => "raleys"
                    }
                  % , title => "Raley's"
                  % , query => {id, "123e4567-e89b-12d3-a456-426614174000"}
                 ]
               }
            , #{ type => category
               , title =>  "Drug stores"
               , link_id => "drugs"
               , sub_items =>
               % LINKS
               % Always refer to the title. In the case of "Raley's" below, it means it has to match the title once it has been expanded above, but the `{query, ...}` syntax is probably less error prone, and it could be used at any time.
                 [ #{ link_to => "safeway" }
                 , #{ link_to => "raleys" }
                 % This should fail, and it does (because why would one include a link to the current section?)
                 % , #{ link_to => "drugs" } % WORKS (as in, it fails)
               % or
               % , #{ type => publication
               %    , query => {path_ref, "raleys"
               %    }
                 % TODO make sure that linked items' type fit the context! For example, the link below points to a "section" type so in this context it should be lifted to publication.
                 % Or should it? would this be an issue?
                 , #{ link_to => "week-29" }
               % or
               % , #{ type => section
               %    , title =>  "Week 7/22/2020 to 7/28/2020"
               %    , query => {tag, "week-30"}
               %    }
                 ]
               }
            , #{ type => category
               , title =>  "Discount stores"
               , sub_items =>
                 [ #{ link_to => "week-29" }
                 , #{ link_to => "week-30" }
                 , #{ link_to => "raleys"  }
                 ]
               }
            ]
          }
       , #{ link_to => "week-29" }
       , #{ link_to => "week-30" }
       ]
     }
.

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
