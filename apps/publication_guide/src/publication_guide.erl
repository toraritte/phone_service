-module(content).
-behaviour(gen_server).

-export(
    [ start_link/0

    % gen_server callbacks
    , init/1
    , handle_call/3
    , handle_cast/2
    % , terminate/2

    % public API
    , pick/2
    , root/0
    , all_vertices/0
    , redraw/0
    , add_label/2
    , get_label/1
    , publication_guide/0

    % private functions
    , draw_content_graph/0
    % , refresh_content_graph/1
    % TODO Make this part of the public API
    % , realize/0
    , get_vertex/3
    , process_call/3
    % , get_vertex_data/1
    % , current/1
    % , update_history/2
    ]).

% This belongs to a content_storage API implementation
% -define(PUBLICATIONS_DIR, lofa(phone_service, publications_dir)).
-define(PUBLICATION_GUIDE, application:get_env(phone_service, publication_guide, [])).


start_link() ->
    % {ok, Pid} =
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
% ,   Pid.

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

% lofa(Application, Par) ->
%     {ok, Val} =
%     application:get_env(Application, Par),
%     Val.

init(_Args) -> % {{-
    % TODO PROD How to set up the graph? ("do not overthink" notes below) {{-
    % It `realize/0`s the dir structure at the moment if it does not exit, but in subsequent phases the graph will be based on a remote cloud storage - it is cheap to redraw the entire graph by reading local files, but that will not cut it later. The graph will need to be de-serialized and kept up to date via messages, then saved to disk on startup.
    % }}-

    % TODO This should go to storage_api (and also rewrite because this is not valid)
    % PublicationGuide =
    %     case content_storage_api:list() of
    %         [] ->
    %             % how this  is "realized" will depend  on the backend.
    %             % local fs might as well just create the dir structure
    %             % as  it is  fast  and  low "cost",  but  for a  cloud
    %             % storage it  would be a  huge load, so  probably only
    %             % create paths  that have actuals files  to upload and
    %             % just  save the  `publication_guide`  as a  blueprint
    %             % (and keep it up to date).
    %             content_storage:realize(?PUBLICATION_GUIDE),
    %             ?PUBLICATION_GUIDE;
    %         ContentList ->
    %             ContentList
    %     end,

    Graph =
        % futil:pipe(
        %   [ content_storage_api:list()
        %   , draw_content_graph/1
        %   ]),
        draw_content_graph(?PUBLICATION_GUIDE),

    {ok, Graph}.
% }}-

handle_call({pick, Direction, Vertex}, _From, Graph)
  when Direction =:= parent       % \
     ; Direction =:= first        % |
     ; Direction =:= last         % |
     ; Direction =:= next         % | Vertex
     ; Direction =:= prev         % |
     ; Direction =:= content_root % |
     ; Direction =:= children     %   [ Vertex ]
->
    { reply
    , process_call(Graph, Vertex, Direction)
    , Graph
    };

handle_call(all_vertices, _From, Graph) ->
    {reply, digraph:vertices(Graph), Graph};

handle_call({get_label, Vertex}, _From, Graph) when is_map(Vertex) ->
    {Vertex, Label} = digraph:vertex(Graph, Vertex),
    {reply, Label, Graph}.

handle_cast({add_label, Vertex, Label}, Graph) when is_map(Vertex) ->
    digraph:add_vertex(Graph, Vertex, Label),
    {noreply, Graph};

handle_cast(redraw, Graph) ->
    {ok, NewGraph} = refresh_content_graph(Graph),
    {noreply, NewGraph}.

% HOW TO SERIALIZE A DIGRAPH {{-
% Serialization is implemented in ivr.erl (probably also commented out)
% process_action
%   ( {digraph, Vertices, Edges, Neighbours, Cyclicity}
%   , get
%   , graph
%   )
% ->
%     { serialized_digraph
%     , ets:tab2list(Vertices)
%     , ets:tab2list(Edges)
%     , ets:tab2list(Neighbours)
%     , Cyclicity
%     };

% Direction -> Vertex
% process_action(Graph, get, current) ->
%     current(Graph);
% }}-

% Direction -> Vertex
process_call(Graph, _Vertex, content_root) ->
    [  Vertex
    || Vertex
       <- digraph:vertices(Graph)
       , maps:find(selection, Vertex) =:= {ok, 0}
    ];

%! Direction -> [ Vertex ]
process_call(Graph, Vertex, children) ->
    get_vertex(Graph, Vertex, child);

process_call(Graph, Vertex, Direction) ->
    % TODO These are the only 2 options here, and if this does crash it means there's a logical error somewhere in this module.
    get_vertex(Graph, Vertex, Direction).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Direction -> Vertex
% Direction =
%     parent | first | last | next | prev | content_root
% CurrentVertex =
%     #{ type := ContentType, ...} (see content.erl)
% ContentType =
%     category | publication | article
pick(Direction, CurrentVertex) -> % List Content | []
    gen_server:call
        ( ?MODULE
        , {pick, Direction, CurrentVertex}
        ).

root() ->
    pick(content_root, ignore).

all_vertices() ->
    gen_server:call
        ( ?MODULE
        , all_vertices
        ).

% TODO No diff algo involved so `publications` directory has
%      to be  manually renamed  if one  would like  to have
%      changes in `publication_guide/0` to take effect!
%
%      BEST CURRENT USE CASE
%      Audio files  have been  added to  the `publications`
%      directory, and this will  re-read the directory, and
%      add `article` vertices to the content graph.
%
%      At  the  moment,  the  only way  to  add  new  audio
%      files, on  initial setup  or after  a change  in the
%      publication guide, is to to
%
%      1. Move old `publications` directory
%      2. `publication_guide:redraw()`
%      3. Place files in the desired publications
%      4. `publication_guide:redraw()`
redraw() ->
    gen_server:cast(?MODULE, redraw).

add_label(Vertex, Label) ->
    gen_server:cast
        ( ?MODULE
        , {add_label, Vertex, Label}
        ).

get_label(Vertex) ->
    gen_server:call
        ( ?MODULE
        , {get_label, Vertex}
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO Depends on the internal representation.
%      Refactor when the web service is ready (or usable).
draw_content_graph([]) ->
    digraph:new([cyclic, protected]); % default values made explicit

draw_content_graph(PublicationGuide) ->

    UnfoldedPubGuide =
        unfold_publication_guide:do(PublicationGuide),
    % { RootContentItem
    % , [_|_] = RootItems
    % } =
    #{ sub_items := RootItems } = UnfoldedPubGuide,

    % NOTE Vertex = Data
    % The way  directect graphs are implemented  in Erlang
    % means  that the  actual data  structure a  vertex is
    % created will become the vertex itself. (Vertices can
    % hold additional info in labels.)
    % Just wanted to make the distinction that even though
    % content-wise the two variable holds identical data.
    RootVertex = RootVertexData =
        to_vertex_data(UnfoldedPubGuide, 0),

    FirstRunGraph =
        digraph:new([cyclic, protected]), % default values made explicit

    digraph:add_vertex(FirstRunGraph, RootVertexData),

    First = #{ selection => -1 },

    do_draw
      ( [First|RootItems]
      , FirstRunGraph
      , RootVertex
      ),

    % NOTE Linking hack % {{-
    % This silly solution is to make sure linking works in
    % the  publication  guide.  On  first  go-round,  when
    % `DirOptions` have  a `link`  tuple, but there  is no
    % directory, it  is probably  because it has  not been
    % created yet.  On the second run,  which is basically
    % `refresh_content_graph/1`, the dir  should be there,
    % and linking will take effect.
    %
    % The  elegant  solution would  have  been  to send  a
    % `redraw` message  at the  end of `init/0`  (to allow
    % the process  to finish  starting) but this  setup is
    % more than likely to be temporary anyway.
    %
    % LINKING FAIL:
    % If there  is none, or  more than one,  a "link_fail"
    % directory is created; this will be deleted after the
    % first  run, but  if  there is  still  one after  the
    % second one, it  means that the `publication_guide/0`
    % does have an erroneous entry.
    % }}-
    digraph:delete(FirstRunGraph),
    file:del_dir("link_fail"),
    SecondRunGraph =
        digraph:new([cyclic, protected]),
    digraph:add_vertex(SecondRunGraph, RootVertexData),
    do_draw
      ( [First|RootItems]
      , SecondRunGraph
      , RootVertex
      ),

    SecondRunGraph.

unfold_publication_guide(PublicationGuide) ->
    % TRAVERSAL 1:
    % resolve   queries  (i.e.,   populate
    % sections/publications with articles)

    % TRAVERSAL 2: substitute links

% TODO Don't think this is possible; commenting out to see
% do_draw([], _Graph, _ParentVertex) ->
%     done;

% Empty item's end condition
do_draw([#{ selection => -1 }], _Graph, _ParentVertex) ->
%     erlang:display([done, [first], ParentVertex]),
    done;

% End condition for all other
do_draw  % ContentType, [ PrevItemVertex ] {{-
( [ #{} = _PrevItemVertex
  | [] = _Rest
  ]
, _Graph
, _ParentVertex
)
->
    done;

% }}-

% do_draw :: PrevVertexAndItems -> Graph -> ParentVertex -> ()
% PrevVertexAndItems = [ Vertex | [ ContentItem ] ]
% Vertex      = #{ type => ContentType, title => ContentTitle }
% ContentItem = #{ type => ContentType
%                , title => ContentTitle
%                , sub_items => [ ContentItem ]
%                , path_ref => SimplifiedTitle
%                , id => ContentStorageID
%                }
% ContentType = category | publication | section | article
% Title = String
% SimplifiedTitle = [0-9a-zA-Z_-]
% ContentStorageID = UUID
do_draw  % {{-
( [ #{ selection := Selection } = PrevItemVertex
  , #{} = ContentItem % May be a leaf item, hence no match on `sub_items`
  | RestContentItems
  ]
, Graph
, ParentVertex
)
% Would be nice for Erlang to have Haskell-like type system and then this guard would be unnecessary (as { A, [_|_] } then wouldn't mean both {A, "lofa"} and {A, [{a,b}, {c, d}]}...)
% when is_tuple(ContentItem)
when is_integer(Selection)
->
    { Direction, ItemNumber } =
        case
            { Selection
            , RestContentItems
            }
        of
            { -1, []    } -> { first_and_last,         1             };
            { -1, [_|_] } -> { first,                  1             };
            { _,  []    } -> { {PrevItemVertex, last}, Selection + 1 };
            { _,  [_|_] } -> { PrevItemVertex,         Selection + 1 }
        end,

    % If `ContentItem`  did not have `sub_items`,  it will
    % now  have  one  with  an empty  list,  but  keep  it
    % otherwise.
    SubItemizedContentItem =
        maps:merge
          ( #{ sub_items => [] }
          , ContentItem
          ),

    draw_item
      ( Direction
      , Graph
      , ParentVertex
      , SubItemizedContentItem
      , ItemNumber
      , RestContentItems
      ).

% }}-

refresh_content_graph(Graph) ->
    digraph:delete(Graph),
    init(ignore).

add_content_edge(Graph, Direction, FromVertex, ToVertex) ->
    % `digraph` is just 3 ETS tables, and so edges need to be unique, hence the `erlang:system_time/0`. The `Counter` in the tuple is to make temporal ordering absolutely possible (e.g., `first` and `last` edges) because `erlang:system_time/0` values are not **strictly** monotonically increasing values (that is, calling it fast enough may result in the same value, but not lower).
    digraph:add_edge(
      Graph,                 % digraph
      { Direction            % |
      % the UU(ish)ID is needed (e.g., when 2 siblings will have the same {parent, ...} edge)
      % TODO replace with real UUID
      , erlang:system_time() % | edge
      , ToVertex             % |
      },                     % |
      FromVertex,            % from vertex
      ToVertex,              % to vertex
      []                     % label
    ).

add_hierarchy_edges(Graph, ParentVertex, ChildVertex) -> % {{-
    add_content_edge(Graph, child,  ParentVertex, ChildVertex),
    add_content_edge(Graph, parent, ChildVertex, ParentVertex).
% }}-

% Graph -> Direction -> List Vertex
% TODO Direction is misleading because it means smth else in different contexts
% Direction = parent | next | prev | first | last | child
get_vertex(Graph, Vertex, Direction) ->
    % Current = current(Graph),

    % The below usage would be helpful but pattern matching won't work in the generator pattern.
    % https://erlang.org/doc/programming_examples/list_comprehensions.html#variable-bindings-in-list-comprehensions
    % > [ X || {lofa, X} <- [{lofa, 7}, {lofa, 27}]].
    % [7,27]
    % > [ X || {lofa, X} <- [{lofa, 7}, {miez, 27}]].
    % [7]

    % EdgeResults =
        % [  digraph:edge(Graph, Edge)
    [  V
    || {_Dir, _UUishID, V} = Edge
    <- digraph:out_edges(Graph, Vertex),
       erlang:element(1, Edge) =:= Direction
    ].

    % [  Vx % Vertex % this would not shadow the function argument, but confusing
    % || { {Direction, _} % edge
    %    , _              % from
    %    , Vx             % to
    %    , []             % edge label
    %    }
    %    <- EdgeResults
    % ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL MODEL OF THE YET TO BE BUILT ACCESS NEWS WEB SERVICE      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% {{-
% NOTE for the resulting graph when determining neighbours:
% to avoid excessive amount of connections (and facing the problems updating them would cause), it will be done by finding the parent category, and adding/subtracting one from their "ID".
% The publication guide below is just a representation of future data of the yet-to-be-implemented core web service, and its data may not contain such IDs, but that could be done on this end by ordering and adding that via a script.

% TODO This is configuration data, thus it shouldn't be here.
% TODO could have written a simple title_to_proper_filename function to convert e.g., "The Bickerson's" to "the-bickersons" but it is not applicable everywhere, and did not figure out how to make one opt out (entirely or in certain cases only). Currently it is fully explicit
% TODO NOTE LINKING % {{-
% Linking currently only works if the directory has been created before (
% }}-
% TODO make this explicit (i.e., dir_prefix, dir, title)
% Only 2 places need to be amended, by simply a matching for tuples
% + `make_title_dir/1`
% + `to_vertex_data/2`
% Maybe add both forms
% TODO FAVORITES
% Linking can now be used to add publications to your favorites!
% , {publication, ["",  "Yuba-Sutter Meals On Wheels", "Meals on wheels"]}
publication_guide() -> % {{-
    [ { {category, "Main menu"}
    % [ { {category, "Main category"}
      , [ { { category % ads {{-
            , { "Store sales advertising"
              , [ {dir_prefix, "ads"} ]
              }
            }
          , [ { { category % grocery stores % {{-
                , { "Grocery stores"
                  , [ {dir_prefix, "grocery"} ]
                  }
                }
              , [ { { sectioned_publication
                    , { "Safeway"
                      , [ {dir_prefix, "safeway"} ]
                      }
                    }
                  , [ {section, {"Week 7/15/2020 to 7/21/2020", [{alt, "07152020"}]}}
                    ]
                  }
                , {publication, {"Raley's",            [{ alt, "raleys"      }]}}
                , {publication, {"La Superior",        [{ alt, "la-superior" }]}}
                , {publication, {"Food Source",        [{ alt, "food-source" }]}}
                , {publication, "Savemart"}
                , {publication, {"Foods Co",           [{ alt, "foods-co"    }]}}
                , {publication, {"Trader Joe's",       [{ alt, "trader-joes" }]}}
                , {publication, "Sprouts"}
                , {publication, {"Lucky Supermarkets", [{ alt, "lucky"       }]}}
                ]
              } % }}-
            , { { category % drug stores {{-
                , { "Drug stores"
                  , [ {dir_prefix, "drug"} ]
                  }
                }
              , [ {publication, "CVS"}
                , {publication, {"Rite Aid",   [{ alt, "rite-aid"  }]}}
                , {publication, {"Walgreen's", [{ alt, "walgreens" }]}}
                ]
              } % }}-
            , { { category % discount stores {{-
                , { "Discount stores"
                  , [ {dir_prefix, "discount"} ]
                  }
                }
              , [ {publication, "Target"}
                , {publication, "Walmart"}
                ]
              } % }}-
            ]
          } % }}-
        , { { category % northern california newspapers and magazines {{-
            , { "Northern California newspapers and magazines"
              , [ {dir_prefix, "norcal"} ]
              }
            }
          , [ { { category % sacramento newspapers and mags {{-
                , { "Sacramento newspapers and magazines"
                  , [ {dir_prefix, "sac"} ]
                  }
                }
              , [ { { category % newspapers{{-
                    , { "Sacramento newspapers"
                      , [ {dir_prefix, "newspapers"} ]
                      }
                    }
                  , [ { { sectioned_publication % sacramento bee {{-
                        , { "Sacramento Bee sections"
                          , [ {dir_prefix, "sacbee"} ]
                          }
                        }
                      , [ {section, "Sports"}
                        , {section, "News"}
                        , {section, "Obituaries"}
                        ]
                      } % }}-
                    , {publication, {"Sacramento News & Review",    [{ alt, "SNR"              }]}}
                    , {publication, {"Sacramento Press",            [{ alt, "sacramento-press" }]}}
                    , {publication, {"Sacramento Business Journal", [{ alt, "business-journal" }]}}
                    , {publication, {"Sacramento Observer",         [{ alt, "observer"         }]}}
                    , {publication, {"Sacramento City Express",     [{ alt, "city-express"     }]}}
                    , {publication, {"East Sacramento News",        [{ alt, "east-sac-news"    }]}}
                    , {publication, {"The Land Park News",          [{ alt, "land-park-news"   }]}}
                    , {publication, {"The Pocket News",             [{ alt, "pocket-news"      }]}}
                    ]
                  } % }}-
                , { { category  % magazines {{-
                    , { "Sacramento magazines"
                      , [ {dir_prefix, "magazines"} ]
                      }
                    }
                  , [ {publication, "Comstocks"}
                    , {publication, "SacTown"}
                    , {publication, {"Sacramento Magazine", [{alt, "sacramento-magazine"}]}}
                    ]
                  } % }}-
                ]
              } % }}-
            , { { category  % greater sac {{-
                , { "Greater Sacramento area newspapers"
                  , [ {dir_prefix, "greater-sac"} ]
                  }
                }
              , [ {publication, {"Carmichael Times",                   [{ alt, "carmichael-times"                   }]}}
                , {publication, {"Arden Carmichael News",              [{ alt, "arden-carmichael-news"              }]}}
                , {publication, {"Davis Enterprise",                   [{ alt, "davis-enterprise"                   }]}}
                , {publication, {"Roseville Press Tribune",            [{ alt, "roseville-press-tribune"            }]}}
                , {publication, {"Woodland Daily Democrat",            [{ alt, "woodland-daily-democrat"            }]}}
                , {publication, {"Elk Grove Citizen",                  [{ alt, "elk-grove-citizen"                  }]}}
                , {publication, {"Auburn Journal",                     [{ alt, "auburn-journal"                     }]}}
                , {publication, {"Grass Valley-Nevada City Union",     [{ alt, "grass-valley-nevada-city-union"     }]}}
                , {publication, {"El Dorado County Mountain Democrat", [{ alt, "el-dorado-county-mountain-democrat" }]}}
                , {publication, {"Loomis News",                        [{ alt, "loomis-news"                        }]}}
                ]
              } % }}-
            , { { category  % sf and bay area {{-
                , { "San Francisco and Bay Area newspapers"
                  , [ {dir_prefix, "bay-area"} ]
                  }
                }
              , [ {publication, {"Vallejo Times Herald",       [{ alt, "vallejo-times-herald"      }]}}
                , {publication, {"Santa Rosa Press Democrat",  [{ alt, "santa-rosa-press-democrat" }]}}
                , {publication, {"SF Gate",                    [{ alt, "sf-gate"                   }]}}
                , {publication, {"San Francisco Bay Guardian", [{ alt, "sf-bay-guardian"           }]}}
                , {publication, {"East Bay Times",             [{ alt, "east-bay-times"            }]}}
                , {publication, {"SF Weekly",                  [{ alt, "sf-weekly"                 }]}}
                , {publication, {"KQED Bay Area Bites",        [{ alt, "KQED-bay-area-bites"       }]}}
                ]
              } % }}-
            , { { category % central california {{-
                , { "Central California newspapers"
                  , [ {dir_prefix, "central-cal"} ]
                  }
              }
              , [ {publication, {"Modesto Bee",     [{ alt, "modesto-bee"     }]}}
                , {publication, {"Stockton Record", [{ alt, "stockton-record" }]}}
                ]
              } % }}-
            , { { category % mendocino {{-
                , { "Mendocino county newspapers"
                  , [ {dir_prefix, "mendocino"} ]
                  }
              }
              , [ {publication, {"Fort Bragg Advocate News", [{ alt, "fort-bragg-advocate-news" }]}}
                , {publication, {"The Mendocino Beacon",     [{ alt, "mendocino-beacon"         }]}}
                ]
              } % }}-
            , { { category % humboldt and trinity counties {{-
                , { "Humboldt & Trinity county newspapers"
                  , [ {dir_prefix, "humboldt-trinity"} ]
                  }
              }
              , [ {publication, {"Humboldt Senior Resource Center's Senior News", [{ alt, "senior-news"           }]}}
                , {publication, {"North Coast Journal",                           [{ alt, "north-coast-journal"   }]}}
                , {publication, {"Eureka Times Standard",                         [{ alt, "eureka-times-standard" }]}}
                , {publication, {"Ferndale Enterprise",                           [{ alt, "ferndale-enterprise"   }]}}
                , {publication, {"Mad River Union",                               [{ alt, "mad-river-union"       }]}}
                ]
              } % }}-
            ]
          } % }}-
        , { { category % popular magazines {{-
            , { "Popular magazines"
              , [ {dir_prefix, "pop"} ]
              }
            }
          , [ {publication, {"Braille Monitor",      [{ alt, "braille-monitor"    }]}}
            , {publication, {"Capital Public Radio", [{ alt, "CPR"                }]}}
            , {publication, {"Entertainment Weekly", [{ alt, "EW"                 }]}}
            , {publication, "Fortune"}
            , {publication, {"Mental Floss",         [{ alt, "mental-floss"       }]}}
            , {publication, {"Atlas Obscura",        [{ alt, "atlas-obscura"      }]}}
            , {publication, {"New Scientist",        [{ alt, "new-scientist"      }]}}
            , {publication, "Newsweek"}
            , {publication, {"Travel & Leisure",     [{ alt, "travel-and-leisure" }]}}
            ]
          } % }}-
        , { { category % old time radio {{-
            , { "Old Time Radio Theater"
              , [ {dir_prefix, "OTR"} ]
              }
            }
          , [ { {category, "Mystery and drama"} % {{-
              , [ {publication, {"Broadway's my Beat",                [{ alt, "broadways-my-beat"     }]}}
                , {publication, {"Black Stone the Magic Detective",   [{ alt, "black-stone"           }]}}
                , {publication, {"Boston Blacky",                     [{ alt, "boston-blacky"         }]}}
                , {publication, {"Crime Does Not Pay",                [{ alt, "crime-does-not-play"   }]}}
                , {publication, "Dragnet"}
                , {publication, {"Gang Busters",                      [{ alt, "gang-busters"          }]}}
                , {publication, {"Inner Sanctum",                     [{ alt, "inner-sanctum"         }]}}
                , {publication, {"Mercury Radio Theater",             [{ alt, "mercury-radio-theater" }]}}
                , {publication, {"Mystery Traveler",                  [{ alt, "mystery-traveler"      }]}}
                , {publication, {"Richard Diamond Private Detective", [{ alt, "richard-diamond"       }]}}
                , {publication, {"Adventures of Sam Spade",           [{ alt, "sam-spade"             }]}}
                , {publication, {"The Shadow",                        [{ alt, "the-shadow"            }]}}
                , {publication, "Suspense"}
                , {publication, {"The Whistler",                      [{ alt, "the-whistler"          }]}}
                , {publication, {"Light's Out",                       [{ alt, "lights-out"            }]}}
                ]
              } % }}-
            , { {category, "Comedy"} % {{-
              , [ {publication, {"Abbot and Costello",                  [{ alt, "abbot-and-costello"            }]}}
                , {publication, {"The Adventures of Ozzie and Harriet", [{ alt, "ozzie-and-harriet"             }]}}
                , {publication, {"The Bickerson's",                     [{ alt, "the-bickersons"                }]}}
                , {publication, {"Father Knows Best",                   [{ alt, "father-knows-best"             }]}}
                , {publication, {"Fibber McGee and Molly",              [{ alt, "fibber-mcgee-and-molly"        }]}}
                , {publication, {"The Fred Allen Show",                 [{ alt, "the-fred-allen-show"           }]}}
                , {publication, {"George Burns and Gracie Allen",       [{ alt, "george-burns-and-gracie-allen" }]}}
                , {publication, {"Life of Riley",                       [{ alt, "life-of-riley"                 }]}}
                , {publication, {"The Red Skelton Show",                [{ alt, "the-red-skelton-show"          }]}}
                ]
              } % }}-
            , { {category, "Westerns"} % {{-
              , [ {publication, {"The Cisco Kid",              [{ alt, "the-cisco-kid" }]}}
                , {publication, {"Gun Smoke",                  [{ alt, "gun-smoke"     }]}}
                , {publication, {"The Lone Ranger",            [{ alt, "lone-ranger"   }]}}
                , {publication, {"Tales of the Texas Rangers", [{ alt, "texas-rangers" }]}}
                ]
              } % }}-
            , { {category, "Science fiction and fantasy"} % {{-
              , [ {publication, {"The Blue Beetle",  [{ alt, "blue-beetle"  }]}}
                , {publication, "Escape"}
                , {publication, {"The Green Hornet", [{ alt, "green-hornet" }]}}
                , {publication, {"X Minus 1",        [{ alt, "x-minus-1"    }]}}
                ]
              } % }}-
            , {publication, "Commercials"}
            ]
          } % }}-
        , { { category % games{{-
            , { "Games"
              , [ {dir_prefix, "games"} ]
              }
            }
          , [ {publication, "Crosswords"}
            , {publication, "Trivia"}
            ]
          } % }}-
        , { { category % community {{-
            , { "Community information and resources"
              , [ {dir_prefix, "community-resources"} ]
              }
            }
          , [ { { category, "Podcasts"} % {{-
              , [ { publication
                  , { "Beyond Barriers Project"
                    , [ {dir_prefix, "SFTB"}
                      , {link, "beyond-barriers"}
                      ]
                    }
                  }
                , {publication, {"The Redacted Files Podcast", [{alt, "TRP"}]}}
                ]
              } % }}-
            , { { category, "Poetry" } % {{-
              , [ {publication, {"Brad Buchanan",       [{ alt, "brad-buchanan"      }]}}
                , {publication, {"Writer's on the air", [{ alt, "writers-on-the-air" }]}}
                ]
              } % }}-
            ]
          } % }}-
        , { { category % blindness resources {{-
            , { "Blindness information and resources"
              , [ {dir_prefix, "blindness-resources"} ]
              }
            }
          , [ { { category % organizations {{-
                , { "Organizations"
                  , [ {dir_prefix, "orgs"} ]
                  }
                }
              , [ { { category % SFTB {{-
                    , { "Society for the Blind"
                      , [ {dir_prefix, "SFTB"} ]
                      }
                    }
                  , [ {publication, {"SFB Connection",                           [{ alt, "sfb-connection"   }]}}
                    , {publication, {"Monthly newsletter",                       [{ alt, "newsletter"       }]}}
                    , {publication, {"Society for the Blind's student handbook", [{ alt, "student-handbook" }]}}
                    , {publication, {"Beyond Barriers Project",                  [{ alt, "beyond-barriers"  }]}}
                    ]
                  } % }}-
                , {publication, {"The Earle Baum Center",           [{ alt, "EBC"   }]}}
                , {publication, {"Sierra Services for the Blind",   [{ alt, "SSFTB" }]}}
                , {publication, {"California Council of the Blind", [{ alt, "CCB"   }]}}
                ]
              } % }}-
            , { { category % publications {{-
                , { "Publications"
                  , [ {dir_prefix, "publications" } ]
                  }
                }
              , [ {publication, {"Braille Monitor",           [{ link, "braille-monitor" }]}}
                , {publication, {"Client Assistence Program", [{ link, "CAP"              }]}}
                ]
              } % }}-
            ]
          } % }}-
        , { { category  % education and resources {{-
            , { "Education and resources"
              , [ {dir_prefix, "edu"} ]
              }
            }
          , [ {publication, {"Society for the Blind's student handbook", [{ link, "student-handbook"       }]}}
            , {publication, {"Balance exercises",                        [{ alt, "balance-exercises"       }]}}
            , {publication, {"Achieve a healthy weight by UC Davis",     [{ alt, "uc-davis-healthy-weight" }]}}
            , {publication, {"Yuba-Sutter Meals On Wheels",              [{ alt, "YSMOW"                   }]}}
            , {publication, {"Client Assistence Program",                [{ alt, "CAP"                     }]}}
            ]
          } % }}-
        , { {category, "Society for the Blind" } % {{-
          , [ {publication, {"SFB Connection",                           [{ link, "sfb-connection"   }]}}
            , {publication, {"Monthly newsletter",                       [{ link, "SFTB-newsletter"  }]}}
            , {publication, {"Society for the Blind's student handbook", [{ link, "student-handbook" }]}}
            , {publication, {"Beyond Barriers Project",                  [{ link, "beyond-barriers"  }]}}
            ]
          } % }}-
        ]
      } % main category
    ].
% }}-

% TODO linking is the concern of `content_storage`
% Links are based  on title; if two title  is the same
% then (1) one should either be a link or (2) that's a
% mistake.
% The format  is {link, Title} -  anything else will
% be interpreted as error.
% + linking is also expanded on the storage_api level
% TODO linear queries? use digraph's ETS tables
publication_guide() -> % {{-
% 133> c("apps/publication_guide/src/unfold_publication_guide.erl").
% {ok,unfold_publication_guide}
% 134> unfold_publication_guide:do(R).                              
    #{ type => category
     , title => "Main menu"
     , sub_items =>
       [ #{ link_to => "week-29" }
     % , #{ link_to => "non-existent" } % WORKS
       , #{ link_to => "week-30" }
       , #{ link_to => "drugs" }
       , #{ link_to => "safeway" }
       , #{ link_to => "week-30" }
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
                         % QUERY
                         % can for any property, until it returns results for the same publication. There won't be a crash, but then articles will belong publications/sections where they shouldn't be
                         , query => {issue, "week-29"}
                         }
                      , #{ type => section
                         , title =>  "Week 7/22/2020 to 7/28/2020"
                         , link_id => "week-30"
                         , query => {issue, "week-30"}
                         }
                      , #{ link_to => "raleys" }
                      , #{ link_to => "drugs" }
                      ]
                    }
                 , #{ type => publication
                    , query => {path_ref, "raleys"}
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
                 ]
               }
            ]
          }
       , #{ type => category
          , title =>  "Northern California newspapers and magazines"
          , sub_items =>
            [
            ]
          }
       , #{ link_to => "week-29" }
       , #{ link_to => "week-30" }
       ]
     }
    , [ { #{ type => category, title => "Store sales advertising", path => "ads"}
          , [ { { category,  [{title, "Grocery stores"}]  }
                , [ { { publication, "Safeway" }
                      , [ {section,  "Week 7/15/2020 to 7/21/2020", [{path, "week-29"}] }
                        ]
                    }
                    , { publication, "Raley's" }
                    , { publication, "La Superior" }
                    , { publication, "Food Source" }
                    , { publication, "Savemart" }
                    , { publication, "Foods Co" }
                    , { publication, "Trader Joe's" }
                    , { publication, "Sprouts" }
                    , { publication, "Lucky Supermarkets" }
                  ]
              }
              , { { category, "Drug stores" }
                  , [ {publication, "CVS"}
                      , { publication, "Rite Aid" }
                      , { publication, "Walgreen's" }
                    ]
                }
              , { { category, "Discount stores" }
                  , [ {publication, "Target"}
                      , {publication, "Walmart"}
                    ]
                }
            ]
        }
        , { { category, "Northern California newspapers and magazines", [{path, "norcal"}] }
            , [ { { category, "Sacramento newspapers and magazines" }
                  , [ { { category, "Sacramento newspapers" }
                        , [ { { publication, "Sacramento Bee sections" }
                              , [ { section, "Sports" }
                                  , { section, "News" }
                                  , { section, "Obituaries" }
                                ]
                            }
                            , { publication, "Sacramento News & Review" }
                            , { publication, "Sacramento Press" }
                            , { publication, "Sacramento Business Journal" }
                            , { publication, "Sacramento Observer" }
                            , { publication, "Sacramento City Express" }
                            , { publication, "East Sacramento News" }
                            , { publication, "The Land Park News" }
                            , { publication, "The Pocket News" }
                          ]
                      }
                      , { { category "Sacramento magazines" }
                          , [ { publication, "Comstocks" }
                              , { publication, "SacTown" }
                              , { publication, "Sacramento Magazine" }
                            ]
                        }
                    ]
                }
                , { { category "Greater Sacramento area newspapers" }
                    , [ { publication, "Carmichael Times" }
                        , { publication, "Arden Carmichael News" }
                        , { publication, "Davis Enterprise" }
                        , { publication, "Roseville Press Tribune" }
                        , { publication, "Woodland Daily Democrat" }
                        , { publication, "Elk Grove Citizen" }
                        , { publication, "Auburn Journal" }
                        , { publication, "Grass Valley-Nevada City Union" }
                        , { publication, "El Dorado County Mountain Democrat" }
                        , { publication, "Loomis News" }
                      ]
                  }
                , { { category "San Francisco and Bay Area newspapers" }
                    , [ { publication, "Vallejo Times Herald" }
                        , { publication, "Santa Rosa Press Democrat" }
                        , { publication, "SF Gate" }
                        , { publication, "San Francisco Bay Guardian" }
                        , { publication, "East Bay Times" }
                        , { publication, "SF Weekly" }
                        , { publication, "KQED Bay Area Bites" }
                      ]
                  }
                , { { category, "Central California newspapers" }
                    , [ { publication, "Modesto Bee" }
                        , { publication, "Stockton Record" }
                      ]
                  }
                , { { category, "Mendocino county newspapers" }



                    , [ {publication, {"Fort Bragg Advocate News", [{ alt, "fort-bragg-advocate-news" }]}}
                        , {publication, {"The Mendocino Beacon",     [{ alt, "mendocino-beacon"         }]}}
                      ]
                  }
                , { { category
                      , { "Humboldt & Trinity county newspapers"
                          , [ {dir_prefix, "humboldt-trinity"} ]
                        }
                    }
                    , [ {publication, {"Humboldt Senior Resource Center's Senior News", [{ alt, "senior-news"           }]}}
                        , {publication, {"North Coast Journal",                           [{ alt, "north-coast-journal"   }]}}
                        , {publication, {"Eureka Times Standard",                         [{ alt, "eureka-times-standard" }]}}
                        , {publication, {"Ferndale Enterprise",                           [{ alt, "ferndale-enterprise"   }]}}
                        , {publication, {"Mad River Union",                               [{ alt, "mad-river-union"       }]}}
                      ]
                  }
              ]
          }
        , { { category
              , { "Popular magazines"
                  , [ {dir_prefix, "pop"} ]
                }
            }
            , [ {publication, {"Braille Monitor",      [{ alt, "braille-monitor"    }]}}
                , {publication, {"Capital Public Radio", [{ alt, "CPR"                }]}}
                , {publication, {"Entertainment Weekly", [{ alt, "EW"                 }]}}
                , {publication, "Fortune"}
                , {publication, {"Mental Floss",         [{ alt, "mental-floss"       }]}}
                , {publication, {"Atlas Obscura",        [{ alt, "atlas-obscura"      }]}}
                , {publication, {"New Scientist",        [{ alt, "new-scientist"      }]}}
                , {publication, "Newsweek"}
                , {publication, {"Travel & Leisure",     [{ alt, "travel-and-leisure" }]}}
              ]
          }
        , { { category
              , { "Old Time Radio Theater"
                  , [ {dir_prefix, "OTR"} ]
                }
            }
            , [ { {category, "Mystery and drama"}
                  , [ {publication, {"Broadway's my Beat",                [{ alt, "broadways-my-beat"     }]}}
                      , {publication, {"Black Stone the Magic Detective",   [{ alt, "black-stone"           }]}}
                      , {publication, {"Boston Blacky",                     [{ alt, "boston-blacky"         }]}}
                      , {publication, {"Crime Does Not Pay",                [{ alt, "crime-does-not-play"   }]}}
                      , {publication, "Dragnet"}
                      , {publication, {"Gang Busters",                      [{ alt, "gang-busters"          }]}}
                      , {publication, {"Inner Sanctum",                     [{ alt, "inner-sanctum"         }]}}
                      , {publication, {"Mercury Radio Theater",             [{ alt, "mercury-radio-theater" }]}}
                      , {publication, {"Mystery Traveler",                  [{ alt, "mystery-traveler"      }]}}
                      , {publication, {"Richard Diamond Private Detective", [{ alt, "richard-diamond"       }]}}
                      , {publication, {"Adventures of Sam Spade",           [{ alt, "sam-spade"             }]}}
                      , {publication, {"The Shadow",                        [{ alt, "the-shadow"            }]}}
                      , {publication, "Suspense"}
                      , {publication, {"The Whistler",                      [{ alt, "the-whistler"          }]}}
                      , {publication, {"Light's Out",                       [{ alt, "lights-out"            }]}}
                    ]
                }
                , { {category, "Comedy"}
                    , [ {publication, {"Abbot and Costello",                  [{ alt, "abbot-and-costello"            }]}}
                        , {publication, {"The Adventures of Ozzie and Harriet", [{ alt, "ozzie-and-harriet"             }]}}
                        , {publication, {"The Bickerson's",                     [{ alt, "the-bickersons"                }]}}
                        , {publication, {"Father Knows Best",                   [{ alt, "father-knows-best"             }]}}
                        , {publication, {"Fibber McGee and Molly",              [{ alt, "fibber-mcgee-and-molly"        }]}}
                        , {publication, {"The Fred Allen Show",                 [{ alt, "the-fred-allen-show"           }]}}
                        , {publication, {"George Burns and Gracie Allen",       [{ alt, "george-burns-and-gracie-allen" }]}}
                        , {publication, {"Life of Riley",                       [{ alt, "life-of-riley"                 }]}}
                        , {publication, {"The Red Skelton Show",                [{ alt, "the-red-skelton-show"          }]}}
                      ]
                  }
                , { {category, "Westerns"}
                    , [ {publication, {"The Cisco Kid",              [{ alt, "the-cisco-kid" }]}}
                        , {publication, {"Gun Smoke",                  [{ alt, "gun-smoke"     }]}}
                        , {publication, {"The Lone Ranger",            [{ alt, "lone-ranger"   }]}}
                        , {publication, {"Tales of the Texas Rangers", [{ alt, "texas-rangers" }]}}
                      ]
                  }
                , { {category, "Science fiction and fantasy"}
                    , [ {publication, {"The Blue Beetle",  [{ alt, "blue-beetle"  }]}}
                        , {publication, "Escape"}
                        , {publication, {"The Green Hornet", [{ alt, "green-hornet" }]}}
                        , {publication, {"X Minus 1",        [{ alt, "x-minus-1"    }]}}
                      ]
                  }
                , {publication, "Commercials"}
              ]
          }
        , { { category
              , { "Games"
                  , [ {dir_prefix, "games"} ]
                }
            }
            , [ {publication, "Crosswords"}
                , {publication, "Trivia"}
              ]
          }
        , { { category
              , { "Community information and resources"
                  , [ {dir_prefix, "community-resources"} ]
                }
            }
            , [ { { category, "Podcasts"}
                  , [ { publication
                        , { "Beyond Barriers Project"
                            , [ {dir_prefix, "SFTB"}
                                , {link, "beyond-barriers"}
                              ]
                          }
                      }
                      , {publication, {"The Redacted Files Podcast", [{alt, "TRP"}]}}
                    ]
                }
                , { { category, "Poetry" }
                    , [ {publication, {"Brad Buchanan",       [{ alt, "brad-buchanan"      }]}}
                        , {publication, {"Writer's on the air", [{ alt, "writers-on-the-air" }]}}
                      ]
                  }
              ]
          }
        , { { category
              , { "Blindness information and resources"
                  , [ {dir_prefix, "blindness-resources"} ]
                }
            }
            , [ { { category
                    , { "Organizations"
                        , [ {dir_prefix, "orgs"} ]
                      }
                  }
                  , [ { { category
                          , { "Society for the Blind"
                              , [ {dir_prefix, "SFTB"} ]
                            }
                        }
                        , [ {publication, {"SFB Connection",                           [{ alt, "sfb-connection"   }]}}
                            , {publication, {"Monthly newsletter",                       [{ alt, "newsletter"       }]}}
                            , {publication, {"Society for the Blind's student handbook", [{ alt, "student-handbook" }]}}
                            , {publication, {"Beyond Barriers Project",                  [{ alt, "beyond-barriers"  }]}}
                          ]
                      }
                      , {publication, {"The Earle Baum Center",           [{ alt, "EBC"   }]}}
                      , {publication, {"Sierra Services for the Blind",   [{ alt, "SSFTB" }]}}
                      , {publication, {"California Council of the Blind", [{ alt, "CCB"   }]}}
                    ]
                }
                , { { category
                      , { "Publications"
                          , [ {dir_prefix, "publications" } ]
                        }
                    }
                    , [ {publication, {"Braille Monitor",           [{ link, "braille-monitor" }]}}
                        , {publication, {"Client Assistence Program", [{ link, "CAP"              }]}}
                      ]
                  }
              ]
          }
        , { { category
              , { "Education and resources"
                  , [ {dir_prefix, "edu"} ]
                }
            }
            , [ {publication, {"Society for the Blind's student handbook", [{ link, "student-handbook"       }]}}
                , {publication, {"Balance exercises",                        [{ alt, "balance-exercises"       }]}}
                , {publication, {"Achieve a healthy weight by UC Davis",     [{ alt, "uc-davis-healthy-weight" }]}}
                , {publication, {"Yuba-Sutter Meals On Wheels",              [{ alt, "YSMOW"                   }]}}
                , {publication, {"Client Assistence Program",                [{ alt, "CAP"                     }]}}
              ]
          }
        , { {category, "Society for the Blind" }
            , [ {publication, {"SFB Connection",                           [{ link, "sfb-connection"   }]}}
                , {publication, {"Monthly newsletter",                       [{ link, "SFTB-newsletter"  }]}}
                , {publication, {"Society for the Blind's student handbook", [{ link, "student-handbook" }]}}
                , {publication, {"Beyond Barriers Project",                  [{ link, "beyond-barriers"  }]}}
              ]
          }
      ]

% }}-
% TODO converting this to JSON should be trivial
% TODO `meta` edges
%      "directional" edges follow the pattern {dir, #{...}} so tag, type, periodicity (what else?) nodes will be tagged as meta and will be incident edges (is that the right term?)

% Yes, this could have been just the one string below, but it is not.
% string:join([ erlang:integer_to_list(erlang:system_time()) | tl(string:lexemes(erlang:ref_to_list(erlang:make_ref()), ".>"))], "-")
% TODO So what was the point of this? {{-
% make_id() ->
%     NowString =
%         f:pipe(
%           [ erlang:system_time()
%           , fun erlang:integer_to_list/1
%           ]
%         ),
%     RefNumbers =
%         f:pipe(
%           [ erlang:make_ref()
%           , fun erlang:ref_to_list/1
%           , (f:cflip(fun string:lexemes/2))(".>")
%           ]
%         ),
%     string:join([NowString|tl(RefNumbers)], "-").
% }}-

% digraph ets query notes {{-
% 201> {_,V,_,_,_} = G = digraph:new().
% {digraph,#Ref<0.3770885502.3804626945.151678>,
%          #Ref<0.3770885502.3804626945.151679>,
%          #Ref<0.3770885502.3804626945.151680>,true}
% 202> ets:tab2list(V).
% []
% 203> digraph:add_vertex(G, A, D).
% {#Ref<0.3770885502.3804495875.151576>,1587669065273556493}
% 204> digraph:add_vertex(G, B, E).
% {#Ref<0.3770885502.3804495875.151577>,1587669065273569069}
% 205> digraph:add_vertex(G, C, F).
% {#Ref<0.3770885502.3804495875.151578>,1587669065273579919}
% 206> ets:tab2list(V).
% [{{#Ref<0.3770885502.3804495875.151577>,1587669065273569069},
%   #{"id" => 6,"items" => [],"periodicity" => "weekly",
%     "tags" => ["flyer","ads"],
%     "title" => "La Superior","type" => "publication"}},
%  {{#Ref<0.3770885502.3804495875.151578>,1587669065273579919},
%   #{"id" => 7,"items" => [],"periodicity" => "weekly",
%     "tags" => ["flyer","ads"],
%     "title" => "Food Source","type" => "publication"}},
%  {{#Ref<0.3770885502.3804495875.151576>,1587669065273556493},
%   #{"id" => 5,"items" => [],"periodicity" => "weekly",
%     "tags" => ["flyer","ads"],
%     "title" => "Raley's","type" => "publication"}}]

% `ets:lookup/2` or `match_object/2` can be used to find row by ID, and `ets:select/[1,2,3]` if lookup is needed by other columns.

% 208> ets:select(V, [{{'_', #{ "title" => "Food Source"}}, [], ['$_']}]).
% [{{#Ref<0.3770885502.3804495875.151578>,1587669065273579919},
%   #{"id" => 7,"items" => [],"periodicity" => "weekly",
%     "tags" => ["flyer","ads"],
%     "title" => "Food Source","type" => "publication"}}]
%
% 210> ets:match_object(V, {'_', #{ "title" => "Raley's"}}).
% [{{#Ref<0.3770885502.3804495875.151576>,1587669065273556493},
%   #{"id" => 5,"items" => [],"periodicity" => "weekly",
%     "tags" => ["flyer","ads"],
%     "title" => "Raley's","type" => "publication"}}]

% `ets:match/2` can be used to return only certain fields. For example, return all the available publications. (Adding a category vertex to as all the current ones are publications.)
% 213> digraph:add_vertex(G, 1, #{"type" => "category", "title" => "Grocery stores"}).
% 1
% 214> ets:match(V, {'_', #{ "title" => '$1'}}).
% [["La Superior"],
%  ["Grocery stores"],
%  ["Food Source"],
%  ["Raley's"]]
% 215> ets:match(V, {'_', #{ "type" => "publication", "title" => '$1'}}).
% [["La Superior"],["Food Source"],["Raley's"]]

% http://erlang.org/doc/efficiency_guide/tablesDatabases.html
% The functions ets:select/2 and mnesia:select/3 are to be preferred over ets:match/2, ets:match_object/2, and mnesia:match_object/3.

% Overkill, but want to remember the thought:
% The rows for the content digraph (i.e., the vertices and edges stored in the underlying ETS tables) could be normalized for easier lookup by creating another table that will simply store the map as an N-tuple (N equals the number of keys in the map).
% (or, better yet, replicate and normalize to mnesia)

% !!!! ETS performance and limits
% http://erlang.org/pipermail/erlang-questions/2018-September/096299.html
% }}-

% STAGES and NOTES {{-
% 1. this
% 2. move everything over to the Core Phoenix web server,
%    and implement API to query data from here (i.e., the phone service)
% 3. see what better models there are for more robust persistence
%    (currently digraph (i.e., ETS, meaning in-memory) and cloud storage + metadata)
%    there is an mnesia-backed digraph project on github, but mnesia is no panacea
%    (if things crash, some stuff in memory still won't get written out)
% 4. maybe a "proper" database?

% See hypothes.is notes to https://docs.microsoft.com/en-us/azure/architecture/best-practices/api-design


% }}-

% TODO So this could safely be set to [], but need to make sure that it doesn't affect anything. At least, tried renaming it Rest, and use the
% logger:debug(#{make_title_dir => Rest})
% and the only thing that came up was []
%                                      |
%                                      V
% make_title_dir([ Prefix, Title | _]) when is_list(Prefix) ->
%     make_title_dir(Prefix ++ Title);
%  NOTE This was unnecessary here; taken care of in `prefix_leaf_items/2` where it should be

do_dir_options(Title, []) ->
    make_title_dir(Title);

do_dir_options % alt {{-
  ( Title
  , [ {alt, AlternativeName} | Opts ]
  )
->
    do_dir_options(AlternativeName, Opts);

% }}-
do_dir_options % dir_prefix {{-
  ( Title
  , [ {dir_prefix, Prefix} | Opts ]
  )
->
    do_dir_options
      ( Prefix ++ "-" ++ Title
      , Opts
      );

% }}-
do_dir_options % link {{-
  ( _Title
  , [ {link, EndMatch} | Opts ]
  )
->
    {ok, PublicationDirs} =
        file:list_dir(?PUBLICATIONS_DIR),

    % Could have matched anywhere else, but matching at the end seemed the most logical
    MatchAtTheEnd =
        fun(Dirname) ->
            case re:run(Dirname, EndMatch ++ "$") of
                nomatch -> false;
                      _ -> true
            end
        end,

    % There should only be one match when linking.
    LinkDir =
        case lists:filter(MatchAtTheEnd, PublicationDirs) of
            [ Dir ] ->
                Dir;
            _ ->
                FailDir = "link_fail",
                file:make_dir(FailDir),
                FailDir
        end,

    do_dir_options
      ( LinkDir
      , Opts
      ).

% }}-
make_title_dir({Title, UnsortedOpts}) ->
    do_dir_options
      ( Title
      % `alt` has to come before dir_prefix
      % see NOTE at `draw_content_with_subitems/7`
      %
      % Preserves the order of the tuples of a given key
      % lists:keysort(1, [{b, "miez"}, {a, "lofa"}, {b, "balabab"}]).
      % [{a,"lofa"},{b,"miez"},{b,"balabab"}]
      , lists:keysort(1, UnsortedOpts)
      );

% NOTE Fixed the  above clause from  list to tuple,  so the
%      guard could simply read `is_list(Title)` but this is
%      way more explicit that a string is needed
make_title_dir([Char|_] = Title) when is_integer(Char) -> % {{-
    Dir =
        filename:join(?PUBLICATIONS_DIR, Title),
    % no fuss if exists, won't throw
    file:make_dir(Dir),
%     erlang:display([make_title_dir, Dir]),
    Dir.

% }}-
list_recording_vertices(Dir) -> % {{-
    % Only  MP3s  and  WAVs are  supported  by  FreeSWITCH
    % out-of-the-box. Also, could've  just simply `ls` the
    % publication folder, and crash on unsupported formats
    % but  I'm an  idiot,  and this  is  may help  prevent
    % disasters. (Or cause some more.)
    Extensions =
        fun (Filename) ->
            case filename:extension(Filename) of
                ".wav" -> true;
                ".mp3" -> true;
                % TODO ffmpeg
                _ -> false
            end
        end,

    MakeRecordingVertexData =
        fun (Filename) ->
            futil:pipe
              ([ Filename
               , (futil:cflip(fun filename:absname/2))(Dir)
               , fun make_recording_vertex_data/1
               ])
        end,

%         erlang:display([list_recording_vertices, enter, Dir]),
    futil:pipe
      % NOTE The extra  quotes are  needed because  otherwise the {{-
      %      special  characters  in   `Dir`  will  be
      %      treated literally by the shell.
      % ``` text
      % $ ls Raley's
      % # VS
      % $ ls "Raley's"
      % ```
      % Also,  `-r` because  right  now  the recordings  are
      % numbered, and  the higher  the number the  newer the
      % recording.
      % }}-

      % NOTE Calling `ls`  without any  modifiers means  that the
      %      order  of the  returned list  depends on  consistent
      %      file naming!
      % TODO Make  an upload  mechanism  that takes  care of  the
      %      naming  based on  given  parameters  (e.g., sort  by
      %      time, numbering, etc.).
      ([ os:cmd("ls \"" ++ Dir ++ "\"")
       , (futil:cflip(fun string:lexemes/2))([$\n])
%      , fun (X) -> erlang:display([list_recording_vertices, X]), X end
       , (futil:curry(fun lists:filter/2))(Extensions)
%      , fun (X) -> erlang:display([list_recording_vertices, X]), X end
       % NOTE It would have made more logical sense to put this in
       % `draw_item/7`  but it  would have  been a  hassle to
       % figure  out `Dir`  - plus  it would  have
       % been an extra loop
       , (futil:curry(fun lists:map/2))(MakeRecordingVertexData)
%      , fun (X) -> erlang:display([list_recording_vertices, X]), X end
       ]).
% }}-

draw_item
( Direction
, Graph
, ParentVertex
% , #{} = ItemVertex
% , SubItems
, #{ sub_items := SubItems } = ContentItem
, ItemNumber
, RestContentItems
)
when is_tuple(ContentItem)
->
%     % erlang:display([draw_item, #{direction => Direction}]),
%     % erlang:display([draw_item, #{ parent => ParentVertex}]),
%     % erlang:display([draw_item, #{ item => ItemVertex}]),
%     % erlang:display([draw_item, #{ subitems => SubItems}]),
%     % erlang:display([draw_item, #{ rest => RestContentItems}]),

    ItemVertexData = ItemVertex =
        to_vertex_data(ContentItem, ItemNumber),

    digraph:add_vertex(Graph, ItemVertexData),
    add_hierarchy_edges(Graph, ParentVertex, ItemVertexData),

    case Direction of
        first ->
            add_content_edge(Graph, first, ParentVertex, ItemVertex);

        % NOTE Not possible, because the being the last will always
        %      involve  other  edges  as well;  either  `first`  or
        %      `prev` and `next` respectively.
        % last ->
        %     add_content_edge(Graph, last, ParentVertexData, VertexDataB);

        first_and_last ->
            add_content_edge(Graph, first, ParentVertex, ItemVertex),
            add_content_edge(Graph, last, ParentVertex, ItemVertex);

        PrevItemVertex when is_map(PrevItemVertex) ->
            add_content_edge(Graph, prev, ItemVertex, PrevItemVertex),
            add_content_edge(Graph, next, PrevItemVertex, ItemVertex);

        {PrevItemVertex, last} ->
            add_content_edge(Graph, prev, ItemVertex, PrevItemVertex),
            add_content_edge(Graph, next, PrevItemVertex, ItemVertex),
            add_content_edge(Graph, last, ParentVertex, ItemVertex)
    end,

    First = #{ selection => -1 },

    do_draw([First|SubItems], Graph, ItemVertex),
    do_draw([ItemVertex|RestContentItems], Graph, ParentVertex).

make_recording_vertex_data(AbsFilename) ->
    BaseVertexData =
        #{ type  => article
         , path  => AbsFilename
         , title => ""
         },
    add_id(BaseVertexData).

% TODO, NOTE, whatever
% `ContentType` of each vertex in the content graph is
% used as  the state of  the IVR state machine  at one
% point - that  is, almost; `ivr:derive_state/1` keeps
% `content.erl` and `ivr.erl` decoupled.
% to_vertex_data
% ( { ContentType, {Title, _Options} } = _ContentItem
% , ItemNumber
% ) ->
%     to_vertex_data
%       ( { ContentType, Title }
%       , ItemNumber
%       );

% to_vertex_data
% ( { ContentType, [Char|_] = Title } = _ContentItem
% , ItemNumber
% )
% when is_integer(Char)
% ->
%     BaseVertexData =
%         #{ type      => ContentType
%          , selection => ItemNumber
%          , title     => Title
%          },
%     add_id(BaseVertexData).

to_vertex_data
% ( { ContentType, Title, Meta }
( #{} = ContentItemHeader
, ItemNumber
) ->
    Adds =
        % TODO replace with uuid
        #{ selection => ItemNumber
        % TODO this is only important if there is no ID!
         , id => erlang:make_ref()
         },

    futil:pipe(
      [ ContentItemHeader
      , (futil:curry(fun maps:merge/2))(Adds)
      % The entire  point of this  module is to  convert the
      % hierarchical `publication_guide/0` map  into a`digraph`;  `sub_items` will
      % be saved  before calling this function  and supplied
      % to `do_draw/3`.  (`to_vertex_data/2` is  only called
      % twice,  once in  `draw_content_graph/1` and  once in
      % `draw_item/6`)
      , (futil:curry(fun maps:remove/2))(sub_items)
      ]).
    % add_id(BaseVertexData).

% This is what makes linking the same  publication  in
% different categories possible.  Without IDs the same
% `publication`  item   in  the  `publication_guide/0`
% would link  to multiple categories, that  is, create
% loop, and content graph would not be a tree anymore.
% add_id(BaseVertexData) ->
%     BaseVertexData#{id => erlang:make_ref()}.

% prefix_subitems
% ( [ {publication, [_, _]}
%   | _
%   ] = PrefixedPublicationsOrSections
% , _Prefix
% ) ->
%     PrefixedPublicationsOrSections;

% prefix_subitems
% ( [ {section, Title}
%   | Rest
%   ]
% , Prefix
% ) ->
%     prefix_subitems([{publication, Title}|Rest], Prefix);

% prefix_subitems
% ( [ {SubItemType, Title}
%   | Rest
%   ]
% , Prefix
% ) ->
%     NewRest =
%         Rest ++ [{SubItemType, [Prefix, Title]}],

%     prefix_subitems(NewRest, Prefix).

% NOTE If  any of  the parent  had a  `DirOptions` list,  we
%      would never  get here, because it  will trickle down
%      once present
prefix_subitems
  ( SubItems
  , [Char|_] = _TitleMaybeWithOptions
  )
when is_integer(Char)
->
    SubItems;

prefix_subitems
  ( SubItems
  , { Title, DirOptions } = TitleMaybeWithOptions
  )
->
    do_prefix(SubItems, DirOptions, []).

do_prefix([], _DirOptions, Acc) ->
    % The order matters because of automatic item numbering
    lists:reverse(Acc);

do_prefix
  ( [ SubItem | RestSubItems ]
  , DirOptions
  , Acc
  )
->
    SubItemWithMergedOptions =
        case SubItem of
            { { ContentItemWithSubItemsType
              , TitleMaybeWithOptions
              } = ContentItemWithSubItems
            % ContentItemWithSubItems should never be empty
            , [_|_] = SubItemSubItems
            }
            ->
                { { ContentItemWithSubItemsType
                  , merge_dir_options(TitleMaybeWithOptions, DirOptions)
                  }
                , SubItemSubItems
                };

            { LeafItemType, TitleMaybeWithOptions } ->
                { LeafItemType
                , merge_dir_options(TitleMaybeWithOptions, DirOptions)
                }
        end,

    do_prefix
      ( RestSubItems
      , DirOptions
      , [ SubItemWithMergedOptions | Acc ]
      ).

merge_dir_options
  ( TitleMaybeWithOptions
  , DirOptions
  )
->
    case TitleMaybeWithOptions of

        [Char|_] = Title when is_integer(Char) ->
            { Title, DirOptions };

        { Title, SubItemDirOptions } ->
            { Title
            , SubItemDirOptions ++ DirOptions
            }
    end.

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
