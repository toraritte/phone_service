-module(menus).

-export(
    [
      play/2
    , stop_playback/1
    , comfort_noise/1
    ]).

% TODO PROD don't forget to document this in the deployment instructions that these directories will have to be created!
-define(PROMPT_DIR, "/home/toraritte/clones/phone-service/prompts/").
% TODO PROD ?REC_DIR may be omitted altogether; try using just $$sound_prefix of FreeSWITCH and upload recordings directly to cloud storage
-define(REC_DIR,    "/home/toraritte/clones/phone-service/recordings/").

% TODO set verbosity levels in main_menu (0 - lean menus, 1 - help menus are added to all menus explicitly)

% TODO Make menus more responsive on prompt-speed change {{-

% Currently TTS audio files of  a given speed are only
% created on  demand, but  this has the  negative side
% effect  that the  menus are  less responsive  to key
% presses,  and sometimes  the entire  menu is  played
% before taking action.

% A      solution     could      be     that      when
% `prompt_playback_speed`  menu is  left, run  all TTS
% audio commands to produce the files.
% OR
% Just generate all the audio files for speeds between
% 57 and  257. (The  default is 87,  and the  delta is
% 10.)
% }}-

% === PLAYS ========================================== {{-
play(init, Data) -> Data;
play(incoming_call, Data) -> Data;
play(collect_digits, Data) -> Data;

% greeting/announcement
play % GREETING {{-
( greeting = State
, #{ auth_status := AuthStatus } = Data
)
->
    % TODO tutorial, blindness services {{-
    % GoToTutorial = "To listen to the tutorial, dial zero one.",
    % GoToBlindnessServices = "To learn about other blindness services, dial zero four.",
    % }}-

    IsRegistered =
        case AuthStatus of
            registered ->
                fun(D) -> D end;
            unregistered ->
                q(greeting_demo_warning)
        end,

    menu_queue
      ( State
      , Data
      % , [ curried_comfort(250)
        % TODO !!!
        % , (cp(lofa))("https://accessnews.blob.core.windows.net/safeway/ro.mp3")
      , [ q(greeting)
        , IsRegistered
        , q(option_changes)
        ]
      );

% }}-
play % MAIN_MENU {{-
  ( main_menu = State
  , #{ auth_status := AuthStatus
     , current_content :=
       #{ title := Title
        , type  := Type
        }
     } = Data
  )
->
    IsRegistered =
        case AuthStatus of
            registered ->
                fun(D) -> D end;
            unregistered ->
                q(help_menu_demo)
        end,

    Back =
        case Type =:= article of
            true ->
                "the article";
            false ->
                Title
        end,

    menu_queue
      ( State
      , Data
      , [ q(help_menu)
        , IsRegistered
        , q("go" ++ Back)
        , q(help_menu_options)
        ]
      );

% }}-
play % PROMPT_PLAYBACK_SPEED {{-
  ( prompt_playback_speed = State
  , Data
  )
->
    menu_queue(State, Data);

% }}-
play % LEAVE_MESSAGE {{-
  ( leave_message        = State
  , Data
  )
->
    menu_queue(State, Data);

% }}-
play % IN_RECORDING {{-
  ( in_recording        = State
  , Data
  )
->
    % NOTE Cannot  use  `menu_queue/3`  because  it  implicitly
    %      calls  `endplay/?` and  it would  result in  a crash
    %      (probably)  in CHANNEL_EXECUTE_COMPLETE  speak/play;
    %      this  state  is  not  listed in  the  looped  states
    %      clause   and   state   change  only   happens   when
    %      recording  stops  and  it  is  handled  in  its  own
    %      CHANNEL_EXECUTE_COMPLETE record clause
    futil:pipe
      ([ Data
       , q(record_prompt)
       , (cp(beep))("tone_stream://L=1;%(500,500,1000)")
       , fun record/1
       ]);
% }}-
play % CONTENT_ROOT {{-
  ( content_root = State
  , #{ current_content := CurrentContent
     ,     auth_status := AuthStatus
     } = Data
  )
->
    IsRegistered =
        case AuthStatus of
            registered ->
                fun(D) -> D end;
            unregistered ->
                q(demo_mode)
        end,

    menu_queue
      ( State
      , Data
      , [ title(CurrentContent)
        , q(get_help)
        , selections(CurrentContent)
        , IsRegistered
        ]
      );

% }}-
play % CATEGORY {{-
  ( category = State
  , #{ current_content := CurrentContent
     } = Data
  )
->
    ParentCategoryTitle = pick_title(parent, CurrentContent),
    NextCategoryTitle   = pick_title(next,   CurrentContent),

    menu_queue
      ( State
      , Data
      , [ title(CurrentContent)
        , selections(CurrentContent)
        , q("go" ++ ParentCategoryTitle)
        , q("next_category" ++ NextCategoryTitle)
        , q(to_main_menu)
        ]
      );

% }}-
play % PUBLICATION {{-
  ( publication = State
  , #{ current_content := CurrentContent
     } = Data
  )
->
    ArticleNumberString =
        futil:pipe
          ([ content:pick(children, CurrentContent)
           , fun erlang:length/1
           , fun erlang:integer_to_list/1
           ]),

    ParentCategoryTitle =
        pick_title(parent, CurrentContent),

    menu_queue
      ( State
      , Data
      , [ title(CurrentContent)
        , q("article-number-" ++ ArticleNumberString)
        , q(to_publication_help)
        , q("go" ++ ParentCategoryTitle)
        ]
      );

% }}-
play % PUBLICATION_HELP {{-
  ( publication_help = State
  , #{ current_content :=
       #{title := Title} = CurrentContent
     } = Data
  )
->
    ArticleNumber =
        futil:pipe
          ([ content:pick(children, CurrentContent)
           , fun erlang:length/1
           ]),

    LastFirst =
        case ArticleNumber of
            0 -> [];
            1 -> [ q(first_article) ];
            _ -> [ q(first_article), q(last_article) ]
        end,

    % TODO keep an eye on these (here and in other places, e.g., in CATEGORY) for perf issues
    NextPublicationTitle = pick_title(next, CurrentContent),
    PrevPublicationTitle = pick_title(prev, CurrentContent),

    menu_queue
      ( State
      , Data
      , [
         % title(CurrentContent)
        % , q("article-number-" ++ ArticleNumberString)
          q(publication_help)
        ]
        ++ LastFirst ++
        [ q("prev_publication" ++ PrevPublicationTitle)
        , q("next_publication" ++ NextPublicationTitle)
        , q("go" ++ Title)
        , q(to_main_menu)
        , q(extra_publication_help)
        ]
      );

% }}-
play % ARTICLE_INTRO {{-
% NOTE - DO NOT TOUCH `article_intro` STATE {{-
% Yes, this could have  been included in ARTICLE state
% as  an  extra  playback  (and as  long  as  it  does
% not  have  the  same  playback name,  it  would  not
% have interfered  with state progression laid  out in
% CHANNEL_EXECUTE_COMPLETE  `handle_event/4`  clause),
% but  all  the  controls defined  for  ARTICLE  would
% also  apply  to  it,  and as  soon  as  the  article
% starts,  everything would  have  been reset,  making
% this probably confusing for the listener.
% }}-
( article_intro = State
, Data
)
->
    % TODO FEATURE read article meta and figure out how to store the latter
    %      (also, should article_meta be a separate state?)
    % Saving this snippet to demonstrate a solution {{-
    %
    %   1. that multiple heterogeneous playbacks (e.g., Application="speak" &  Application="playback") can be queued
    %      TODO Figure out how to handle CHANNEL_EXECUTE_COMPLETEs
    %
    %   2. how to play audio from cloud blob storage
    %
    % sendmsg_locked(
    %     #{ sendmsg_command => execute
    %     , arguments       => ["speak", "flite|kal|" ++ "Testing! Testing! 27."]
    %     , app_uuid_prefix   => testrun
    %     }),

    % sendmsg_locked(
    %     #{ sendmsg_command => execute
    %     , arguments       => ["playback",  "shout://accessnews.blob.core.windows.net/safeway/ro.mp3"]
    %     , app_uuid_prefix   => lofa
    %     });
    % }}-
    menu_queue
      ( State
      , Data
      , [ q(article_intro) ]
      );

% }}-
play % ARTICLE {{-
  ( article = State
  , #{ current_content :=
       #{ path := Path
        } = CurrentContent
     , playback_offset := Offset
     } = Data
  )
->
    % NOTE Why ARTICLE_INTRO had to split out into its own state {{-
    % speak(article_intro, Data, ...),
    % The CHANNEL_EXECUTE_COMPLETE clause could have been twisted into compliance, but it would have been more complex. As it stands this is still not a piece of cake.
    % }}-

    % TODO FEATURE resume playback from last position (see offset in `init/1`)
    playback(State, Data, Path ++ "@@" ++ Offset );

% }}-
play % ARTICLE_HELP {{-
  ( article_help = State
  , #{ current_content := CurrentContent
     } = Data
  )
->
    ParentPublicationTitle =
        pick_title(parent, CurrentContent),

    menu_queue
      ( State
      , Data
      , [ q(article_paused)
        , q(skip_backward)
        , q(pause_resume)
        , q(skip_forward)
        , q(restart)
        , q(slow_down)
        , q(speed_up)
        , q(volume_down)
        , q(volume_up)
        , q(prev_article)
        , q("go" ++ ParentPublicationTitle)
        , q(to_main_menu)
        , q(next_article)
        ]
      );

% }}-
play % INVALID_SELECTION % {{-
( invalid_selection = State
, Data
)
->
    menu_queue(State, Data);

% }}-
% TODO PROD Does this work?
play % INACTIVITY_WARNING {{-
( inactivity_warning = State
, Data
)
->
    menu_queue(State, Data);

% }}-
play % DEMO_HANGUP {{-
( demo_hangup = State
, Data
)
->
    menu_queue(State, Data);

% }}-
% TODO PROD Does this work?
play % INACTIVITY_HANGUP {{-
( inactivity_hangup = State
, Data
)
->
    menu_queue(State, Data);

% }}-
play % NO_CHILDREN {{-
( no_children = State
, #{current_content := #{ type := ContentType }} = Data
)
->
    % Prompt =
    %     "This "
    %     ++ atom_to_list(ContentType)
    %     ++ " is empty.",

    % speak(State, Data, [Prompt]);

    case ContentType of
        category ->
            menu_queue(State, Data, [ q(empty_category) ]);
        publication ->
            menu_queue(State, Data, [ q(empty_publication) ])
    end;

% }}-
play % NO_PREV_ITEM {{-
( no_prev_item = State
, #{current_content := #{ type := ContentType }} = Data
)
->
    Prompt =
        case ContentType of
            category    -> q(no_prev_category);
            publication -> q(no_prev_publication);
            article     -> q(no_prev_article);
            section     -> q(no_prev_section)
        end,

    menu_queue(State, Data, [ Prompt ]);

% }}-
play % NO_NEXT_ITEM {{-
( no_next_item = State
, #{current_content := #{ type := ContentType }} = Data
)
->
    Prompt =
        case ContentType of
            section     -> q(no_next_section);
            category    -> q(no_next_category);
            publication -> q(no_next_publication);
            article     -> q(no_next_article)
        end,

    menu_queue(State, Data, [ Prompt ]).

% }}-

% }}-
% === STRINGS TO BE READ BY TTS ====================== {{-
% TODO If the  `prompts` dir  does not exist,  nothing will
%      fail  and the  app happily  goes along  without ever
%      playing anything
prompt(PromptName) -> % {{-
    case PromptName of

        "go" ->
            "";
        "go" ++ Back ->
            "Press star to go back to " ++ Back ++ ".";

        % GREETING {{-
        "greeting" ->
            "Welcome to Access News!";

        "option_changes" ->
            "Please  listen carefully,  as  menu and  navigation "
            "options have recently changed.";

        % "greeting_welcome" ->
        %     "Welcome  to  Access  News,  a  service  of "
        %     "Society  For  The   Blind  in  Sacramento, "
        %     "California,  for  blind,  low-vision,  and "
        %     "print-impaired  individuals.  If you  know "
        %     "your selection,  you may  enter it  at any "
        %     "time, or  press pound to skip  to the main "
        %     "categories.";

        "greeting_demo_warning" ->
            "You are  currently in demo mode,  and have "
            "approximately  5 minutes  to  try out  the "
            "system before getting disconnected.";

        % "greeting_footer" ->
        %     "If  you have  any questions,  concerns, or "
        %     "suggestions, please  call 916 889  7519 or "
        %     "leave a message by  pressing zero and then "
        %     "pound.";

        % }}-
        % MAIN_MENU {{-
        "help_menu" ->
            "Help and other options.";

        "help_menu_options" ->
            "Press pound to record a message. "
            "Press 0 to jump to the main category. "
            "Press 2 to change prompt playback speed.";
        
        "help_menu_demo" ->
            "You are  currently in demo mode,  and have "
            "approximately  5 minutes  to  try out  the "
            "system before getting disconnected. "
            "If  you would like to register or have any "
            "questions, please call 9 1 6, 8 8 9, 7 5 1 "
            "9, or leave a message by pressing pound.; ";

        % }}-
        % PROMPT_PLAYBACK_SPEED {{-
        "prompt_playback_speed" ->
            "Press star to go back. "
            "Press 1 to slow down prompts. "
            "Press 2 to reset. "
            "Press 3 to speed up prompts.";

        % }}-
        % LEAVE_MESSAGE {{-
        "leave_message" ->
            "Press star to go back. "
            "Press any other button to start recording.";

        % }}-
        % IN_RECORDING {{-
        % NOTE Deliberately avoiding naming this "in_recording" as it would trigger something unexpected (or just crash) in CHANNEL_EXECUTE_COMPLETE speak/play
        "record_prompt" ->
            "Recording will start after the beep. "
            "To stop recording press any button.";

        % }}-
        % CONTENT_ROOT (empty){{-
        % TODO add "Press pound to enter first category."
        "get_help" ->
            "Press 0 for help at any time.";

        "demo_mode" ->
            "Please note that you are currently in demo mode.";

        % }}-
        % ARTICLE_HELP {{-
        "article_paused" ->
            "Article  paused. Press  2  to resume.  The "
            "following  controls  are available  during "
            "playback.";

        "next_article" ->
            "Press pound to jump to the next article.";

        "prev_article" ->
            "Press 9 to play previous article.";

        "skip_backward" ->
            "Press 1 to skip back ten seconds.";

        "skip_forward" ->
            "Press 3 to jump forward ten seconds.";

        "pause_resume" ->
            "Press 2 to pause or resume article.";

        "slow_down" ->
            % "Press 4 to slow down the recording.";
            "Press 5 to slow down the recording.";

        "speed_up" ->
            "Press 6 to speed up playback.";

        "volume_down" ->
            "Press 7 to lower the volume.";

        "volume_up" ->
            "Press 8 to turn up the volume.";

        "restart" ->
            % "Press 7 to restart the article.";
            "Press 4 to restart the article.";

        % }}-
        % CATEGORY {{-
        "next_category" ->
            "";
        "next_category" ++ Title ->
            "Press pound to jump to the next category, " ++ Title ++ ".";

        % }}-
        % PUBLICATION {{-
        % number of articles {{-
        "article-number-0" ->
            "No articles in this publication.";

        "article-number-1" ->
            "One article in this publication.";

        "article-number-" ++ Number ->
            Number ++ " articles in this publication.";
        % }}-
        % If they press 2 while still in publication -> PUBLICATION_HELP
        % If the article already started playing -> ARTICLE_HELP
        "to_publication_help" -> 
            "Press two for help at any time.";

        % }}-
        % PUBLICATION_HELP {{-
        "publication_help" ->
            "The  following controls  are available: ";

        "next_publication" ->
            "";
        "next_publication" ++ Title ->
            "Press pound to jump to the next publication, "
            "in this case, to " ++ Title ++ ".";

        "prev_publication" ->
            "";
        "prev_publication" ++ Title ->
            "Press nine for the previous publication, "
            "in this case, to " ++ Title ++ ".";

        "first_article" ->
            "Press one to start the newest article.";

        "last_article" ->
            "Press three to start the oldest article.";

        "extra_publication_help" ->
            "The     newest   article     will    start "
            "automatically   once   the  name   of  the "
            "publication  and  the article  count  have "
            "been announced. "
            "Pressing two while listening an to article "
            "will pause playback, and the upcoming menu "
            "will tell you  about all available article "
            "controls. ";

        % }}-
        % TODO This should hold the article title at one point.
        % TODO or, any kind of metadata, that could be customizeable.
        % For example, user wants to hear the date of the article (recorded, written, or both) , and/or the name of the reader, whatever, and they should be able to customize this in `main_menu``
        % ARTICLE_INTRO {{-
        "article_intro" ->
            % "Press two for help at any time.";
            "";

        % }}-
        % ARTICLE (empty){{-
        % }}-
        % TODO Either automatically read the article metadata (date recorded/written, name of reader, title, article author, etc), and have a submenu with the article controls (e.g. "Article paused. To listen to the available article playback controls, press x. For article information press y." or just straight up start reading article metadata).
        % INVALID_SELECTION {{-
        "invalid_selection" ->
            "Invalid selection.";

        % }}-
        % INACTIVITY_WARNING {{-
        "inactivity_warning" ->
            "Please note that we haven't registered any "
            "activity  from  you  for  some  time  now. "
            "Please press any  button  if you are still "
            "there.";

        % }}-
        % DEMO_HANGUP {{-
        "demo_hangup" ->
            "End of demo session.  If you would like to "
            "sign  up  for  Access News,  or  have  any "
            "questions, please call 916-889-7519. Thank "
            "you, and have a wondeful day!";

        % }}-
        % INACTIVITY_HANGUP {{-
        "inactivity_hangup" ->
            "Goodbye.";

        % }}-
        % NO_CHILDREN {{-
        "empty_category" ->
            "This category is empty.";

        "empty_publication" ->
            "";

        % }}-
        % NO_PREV_ITEM {{-
        "no_prev_category" ->
            "This is the first category.";

        "no_prev_publication" ->
            "This is the first publication.";

        "no_prev_article" ->
            "This is the first article.";

        "no_prev_section" ->
            "This is the first section.";

        % }}-
        % NO_NEXT_ITEM {{-
        "no_next_category" ->
            "This is the last category.";

        "no_next_publication" ->
            "This is the last publication.";

        "no_next_article" ->
            "This is the last article.";

        "no_next_section" ->
            "This is the last section.";

        % }}-

        % "go_up" ->
        %     "Press star to go up";

        "to_main_menu" ->
            % "For the main  menu press 0."
            "For help and other options, press 0."
    end.

% }}-

% }}-
% === MENU FUNCTIONS ================================= {{-

% TODO Re-write  so  that  menu  accepts  only  a
%      StringList  with  the  name of  the  menus
%      only, and calls `q/1` internally?
% `menu_queue/2,3` {{-
% CAVEAT None  of the  enqueued playbacks  can have
%        the same `PlaybackName` as the name of the
%        current state:
% {{-
% The `FunList`  argument accepts  anonymous functions
% that accept one argument (IVR `gen_statem`'s `Data`)
% and  that  schedule  a playback  on  the  FreeSWITCH
% server (via `sendmsg_locked/1`).

% The    `gen_statem`'s     `handle_event/4`    clause
% processing   `CHANNEL_EXECUTE_COMPLETE`   FreeSWITCH
% events for  `mod_speak` and  `mod_playback` compares
% the  finished  playback's  names  with  the  current
% state,  and if  they don't  match it  will keep  the
% state, and let things roll.  If they match it either
% `repeat_state`-s  or prods  the  state machine  into
% another state.

% `menu_queue/3` implicitly inserts  a playback at the
% end  matching the  current  state's  name, hence  if
% there is another one in  the `FunList` it will cause
% premature termination!
% }}-
menu_queue(State, Data, FunList) ->
    futil:pipe(
         [ Data ]
      ++ FunList
      ++ [ endplay(State) ]
    ).

menu_queue(State, Data) ->
    menu_queue(State, Data, [ q(State) ]).

% }}-
% `queue_prompt/3`    and `q/1`           {{-
queue_prompt
( Prompt % String
, TTSEngine
, #{ prompt_speed := PromptSpeed } = Data
)
->
    PromptText =
        prompt(Prompt),

    ID =
        id_from_hash(PromptText),
    AudioFilename =
        get_audiofilename
          ( ID
          , futil:sanitize_string(Prompt)
          , PromptSpeed
          ),

    case filelib:is_file(AudioFilename) of
        true ->
            noop;
        false ->
            % google_TTS_to_wav(PromptText, Prompt, PromptSpeed)
            TTSEngine({PromptText, AudioFilename, PromptSpeed})
    end,

    playback(smarties_cereal, Data, AudioFilename).

% https://gist.github.com/dch/bb33330a6d68b8149103 (archived as well)
id_from_hash(Text) ->
    <<Hash:256/big-unsigned-integer>> = crypto:hash(sha3_256, Text),
    lists:flatten(io_lib:format("~64.16.0b", [Hash])).

q(PromptName) when is_atom(PromptName) ->
    q(atom_to_list(PromptName));

q(PromptName) ->
    ((futil:curry(fun queue_prompt/3))
      (PromptName))
      (fun tts:google_TTS_to_wav/1).

% }}-
% `play_title/2`      and `title/1`       {{-
play_title
( #{ title := Title } = Vertex
, #{ prompt_speed := PromptSpeed } = Data
)
->
    TitleFile =
        do_check
        ( Title ++ "."
        , Vertex
        , "title"
        , PromptSpeed
        , fun tts:google_TTS_to_wav/1
        ),

    playback
      ( vertex_title
      , Data
      , TitleFile
      ).

title(Vertex) ->
    (futil:curry(fun play_title/2))(Vertex).

% }}-
% `play_selections/2` and `selections/1`  {{-

% TODO Improve responsiveness
% THE ISSUE: Sometimes when  pressing a  digit, the DTMF  tone is
%            seemingly ignored,  and then  it does tend  to start
%            working  after a  couple  of seconds  -  even if  it
%            should do so instantly (e.g., because there are only
%            <10 choices in the menu, hence no wait time needed).
%            The user  may become  irritated and  start pummeling
%            the  button,  causing  them to  enter  subitems  not
%            intended originally.
%
% I think there are 2 reasons for this: 
% 
% + prompts  are generated  on-demand when  it does  not
%   exist (so this would make it a one-time issue)
% 
% + when entering a vertex,  ALL selections are enqueued
%   to be  played, and then  ALL of them are  stopped on
%   valid DTMF event
% 
% Would  the  app  be  more  responsive  if  only  one
% playback would  be scheduled and it  would start the
% next one on  CHANNEL_EXECUTE_COMPLETE? It would also
% make it more complicated.
play_selections(Vertex, Data) ->
    PlaybackFunctions =
        [  (futil:curry(fun do_play_selection/2))(Child)
        || Child <- content:pick(children, Vertex)
        ],

    futil:pipe
      ([ Data ]
       ++ PlaybackFunctions
      ).

selections(Vertex) ->
    (futil:curry(fun play_selections/2))(Vertex).

% }}-

% }}-
% === AUDIO FUNCTIONS ================================ {{-

% NOTE on use of  sync and async FreeSWITCH events {{-
% ====================================================
% Using  `sendmsg_locked`  most   of  the  time  (with
% `speak`   or   `playback`)    because   it   enables
% synchronous execution on FreeSWITCH, so `event_lock`
% events  get  queued   up  and  called  sequentially.
% Otherwise playbacks would overlap.

% "Locked" (i.e., synchronous) and async events can be
% mix-and-matched.   Just  did   a  `comfort_noise/0`,
% followed   by  a   `speak`   (both  "locked"),   and
% then  an  async  hangup (with  simple  `sendmsg/2`).
% Everything  went  fine,  and  after  the  all  media
% played the  channel got hung up.  Another test: sync
% `comfort_noise/0` followed  by an async  `speak` and
% async `hangup`. Could hear  the `speak` kick in, but
% once `hangup` was received the call got terminated.

% Although  this  does not  mean  that  the IVR  state
% machine (i.e., the  `gen_statem`) stands still! Once
% the command  is sent off with  `sendmsg_locked`, the
% process  continues execution,  and waits  for coming
% events (such as DTMF events).
% }}-

playback % and `cp/1` {{-
( PlaybackName
, #{ call_UUID := UUID } = Data
, Path
)
->
    % return the application UUID string.
    ApplicationUUID =
        fs:sendmsg_locked(UUID, execute, ["playback", Path]),
          % #{ command => execute
          %  , args       => ["playback", Path]
          %  % , app_uuid_prefix   => PlaybackName
          %  }),

    save_playback_meta(ApplicationUUID, PlaybackName, Data).

cp % time with Roy Wood Jr. {{-
(PlaybackName) ->

    PlaybackWithReorderedArgs =
        fun(Name, Path, Data) ->
            playback(Name, Data, Path)
        end,

    (futil:curry(PlaybackWithReorderedArgs))(PlaybackName).

% }}-
% }}-
record %/1       {{-
( #{ call_UUID := UUID
   , caller_number := CallerNumber
   } = Data
)
->

    PadInt =
        fun(Integer) ->
            futil:pipe
              ([ Integer
               , fun erlang:integer_to_list/1
               , fun (S) -> string:pad(S, 2, leading, $0) end
               , fun lists:flatten/1
               ])
        end,

    {{Year, Month, Day},{Hour, Min, Sec}} =
        calendar:system_time_to_universal_time
          ( erlang:system_time()
          , native
          ),
    Filename =
           tl(CallerNumber)
        ++ "_"
        ++ integer_to_list(Year)
        ++ PadInt(Month)
        ++ PadInt(Day)
        ++ "-"
        ++ PadInt(Hour)
        ++ PadInt(Min)
        ++ PadInt(Sec)
        ++ ".wav",

    % Will be reset to `none` in "record CHANNEL_EXECUTE_COMPLETE" handle_event clause
    fs:fsend({api, uuid_setvar, UUID ++ " playback_terminators any"}),

    % NOTE using `uuid_record` is either broken or there is a setting that I did not find; recording starts alright, but it cannot be stopped (tried from fs_cli), and it does not honour `playback_terminator` but also blocks events going out. At least, the outbound erlang process never received any DTMF events. Should have sent it via `bgapi` instead of `api`?
    % fsend({api, uuid_record, stitch([get(uuid), "start",  Filename, "5400"])});
    % fsend({api, uuid_record, stitch([get(uuid), "start", ?REC_DIR ++ Filename, "5400"])});

    fs:sendmsg_locked
      ( UUID
      , execute
      , [ "record"
        ,    ?REC_DIR
          ++ Filename
          ++ " 5400" % 90 minutes
        ]
      ),

      Data.
        % #{ command => execute
         % , args =>
         %   [ "record"
         %   ,    ?REC_DIR
         %     ++ Filename
         %     ++ " 5400" % 90 minutes
         %   ]
        % }).

% }}-
% `comfort_noise/1,2,3`, `curried_comfort/1`, `endplay/1` {{-
comfort_noise(Name, Milliseconds, Data) -> 
    % logger:debug("play comfort noise"),
    ComfortNoise =
           "silence_stream://"
        ++ integer_to_list(Milliseconds)
        ++ ",1400",

    playback(Name, Data, ComfortNoise).

comfort_noise(Milliseconds, Data) ->
    comfort_noise(comfort_noise, Milliseconds, Data).

comfort_noise(Data) ->
    comfort_noise(750, Data).

curried_comfort(Milliseconds) ->
    (futil:curry(fun comfort_noise/2))(Milliseconds).

endplay(State) -> % Data -> Data (point is, it's a function of 1 arity)
    ((futil:curry(fun comfort_noise/3))(State))(0).

% }}-

% TODO Clean up (see TODOs inside), and figure
%      out which clauses are not in use
stop_playback(#{ playbacks := [] } = Data) -> % {{-
    logger:debug(#{ a => "STOP_PLAYBACK []"}),
    Data;

% Nothing is playing and the most recent playback has been stopped before.
% For example, ?CATEGORIES is playing, "*" is sent when nothing is in history, `warning/?` stops playback, warning starts playing, ends naturally, so HANDLE_CHANNEL_EXECUTE_COMPLETE get clears {warning, false}, and calls that scenario in its `case`, which is `repeat/?`, that also has a `stop_playback/1`, and the most recent playback is the stopped ?CATEGORIES. Subsequent HANDLE_CHANNEL_EXECUTE_COMPLETE clauses will clear out these entries. (or should...)
% TODO make sure that the Data#playbacks stack gets cleared properly
% TODO QUESTION Where are the stopped recordings filtered out? Or does this act as a history? But not all menus play prompts.
% TODO QUESTION If there is only one playback allowed at one time, why is `Data#playbacks a list?
stop_playback(#{ playbacks := [{_, _, {stopped, true}}|_] = Playbacks } = Data) ->
    logger:debug(#{ a => "STOP_PLAYBACK stopped=true", playbacks => Playbacks}),
    Data;

% Stop the currently playing prompt
% (but just in case, stop all, even though only one should be playing at any time)
stop_playback(
  #{ call_UUID := UUID
   , playbacks :=
       [ { ApplicationUUID  % |
         , PlaybackName     % | currently playing
         , {stopped, false} % |
         }
         | Rest
       ] = Playbacks
   } = Data
) ->
    % logger:debug("stop playback"),
    % TODO Should this  be `bgapi`? Will the  synchronous `api`
    %      call wreak havoc when many users are calling?

    % TODO: is stopped status necessary? If not, is there a scenario where it may be in the future?
    % NOTE it is superfluous: {{-
    % When CHANNEL_EXECUTE_COMPLETE playback of the same name as the state comes in, it means that nothing has stopped the playback, because the `stopped_playback/1` function is only called in the state enter function on state change. As a corollary, every other time where PlaybackName =/= State means that the playback has been stopped
    % That is, the stopped status can be derived from the current state and ended playback.
    % }}-
    fs:fsend({api, uuid_break, UUID ++ " all"}),
    StoppedPlayback = {ApplicationUUID, PlaybackName, {stopped, true}},
    logger:debug(#{ a => "STOP_PLAYBACK stopped=false", playbacks => Playbacks, stopped => StoppedPlayback}),
    Data#{ playbacks := [StoppedPlayback|Rest] }.

% }}- (stop_playback)

% }}-
% === INTERNAL ======================================= {{-

% TODO clean up (see TODOs inside)
save_playback_meta % {{-
( ApplicationUUID
% TODO PROD remote PlaybackName - it is unnecessary (it is already part of ApplicationUUID) and just messes things up
, PlaybackName
, #{ playbacks := Playbacks } = Data
)
->
    Playback =
        { ApplicationUUID
        , PlaybackName % Only for convenience; already part of `ApplicationUUID`.
        % TODO: is stopped status necessary? If not, is there a scenario where it may be in the future?
        % NOTE it is superfluous: {{-
        % When CHANNEL_EXECUTE_COMPLETE playback of the same name as the state comes in, it means that nothing has stopped the playback, because the `stopped_playback/1` function is only called in the state enter function on state change. As a corollary, every other time where PlaybackName =/= State means that the playback has been stopped
        % That is, the stopped status can be derived from the current state and ended playback.
        % }}-
         % THE STOPPED FLAG IS IMPORTANT: had the false assumptions that simple checking whether PlaybackName =:= CurrentState, but the behaviour should be different when the playback stops naturally, or by a warning that will keep the same state. Without a "stopped" bit there is no way to know how to proceed (e.g., ?CATEGORIES is stopped by a warning, warning starts playing, CHANNEL_EXECUTE_COMPLETE comes in with ?CATEGORIES, but if we simple repeat, than the the warning is stopped immediately.)
        , {stopped, false} % Has the playback been stopped?
        },
        % #{ playback_name => PlaybackName
        %  ,    is_stopped => false
        %  },
    NewPlaybacks =
        [ Playback | Playbacks ],
        % Playbacks#{ ApplicationUUID => Playback },

    Data#{ playbacks := NewPlaybacks }.

% }}-
do_play_selection % {{-
(#{ title := Title
  , selection := Selection
  } = Vertex
, #{ prompt_speed := PromptSpeed } = Data
)
->
    SelectionText =
           "Press "
        ++ integer_to_list(Selection)
        ++ " for "
        ++ Title
        ++ ".",

    futil:pipe
      ([ do_check
           ( SelectionText
           , Vertex
           , "selection"
           , PromptSpeed
           , fun tts:google_TTS_to_wav/1
           )
       , ((futil:curry(fun playback/3))(selection))(Data)
       ]).

% }}-
do_check  % {{-
%        String -> Map -> String -> FileName
( Text
, #{ id := VertexID
   , title := Title
   } = Vertex
, LabelKey
, PromptSpeed
, TTSEngine
)
->

    Label =
        content:get_label(Vertex),

    {SavedText, LabelMap} =
        case Label =:= [] of
            true ->
                {"", #{}};
            false ->
                { maps:get(LabelKey, Label, "")
                , Label
                }
        end,

    FileBasename =
           futil:sanitize_string(Title)
        ++ "-"
        ++ LabelKey,

    % This ID shoud only change when the entire graph is redrawn.
    % NOTE 
    ID =
        lists:filter
          ( fun(C) when C =< 57, C >= 48 -> true;
               (_) -> false
            end,
            futil:stringify(VertexID)
          ),

    AudioFilename =
        get_audiofilename(ID, FileBasename, PromptSpeed),

    % Need to  check both  because vertex label  holds the
    % text while the filename has the speed.
    case
        { Text =:= SavedText
        , filelib:is_file(AudioFilename)
        }
    of
        {true, true} ->
            noop;
        _ ->
            % 1. call the google tts script (and save the audio to ?PROMPT_DIR)
            % google_TTS_to_wav(Text, FileBasename, PromptSpeed),
            TTSEngine({Text, AudioFilename, PromptSpeed}),
            % 2. save the text to vertex label
            content:add_label(Vertex, LabelMap#{ LabelKey => Text })
    end,

    AudioFilename.

% }}-
% NOTE why is the ID needed?
% If the prompt-file already exists, changing the prompt text and recompiling won't take effect, so one either has to change the name of the case option in "STRINGS TO BE READ BY TTS", or delete the file (or entire "prompts" dir).
% This would also cause "numbering" issues -> E.g., the publication_guide has changed and an item is at a different place, but because the prompt-file's name is only the publication name, it will just take the existing one.
% -> detect changes
% SOLUTION: just as vertices has names (to make linking possible), add unique references to the prompt-filenames as well
%
% `ID` could have been simply part of `FileBasename` but making it explicit
get_audiofilename(ID, FileBasename, Speed) -> % {{-
       ?PROMPT_DIR
    ++ ID
    ++ "-"
    ++ integer_to_list(Speed)
    ++ "-"
    ++ FileBasename
    ++ ".wav".

% }}-
pick_title(Direction, ContentItem) ->
    MaybeItem =
        content:pick(Direction, ContentItem),
    case MaybeItem of
        [] ->
            "";
        [ #{title := Title} ] ->
            Title
    end.

% }}-
% === FOR TESTING ONLY =============================== {{-

% Poor TTS, for testing only,
% not used anywhere in PROD
speak % {{-
( PlaybackName
, #{ call_UUID := UUID } = Data
, TextList
)
->
    ApplicationUUID =
        fs:sendmsg_locked
          ( UUID
          , execute
          , [ "speak"
            , "flite|kal|" ++ futil:stitch(TextList)
            ]
          ),

    save_playback_meta(ApplicationUUID, PlaybackName, Data).

% }}-

% }}-

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
