-module(ivr).
-behaviour(gen_statem).

-export(
    [ start_link/0

    % gen_statem callbacks
    , init/1
    , callback_mode/0
    , handle_event/4
    , terminate/3
    ]).

% NOTE document this somewhere, but it's silly to use things like this. Use a proper application scaffold (e.g., with rebar3) or just very very early in the project
% erl -eval 'cover:compile_directory("./outbound_erl").' -eval '{lofa, freeswitch@tr2} ! register_event_handler.' -run filog -run user_db -sname access_news -setcookie OldTimeRadio

% NOTE (TODO?) This entire project could have been written in Elixir from the get-go but wasn't that familiar with FreeSWITCH (or Erlang) and wasn't sure how `mod_erlang_event` would call it. For posterity: FreeSWITCH calls out to an erlang node with an MFA so if an Elixir node is started with the proper shortname and cookie (`erl -sname access_news -setcookie OldTimeRadio`), it would have worked just as well as the underlying infrastructure is Erlang. Once the connection is up, the rest is just message passing to the FreeSWITCH C-node.

% TODO reporting/stats
% Add logs to identify demo mode callers!
% What else? (There's plenty...)

% NOTE On timeouts {{-

% According to [the `gen_statem` section in Design Principles](from https://erlang.org/doc/design_principles/statem.html#cancelling-a-time-out):
% > When a time-out is  started, any running time-out of
% > the same type (`state_timeout`, `{timeout, Name}` or
% > `timeout`), is  cancelled, that is, the  time-out is
% > restarted with the new time.

% The [man pages](https://erlang.org/doc/man/gen_statem.html#ghlink-type-generic_timeout) are clearer:
% > Setting a [generic] timer with the same `Name` while
% > it is running will restart  it with the new time-out
% > value. Therefore it is possible to cancel a specific
% > time-out by setting it to infinity.
% }}-
-define(DEMO_TIMEOUT, 300000). % 5 min
% -define(INTERDIGIT_TIMEOUT, 2000).

% NOTE Inactivity warnings and timeouts {{-
% are not  defined here,  becasue they  have different
% values  whether one  is listening  to an  article or
% somewhere in the menu system.
%
% See `re_start_inactivity_timer/1`  (which is invoked
% for every incoming DTMF digit).
% }}-

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

init(_Args) -> % {{-
    %% TODO Set up logging to file. {{-
    % It  does not  work  here  because the  `gen_statem`s
    % spawned  by  each call  do  not  have unique  names.
    % What  about giving  them a  unique one?  Is it  even
    % important? (It may be better to  log  everything  in
    % one file when it comes to calls.)
    %
    % filog:add_process_handler(?MODULE, Ref),
    % filog:process_handler_filter(?MODULE, Ref),
    %% }}-

    % TODO Why was this necessary?
    process_flag(trap_exit, true),

    Data =
        #{ received_digits  => []
         , playback_offset  => "0"
         , article_speed    => "0"
         , auth_status      => unregistered % | registered
         , current_content  => hd(publication_guide:root())
         , current_children => []
         , playbacks        => []
         , prompt_speed     => 87
         , call_UUID        => ""
         , caller_number    => ""
         },

    {ok, init, Data}.
% }}-

%% Rationale for using `handle_event_function` callback mode {{-
%% -----------------------------------------------------
%%
%% Short
%% ====================================================
%% Found  this callback  mode  the  most convenient  to
%% pre-process all incoming  FreeSWITCH events (emitted
%% by `mod_erlang_event`) into a form that is easier to
%% pattern match.
%%
%% See "%%  `mod_erl_event` pre-processing %%"  for the
%% details.

%% Long
%% ====================================================
%%
%% ### Mostly `info` type `gen_statem` events {{-
%%
%% FreeSWITCH  events  are  sent as  regular  messages,
%% hence  they  are  handled   by  matching  on  `info`
%% type  `gen_statem`  events  in the  callbacks.  With
%% `state_functions`,  the  `info`  events need  to  be
%% checked  in every  state,  and even  though a  macro
%% could have  been used  to type  less, it  would have
%% been harder to oversee what is happening.
%%
%% With  `handle_event_function`  there   is  only  one
%% callback,   `handle_event/4`,  so   there  is   only
%% one  pre-processing  clause  is needed  (for  `info`
%% `gen_statem` events).
%%
%% This  is  what the  module  would  have looked  like
%% otherwise:
%%
%% ```erl
%% %% `state_functions` pseudo-code
%%
%% incoming_call(info, ModErlEventMsg, _Data) ->
%%     MassagedModErlEvent = do_the_deed,
%%     {keep_state_and_data, {next_event, internal, MassagedModErlEvent}};
%% incoming_call(internal, MassagedFSEvent, Data) ->
%%     do_something_here,
%%     {next_state, ...}.
%%
%% %% ... N more other states
%%
%% publication(info, ModErlEventMsg, _Data) ->
%%     MassagedModErlEvent = do_the_deed,
%%     {keep_state_and_data, {next_event, internal, MassagedModErlEvent}};
%% publication(internal, MassagedFSEvent, Data) ->
%%     do_something_here,
%%     {next_state, ...}.
%% ```
%%
%% Note: Using  **inserted events**  to pre-process
%%       events.     See    "%%     `mod_erl_event`
%%       pre-processing %%" for the details.

%% }}-
%% ### Event-focused approach {{-
%%
%% An event-focused  approach is justified  (as opposed
%% to  a state-focused  one  above)  by how  FreeSWITCH
%% events drive the IVR state machine forward.
%%
%% (Again, as  noted above,  the majority  of incomiing
%% `gen_statem` events  are of  the `info`  event type,
%% and they  are hard to  work with, so it  makes sense
%% to  concentrate processing  into  a single  function
%% instead  of  the  same functionality  scattered  all
%% aroudnd the place.)

%% }}-
%% ### Clearer structure {{-
%%
%% Also, if external call control use cases will arise,
%% calls and casts can have their own sections.
%%
%%     ```erlang
%%     %% `handle_event_function` pseudocode
%%
%%     %%%%%%%%%%%%%%%%%%%%%%%
%%     %% FreeSWITCH events %%
%%     %%%%%%%%%%%%%%%%%%%%%%%
%%
%%     handle_event(info, ModErlEventMsg, _State, _Data) ->
%%         MassagedModErlEvent = do_the_deed,
%%         {keep_state_and_data, {next_event, internal, MassagedModErlEvent}};
%%
%%     handle_event(internal, MassagedModErlEvent, State, Data) ->
%%         %% ...
%%         {next_state, ...}.
%%
%%     handle_event(internal, MassagedModErlEvent, AnotherState, Data) ->
%%         %% ...
%%         {next_state, ...}.
%%
%%     %%%%%%%%%%%%%%%%%%%%%%%
%%     %% External commands %%
%%     %%%%%%%%%%%%%%%%%%%%%%%
%%
%%     handle_event(cast, ...) ->
%%
%%     handle_event(call, ...) ->
%%     ```

%% }}-
%% ### Complex states {{-
%%
%% Not used, but good to have.
%%
%% Note: In the  beginning almost  all state  was a
%%       complex  one;  first,  the  authentication
%%       status lived  there (but made no  sense to
%%       check  it  in  every state),  and  then  a
%%       tuple  was  used  to track  where  we  are
%%       in  the content  (but  that separated  out
%%       into  the `content`  `gen_server` using  a
%%       `digraph`, because  the IVR  controls only
%%       depend  on  the  type  of  content,  i.e.,
%%       category, publication, or article).
%%
%%       See pre-"cleanup" commits for examples.
%%   }}-
%% }}-
callback_mode() ->
    [handle_event_function, state_enter].

terminate(Reason, State, Data) ->
    logger:debug(#{ self() => ["TERMINATE (normal-ish)"
                              , #{ data => Data, reason => Reason, state => State }
                              ]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% `gen_statem:handle_event/4` clauses    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% STATE ENTER %% %{{-
handle_event(enter, OldState, State, Data) ->

    % TODO OFFSET-RESET Does this actually do anything? {{-
    % Had  issues when  trying  to  reset article  offsets
    % when   navigating  away   (i.e.,  next,   prev,  up,
    % MAIN_MENU -> CONTENT_ROOT, ARTICLE_HELP -> MAIN_MENU
    % ->  CONTENT_ROOT)  but do  not  when  just going  to
    % MAIN_MENU, ARTICLE_HELP, next/prev  when there is no
    % article there.  So tried  a couple things,  this was
    % one. }}-
    % UPDATE: it does
    OffsetReset =
        fun(GenStatemData) ->
            case lists:member(OldState, [publication, category, content_root]) of
                true  -> GenStatemData#{ playback_offset := "0" };
                false -> GenStatemData
            end
        end,

    NewData =
        futil:pipe(
          [ Data
          % There  should be  only one  playback running  at any
          % time.
          , fun menus:stop_playback/1
          , fun menus:comfort_noise/1
          , OffsetReset
          , (futil:curry(fun menus:play/2))(State)
          % , fun winnow_playbacks/1
          ]
        ),

    {keep_state, NewData};

% }}-
%% `info` CLAUSES (FOR FreeSWITCH EVENTS) %% {{-

% }}-
%% Step 1. Pre-process FreeSWITCH events {{- {{-
%% -------------------------------------
%% Convert all incoming FreeSWITCH events into a form that is easier to pattern match (in this case, a map).
%%
%% The `mod_erlang_event` format of `FSEventHeaders` is of
%% `[{HeaderString1, ValueString1}, ..., {HeaderStringN, ValueStringN}]`
%% , and the fastest way to parse that is using `proplists` function, and it can't be matched in function headers.
%% }}- }}-

%% TODO Once the  IVR is  implemented, filter out  FS events
%%      (and/or parts of the events) that are  irrelevant to
%%      lower overhead.

%% MOD_ERL_EVENT_MASSAGE (info)
%% `mod_erl_event` pre-processing %% {{-

%% Using _inserted events_ (or  _stored events_) to add
%% an event  pre-processing step via  the `{next_event,
%% EventType,  EventContent}`   **transition  action**.
%% `gen_statem` also ensures  that ["_the stored events
%% are inserted  in the  queue as  the next  to process
%% before  any  already  queued events.  The  order  of
%% these  stored  events  is preserved,  so  the  first
%% next_event in the containing  list becomes the first
%% to process._"](https://erlang.org/doc/man/gen_statem.html#type-action).

%% TODO: Update https://erlang.org/doc/design_principles/statem.html#Inserted%20Events with the above quote from the `gen_statem` man page.

%% The   event  content   is   name  `ModErlEventMsg`   on {{- {{-
%% purpose, and not  `FSEvent` (i.e., FreeSWITCH event)
%% or   similar,  because   `mod_erlang_event`  creates
%% its  own  arbitrary  messages  and  wrappers  around
%% FreeSWITCH  events. Most  are  consistent in  having
%% the form  of `{ModErlEventCallStatus,  {event, [UUID
%% |  FSEventHeaders]}}`  but there  is  `{call_hangup,
%% UUID}`,  even  though   there  are  `CHANNEL_HANGUP`
%% events received besides it.
%%
%% The   actual    FreeSWITCH   event    is   basically
%% `FSEventHeaders`,  as they  consist of  header-value
%% pairs anyways, and an optional body.
%%
%% TODO It is rare for FreeSWITCH events to have body (or is
%%      it?) and not sure how  the Erlang message looks like
%%      in that case.
%% }}- }}-

handle_event(
  info,
  {ModErlEventCallStatus, {event, [UUID | FSEventHeaders]}} = _ModErlEventMsg,
  % {ModErlEventCallStatus, {event, [UUID | FSEventHeaders]}} = ModErlEventMsg,
  _State,
  _Data
) ->
    %% As this  clause preceeds **every**  state transition
    %% (not   just  state   changes!)   AND  always   keeps
    %% `gen_statem` state and data,  this log does not need
    %% to be  repeated in  `handle_event/4` below,  only to
    %% double-check matched values, calculations etc.
    % logger:debug(#{ self() => ["MOD_ERL_EVENT_MASSAGE", #{ data => Data, state => State, mod_erl_event_call_status => ModErlEventCallStatus }]}),

    MassagedModErlEvent =
        { UUID
        , ModErlEventCallStatus
        , maps:from_list(FSEventHeaders)
        },
    TransitionActions =
        [ {next_event, internal, MassagedModErlEvent}
        ],

    % {_, _, E} = MassagedModErlEvent,
    % #{ "Event-Name" := EventName } = E,
    % logger:emergency(#{ event => { EventName, MassagedModErlEvent}}),

    %% Keeping  state  and  data,  because  the  FreeSWITCH
    %% `info` messages  can come any time,  and this clause
    %% only transforms them.
    {keep_state_and_data, TransitionActions};
%% }}-

%% CALL_HANGUP (info) {{-

%% NOTE There  are two  related  events, CHANNEL_HANGUP  and
%%      CHANNEL_HANGUP_COMPLETE. Not sure  why, but probably
%%      to  denote certain  stages of  the call  termination
%%      process,  just  as  the  `Answer-State`  header  has
%%      multiple values (e.g., `early`, `answered`).
handle_event(
  info,                 % EventType
  {call_hangup, _UUID}, % EventContent = ModErlEventMsg
  _State,                % State
  Data                  % Data
) ->
    % logger:debug(#{ self() => ["CALL_HANGUP", #{ data => Data, state => State }]}),
    %% Gave `normal` as reason, otherwise `gen_statem` will {{-
    %% crash  (which isn't  really a  problem, because  the
    %% started `gen_statem`  processes are not  linked, and
    %% could've  just trap  exits, but  why go  through the
    %% hassle, when  a demo timeout is  considered a normal
    %% behaviour as well.) }}-
    {stop, normal, Data};
%% }}-

%% SENDMSG_CONFIRMATION (info) {{-
handle_event(
  info,                          % EventType
  % ok = Msg,
  ok,
  _State,
  _Data                               % Data
) ->
    % logger:debug(#{ self() => ["SENDMSG_CONFIRMATION", #{ message => Msg, state => State}]}),
    keep_state_and_data;
%% }}-

%%%%%%%%%%%%%%%%%%%%%%%%
%% `internal` clauses %%
%%%%%%%%%%%%%%%%%%%%%%%%

% INTERNAL HANGUP (demo_hangup, inactivity_hangup) {{-
    % see `demo_hangup` on what happens next or how the call is terminated from the server side
% Hanging up, so stop processing other `internal` events [..]
% NOTE `info` (i.e., external) events are still processed, but they are irrelevant at this point. If need to save them for debugging, just modify MOD_ERL_EVENT_MASSAGE, or the DEBUG clause in the "`info` clauses (for FreeSwITCH events)" section above
handle_event(
  internal, % EventType
  _EventContent,
  % TODO This does not seem right. I believe complex hangup states like this have been abolished. How would we get here anyway?
  {hangup, _},                % State
  #{ call_UUID := UUID } = _Data                  % Data
) ->
    % logger:debug(#{ self() => ["in HANGUP state", #{ data => Data, state => hangup, event_content => EC }]}),

    % sending it synchronously to allow the playback to end
    fs:sendmsg_locked(UUID, hangup, ["16"]),
        % #{ command => hangup
        %  , args       => ["16"]
        %  }),
    %% Not stopping the  `gen_statem` process here, because
    %% there will be further events related to the `hangup`
    %% command above. See "CALL_HANGUP" `info` clause below.
    keep_state_and_data;
% }}-

%% (STATE: init -> incoming_call)
%% MOD_ERLANG_EVENT: call {{-

%% Call init {{-
%% ====================================================
%% This is the only state where `call` event can arrive
%% -  or at  least this  is  how it  should be;  `call`
%% should happen only once in  a call, and becuase this
%% is  outbound mode,  and  the  process just  started,
%% there is not much else flying around.

%% Long story short, this is kind of the real `init` of
%% the call (`init/1` is to initialize the `gen_statem`
%% process), and thus some  variables are set here, and
%% will document call-global notes here as well.

%% }}-

handle_event
( internal                           % EventType
, { UUID                             % \
  , call                             % |
  , #{ "Channel-ANI" := CallerNumber % | EventContent = MassagedModErlEvent
     }                               % |
  }                                  % /
, init                               = State
, #{ auth_status := unregistered }   = Data
)
->
    logger:debug(#{ CallerNumber => ["INCOMING_CALL", #{ data => Data, state => State, uuid => UUID }]}),

    fs:sendmsg_locked(UUID, execute, ["answer", []]),

    %% REMINDER (`playback_terminator`) {{-
    %% ====================================================
    %% https://freeswitch.org/confluence/display/FREESWITCH/playback_terminators
    %%
    %% Kind of the FreeSWITCH-equivalent of the CSS reset.
    %%
    %% The   default   `playback_terminator`  is   *,   and
    %% that   should  be   ok   because  it   is  tied   to
    %% going   up/back   a   menu,  but   playback   should
    %% be  killed  in most  cases  anyway, and disabling it
    %% would make it explicit (and also avoiding mysterious
    %% errors in the future).
    %%
    %% The same  could've been done in  the dialplan (e.g.,
    %% `freeswitch/dialplan/default.xml`) using
    %% ```xml
    %% <action application="set" data="playback_terminators=none"/>
    %% ```
    %% but this pertains  to each call, and  better to have
    %% everything related to the same call in one place.
    %% }}-
    fs:fsend({api, uuid_setvar, UUID ++ " playback_terminators none"}),

    { NewAuthStatus
    , TransitionActions
    } =
        case is_user_registered(CallerNumber) of
            true ->
                { registered
                , []
                };
            false ->
                DemoTimeout =                       % Generic timeout
                    { {timeout, unregistered_timer} % { {timeout, Name}
                    , ?DEMO_TIMEOUT                 % , Time
                    , demo_hangup                   % , EventContent }
                    },
                { unregistered
                , [ DemoTimeout
                  ]
                }
        end,

    { next_state
    , incoming_call
    , Data#{ auth_status   := NewAuthStatus
           , call_UUID     := UUID
           , caller_number := CallerNumber
           }
    , TransitionActions
    };
%% }}-

%% (STATE: incoming_call -> greeting)
%% MOD_ERLANG_EVENT: call_event(CHANNEL_ANSWER) {{-
handle_event
( internal                                % EventType
, { _UUID                                 % \
  , call_event                            % |
  , #{ "Event-Name" := "CHANNEL_ANSWER" } % | EventContent = MassagedModErlEvent
  }                                       % /
, incoming_call                           = State
, Data
)
->
    {next_state, greeting, Data};

%% }}-

%% (STATE: * -> *)
%% MOD_ERLANG_EVENT: call_event(DTMF) {{-
%% tag: HANDLE_DTMF_FOR_ALL_STATES
handle_event(
  internal,                   % EventType
  { UUID                      % \
  , call_event                % |
  , #{ "Event-Name" := "DTMF" % | EventContent = MassagedModErlEvent
     , "DTMF-Digit" := Digit  % | TODO TEST THIS!
     }                        % |
  },                          % /
  State,
   % if `ReceivedDigits =/= []`, we are collecting digits
  #{
      received_digits := ReceivedDigits
   % ,  auth_status := AuthStatus
   % ,  menu_history := MenuHistory
   } = Data
) ->
    % logger:debug(#{ self() => ["HANDLE_DTMF_FOR_ALL_STATES", #{ digit => Digit, state => State}]}),
    logger:debug( #{ a => "HANDLE_DTMF", state => State, digit => Digit, collected_digits => ReceivedDigits }),

    % Reset inactivity timers whenever a new DTMF signal comes in (i.e., a button is pressed)
    % TODO Amend when voice commands get implemented
    re_start_inactivity_timer(State),

    % {{-
    %% 1 2 3   \
    %% 4 5 6    > Interdigit time-out (IDT) = 2 sec (?)
    %% 7 8 9   /  EXCEPT when playing an article, then IDT = 0
    %% -----
    %% * 0 #   Interdigit time-out = 0

    % Then notion is that in every state when one of "123456789" is pressed, "collecting_digits" mode is initiated, and "*" and "#" will be ignored (except when received after the interdigit time-out (IDT) expired), but they will trigger an extra IDT seconds to get new digits.

    % DTMF tones are divided into two categories: category selectors, and single digit instant actions. Category selectors have interdigit timeout (that may be configurable), whereas single digits instant actions immediately respond by changing state, performing a command, etc.
    % Another difference is that category selectors only change state once the `interdigit_timer` expires, hence they will keep the state as is in this clause, but single digit instant actions do whatever necessary to move on (stop playback, change state, etc.).
    % }}-
    % {{- {{-
    % When `idt_running` is true, it means we are collecting digits for category selection. So when IDT is not running and 0 comes in, then go to main menu, but when IDT is running, then add it to `ReceivedDigits`, renew IDT, and keep collecting. Another example is when IDT is not running, and * (star) and # (pound) arrives, go back or go to next category respectively, but when IDT is running, then ignore these inputs and renew IDT, and keep collecting.

    % IDT race condition when collecting digits
    % ====================================================
    % It is possible that IDT times out while processing a DTMF digit. What then?

    % This probably isn't as bad as it sounds, or maybe not even bad at all. For example, a user is having trouble to dial digits quickly (device issues, disability, etc.), and one of the digits hits this race condition, but this is an important input, so IDT time-out can be ignore, and now this digit is saved and IDT extended.

    % Is there a scenario where this can turn sour? Someone start dialing for a category, but then change their minds, and wants to hit the main menu, go back, etc. Even in not a race condition status they would have to wait until the IDT runs out. If they do get into a category, they can always go back with * (star).

        %% Keep  playing  the   greeting,  continue  collecting
        %% digits,  and caller  will be  sent to  the specified
        %% category, if the `DTMFString` contains a valid entry
        %% after  the  interdigit  time-out.  Once  the  playback
        %% finishes,  the `PLAYBACK_STOP`  event is  emitted by
        %% FreeSWITCH,  and  the  state   will  be  changed  to
        %% `?CATEGORIES`, but  the behavior  there will  be the
        %% same, and  the digits will continue  to be collected
        %% (until they make sense).
        % NOTE ONLY: * (star) and # (pound) in `?CATEGORIES` {{-
        % a) Put category browsing with # (pound) on hold for now (see clause "# (pound) in any state"
        % b) Decided to put an option `main_menu` to be able to jump to the main categories, which means that * (star) should make it possible to jump back to where we came from
        % #{                state := ?CATEGORIES
        %  ,       received_digit := Digit
        %  ,    collecting_digits := false
        %  , digit_is_one_to_nine := false % not necessary, but explicit
        %  }
        % when Digit =:= "*";
        %      Digit =:= "#"
        % ->
        %     % Ignore
        %     keep_state_and_data;
        % }}-

    % }}- }}-

    % Only needed for COLLECT DIGITS case clause's guards
    % RootState =
    %     case is_tuple(State) of
    %         false ->
    %             State;
    %         true ->
    %             element(1, State)
    %     end,

    % case {State, Digit} of
    case State of
        % TODO # has no function (except greeting, main_menu, article)
        % Use it to browse categories (step into the next one, like in article?)
        % or forward in history instead of back?

        % === COLLECT_DIGITS % {{-
        % { collect_digits, Digit } ->
        collect_digits ->
            collect_digits(Digit, Data);

        % }}-
        % === GREETING (-> content_root) {{-
        greeting ->
            % Set `current_content` (it is `none` at this point)
            % QUESTION Should this be set in `init/1` instead?
            %          I think it is easier to understand it this way.
            % NewData = set_current(publication_guide:root(), Data),
            % NOTE set in init because of `next_content/3` complications

            case Digit of
                % [*#]       {{- => content root
                _ when Digit =:= "*"
                     ; Digit =:= "#"
                ->
                    {next_state, content_root, Data};
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % [1-9]      {{- => collect digits
                _ ->
                    collect_digits(Digit, Data)
                % }}-
            end;

        % }}-
        % === MAIN_MENU (loop) {{-
        main_menu ->
            case Digit of
                % * (star)  {{- <- Go back (current content)
                "*" ->
                    { next_state
                    , derive_state(Data)
                    , Data
                    };
                % }}-
                % TODO PROD sign_in
                % # (pound) {{- => leave_message
                "#" ->
                    {next_state, leave_message, Data};
                % }}-
                % 0         {{- => content_root
                "0" ->
                    next_content(content_root, State, Data);
                % }}-
                % TODO FEATURE tutorial
                % 1         {{- => tutorial
                "1" ->
                    % {next_state, tutorial, Data};
                    keep_state_and_data;
                % }}-
                % 2         {{- => prompt_playback_speed
                "2" ->
                    {next_state, prompt_playback_speed, Data};
                % }}-
                % TODO FEATURE settings
                % 3         {{- => settings
                "3" ->
                    % {next_state, settings, Data};
                    keep_state_and_data;
                % }}-
                % TODO PROD blindness_services
                % 4         {{- => blindness_services
                "4" ->
                    % {next_state, blindness_services, Data};
                    keep_state_and_data;
                % }}-
                % 5-9       {{- => UNASSIGNED (keep state and data)
                % TODO FEATURE random_play
                "5" -> keep_state_and_data;
                "6" -> keep_state_and_data;
                "7" -> keep_state_and_data;
                % TODO FEATURE where_am_i
                "8" -> keep_state_and_data;
                "9" -> keep_state_and_data
                % }}-
            end;

        % }}-
        % === PROMPT_PLAYBACK_SPEED (-> (loop)) {{-
        prompt_playback_speed ->

            PromptSpeed =
                maps:get(prompt_speed, Data),

            case Digit of
                % *          {{- => main_menu
                "*" ->
                    {next_state, main_menu, Data};
                % }}-
                % 1          {{- => prompt_playback_speed (slower)
                "1" ->
                    {repeat_state, Data#{ prompt_speed := PromptSpeed - 10 }};
                % }}-
                % 1          {{- => prompt_playback_speed (reset)
                "2" ->
                    {repeat_state, Data#{ prompt_speed := 87 }};
                % }}-
                % 1          {{- => prompt_playback_speed (faster)
                "3" ->
                    {repeat_state, Data#{ prompt_speed := PromptSpeed + 10 }};
                % }}-
                % [#04-9]     {{- => content root
                _ ->
                    keep_state_and_data
                % }}-
            end;

        % }}-
        % === LEAVE_MESSAGE (loop) {{-
        leave_message ->

            case Digit of
                % *          {{- => main_menu
                "*" ->
                    {next_state, main_menu, Data};
                % }}-
                % [#1-9]     {{- => content root
                _ ->
                    {next_state, in_recording, Data}
                % }}-
            end;

        % }}-
        % === (inactive) IN_RECORDING (-> (loop))  (see NOTE below) {{-

        % NOTE This  clause  will never  come  into  play; as  soon
        %      as  recording is  started the  control is  hijacked,
        %      and  no  events will  be  sent  through  the
        %      event socket. That  is why `playback_terminators` is
        %      set  to `any`,  and  when that  stops the  recording
        %      the  generated  CHANNEL_EXECUTE_COMPLETE event  will
        %      be  evaluated, and  control returns  to this  Erlang
        %      process.

        % in_recording ->

        %     case Digit of
        %         % [*#0-9]    {{- => leave_message
        %         _ ->
        %             fs:fsend
        %               ({ api
        %                , uuid_record
        %                , futil:stitch([UUID, "stop", "all"])
        %                }),
        %             {next_state, leave_message, Data}
        %         % }}-
        %     end;

        % % }}-
        % === CONTENT_ROOT (loop) {{-
        content_root ->
            case Digit of
                % TODO FEATURE implement content history so this will bring back to a previuos content
                % *          {{- => ignore
                "*" ->
                    keep_state_and_data;
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => enter first child category
                "#" ->
                    next_content(first, State, Data);
                % }}-
                % [1-9]      {{- => collect digits
                _ when Digit =:= "1"
                     ; Digit =:= "2"
                     ; Digit =:= "3"
                     ; Digit =:= "4"
                     ; Digit =:= "5"
                     ; Digit =:= "6"
                     ; Digit =:= "7"
                     ; Digit =:= "8"
                     ; Digit =:= "9"
                ->
                    collect_digits(Digit, Data)
                % }}-
            end;

        % }}-
        % === CATEGORY (loop) {{-
        category ->
            case Digit of
                % *          {{- => go_up (i.e., up in content hierarchy)
                "*" ->
                    next_content(parent, State, Data);
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => next category (i.e., next sibling)
                "#" ->
                    next_content(next, State, Data);
                % }}-
                % [1-9]      {{- => collect digits
                _ when Digit =:= "1"
                     ; Digit =:= "2"
                     ; Digit =:= "3"
                     ; Digit =:= "4"
                     ; Digit =:= "5"
                     ; Digit =:= "6"
                     ; Digit =:= "7"
                     ; Digit =:= "8"
                     ; Digit =:= "9"
                ->
                    collect_digits(Digit, Data)
                % }}-
            end;

        % }}-
        % === PUBLICATION (-> first article) {{-
        publication ->
            case Digit of
                % *          {{- => go_up (i.e., up in content hierarchy)
                "*" ->
                    next_content(parent, State, Data);
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => next publication (i.e., next sibling)
                "#" ->
                    next_content(next, State, Data);
                % }}-
                % 1         {{- => Play FIRST article
                "1" ->
                    next_content(first, State, Data);
                % }}-
                % TODO FEATURE list articles
                % 2         {{- => publication_help
                "2" ->
                    {next_state, publication_help, Data};
                % }}-
                % 3         {{- => Play LAST article
                "3" ->
                    next_content(last, State, Data);
                % }}-
                % 4-8       {{- => UNASSIGNED (keep_state_and_data)
                "4" -> keep_state_and_data;
                % TODO FEATURE play where_am_i
                "5" -> keep_state_and_data;
                % TODO FEATURE list articles (basically a shortcut for "*" to emphasize that one can just do that to listen to available publications in a category)
                "6" -> keep_state_and_data;
                "7" -> keep_state_and_data;
                % TODO FEATURE "continue from last time"
                "8" -> keep_state_and_data;
                % }}-
                % 9         {{- => PREVIOUS publication
                "9" ->
                    next_content(prev, State, Data)
                % }}-
            end;

        % }}-
        % === PUBLICATION_HELP (loop) {{-
        publication_help ->
            case Digit of
                % * (star)  {{- <- Go back (current content)
                "*" ->
                    { next_state
                    , derive_state(Data)
                    , Data
                    };
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => next publication (i.e., next sibling)
                "#" ->
                    next_content(next, State, Data);
                % }}-
                % 1         {{- => Play FIRST article
                "1" ->
                    next_content(first, State, Data);
                % }}-
                % TODO FEATURE list articles
                % 2         {{- => UNASSIGNED (keep_state_and_data)
                % "2" -> keep_state_and_data;
                "2" ->
                    keep_state_and_data;
                % }}-
                % 3         {{- => Play LAST article
                "3" ->
                    next_content(last, State, Data);
                % }}-
                % 4-8       {{- => UNASSIGNED (keep_state_and_data)
                "4" -> keep_state_and_data;
                % TODO FEATURE play where_am_i
                "5" -> keep_state_and_data;
                % TODO FEATURE list articles (basically a shortcut for "*" to emphasize that one can just do that to listen to available publications in a category)
                "6" -> keep_state_and_data;
                "7" -> keep_state_and_data;
                % TODO FEATURE "continue from last time"
                "8" -> keep_state_and_data;
                % }}-
                % 9         {{- => PREVIOUS publication
                "9" ->
                    next_content(prev, State, Data)
                % }}-
            end;

        % }}-
        % === ARTICLE_INTRO (-> article) {{-
        article_intro ->
            case Digit of
                % *          {{- => back to publication
                "*" ->
                    next_content(parent, State, Data);
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => next article (i.e., next sibling)
                "#" ->
                    next_content(next, State, Data);
                % }}-
                % 1          {{- => skip rest of intro and start article
                "1" ->
                    {next_state, article, Data};
                % }}-
                % 2          {{- => ARTICLE_HELP
                "2" ->
                    {next_state, article_help, Data};
                % }}-
                % TODO FEATURE reset volume/speed
                % NOTE Difference between this and the same clause in ARTICLE_HELP is that this will always start the article from the beginning (offset =:= 0), while ARTICLE_HELP should resume the playback from last saved position
                % 1-8        {{- => skip rest of intro and start article
                _ when Digit =:= "3"
                     ; Digit =:= "4"
                     ; Digit =:= "5"
                     ; Digit =:= "6"
                     ; Digit =:= "7"
                     ; Digit =:= "8"
                ->
                    {next_state, article, Data};
                % }}-
                % 9          {{- => previous article
                "9" ->
                    next_content(prev, State, Data)
                % }}-
            end;

        % }}-
        % === ARTICLE (i.e., article playback) -> next article {{-
        %      see `play % article`
        article ->
            case Digit of
                % *          {{- => go_up (i.e., up in content hierarchy)
                "*" ->
                    next_content(parent, State, Data);
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    % `playback_offset` is saved at CHANNEL_EXECUTE_COMPLETE clause in {article, is_stopped: true}
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => next_article (i.e., next sibling)
                "#" ->
                    next_content(next, State, Data);
                % }}-
                % NOTE using `api` for now instead of `bgapi`
                % 1          {{- => skip_backward (10s)
                "1" ->
                    fs:uuid_fileman(UUID, "seek:-10000"),
                    keep_state_and_data;
                % }}-
                % 2          {{- => pause_resume
                "2" ->
                    % `playback_offset` is saved at CHANNEL_EXECUTE_COMPLETE clause in {article, is_stopped: true}
                    {next_state, article_help, Data};
                % }}-
                % 3          {{- => skip_forward (10s)
                "3" ->
                    fs:uuid_fileman(UUID, "seek:+10000"),
                    keep_state_and_data;
                % }}-
                % 4          {{- => restart
                "4" ->
                    fs:uuid_fileman(UUID, "restart"),
                    {keep_state, Data};
                % }}-
                % 5          {{- => slow_down
                "5" ->
                    change_speed("-", Data);
                % }}-
                % 6          {{- => speed_up
                "6" ->
                    change_speed("+", Data);
                % }}-
                % 7          {{- => volume_down
                "7" ->
                    % TODO PROD figure out the right amount
                    fs:uuid_fileman(UUID, "volume:-2"),
                    keep_state_and_data;
                % }}-
                % TODO FEATURE add to favourites
                % 8          {{- => volume_up
                "8" ->
                    fs:uuid_fileman(UUID, "volume:+2"),
                    keep_state_and_data;
                % }}-
                % 9          {{- => prev_article
                "9" ->
                    next_content(prev, State, Data)
                % }}-
            end;

        % }}-
        % === ARTICLE_HELP (loop) {{-
        % _ when State =:= article_help
        %      ; State =:= article_intro
        article_help ->
            case Digit of
                % *          {{- => go_up to publication
                "*" ->
                    next_content(parent, State, Data);
                % }}-
                % 0          {{- => main_menu
                "0" ->
                    {next_state, main_menu, Data};
                % }}-
                % #          {{- => next article (i.e., next sibling)
                "#" ->
                    next_content(next, State, Data);
                % }}-
                % TODO FEATURE reset volume/speed
                % Almost the same here as in ARTICLE_INTRO; see note there
                % 1-8        {{- => resume article
                _ when Digit =:= "1"
                     ; Digit =:= "2"
                     ; Digit =:= "3"
                     ; Digit =:= "4"
                     ; Digit =:= "5"
                     ; Digit =:= "6"
                     ; Digit =:= "7"
                     ; Digit =:= "8"
                ->
                    {next_state, derive_state(Data), Data};
                % }}-
                % 9          {{- => previous article
                "9" ->
                    next_content(prev, State, Data)
                % }}-
            end;

        % }}-
        % === INACTIVITY_WARNING (loop) {{-
        % Not checking  for digits because their  only meaning
        % here  is  to  skip   this  prompt,  and  cancel  the
        % inactivity timers.
        inactivity_warning ->
            { next_state
            , derive_state(Data)
            , Data
            };

        % }}-
        % === UNINTERRUPTABLE STATES
        % warnings -> current content
        % hangups  -> keep_state_and_data
        % [*#0-9]    {{- => ignore (keep_state_and_data)
        _ when State =:= invalid_selection
             % ; State =:= inactivity_warning
             ; State =:= demo_hangup
             ; State =:= inactivity_hangup
             ; State =:= no_children
             ; State =:= no_prev_item
             % TODO FEATURE add option to jump to the first element
             ; State =:= no_next_item
        ->
            keep_state_and_data;

        % }}-

        % TODO Nothing should ever end up here, so remove once the basics are done
        % EXCEPT when adding a new state, and forgot to add a clause here to deal with DTMF input (e.g., to ignore them completely, such as with "collect_digits")
        Unhandled ->
            logger:emergency(#{ self() => ["UNHANDLED_DIGIT", Unhandled, {state, State}]}),
            keep_state_and_data
    end;

    %% (Why `keep_state`)FALSE and no inserted events? {{- {{-
    %% ====================================================
    % ### 1. Why no inserted events?
    %
    % Already using inserted events to make FreeSWITCH events more manageable (see `MOD_ERL_EVENT_MASSAGE`), and adding extra `internal` events would cause a race condition; "_the order of these stored events is preserved, so the first next_event in the containing list becomes the first to process_", thus massaged FreeSWITCH earlier (read further why this is a problem).
    %
    % It is also unnecessary because more natural transition opportunities will present themselves, such as `PLAYBACK_STOP` FreeSWITCH events whenever the playback is stopped. Adding an extra internal event is therefore superfluous as well.
    %
    % There is a hitch though: `PLAYBACK_STOP` events won't provide a context regarding what key press (i.e., `DTMF` event) caused the playback to stop, so these will need to get stored.
    %
    % ### 2. Why keep the state? (KEEP! See update below)
    %
    % Many key presses (in many states) should stop the playback and cause a state change (e.g., from `greeting` to `?CATEGORIES`), but if state is changed in this `handle_event/4` clause then it will be hard to understand how subsequent events need to handled.
    %
    % For example, `stop_playback/0` will cause FreeSWITCH to emit a `PLAYBACK_STOP` event, and if the state is changed, these events will have to be handled in those next states. But these next states will also receive DTMF events that stop the playback, and the handling of `PLAYBACK_STOP` events will be pushed to the subsequent states, and so on.
    %
    % It seems more logical to keep the state, and handle the `PLAYBACK_STOP` events where the playbacks have been started. Not to mention the natural state changes that result in playbacks stopping because all the text has been read (so loop, or next state). So if a state change is forced on `DTMF` events, and then playback relating to a specific state will have to be handled in the same state and in the next one. Will get messy real quick.
    %
    %% REMINDER
    %%
    %% When  the   `PLAYBACK_STOP`  event  comes   in  from
    %% FreeSWITCH it first
    %% 1. enters as an `info` message,
    %% 2. gets massaged  into more  pattern-matchable form
    %%    that
    %% 3. will be inserted as an `internal` event,
    %% 4. and only then will the `PLAYBACK_STOP` FreeSWITCH
    %%    event really handled.

% #### 2.1 Update - don't keep state

% The problem is race condition again. Take the 0# combo (for unregistered users to sign in) in `greeting`: user hits 0, playback stops, and if # (pound) comes in almost immediately then it will be evaluated in `greeting`, having completely different semantics.

% Another false assumption I made is that `PLAYBACK_STOP` is the only thing that needs checking, but `speak` (and probably other TTS engine players) only generate `CHANNEL_EXECUTE_COMPLETE`, although the problem remains the same.

    %% }}- }}-
%% }}-

%% HANDLE_CHANNEL_EXECUTE_COMPLETE (catching stopped playbacks mostly) {{-
handle_event(
  internal,                    % EventType
  { _UUID                      % \
  , call_event                 % |
  , #{ "Event-Name" := "CHANNEL_EXECUTE_COMPLETE"
       % TODO re-eval on any playback change! (external engine etc)
     , "Application" := Application
     , "Application-UUID" := ApplicationUUID
     , "variable_playback_last_offset_pos" := LastOffset
     }                            % | EventContent = MassagedModErlEvent
  } = E,                           % /
  State,
  #{    playbacks := Playbacks
   ,    playback_offset := SavedOffset
   } = Data
)
when Application =:= "speak"
   ; Application =:= "playback"
->
    { ApplicationUUID
    , StoppedPlayback
    , {stopped, IsStopped}
    } =
        proplists:lookup(ApplicationUUID, Playbacks),

    % Simply deleting the actual playback only that comes in this clause will cause stale playback handles to accumulate, because `play/2` functions queue multiple playbacks on the FreeSWITCH server via `sendmsg_locked/?`, save them all to Data, but if a menu is interrupted (i.e., navigated away), those queued up playbacks simply just vanish, and because they never even played, there will be no CHANNEL_EXECUTE_COMPLETE event generated, leaving all those entries in the Data#playbacks proplist.
    % With this approach, because Data#playbacks is used like a stack, all the entries before the actual stopped playback (either naturally or by user intervention) are obsolete and only represent the artifacts of a previous menu (I know, I love tautology).
    NewPlaybacks =
        lists:takewhile
          ( fun({AppID, _, _}) ->
                AppID =/= ApplicationUUID
            end
          , Playbacks
          ),
    NewData =
        Data#{ playbacks := NewPlaybacks },

    case State of

        % TODO list_articles (and lots others)

        _ when StoppedPlayback =:= article
             , IsStopped =:= true
        ->
            {keep_state, NewData#{playback_offset := LastOffset}};

        % 1
        % `is_stopped` should always be TRUE in this scenario
        _ when StoppedPlayback =/= State ->
            {keep_state, NewData};

        % 2
        % For  example, selecting  a category  that interrupts
        % the menu playback, so `gen_statem` state enter stops
        % all playback when going to another state, so let the
        % new menu keep playing.
        _ when StoppedPlayback =:= State
             , IsStopped =:= true
        ->
            {keep_state, NewData};

        % 3
        % STATES (that stopped naturally) TO BE LOOPED
        % ------------------------------------------
        % TODO It is kinda just a fluke that this construct works here. Investigate and document to avoid future surprises.
        _ when StoppedPlayback =:= State
             , IsStopped =:= false
             , State =:= main_menu
             ; State =:= prompt_playback_speed
             ; State =:= content_root
             ; State =:= category
             ; State =:= article_help
             ; State =:= leave_message
             ; State =:= publication_help
        ->
            {repeat_state, NewData};

        % STATES THAT (stopped naturally and)
        % HAVE SPECIAL DIRECTIVES
        % ------------------------------------------
        % This clause applies implicitly at this point:
        % ```
        % _ when StoppedPlayback =:= State
        %      , IsStopped =:= false
        % ->
        % ```

        greeting ->                     % |
            {next_state, content_root, NewData};

        publication ->
            next_content(first, State, NewData);

        article ->
            next_content(next, State, NewData);

        collect_digits ->
            {keep_state, NewData};

        article_intro ->
            {next_state, article, NewData};

        inactivity_warning ->
            {next_state, derive_state(NewData), NewData};

        % UNINTERRUPTABLE states that only play a prompt
        _ when State =:= invalid_selection
             ; State =:= no_children
             ; State =:= no_prev_item
             ; State =:= no_next_item
        ->
            {next_state, derive_state(NewData), NewData};

        _ when State =:= demo_hangup;
               State =:= inactivity_hangup
        ->
            {keep_state, NewData}
    end;
%% }}-

handle_event
( internal                     % EventType
, { UUID                       % \
  , call_event                 % |
  , #{ "Event-Name" := "CHANNEL_EXECUTE_COMPLETE"
     , "Application" := "record"
     }                            % | EventContent = MassagedModErlEvent
  } = E                            % /
, in_recording = State
, Data
)
->
            % logger:debug(
            %     #{ a => "record CHANNEL_EXECUTE_COMPLETE"
            %     , state => State
            %     , data => Data
            %     }),
    fs:fsend({api, uuid_setvar, UUID ++ " playback_terminators none"}),
    {next_state, leave_message, Data};

%% Debug clauses for `internal` events {{-
handle_event(
  internal,                          % EventType
  { _UUID                             % \
  , call_event                             % |
  % , #{ "Event-Name" := EventName } = FSEvent % | EventContent = MassagedModErlEvent
  , #{ "Event-Name" := EventName } = FSEvent % | EventContent = MassagedModErlEvent
  } = E,                                 % /
  State,
  _Data                               % Data
)
->
    % logger:debug(#{ self() => ["UNKNOWN_INTERNAL_CALL_EVENT", #{ event_name => EventName, state => State, fs_event => FSEvent}]}),
    % logger:debug(""),
    keep_state_and_data;

handle_event(
  internal,                          % EventType
  Msg,
  State,
  _Data                               % Data
) ->
    % logger:emergency(#{ self() => ["UNKNOWN_INTERNAL", #{ unknown_msg => Msg, state => State}]}),
    keep_state_and_data;

% handle_event(
%   internal,                          % EventType
%   _MassagedModErlEvent,
%   _State,
%   _Data                               % Data
% ) ->
    % filog:process_log(debug, #{ from => "another event that is not INCOMING_CALL and CALL_HANGUP" });

%% }}-

% }}-

%%%%%%%%%%%%%%
%% Timeouts %%
%%%%%%%%%%%%%%

%% inactivity timers (info) {{-
% The timers are started with `re_start_inactivity_timer/0` when a call comes in, or restarted when a DTMF signal is received. The purpose of button presses is to change the state, so it is not important to restart the timers on the actual state change, and interdigit timers have a short value compared to the inactivity ones.
% This way is even better, because on invalid category selection the user will remain in the same state as long as they can't input a correct number, but the timers will get reset on each try.
handle_event
  ( info               % EventType
  , inactivity_warning % EventContent
  , _State
  , Data
  )
->
    % TODO Add notes on how this works. See `init/1` also for Data#inactivity
    {next_state, inactivity_warning, Data};

handle_event(
  info,
  inactivity_hangup,
  _State,
  Data
) ->
    {next_state, inactivity_hangup, Data};
    % see `demo_hangup` on what happens next or how the call is terminated from the server side
%% }}-

% DEBUG (info) {{-
% Catch-all clause to see unhandled `info` events.
handle_event(
  info,  % EventType
  Msg,   % EventContent
  State, % State
  _Data  % Data
) ->
    % logger:emergency(#{ self() => ["UNHANDLED_INFO_EVENTS", #{ unknown_msg => Msg, state => State}]}),
    keep_state_and_data;
% }}-

%% unregistered_timer {{-
handle_event(
  {timeout, unregistered_timer}, % EventType
  demo_hangup,                       % EventContent
  State,
  #{ auth_status := AuthStatus } = Data
) ->
    case AuthStatus of

        registered ->
            % ignore if user already signed in in the meantime
            keep_state_and_data;

        unregistered ->
            {next_state, demo_hangup, Data}
            % A state_enter callback occurs at this point (i.e., `handle_event(enter, ...` is called; an event does not have to come in to trigger it), that triggers playback stop, starts playing the `demo_hangup` playback. Meanwhile, when an event comes in, it will trigger INTERNAL HANGUP clause, formally issuing a hangup request towards freeswitch (synchronously to let playback finish), and it will ignore any subsequent FS events.
            % TODO verify the claim below
            % So any internal event will end up at INTERNAL HANGUP, and when the `call_hangup` event comes in, then will be the gen_statem stopped with `{stop, normal, Data}`
    end;
%% }}-

%% interdigit_timer {{-
handle_event
  ( {timeout, interdigit_timer} % EventType
  , eval_collected_digits       % EventContent
  , collect_digits   % State
  % , {collect_digits, Content}   % State
  , #{ received_digits  := ReceivedDigits
     , current_content  := CurrentContent
     , current_children := CurrentChildren
     } = Data
  )
->
    Selection =
        futil:pipe
          ([ ReceivedDigits
           , fun lists:reverse/1
           , (futil:cflip(fun string:join/2))("")
            % Will throw on empty list but it should never happen the way collect_digits/2 is called
            % (to elaborate: `collect_digits/2` is only triggered by DTMF digits 1-9, hence it should never be empty. If it is, it means that the menu calling (i.e., DTMF-processing) logic is wrong, and should be corrected)
           , fun erlang:list_to_integer/1
           ]),

    % Always clear DTMF buffer when the `interdigit_timer` expires, because this is the point the buffer is evaluated. Outcome is irrelevant, because at this point user finished putting in digits, hence waiting for the result, and so a clean slate is needed.
    NewData =
        Data#{ received_digits  := []
             , current_children := []
             },

    SelectionResult =
        [  Child
        || Child
           % <- publication_guide:pick(children, CurrentContent)
           <- CurrentChildren
           ,  maps:find(selection, Child) =:= {ok, Selection}
        ],

    % logger:debug(
    %     #{ a => "INTERDIGIT_TIMEOUT"
    %      , current_content => CurrentContent
    %      , selection_result => SelectionResult
    %      , selection => Selection
    %      }),

    case SelectionResult of
        [] ->
            {next_state, invalid_selection, NewData};
        [SelectedContent] ->
            { next_state
            % `content_root` will never be possible here so pattern matching would have sufficed but this is more uniform
            , derive_state(SelectedContent)
            , NewData#{ current_content := SelectedContent }
            }
    end.
%% }}-

%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions %%
%%%%%%%%%%%%%%%%%%%%%%%

% TODO Eval on 2 digits immediately to speed up things
collect_digits(Digit, Data)
    when Digit =:= "*"
       ; Digit =:= "#"
->
    collect_digits("", Data);

collect_digits % {{-
  ( Digit
  , #{ received_digits  := ReceivedDigits
     , current_content  := CurrentContent
     , current_children := CurrentChildren
     } = Data
  )
when Digit =:= "1"
   ; Digit =:= "2"
   ; Digit =:= "3"
   ; Digit =:= "4"
   ; Digit =:= "5"
   ; Digit =:= "6"
   ; Digit =:= "7"
   ; Digit =:= "8"
   ; Digit =:= "9"
   ; Digit =:= "0"
->
    NewCurrentChildren =
        case CurrentChildren =:= [] of
            true ->
                publication_guide:pick(children, CurrentContent);
            false ->
                CurrentChildren
        end,

    InterDigitTimeout =
        case length(NewCurrentChildren) >= 10 of
            true ->
                2000;
            false ->
                0
        end,

    NewData =
        Data#{ received_digits  := [Digit|ReceivedDigits]
             , current_children := NewCurrentChildren
             },

    % The notion is that the `InterDigitTimer` is restarted whenever {{-
    % a new DTMF signal arrives while we are collecting digits. According to [the `gen_statem` section in Design Principles](from https://erlang.org/doc/design_principles/statem.html#cancelling-a-time-out) (and whoever wrote it was not a fan of proper punctuation): "_When a time-out is started any running time-out of the same type; state_timeout, {timeout, Name} or timeout, is cancelled, that is, the time-out is restarted with the new time._"  The [man pages](https://erlang.org/doc/man/gen_statem.html#ghlink-type-generic_timeout) are clearer: "_Setting a [generic] timer with the same `Name` while it is running will restart it with the new time-out value. Therefore it is possible to cancel a specific time-out by setting it to infinity._"
    % }}-
    % TODO Does timer cancellation produce an event?
    InterDigitTimer =                 % Generic timeout
        { {timeout, interdigit_timer} % { {timeout, Name}
        , InterDigitTimeout           % , Time
        , eval_collected_digits       % , EventContent }
        },

    % Keeping state because this is only a utility function collecting the DTMF signals. State only changes when the `interdigit_timer` times out.
    % (using `repeat_state` so that state enter clause would get triggered, and with that the entered DTMF digit would get read back (via `collect_digits` `play/2` function))
    { next_state
    , collect_digits
    , NewData
    , [ InterDigitTimer ]
    }.
% }}-

is_user_registered(CallerNumber) -> %% {{-
    PhoneNumber =
        case CallerNumber of
            %% Could've    matched   this    in    the   head    of
            %% `handle_event/4` but if the pattern match fails, the
            %% process would  just go down.  (Calling `terminate/3`
            %% first, in this case, as it is declared above.)
            "+1" ++ Number ->
                Number;

            % TODO handle this more gracefully
            _Invalid ->
                % filog:process_log(emergency, #{ from => ["IS_USER_REGISTERED", #{ caller_number => CallerNumber}]}),
                exit("invalid")
        end,
    user_db:look_up(PhoneNumber).
%% }}-

% !!! Keeps the content  graph (`content.erl`) and the
%     IVR state machine (`ivr.er.`) decoupled!
derive_state(#{ current_content := Content } = _Data) -> % {{-
    derive_state(Content);

derive_state(#{ type := _ } = Content) ->
    case Content of
        #{ selection := 0 } ->
            content_root;
        #{ type := article } ->
            % Implies that ARTICLE_INTRO is always played (even if article is resumed)
            article_intro;
        #{ type := sectioned_publication } ->
            category;
        #{ type := section } ->
            publication;
        #{ type := ContentType } ->
            ContentType
    end.

% }}-

next_content % {{-
( Direction
, OldState
, #{ current_content := CurrentContent
   } = Data
)
  % The `children` direction is omitted on purpose as it
  % is  semantically(?) different  from the  rest; aside
  % from [] and [_], it  can also return lists with more
  % than one  element (i.e., [_,_|_]). It  also does not
  % make  sense  because we  only  need  one element  to
  % determine the next state.
  when Direction =:= parent;
       Direction =:= first;
       Direction =:= last;
       Direction =:= next;
       Direction =:= prev;
       Direction =:= content_root
->
    NextContent =
        case publication_guide:pick(Direction, CurrentContent) of
            [] -> none;
            [Content] -> Content
        end,
    NewData =
        Data#{ current_content := NextContent },
    NextState =
        % When  there  is  no  vertex in  a  given  direction,
        % `publication_guide:pick/2`  will   return  `none`,   in  which
        % case  `NextState` is  set  to  a non-existing  state
        % so  that  control  will  be  redirected  to  special
        % uninterruptable warning  states in the  next `case`;
        % see the last 2 clauses where `NextState` is compared
        % with `OldState`. (The  parallel between the control
        % flow and current state  of things is accidental, but
        % fitting.)
        case NextContent =:= none of
            false ->
                case {OldState, NextContent} of
                    {article, #{ type := article }} ->
                        article;
                    _ ->
                        derive_state(NextContent)
                end;
            true ->
                'Black_Lives_Matter'
        end,

    case {NextContent, Direction} of
        {none, next}  -> {next_state, no_next_item, Data}; % \
        {none, prev}  -> {next_state, no_prev_item, Data}; % | Not `NewData`, because
        {none, first} -> {next_state, no_children, Data};  % | that direction is empty.
        {none, last}  -> {next_state, no_children, Data};  % /
     % `{none,   parent}`  would   only   be  possible   if
     % `content_root` would  be the current content,  but *
     % is disabled  there so this should  never happen (see
     % DTMF processing `handle_event/4` clause)

        _ when OldState =:= NextState
             , CurrentContent =/= NextContent
        ->
            case OldState =:= article of
                true  -> {repeat_state, NewData#{ playback_offset := "0" }};
                false -> {repeat_state, NewData}
            end;

        _ when OldState =/= NextState
        ->
            {next_state, NextState, NewData}
    end.

%   }}-
change_speed(Direction, #{ call_UUID := UUID } = Data) -> % {{-
    #{ article_speed := Speed } = Data,
    NewSpeed =
        futil:pipe
         ([ Speed
          , fun erlang:list_to_integer/1
          , fun(X) ->
                case Direction of
                    "-" -> X - 1;
                    "+" -> X + 1
                end
            end
          , fun erlang:integer_to_list/1
          ]),
    fs:uuid_fileman(UUID, "speed:" ++ NewSpeed),
    NewData = Data#{ article_speed := NewSpeed },
    {keep_state, NewData}.

% }}-

% TODO clean up comments below and TODOs {{-
% start,   when call is answered
% restart, when a DTMF signal comes in
% NOTE: impossible to time out while collecting digits, because a DTMF signal restarts the timers, and the interdigit timeout is a couple seconds
% Only triggered when  a new DTMF event  comes in, and
% not  affected by  state transition/change  (as those
% are mostly automatic).
% }}-
re_start_inactivity_timer(State) -> % {{-
% TODO PROD test this (with corresponding handle clause); amended after the re-write, but no clue if it will blow up or not
    timer:cancel( get(inactivity_warning) ),
    timer:cancel( get(inactivity_hangup ) ),

    Timeouts = % in minutes
        case State of
            % TODO Should this be a varible depending on article length?  there can be long articles, but after e.g., 3 hours of no activity they may be asleep (nudge: "if you are still there, please press 0 (that will pause and they can always start from the same place)
            article ->
                #{ warning => 90
                 , timeout => 100
                 };
            _ ->
                #{ warning => 10
                 , timeout => 12
                 }
        end,

    WarningTimeout    = maps:get(warning, Timeouts) * 60 * 1000,
    InactivityTimeout = maps:get(timeout, Timeouts) * 60 * 1000,

    {ok, IWref} = timer:send_after(WarningTimeout,    inactivity_warning),
    {ok, ITref} = timer:send_after(InactivityTimeout, inactivity_hangup),

    put(inactivity_warning, IWref),
    put(inactivity_hangup,  ITref).

% }}-

% TODO Try out `mod_vlc` to play aac and m4a files. {{-
% sendmsg(UUID, execute, ["playback", "/home/toraritte/clones/main.mp3"]),
% sendmsg_locked(UUID, execute, ["playback", "/home/toraritte/clones/phone-service/ro.mp3"]),
% }}-

% TODO PROD users, docs, cloud {{-

% 1. Update   deployment   docs   (and   automate):   the
%    "recordings"  folder   needs  to  be   created,  and
%    `chown`ed to user  and group `freeswitch`, otherwise
%    the  gen_server  will  crash  on  the  first  record
%    attempt.

% 2.  Upload recordings to cloud storage. (both volunteer recordings and messages left in main menu.
% }}-

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
