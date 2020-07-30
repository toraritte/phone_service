-module(user_db).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Requires `phone_numbers` file to be present, holding %%
%% the registered users' phone numbers!                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export(
   [ start_link/0

   % gen_server callbacks
   , init/1
   , handle_call/3
   , handle_cast/2
   , terminate/2

   % public API
   , look_up/1
   , reload/0

   % private functions
   % , load_phone_numbers/0
   ]).

-define(USER_FILE, "/home/toraritte/clones/phone-service/phone_numbers").

start_link() ->
    % {ok, Pid} =
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
% ,   Pid.

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init(_Args) ->
    %% Set up logging.
    filog:add_singleton_handler(?MODULE),
    filog:singleton_handler_filter(?MODULE),
    %% Init DB
    PhoneNumberSet = load_phone_numbers(),
    {ok, PhoneNumberSet}.

handle_call({look_up, PhoneNumber}, {Pid, _Ref}, PhoneNumberSet) ->
    IsRegistered =
        sets:is_element(
            list_to_binary(PhoneNumber),
            PhoneNumberSet
         ),
    log(debug, [lookup, Pid, PhoneNumber, IsRegistered]),
    {reply, IsRegistered, PhoneNumberSet}.

handle_cast(reload_db = Request, _PhoneNumberSet) ->
    log(debug, [reload_db, Request]),
    NewPhoneNumberSet = load_phone_numbers(),
    {noreply, NewPhoneNumberSet}.

terminate(Reason, _PhoneNumberSet) ->
    log(debug, [terminate_making_sure, Reason]),
    filog:remove_singleton_handler(?MODULE).

% === PUBLIC API ===================================== {{-

% TODO re-evaluate these comments when there will be a central DB
%% The async  version of  `is_user_registered/1`. Probably {{- {{-
%% better  suited  for  when the  phone  number  lookup
%% will  try to  reach  a remote  database. Handled  in
%% `handle_cast/2` above.
%%
%% CAVEAT: Probably  introduces a race  condition: User
%% calls  service, assumes  they are  registered, start
%% dialing single/double  digits for menus, and  if the
%% DB lookup is slow enough, it could be that the timer
%% (for DTMF  digits) expires, and  its `handle_info/2`
%% callback  fires   before  the  lookup   returns  (in
%% `handle_cast/2`)  that  a  user is  registered.  The
%% former  would  emit  a  warning  that  user  is  not
%% registered,  will be  kicked out  in 5  minutes, but
%% then  the  system  silently does  register  (if  the
%% lookup is successful).
%% Either  add another  notification that  registration
%% successful, or stay silent the first time and give a
%% warning  that 1  minute  remaining  for example,  if
%% lookup not successful.
%% }}- }}-
% caller_status(EventHeaders) -> % {{-
%     CallPid = self(),
%     F = fun() ->
%             CallerStatus = register_status(EventHeaders),
%             gen_statem:cast(CallPid, CallerStatus)
%         end,
%     spawn(F).
%% }}-

look_up(PhoneNumber) ->
    gen_server:call(user_db, {look_up, PhoneNumber}).

reload() ->
    gen_server:cast(user_db, reload_db).

% TODO `add_phone_number/1`
% TODO `toggle_phone_number/1`

% }}-
%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions %%
%%%%%%%%%%%%%%%%%%%%%%%

load_phone_numbers() ->
    {ok, FileBin} = file:read_file(?USER_FILE),
    BinStrings = string:split(FileBin, "\n", all),
    Filtered = lists:filter(fun(<<"#",_/binary>>) -> false; (_) -> true end, BinStrings),
    % sets:from_list(BinStrings).
    sets:from_list(Filtered).

log(Level, ValueList) ->
    filog:log(Level, ?MODULE, ValueList).

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
