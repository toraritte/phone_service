-module(fs).

-export(
    [ sendmsg/3
    , sendmsg_locked/3
    , fsend/1
    , uuid_fileman/2
    ]).

-define(FS_NODE, 'freeswitch@tr2').

%%%%%%%%%%%%%%%%%%%%%%%%
%% FreeSWITCH helpers %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% `sendmsg/1` and `sendmsg_locked/1` {{-

%% Only implemented for `execute` and `hangup` for now;
%% even these  are lacking a thorough  documentation in
%% the official FreeSWITCH Confluence.
%%
%% See available `sendmsg` commands at
%% https://freeswitch.org/confluence/display/FREESWITCH/mod_event_socket#mod_event_socket-3.9.1Commands

sendmsg(CallUUID, Cmd, Args) ->

    SendmsgHeaders =
           [{"call-command", atom_to_list(Cmd)}]
        ++ to_sendmsg_headers(Cmd, Args),

    do_sendmsg(SendmsgHeaders, CallUUID).

sendmsg_locked(CallUUID, Cmd, Args) ->

    SendmsgHeaders =
         [{"call-command", atom_to_list(Cmd)}]
      ++ to_sendmsg_headers(Cmd, Args)
      ++ [{"event-lock", "true"}],

    do_sendmsg(SendmsgHeaders, CallUUID).

%% Internals {{-
do_sendmsg(SendmsgHeaders, CallUUID) ->

    PoorMansUUID =
        erlang:make_ref(),
    ApplicationUUID =
        futil:stringify(PoorMansUUID),
    EventUUIDHeaderList =
        [{"Event-UUID", ApplicationUUID}],

%% NOTE on "Event-UUID" {{-
%%
%% > When  an  application  is  executed  via  `sendmsg`,
%% > CHANNEL_EXECUTE and  CHANNEL_EXECUTE_COMPLETE events
%% > are  going to  be generated.  If you  would like  to
%% > correlate these two events  then add an `Event-UUID`
%% > header with  your custom UUID. In  the corresponding
%% > events, the  UUID will be in  the `Application-UUID`
%% > header.  If  you  do not  specify  an  `Event-UUID`,
%% > Freeswitch  will automatically  generate a  UUID for
%% > the `Application-UUID`.
%%
%% https://freeswitch.org/confluence/display/FREESWITCH/mod_event_socket
%% }}-

    FinalHeaders =
        % ++ to_sendmsg_headers(SendmsgCommand, SendmsgArgs)
        % ++
           SendmsgHeaders
        ++ EventUUIDHeaderList,
        % ++ LockHeaderList,

    fsend({sendmsg, CallUUID, FinalHeaders}),

    ApplicationUUID.

% Atom -> [String] -> [Tuple]
to_sendmsg_headers(execute, [App, Args]) when is_list(Args) ->
    %% TODO "loops"  header   and  alternate  format   for  long
    %%      messages (is it needed here?)  not added as they are
    %%      not needed yet.
    [ {"execute-app-name", App}
    , {"execute-app-arg", Args}
    ];

% Atom -> [String] -> [Tuple]
to_sendmsg_headers(hangup, [HangupCode, _ApplicationUUID]) ->
    %% For hangup codes, see
    %% https://freeswitch.org/confluence/display/FREESWITCH/Hangup+Cause+Code+Table
    [{"hangup-cause", HangupCode}];

%% This will  blow up,  one way or  the other,  but not
%% planning to get there anyway.
to_sendmsg_headers(_SendmsgCommand, _Args) ->
    [].
%%   }}-
%% }}-

fsend(Msg) ->
    %% Why the `lofa` atom:
    %% https://stackoverflow.com/questions/58981920/
    {lofa, ?FS_NODE} ! Msg.

uuid_fileman(CallUUID, Command) ->
    fs:fsend
      ({ api
       , uuid_fileman
       , futil:stitch([CallUUID, Command])
       }).

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
