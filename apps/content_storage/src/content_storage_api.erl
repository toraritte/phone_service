-module(content_storage_api).

-export([]).

% This should only return articles, so maybe list()?
% NOTE
% All queries are article  queries, and these could be
% from the same publication  or by a certain criteria,
% such as  having the same tag(s).  (Requesting a list
% of  publications would  make their  order undefined,
% and  that is  something  listeners rely  on, but  it
% could not be guaranteed among restarts.)
% Result = [ Article ]
% Article = #{ title => String
%            , path  => Path
%            , publication => String
%            , issue => String (?)
%            , ...
%            }
% Path = URL | FilesystemPath

% As established  in the  NOTE, the query  results are
% always  articles,  hence  they  have  an  associated
% publication  (or should.  By an  arbitrary decision,
% saving the  publication of the first  one, that will
% be  used  as the  title  of  the content  item  that
% has  the query  - unless  it provides  its own.  See
% `NewContentItem` below.
% ( If the query returns a collection of articles from
% different publications, it is obviously more prudent
% to  provide  the  title  in the  content  item  when
% drawing up the publication guide. )
%
% TODO return should be `list( article() )`
%      (where to define article()?)
-callback query( map() ) -> list( map() ).
%                                        URL/PATH
-callback add_recording( map() ) -> {ok, term()} | {error, term()}
% -callback dump() -> list( map() ).


% -callback create(service_discovery:service()) -> binary() | {error, term()}.
% -callback read(unicode:unicode_binary()) -> service_discovery:service() | {error, term()}.
% -callback read_endpoints(unicode:unicode_binary()) -> [service_discovery:endpoint()] | {error, term()}.
% -callback add_named_ports(unicode:unicode_binary(), service_discovery:named_ports()) -> ok | {error, term()}.
% -callback list() -> [service_discovery:service()] | {error, term()}.
% -callback register(service_discovery:name(), service_discovery:endpoint()) -> ok | {error, term()}.

