
% gen_server
% http://20bits.com/articles/erlang-a-generic-server-tutorial/

% use crypto for cookie.
% http://d.hatena.ne.jp/perezvon/20110111/1294761154

%
%my $usable;
%
%sub _find_digest () {
%    unless ($usable) {
%        foreach my $alg (qw/SHA-1 SHA-256 MD5/) {
%            if ( eval { Digest->new($alg) } ) {
%                $usable = $alg;
%                last;
%            }
%        }
%        Catalyst::Exception->throw(
%                "Could not find a suitable Digest module. Please install "
%              . "Digest::SHA1, Digest::SHA, or Digest::MD5" )
%          unless $usable;
%    }
%
%    return Digest->new($usable);
%}


-module(webmachine_session).
-author('Youichi Fujimoto <yofujimo@gmail.com>').
-behaviour(gen_server).

-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3,
         initialize/2,
         finalize/3,
         session_init/2,
         session_update/2,
         create_session_id/0]).

-export([start/0, put/2, get/1, clear/1]).

initialize(ReqData, State)->
    session_init(ReqData, State).

% -> {Response}
session_init(ReqData, State)->
    case wrq:get_cookie_value("session", ReqData) of % TODO: sid
        undefined ->
            SessionId = create_session_id(),
            SessionCookie = mochiweb_cookies:cookie("session", SessionId, [{path, "/"}, {httponly, ""}]),
            Map = dict:new(),
            webmachine_session:put(list_to_atom(SessionId), Map),
            {SessionId, SessionCookie, Map};
        SessionId ->
            {Result, Map} = webmachine_session:get(list_to_atom(SessionId)),
            %io:format("Map: ~s~n", [Map]),
            SessionCookie = mochiweb_cookies:cookie("session", SessionId, [{path, "/"}, {httponly, ""}]),
            case Result of
                value ->
                    {SessionId, SessionCookie, Map};
                _ ->
                    NewMap = dict:new(),
                    webmachine_session:put(list_to_atom(SessionId), NewMap),
                    {SessionId, SessionCookie, NewMap}
            end
    end.

finalize(SessionId, List, Response)->
    session_update(SessionId, List),
    Response.

session_update(SessionId, List)->
    {Result, Map} = webmachine_session:get(list_to_atom(SessionId)),
    case Result of
        value ->
%            erlang:display(Map),
%            erlang:display(dict:from_list(List)),
            mochikit_session:put(
                list_to_atom(SessionId),
                dict:merge(fun(_, _, V) -> V end, Map, dict:from_list(List)))
    end.

create_session_id() ->
    generate_session_id().

generate_session_id() ->
    %    return join( "", ++$counter, time, rand, $$, {}, overload::StrVal($c), );
    {Megasecs, Secs, _} = now(),
    Time = integer_to_list(Megasecs * 1000000 + Secs),
    Rand = binary_to_list(crypto:rand_bytes(16)),
    Pid = pid_to_list(self()),
    <<Mac:160/integer>> = crypto:sha(lists:concat([Time, Rand, Pid])),
    lists:flatten(io_lib:format("~40.16.0b", [Mac])).
    %io:format("<<~s>>~n", [[io_lib:format("~2.16B",[X]) || <<X:8>> <= Mac ]]).


% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
put(Key, Value) -> gen_server:call(?MODULE, {put, Key, Value}). 
get(Key) -> gen_server:call(?MODULE, {get, Key}).
clear(Key) -> gen_server:call(?MODULE, {clear, Key}).

% This is called when a connection is made to the server
init([]) ->
    Dictionary = dict:new(),
    {ok, Dictionary}.

% handle_call is invoked in response to gen_server:call
handle_call({put, Key, Value}, _From, Dictionary) ->
    Response = case dict:is_key(Key, Dictionary) of
        true ->
            NewDictionary = dict:store(Key, Value, Dictionary);
        false ->
            NewDictionary = dict:store(Key, Value, Dictionary),
            ok
    end,
    {reply, Response, NewDictionary};

handle_call({get, Key}, _From, Dictionary) ->
    Response = case dict:is_key(Key, Dictionary) of
        true ->
            {value, dict:fetch(Key, Dictionary)};
        false ->
            {empty_value, Key}
    end,
    {reply, Response, Dictionary};

handle_call({clear, Key}, _From, Dictionary) ->
    NewDictionary = dict:erase(Key, Dictionary),
    {reply, ok, NewDictionary};

handle_call(_Message, _From, Dictionary) ->
    {reply, error, Dictionary}.

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Dictionary) -> {noreply, Dictionary}.
handle_info(_Message, Dictionary) -> {noreply, Dictionary}.
terminate(_Reason, _Dictionary) -> ok.
code_change(_OldVersion, Dictionary, _Extra) -> {ok, Dictionary}.