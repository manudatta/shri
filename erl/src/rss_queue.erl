-module(rss_queue).
-include("../data/logging.hrl").
-compile(export_all).
-define(TIMEOUT,10000).
-include_lib("xmerl/include/xmerl.hrl").
% The server implements the gen_server behavior.
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% Additional helper functions exported by the callback module.
-export([start/1]).

-record(rssQ,{queue,subscribers}).

start(Name) -> 
  gen_server:start({local, Name}, ?MODULE, [], []).
start(Name,Url)->
  gen_server:start({local, Name}, ?MODULE, [Url], []).

init([]) ->
  process_flag(trap_exit,true) 
  ,{ok, #rssQ{queue=[],subscribers=sets:new()} };
init([Url]) -> 
  State = #rssQ{queue=[],subscribers=sets:new()} 
  ,process_flag(trap_exit,true) 
  ,rss_reader:start(Url,self())
  ,{ok, State }.

handle_call(_Request={subscribe,QPid}, _From, State=#rssQ{queue=Q,subscribers=Subs}) ->
  {Reply,NewState} = case sets:is_element(QPid,Subs) of
    true -> {{error,already_subscribed},State};
    false ->  erlang:monitor(process,QPid)
      ,?INFO("New subscriber ~p to ~p~n",[QPid,self()]) 
      ,[add_item(QPid,Item) || Item <- Q]
      ,{ok,State#rssQ{subscribers=sets:add_element(QPid,Subs)}}
   end
  ,{reply,Reply, NewState};
handle_call(_Request={get_all}, _From, State=#rssQ{queue=Q}) -> 
  {reply,Q,State};
handle_call(_Request, _From, State) -> 
  {reply,{error,{unknown_request,_Request}}, State}.

% Adds item to internal queue, if it is new or updated. 
% Also, if item is new or updated, forwards item to all subscribers.

handle_cast(_Msg={add_item,RSSItem=#xmlElement{name=item}}, State=#rssQ{queue=Q,subscribers=Subs}) -> 
  NewQ = add_item_to_q(RSSItem,Q,Subs) 
  ,{noreply,State#rssQ{queue=NewQ}};

handle_cast(_Msg={unsubscribe,QPid}, State=#rssQ{subscribers=Subs}) -> 
  {noreply,State#rssQ{subscribers=sets:del_element(QPid,Subs)}};

handle_cast(_Msg, State) -> 
  ?WARN("Unknown msg {~p} to Q{~p}",[_Msg,State]) 
  ,{noreply, State}.

handle_info(_Info={'DOWN',_,_,QPid,_Reason},State=#rssQ{subscribers=Subs})->
  {noreply, State#rssQ{subscribers=sets:del_element(QPid,Subs)}};
handle_info(_Info={'EXIT',FromPid,_Reason},State)->
  ?ERROR("RSS Reader ~p died for ~p with reason ~n",[FromPid,self(),_Reason]) 
  ,{noreply, State};
handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% helper function
%
subscribe(From,To)->
  gen_server:call(To,{subscribe,From}).
add_item_to_q(NewItem,L1,[],Subs)->
  ?INFO("New item ~p ~n",[self()])
  ,broadcast(NewItem,Subs)
  ,L1++[NewItem];
add_item_to_q(NewItem,L1,L=[OldItem|Rest],Subs)->
  case rss_parse:compare_feed_items(OldItem,NewItem) of
    same -> 
      L1++L ;
    updated -> 
      ?INFO("Updated item ~p ~n",[self()])
      ,broadcast(NewItem,Subs)
      ,L1++Rest++[NewItem] ;
    different -> 
      add_item_to_q(NewItem,L1++[OldItem],Rest,Subs)
  end.
add_item_to_q(NewItem,Q,Subs)->
  add_item_to_q(NewItem,[],Q,Subs).
   
add_item(QPid,Item)->
  ok = gen_server:cast(QPid , {add_item,Item} )
  ,ok.

add_feed(QPid,RSS2Feed)->
 Items=rss_parse:get_feed_items(RSS2Feed)
 ,[add_item(QPid,Item) || Item <- Items]
 ,?INFO("Added N=~p items from the feed to ~p ~n",[length(Items),QPid]) 
 , ok. 

get_all(QPid)->
  gen_server:call(QPid,{get_all}).
broadcast(Item,PidSet)->
  [ add_item(Pid,Item) || Pid <- sets:to_list(PidSet) ]. 
