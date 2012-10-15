-module(rss_reader).
-include("../data/logging.hrl").
-compile(export_all).
-define(RETRIEVE_INTERVAL,1800060).
start(Url,QPid)->
  inets:start()
  ,spawn(?MODULE,server,[Url,QPid]).
server(Url,QPid)->
  {ok,{Status={_,Code,_},_,Load}}=httpc:request(Url)
  ,case Code of 
    200 ->
       {Feed,_} = xmerl_scan:string(Load)
       ,case rss_parse:is_rss2_feed(Feed) of
        ok -> 
          rss_queue:add_feed(QPid,Feed)
          , receive 
            after ?RETRIEVE_INTERVAL -> 
              server(Url,QPid)
            end;
        _ -> erlang:exit(not_rss2_feed)
        end ; 
    _ -> erlang:exit(Code)
   end.
