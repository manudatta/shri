-module(rss_parse).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").
is_rss2_feed(Doc)->
 VerXPathStr = "@version"
 ,case Doc#xmlElement.name of
  rss -> case (lists:nth(1,xmerl_xpath:string(VerXPathStr,Doc)))#xmlAttribute.value of
    "2.0" -> ok ;
    _ -> false
    end ; 
  _ -> false
 end.
 get_feed_items(RSS2Feed)->
  ItemsXPathStr="//item"
  ,xmerl_xpath:string(ItemsXPathStr,RSS2Feed). 
get_item_time(Item)->
 PubDateXPathStr = "//pubDate"
 ,[PubDateNode|_]=xmerl_xpath:string(PubDateXPathStr,Item)
 ,[PubDate|_] = PubDateNode#xmlElement.content
 ,DateTime=httpd_util:convert_request_date(PubDate#xmlText.value)
 ,calendar:datetime_to_gregorian_seconds(DateTime).
is_same_subelement(OldItem,NewItem,XPathStr)->
  OldGUIDL = xmerl_xpath:string(XPathStr,OldItem)
  ,case OldGUIDL of
    [] -> false;
    _ -> NewGUIDL = xmerl_xpath:string(XPathStr,NewItem)
        ,case NewGUIDL of
          [] -> false ;
          _ -> [OldGUID] = OldGUIDL
            ,[NewGUID] = NewGUIDL
            ,OldGUID == NewGUID
         end
   end.
compare_feed_items(OldItem,NewItem)->
  case OldItem =:= NewItem of
    true -> same ;
    _ -> case lists:foldl( fun(E,Acc) -> Acc or is_same_subelement(OldItem,NewItem,E) end,false,["//guid","//title","//link"])  of
      true -> updated ;
      _ -> different
    end
  end.
