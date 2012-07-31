-module(proc_sieve).
-compile(export_all).
sieve(N,ReqPid,gather) ->
  receive 
    X -> ReqPid ! [N|X]
  end.
sieve(N,nil)->
  receive
    {done,ReqPid} -> ReqPid ! [N] ;
    Num when is_number(Num) -> NextPid = case Num rem N of
      0 -> nil ; 
      _ -> Pid = spawn(proc_sieve,sieve,[])
          , Pid ! Num
          , Pid
      end,
      sieve(N,NextPid)
  end;
sieve(N,NextPid)->
  receive
    {done,ReqPid} -> NextPid ! {done,self()} 
      ,sieve(N,ReqPid,gather)  ;
    Num -> case Num rem N of
      0 -> nil ; 
      _ -> NextPid ! Num 
      end,
     sieve(N,NextPid)
    end.
sieve()->
  receive
    _N -> sieve(_N,nil)
  end.
generate(MaxN)->
  Pid = spawn(?MODULE,sieve,[])
  ,[ Pid ! X || X <- lists:seq(2,MaxN) ]
  , Pid ! {done,self()}
  , receive 
      L -> io:format("~p~n",[L])
  end.
