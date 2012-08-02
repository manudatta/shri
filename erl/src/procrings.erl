-module(procrings).
-compile(export_all).
-record(loop0state,{next_proc=not_started_yet}).
-record(loopfibstate,{next_proc=not_started_yet}).
start(M,N,Message)->
  RawProcs = [ spawn(?MODULE,loop0,[M,#loop0state{}]) || _P <- lists:seq(1,N) ]
  ,[HeadProc|RestProcs] = RawProcs
  ,lists:foldl(fun(Proc,LastProc) -> LastProc ! {set_next_proc,Proc},Proc end, HeadProc,RestProcs )  
  ,lists:last(RestProcs) ! {set_next_proc,HeadProc}
  ,HeadProc ! Message
  ,RawProcs.

startfibring(M,N)->
  RawProcs = [ spawn(?MODULE,loopfib,[M,#loopfibstate{}]) || _P <- lists:seq(1,N) ]
  ,[HeadProc|RestProcs] = RawProcs
  ,lists:foldl(fun(Proc,LastProc) -> LastProc ! {set_next_proc,Proc},Proc end, HeadProc,RestProcs )  
  ,lists:last(RestProcs) ! {set_next_proc,HeadProc}
  ,HeadProc ! {1,1} 
  ,RawProcs.

%startmatmulitplyring(Matrix1,Matrix2)->
  
  
% simple loop which sends msg around n times

loop0(_Left=0,#loop0state{next_proc=NextProc})->
  io:format("Pid=~p quitting left Msgs=~p.~n",[self(),_Left]) 
  ,NextProc ! quit;
loop0(Left,State=#loop0state{next_proc=not_started_yet})->
  receive 
    quit ->  io:format("Pid=~p quitting.",[self()]) ;
    {set_next_proc,NextProc} when is_pid(NextProc) ->
      loop0(Left,State#loop0state{next_proc=NextProc}) ;
    _ ->
      loop0(Left,State)
  end;
loop0(Left,State=#loop0state{next_proc=NextProc})->
  receive 
    _Msg -> io:format("Pid=~p got msg=~p~n",[self(),_Msg])
      ,NextProc ! _Msg
      ,loop0(Left-1,State)
  end.

% Loop which calculates Fib(n) 

loopfib(_N=0,#loopfibstate{next_proc=NextProc})->
  NextProc ! quit;
loopfib(N,State=#loopfibstate{next_proc=not_started_yet})->
  receive 
    quit ->  io:format("Pid=~p quitting.",[self()]) ;
    {set_next_proc,NextProc} when is_pid(NextProc) ->
      loopfib(N,State#loopfibstate{next_proc=NextProc}) ;
    _ ->
      loopfib(N,State)
  end;
loopfib(N,State=#loopfibstate{next_proc=NextProc})->
  receive 
    _Msg = {FibNmin1,FibN} -> io:format("Pid=~p got msg=~p~n",[self(),_Msg])
      ,NextProc ! {FibN,FibN+FibNmin1} 
      ,loopfib(N-1,State)
  end.



