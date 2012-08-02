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

% matrix multiplication on process ring
print_results(Matrix)->
  [io:format("~p~n",[array:to_list(X)]) || X <- array:to_list(Matrix)]. 
loopmatmultacc(Result)->
  receive 
    {I,J,Val} = _Msg -> 
      io:format("Got update => ~p~n",[_Msg])
      ,_Result = array:set(I-1,array:set(J-1,Val,array:get(I-1,Result)),Result) 
      ,print_results(_Result)
      ,loopmatmultacc(_Result);
    quit ->
      print_results(Result)
  end.

-record(mat_mult_state,{next_proc=not_started_yet,row_num,row_data,col_num,col_data,curr_col_num,acc_ref,col_count}).
loopmatmulitply(_State=#mat_mult_state{next_proc=NextProc,row_num=RowNum,row_data=Row,col_num=MyColNum,col_data=MyCol,acc_ref=AccRef,col_count=ColCount})->
  receive
    {col,Col,ColNum}=_Msg ->
      io:format("~p Got col ~p MyRow=~p~n",[self(),_Msg,Row])
      ,Val = lists:foldl( fun({X,Y},Acc) -> Acc + X*Y end , 0 , lists:zip(Row,Col) )
      ,AccRef ! {RowNum,ColNum,Val}
      ,NextProc ! {col,Col,ColNum}
      , case ColCount > 1 of
        true -> loopmatmulitply(_State#mat_mult_state{col_count=ColCount-1});
        _ -> ok % My work is done!
        end;
    {set_next_proc,Next} when is_pid(Next) ->
      loopmatmulitply(_State#mat_mult_state{next_proc=Next});
    start ->
      Val = lists:foldl( fun({X,Y},Acc) -> Acc + X*Y end , 0 , lists:zip(Row,MyCol) )
      ,io:format("~p sending to acc val ~p~n",[self(),{RowNum,MyColNum,Val}])
      ,AccRef ! _Msg={RowNum,MyColNum,Val}
      ,NextProc ! {col,MyCol,MyColNum} 
      ,loopmatmulitply(_State#mat_mult_state{next_proc=NextProc}) ;
    _Msg ->
      io:format("~p quitting on~p~n",[self(),_Msg])
  end.
make_matrix(L)->
  array:from_list( [ array:from_list(X) || X <- L] ).
mat1()->
  [[1,2,3],[4,5,6]].
mat2()->
  [[7,8],[9,10],[11,12]].
transpose([H|_]=L)->
  Len = length(H)
  ,[[lists:nth(N,X) || X <- L ] || N <- lists:seq(1,Len)].
zeros(Row,Col)->
  [ lists:foldl( fun(_,A) -> [0|A] end , [] , lists:seq(1,Col)) || _X <- lists:seq(1,Row)].

test_mat()->
  M1 = mat1()
  ,M2 = mat2()
  ,Arr = make_matrix(zeros(length(M1),length(lists:nth(1,M2)))) 
  ,AccRef = spawn(?MODULE,loopmatmultacc,[Arr])
  ,ColCount= length(M1) - 1
  ,L=[#mat_mult_state{row_data=Row,row_num=Count,col_num=Count,col_data=Col,acc_ref=AccRef,col_count=ColCount} || {Count,Row,Col} <- lists:zip3(lists:seq(1,length(M1)),M1,transpose(M2))]
  ,[H|R] = Procs = [ spawn(?MODULE,loopmatmulitply,[X]) || X <- L ] 
  ,lists:foldl(fun(Proc,LastProc) -> LastProc ! {set_next_proc,Proc},Proc end, H,R )  
  ,lists:last(R) ! {set_next_proc,H}
  ,[P ! start || P <- Procs]
  ,{AccRef,Procs}.
