%
% Author: Manu Datta (manu.datta@gmail.com)
%
-module(numerl).
-include_lib("eunit/include/eunit.hrl").
-include("numerl.hrl").
-compile(export_all).
%
% determinant code
%
remove_nth_row(N,Mat)->
  {A,[_Head|Rest]} = lists:split(N,Mat)
  , lists:append(A,Rest).
remove_nth_col(N,Mat)->
   F = fun(X,{NN,Acc}) ->
    {A,[_Head|Rest]} = lists:split(NN,X)
    ,{N,[lists:append(A,Rest)|Acc]}
    end
  ,{_,Val}=lists:foldl(F,{N,[]},lists:reverse(Mat))
  ,Val.
minor(I,J,Mat)->
  remove_nth_col(J,remove_nth_row(I,Mat)).
det([[Elem]])->
  Elem;
det(Mat=[Row|_Rows])->
  F = fun({Val,Minor},{Pos,Acc}) ->
        _Next={_NewPos,_NewAcc}=case Pos of
          even -> {odd,Val*det(Minor)+Acc};
          odd -> {even,-1*Val*det(Minor)+Acc}
        end
       end
  ,ColNum = length(Row)
  ,Minors = [minor(0,J-1,Mat) || J <- lists:seq(1,ColNum)]
  ,{_,Val} = lists:foldl(F,{even,0},lists:zip(Row,Minors))
  ,Val.
  
% matrix multiplication on process ring
print_array_matrix(Matrix)->
  [io:format("~w~n",[array:to_list(X)]) || X <- array:to_list(Matrix)]. 
% gather all the 
gather_mat_multiply(Parent,Result,_WorkerCount=0)->
  Parent!{done,Result};
gather_mat_multiply(Parent,Result,WorkerCount)->
  receive 
    {'EXIT',From,Reason} -> exit({worker_error,From,Reason});
    {I,J,Val} = _Msg -> 
      _Result = array:set(I-1,array:set(J-1,Val,array:get(I-1,Result)),Result)
      %, io:format("Gathering -> ~p~n",[_Msg])
      ,gather_mat_multiply(Parent,_Result,WorkerCount);
    done ->
      gather_mat_multiply(Parent,Result,WorkerCount-1)
  end.

loop_matrix_multiply(_State=#mat_mult_state{acc_ref=AccRef,col_count=0})->
  AccRef ! done;

loop_matrix_multiply(_State=#mat_mult_state{next_proc=NextProc,row_num=RowNum,row_data=RowData,acc_ref=AccRef,col_count=ColCount})->
  receive
    {col,ColNum,ColData}=_Msg ->
      %io:format("~p got ~p~n",[self(),_Msg]),
      Val = lists:foldl( fun({X,Y},Acc) -> Acc + X*Y end , 0 , lists:zip(RowData,ColData) )
      ,AccRef ! {RowNum,ColNum,Val}
      %,io:format("~p -> ~p ~p~n",[self(),NextProc,_Msg])
      ,NextProc ! {col,ColNum,ColData}
      ,loop_matrix_multiply(_State#mat_mult_state{col_count=ColCount-1});
    {set_next_proc,Next} when is_pid(Next) ->
      %io:format("~p got ~p~n",[self(),Next]) ,
      link(Next)
      ,link(AccRef)
      ,loop_matrix_multiply(_State#mat_mult_state{next_proc=Next})
  end.


send_cols([],_ColNum,_LeftWorker,_OrigWorkers) ->
  ok;
send_cols([Col|ColData],ColNum,Workers,OrigWorkers) ->
  [H|R] = case Workers of
    [] -> OrigWorkers;
    _ -> Workers
  end
  ,H ! {col,ColNum,Col}
  ,send_cols(ColData,ColNum+1,R,OrigWorkers).

make_matrix(L)->
  array:from_list( [ array:from_list(X) || X <- L] ).
make_row_major(A)->
  [array:to_list(X) || X <- array:to_list(A)].
matrix_multiply(A,B)->
  Arr = make_matrix(zeros(length(A),length(lists:nth(1,B)))) 
  ,T=transpose(B)
  ,ColCount= length(T) 
  ,RowCount= length(A) 
  ,AccRef = spawn(?MODULE,gather_mat_multiply,[self(),Arr,RowCount])
  ,link(AccRef)
  ,WorkerState=[#mat_mult_state{row_data=RowData,row_num=RowNum,acc_ref=AccRef,col_count=ColCount} || {RowNum,RowData} <- lists:zip(lists:seq(1,RowCount),A)]
  ,[Head|Rest] = Workers = [ spawn(?MODULE,loop_matrix_multiply,[State]) || State <- WorkerState ]
  ,case Rest of
    [] -> Head ! {set_next_proc,Head};
    _ -> lists:last(Rest) ! {set_next_proc,Head}
        ,lists:foldl(fun(Proc,LastProc) -> LastProc ! {set_next_proc,Proc},Proc end,Head,Rest)
  end
  ,send_cols(T,1,Workers,Workers)
  , receive 
      {done,Result} -> make_row_major(Result);
      {'EXIT',From,Reason} -> {error,worker,From,Reason}
    after 600000 ->
      {error,timeout}
  end.

% some helper functions

rand_mat(M,N)->
  random:seed(now()) 
  ,[ lists:foldl( fun(_,A) -> [random:uniform()|A] end , [] , lists:seq(1,N)) || _X <- lists:seq(1,M)].
mat1()->
  [[1,2,3],[4,5,6]].
mat2()->
  [[7,8],[9,10],[11,12]].
transpose([H|_]=L)->
  Len = length(H)
  ,[[lists:nth(N,X) || X <- L ] || N <- lists:seq(1,Len)].
zeros(Row,Col)->
  [ lists:foldl( fun(_,A) -> [0|A] end , [] , lists:seq(1,Col)) || _X <- lists:seq(1,Row)].
