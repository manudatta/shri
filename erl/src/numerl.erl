%
% Author: Manu Datta (manu.datta@gmail.com)
% Please send any bug report to manu.datta@gmail.com
%
-module(numerl).
-include_lib("eunit/include/eunit.hrl").
-include("numerl.hrl").
-compile(export_all).
%
% horner's method -> input in form of [{Coff,N}]
%
horner([],_N,_X,Acc)->
  Acc;
horner([{Coff0,N=0}],N,X,Acc)->
  Acc*X+Coff0;
horner([{CoffN,N}|Rest],N,X,Acc)->
  horner(Rest,N-1,X,Acc*X+CoffN);
horner(P=[{_CoffN,_N}|_],M,X,Acc)->
  horner(P,M-1,X,Acc*X).
horner(Poly,X)->
  _Sorted=[{CoffN,MaxN}|Rest]=lists:sort( fun({_,N},{_,M}) -> M < N end , Poly)
  ,horner(Rest,MaxN-1,X, _Acc = CoffN ).
%
% Algebric equation solver 
%

%
% method of chords first call uses halfing
%
solve(Fun,Bounds={XMin,XMax},Scheme=chords,Eps,Calls=0) ->
  Avg = Avg=(XMin+XMax)/2
  ,FAvg = Fun(Avg)
  ,case abs(FAvg) > Eps of
      true -> NewBounds = case FAvg > 0 of 
        true -> {XMin,Avg};
        false -> {Avg,XMax}
        end
       ,solve(Fun,NewBounds,Scheme,Eps,Calls+1);
    false -> Avg
  end;
solve(Fun,Bounds={XMin,XMax},Scheme=chords,Eps,Calls)
  when Calls < 1000 ->
  FXMin = Fun(XMin)
  ,FXMax = Fun(XMax)
  ,D = XMin - (XMax-XMin)*FXMin/(FXMax-FXMin)
  ,FD = Fun(D)
  ,case abs(FD) > Eps of
    true ->
      case FD > 0.0 of
        true -> solve(Fun,_NewBounds={XMin,D},Scheme=chords,Eps,Calls);
        false -> solve(Fun,_NewBounds={D,XMax},Scheme=chords,Eps,Calls)
      end;
    false -> D 
  end;
%
% method of halfing
%
solve(Fun,Bounds={XMin,XMax},Scheme=halfing,Eps,Calls)
  when Calls < 1000 ->
  Avg = (XMin+XMax)/2
  ,Favg = Fun(Avg)
  , case abs(Favg) > Eps of
      true -> NewBounds = case Favg > 0 of 
        true -> {XMin,Avg};
        false -> {Avg,XMax}
        end
       ,solve(Fun,NewBounds,Scheme,Eps,Calls+1);
      false -> Avg
   end;
solve(_Fun,_Bounds={_XMin,_XMax},_Scheme,_Eps,_Calls) ->
  {error,max_call_count_exceed}.
solve(Fun,Bounds={XMin,XMax},Scheme,Eps)->
  solve(Fun,Bounds={XMin,XMax},Scheme,Eps,0).
solve(Fun,Bounds={XMin,XMax},Eps)->
  Scheme = halfing
  ,solve(Fun,Bounds,Scheme,Eps).
solve(Fun,Bounds={XMin,XMax})->
  Scheme = halfing
  ,Eps = 0.0000001
  ,solve(Fun,Bounds,Scheme,Eps).

%
% inverse of a row major matrix 
%
inv(Mat)->
  Det = det(Mat)
  , F = fun(L) -> [ X/Det || X <- L ] end
  ,case Det of 
    0 -> {error,non_singular};
    _ -> lists:foldl(fun(L,Acc) -> [F(L)|Acc] end , [],lists:reverse(adjoint(Mat))) 
   end.
%
% adjoint of a matrix
%
adjoint(Mat)->
  transpose(cofactors(Mat)).
% cofactors
cofactors([H|_]=Mat)->
  Rows = length(Mat)
  ,Cols = length(H)
  ,F = fun ({I,J}) -> Val = det( minor(I-1,J-1,Mat) )
    ,case (I+J-2) rem 2 of 
      0 -> Val ;
      1 -> -1*Val
     end
  end
  ,M = fun(L,Acc) -> [lists:map(F,L)|Acc] end
  ,V=lists:foldl(M,[], [[{I,J}||J<-lists:seq(1,Cols)]|| I <- lists:seq(1,Rows)])
  ,lists:reverse(V).
%
% determinant of a matrix 
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
%iszero
iszero([H|_]=Mat)->
 Row=length(Mat)
 ,Col=length(H)
 ,ZeroMat = zeros(Row,Col)
 ,Mat==ZeroMat.
%
% row echelon form
%

% return true if all elements in col are zero at Pth
is_col_all_zero(_P,[[0]],_) ->
  true;
is_col_all_zero(_P,_,0) ->
  true;
is_col_all_zero(P,_Mat=[Row|Rows],N) ->
  {_,[H|_]} = lists:split(P-1,Row)
  ,case H==0 of
    true -> is_col_all_zero(P,Rows,N-1);
    false -> false
   end.
is_col_all_zero(P,Mat) ->
  is_col_all_zero(P,Mat,length(Mat)). 
% return 1 indexed row which has leading non zero column 
first_non_zero_col(_,End,End)->
  -1;
first_non_zero_col(Mat,P,End)->
  case is_col_all_zero(P,Mat) of
    false -> P ;
    true -> first_non_zero_col(Mat,P+1,End)
  end.
first_non_zero_col([H|_]=Mat)->
  first_non_zero_col(Mat,1,length(H)+1).

first_row_with_non_zero_entry_after_i(_I,_Mat=[],_)->
  -1;
first_row_with_non_zero_entry_after_i(I,_Mat=[Row|Rows],Count)->
  {_,[H|_]} = lists:split(I-1,Row)
  , case H+0.0 =:= 0.0 of
    false -> Count;
    true -> first_row_with_non_zero_entry_after_i(I,Rows,Count+1)
  end.
first_row_with_non_zero_entry_after_i(I,_) when 0 >= I ->
  -1;
first_row_with_non_zero_entry_after_i(I,Mat)->
    first_row_with_non_zero_entry_after_i(I,Mat,1).
%exchange Ith row with first row for Matrix
exchange_first_row(1,Matrix)-> 
  Matrix;
exchange_first_row(I,Matrix)-> 
  {[First|Rest1],[Ith|Rest2]} = lists:split(I-1,Matrix)
  ,[Ith|Rest1]++[First|Rest2].
%make the leading entry 1 by op1 kind operation on matrix rows 
make_leading_one(I,J,Mat) ->
  Row = lists:nth(I,Mat)
  ,Val = lists:nth(J,Row)
  ,NewRow = [ X/Val || X <- Row]
  ,{H,[R1|Rest]}=lists:split(I-1,Mat)
  , case H of
    [] -> [NewRow|Rest];
    _ -> lists:append(H,[NewRow|Rest])
  end.
%make the leading entry 0 by op2 and op3 kind operation on matrix rows except the given Row Ref 
make_rest_zeros(P,Row,_Mat=[],Acc)->
  Acc;
make_rest_zeros(P,Row,Mat=[Row|Rows],Acc)->
  make_rest_zeros(P,Row,Rows,[Row|Acc]);
make_rest_zeros(P,Row,Mat=[First|Rows],Acc)->
  F = fun()-> 
        {_,[N|_]} = lists:split(P-1,First)
        , X = [E*N || E <- Row ]
        , [ A-B  || {A,B} <- lists:zip(First,X)]
  end
 ,make_rest_zeros(P,Row,Rows,[F()|Acc]).
% main row echelon function     
ref(Matrix,_Parition=[[]],_CurrentRow)->
  Matrix;
ref(Matrix,Partition,CurrentRow)->
  case length(Matrix) < CurrentRow of
    true -> Matrix;
    false ->
        NonZeroColNum=first_non_zero_col(Partition)
        ,RowNonZero = first_row_with_non_zero_entry_after_i(NonZeroColNum,Partition)
        ,Partition1=exchange_first_row(RowNonZero,Partition)
        ,Partition2=[First|_]=make_leading_one(1,NonZeroColNum,Partition1)
        ,{L,_}=lists:split(CurrentRow-1,Matrix)
        ,Partition3= L++Partition2
        ,NewMatrix=lists:reverse(make_rest_zeros(NonZeroColNum,lists:nth(CurrentRow,Partition3),Partition3,[]))
        ,{_,Partition4} = lists:split(CurrentRow,NewMatrix)
         %,io:format("Mat->~p~nPart0->~p~nPart1->~p~nPart2->~p~nPart3->~p~nNewMatrix~p~nNewPart->~p~n",[Matrix,Partition,Partition1,Partition2,Partition3,NewMatrix,Partition4])
        ,case Partition4 of
          [] -> NewMatrix;
          _ -> case iszero(Partition4)  of
                true -> NewMatrix;
                false ->ref(NewMatrix,Partition4,CurrentRow+1)
              end
        end            
  end.
    
ref(Matrix)->
  case iszero(Matrix) of
    true -> Matrix;
    false -> ref(Matrix,Matrix,1)
  end.
%
% determinant
%
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
%
% row major set (1 indexed) (1,1) -> points to first element 
%
row_major_set(Row,Col,Val,Array)->
  {Rows,[TopRow|Rest]} = lists:split(Row-1,Array)
  ,{Before,[_|After]} = lists:split(Col-1,TopRow)
  ,Rows++[Before++[Val|After]]++Rest.
  
% matrix multiplication on process ring
print_array_matrix(Matrix)->
  [io:format("~w~n",[array:to_list(X)]) || X <- array:to_list(Matrix)]. 
% gather all the values from process rings 
gather_mat_multiply(Parent,Result,_WorkerCount=0)->
  Parent!{done,Result};
gather_mat_multiply(Parent,Result,WorkerCount)->
  receive 
    {'EXIT',From,Reason} -> exit({worker_error,From,Reason});
    {I,J,Val} = _Msg -> 
      _Result = row_major_set(I,J,Val,Result)
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
  T=transpose(B)
  ,ColCount= length(T) 
  ,RowCount= length(A) 
  ,AccRef = spawn(?MODULE,gather_mat_multiply,[self(),A,RowCount])
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
      {done,Result} -> Result;
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
  [ lists:foldl( fun(_,A) -> [0.0|A] end , [] , lists:seq(1,Col)) || _X <- lists:seq(1,Row)].
addrow(A,B)->
  [X+Y || {X,Y} <- lists:zip(A,B)].
add(A,B)->
  [addrow(RowA,RowB) || {RowA,RowB} <- lists:zip(A,B)]. 
