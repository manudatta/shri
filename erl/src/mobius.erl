-module(mobius).
-compile(export_all).
%% - is_prime
is_prime_r(N,Curr) when
  N >= Curr*Curr ->
   case N rem Curr of 
    0 -> false ;
    _ -> is_prime_r(N,Curr+1)
   end;
is_prime_r(_N,_Curr) ->
  true.
is_prime(1) ->
  true;
is_prime(2) ->
  true;
is_prime(N) ->
  case N of
    3 -> true ;
    _ ->  is_prime_r(N,2)
  end.
%% - prime_factors 
prime_factors_a(N,Curr,SoFar)
  when N > Curr ->
    case N rem Curr of
      0 -> case is_prime(Curr) of
            true -> prime_factors_a(N,Curr+1,[Curr|SoFar]);
            _ -> prime_factors_a(N,Curr+1,SoFar)
           end;
      _ -> prime_factors_a(N,Curr+1,SoFar)
   end;
prime_factors_a(_N,_Curr,SoFar)->
  SoFar.
prime_factors(N)->
  prime_factors_a(N,1,[]).
%% - is square multiple
is_square_multiple_a(_,[1]) ->
  false;
is_square_multiple_a(N,[H|T]) ->
  case N rem (H*H) of
    0 -> true ;
    _ -> is_square_multiple_a(N,T) 
   end;
is_square_multiple_a(_,_) ->
  false.
is_square_multiple(N)->
  is_square_multiple_a(N,prime_factors(N)).
%% find_square_multiples
find_square_multiples_a(_Count,_Count,_Rng,Last)->
  Last;
find_square_multiples_a(_,_,[],_)->
  fail;
find_square_multiples_a(Count,Found,[H|T],Last)->
  case is_square_multiple(H) of
    true -> case Found of 
      0 -> find_square_multiples_a(Count,Found+1,T,H);
      _ -> find_square_multiples_a(Count,Found+1,T,Last)
      end;
    false -> find_square_multiples_a(Count,0,T,H)
  end.
find_square_multiples(Count,MaxN)->
  Rng = lists:seq(2,MaxN)
  ,find_square_multiples_a(Count,0,Rng,2).
