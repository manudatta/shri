-module(numerl_tests).
-include_lib("eunit/include/eunit.hrl").
horner_test()->
 Poly = [{3,3},{2,2},{-5,1},{7,0}] % 3x^2+2x^2-5x+7 = f(x)
 , ?assertEqual(91,numerl:horner(Poly,3)).
solve_half_test()->
 L4=[{1,4},{2,3},{-1,1},{-1,0}] 
 ,F = fun(X) -> numerl:horner(L4,X) end
 ,Err = 0.000001
 ,?assert( F( numerl:solve(F,{0,1},halfing,Err) ) < Err ).
  
mat1()->
 [[3,2,8,1],[1,-4,0,3]].
mat2()-> 
  [[2,-1],[1,-3],[0,1],[3,1]].
mat3()->
  [[1,2,0],[0,3,1],[0,1,2]].
mat4()->
  [[1,2,3],[-2,-4,-5],[3,5,6]].
mat5()->
  [[1,3,2],[-3,-3,-1],[2,1,0]].
det1_test()->
  ?assertEqual(1,numerl:det(mat4())).
det2_test()->
  ?assertEqual(1,numerl:det([[1]])).
inv1_test()->
  ?assert(mat5()==numerl:inv(mat4())).
