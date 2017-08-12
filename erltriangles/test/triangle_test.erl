%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for the triangle module.
%%% @end
%%% Created : 12. Aug 2017 8:12 PM
%%%-------------------------------------------------------------------
-module(triangle_test).

-include_lib("eunit/include/eunit.hrl").

triangle_validity_test() ->
  ?assert(triangle:is_valid(triangle:new(1.0, 1.0, 1.0))),
  ?assert(triangle:is_valid(triangle:new(3.0, 4.0, 5.0))),
  ?assert(triangle:is_valid(triangle:new(1.4, 1.0, 1.0))),
  ?assertException(error, function_clause, triangle:new(3.0, 0.0, 3.0)),
  ?assertException(error, function_clause, triangle:new(3.0, -3.0, 7.0)),
  ?assertException(error, function_clause, triangle:new(-3.0, -4.0, -5.0)),
  ?assertException(error, function_clause, triangle:new(1.0, 1.0, 7.0)),
  ?assertException(error, function_clause, triangle:new(-3.0, -4.0, not_a_number)).

triangle_type_test() ->
  equilateral = triangle:edge_type(triangle:new(5.0, 5.0, 5.0)),
  isosceles = triangle:edge_type(triangle:new(5.0, 5.0, 3.0)),
  isosceles = triangle:edge_type(triangle:new(5.0, 3.0, 5.0)),
  isosceles = triangle:edge_type(triangle:new(3.0, 5.0, 5.0)),
  scalene = triangle:edge_type(triangle:new(3.0, 4.0, 5.0)).