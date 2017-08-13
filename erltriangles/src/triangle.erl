%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains API for creation and manipulation of triangles.
%%% @end
%%%-------------------------------------------------------------------
-module(triangle).

-type triangle_edge() :: float().
%% The edge of a triangle, that is a positive floating point number.
-type triangle() :: {triangle_edge(), triangle_edge(), triangle_edge()}.
%% The triangle, that is a set of three edges.
-type edge_type() :: equilateral | isosceles | scalene.
%% The type of the triangle, based on its edges.

%% API
-export([new/3, edge_type/1]).

%% @doc Create a new triangle with given valid triangle edges.
-spec new(triangle_edge(), triangle_edge(), triangle_edge()) -> triangle().
new(Edge1, Edge2, Edge3) when (Edge1 + Edge2) > Edge3,
                              (Edge2 + Edge3) > Edge1,
                              (Edge1 + Edge3) > Edge2,
                              is_float(Edge1),
                              is_float(Edge2),
                              is_float(Edge3) ->
  {Edge1, Edge2, Edge3}.

%% @doc Returns the edge type of the triangle based, given a valid triangle as an input.
-spec edge_type(triangle()) -> edge_type().
edge_type({Edge, Edge, Edge})         -> equilateral;
edge_type({Edge, Edge, _AnotherEdge}) -> isosceles;
edge_type({Edge, _AnotherEdge, Edge}) -> isosceles;
edge_type({_AnotherEdge, Edge, Edge}) -> isosceles;
edge_type(_)                          -> scalene.


