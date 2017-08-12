%%%-------------------------------------------------------------------
%%% @doc
%%% Module for simple input that allows usage of the triangles module.
%%% @end
%%%-------------------------------------------------------------------
-module(simple_input).

-define(PROMPT, "Please enter the lengths of the three edges as floating point numbers
                 separated by spaces (e.g. \"3.0 4.0 5.0\"). Type q to quit.\nInput: ").

-export([start/0]).

start() ->
  Input = io:get_line(?PROMPT),
  case string:tokens(string:trim(Input), " ") of
    ["q"] -> init:stop();
    [A, B, C] ->
      try
        TriangleType = process_edges(list_to_float(A), list_to_float(B), list_to_float(C)),
        io:format("Your triangle is ~p~n", [TriangleType]),
        start()
      catch
        error:Type when Type =:= badarg;
                        Type =:= function_clause ->
          io:format("Invalid input!~n"),
          start()
      end;
    _ ->
      io:format("Invalid input!~n"),
      start()
  end.

process_edges(Edge1, Edge2, Edge3) ->
  triangle:edge_type(triangle:new(Edge1, Edge2, Edge3)).




