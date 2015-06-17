%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Parse transform module for `pfun'
-module(pfun_transform).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([parse_transform/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Records & Types & Macros
%%----------------------------------------------------------------------------------------------------------------------
-record(clause,
        {
          line        :: non_neg_integer(),
          args        :: [term()],
          guards = [] :: [term()],
          body        :: body()
        }).

-record(function,
        {
          line    :: non_neg_integer(),
          name    :: atom(),
          arity   :: non_neg_integer(),
          clauses :: [#clause{}]
        }).

-type form() :: #function{}
              | term().

-type body() :: [statement()].
-type statement() :: term().

-define(PFUN_CALL(Line, Function, Args), {call, Line, {remote, Line, {atom, Line, pfun}, {atom, Line, Function}}, Args}).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec parse_transform([erl_parse:abstract_form()], [compile:option()]) -> [erl_parse:abstract_form()].
parse_transform(Forms, _Options) ->
    walk_ast(Forms).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec walk_ast([form()]) -> [form()].
walk_ast(Fs) ->
    [case F of
         #function{} -> F#function{clauses = walk_clauses(F#function.clauses)};
         _           -> F
     end || F <- Fs].

-spec walk_clauses([#clause{}]) -> [#clause{}].
walk_clauses(Cs) ->
    [C#clause{body = walk_body(C#clause.body)} || C <- Cs].

-spec walk_body(body()) -> body().
walk_body(Body) ->
    [transform_statement(Stmt) || Stmt <- Body].

-spec transform_statement(statement()) -> statement().
transform_statement(?PFUN_CALL(Line, lambda, [AnonymousFunExpr, BindingsExpr])) ->
    ?PFUN_CALL(Line, lambda_impl, [erl_parse:abstract(AnonymousFunExpr), BindingsExpr]);
transform_statement(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt)));
transform_statement(Stmt) when is_list(Stmt) ->
    lists:map(fun transform_statement/1, Stmt);
transform_statement(Stmt) ->
    Stmt.
