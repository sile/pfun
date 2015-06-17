%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 複数のノード間やコードローディング間で可搬な関数オブジェクトを提供するモジュール
-module(pfun).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([is_portable_fun/1]).
-export([lambda/2]).
-export([bind/4]).
-export([apply/2]).
-export([to_raw/1]).

-export_type([portable_fun/0]).
-export_type([bindings/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal API
%%----------------------------------------------------------------------------------------------------------------------
-export([lambda_impl/2]). % exported for `pfun_transform` module

%%----------------------------------------------------------------------------------------------------------------------
%% Types & Records & Macros
%%----------------------------------------------------------------------------------------------------------------------
-record(pfun,
        {
          expr                               :: abstract_expr(),
          bindings = erl_eval:new_bindings() :: erl_eval:binding_struct()
        }).

-opaque portable_fun() :: #pfun{}.
-type abstract_expr() :: erl_parse:abstract_expr().
-type bindings() :: [{Name::atom(), Value::term()}]. % see also: `erl_eval:bindings/0'

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc 引数が`portable_fun'型のオブジェクトかどうかを判定する
-spec is_portable_fun(X :: term()) -> boolean().
is_portable_fun(#pfun{}) -> true;
is_portable_fun(_)       -> false.

%% @doc 可搬な無名関数オブジェクトを生成する
%%
%% 注意: <br />
%% この関数は、ドキュメント目的でのみ用意されており、シェル上等から直接呼び出すことは想定されていない。
%%
%% 実際にコード内で使用する際には、コンパイルオプションに`{parse_transform, pfun_transform}'を指定する必要がある。 <br />
%% (`pfun_transform'により、本関数呼び出しはコンパイル時に別の実処理を行う内部関数に置換される)
%%
%% ```
%% %%
%% %% 使用例
%% %%
%% -module(example).
%%
%% -compile({parse_transform, pfun_transform}).  % parse_transformの設定が必須
%%
%% -export([hello_fun/1]).
%%
%% -spec hello_fun(atom()) -> fun (() -> {hello, atom()}).
%% hello_fun(Name) ->
%%   PFun = pfun:lambda(fun () -> {hello, Name} end,
%%                      [{'Name', Name}]),  % 現バージョンでは自由変数の束縛は手動で行う必要がある
%%   pfun:to_raw(PFun).
%% '''
-spec lambda(function(), bindings()) -> function().
lambda(AnonymousFun, Bindings) ->
    apply(erlang, error,
          [{undef, "Try recompiling caller module with the `{parse_transform, pfun_transform}` option"},
           [AnonymousFun, Bindings]]). % `erlang:apply/3' suppresses dialyzer warnings

%% @doc 完全修飾関数に対して引数の束縛(部分適用)を行う
%%
%% {@link lambda/2}とは異なりコンパイル時の`parse_transform'オプションの指定は不要
%%
%% ```
%% > F = pfun:bind(erlang, element, 2, [2]).  % `fun erlang:element/2'の最初の引数を`2'に束縛する
%% > b = pfun:apply(F, [{a, b, c}]).
%% '''
-spec bind(module(), atom(), non_neg_integer(), [term()]) -> portable_fun().
bind(Module, Function, Arity, BindArgs) when is_atom(Module), is_atom(Function), is_integer(Arity), Arity >= 0, is_list(BindArgs) ->
    BindCount = length(BindArgs),
    BindCount =< Arity orelse error({badarity, {fun Module:Function/Arity, BindArgs}}, [Module, Function, Arity, BindArgs]),

    Vars = gen_vars("__Arg", Arity - BindCount),
    Body = [gen_call_remote(Module, Function, quote_terms(BindArgs) ++ Vars)],
    Expr = gen_fun(Vars, Body),
    #pfun{expr = Expr};
bind(Module, Function, Arity, BindArgs) ->
    error(badarg, [Module, Function, Arity, BindArgs]).

%% @doc `Args'を引数として`Pfun'関数を呼び出す
%%
%% ```
%% > F = pfun:bind(erlang, '+', 2, [5]).
%% > 8 = pfun:apply(F, [3]).
%% '''
-spec apply(portable_fun(), [term()]) -> term().
apply(Pfun, Args) ->
    erlang:apply(to_raw(Pfun), Args).

%% @doc `Pfun'を通常のErlang関数(`function'型)に変換する
%%
%% {@link apply/2}関数は、内部的には実行の度に本関数を呼び出しているので、
%% 本関数で一度`function'型に変換し、それを持ち回した方が性能的には有利。
%%
%% また、通常のErlang関数と同様に扱うことができるので、利便性もより高い。
%%
%% その分、可搬性は(`portable_fun'型に比べて)若干損なわれるので注意が必要。<br />
%% ただし、変換後でも同じバージョンの`erl_eval'モジュールを使用しているノード間では可搬性が維持されるので、
%% 単一ノード内での使用や、異なるバージョンのErlang/OTPの混在の無い環境での使用であれば、
%% これが問題となることはほぼないものと思われる。
%%
%% ```
%% > F0 = pfun:bind(erlang, '+', 2, [4]).
%% > F1 = pfun:to_raw(F0).
%% > true = is_function(F1, 1).
%% > 1234 = F1(1230).
%% '''
-spec to_raw(Pfun :: portable_fun()) -> function().
to_raw(#pfun{expr = Expr, bindings = Bindings}) ->
    erl_eval:expr(Expr, Bindings, none, none, value).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec lambda_impl(abstract_expr(), bindings()) -> portable_fun().
lambda_impl(Expr, Bindings) when is_tuple(Expr), is_list(Bindings),
                                 (element(1, Expr) =:= 'fun' orelse element(1, Expr) =:= named_fun) ->
    _ = case erl_eval:check_command([Expr], Bindings) of
            ok                      -> ok;
            {error, {_, _, Reason}} -> error(Reason, [Expr, Bindings])
        end,
    BindingStruct =
        lists:foldl(fun ({Name, Value}, Acc) -> erl_eval:add_binding(Name, Value, Acc) end,
                    erl_eval:new_bindings(),
                    Bindings),
    #pfun{expr = Expr, bindings = BindingStruct};
lambda_impl(Expr, Bindings) ->
    error(badarg, [Expr, Bindings]).

-spec quote_terms([term()]) -> [abstract_expr()].
quote_terms(Xs) ->
    lists:map(fun erl_parse:abstract/1, Xs).

-spec gen_vars(string(), non_neg_integer()) -> [abstract_expr()].
gen_vars(Prefix, N) ->
    [{var, 1, Prefix ++ "_" ++ integer_to_list(I)} || I <- lists:seq(1, N)].

-spec gen_call_remote(module(), atom(), [abstract_expr()]) -> abstract_expr().
gen_call_remote(Module, Function, Args) ->
    {call, 1, {remote, 1, {atom, 1, Module}, {atom, 1, Function}}, Args}.

-spec gen_fun([abstract_expr()], [abstract_expr()]) -> abstract_expr().
gen_fun(Args, Body) ->
    {'fun', 1, {clauses, [{clause, 1, Args, [], Body}]}}.
