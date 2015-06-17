

# Module pfun #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


複数のノード間やコードローディング間で可搬な関数オブジェクトを提供するモジュール.


<a name="types"></a>

## Data Types ##




### <a name="type-bindings">bindings()</a> ###



<pre><code>
bindings() = [{Name::atom(), Value::term()}]
</code></pre>



 see also: `erl_eval:bindings/0`



### <a name="type-portable_fun">portable_fun()</a> ###


__abstract datatype__: `portable_fun()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply-2">apply/2</a></td><td><code>Args</code>を引数として<code>Pfun</code>関数を呼び出す.</td></tr><tr><td valign="top"><a href="#bind-4">bind/4</a></td><td>完全修飾関数に対して引数の束縛(部分適用)を行う.</td></tr><tr><td valign="top"><a href="#is_portable_fun-1">is_portable_fun/1</a></td><td>引数が<code>portable_fun</code>型のオブジェクトかどうかを判定する.</td></tr><tr><td valign="top"><a href="#lambda-2">lambda/2</a></td><td>可搬な無名関数オブジェクトを生成する.</td></tr><tr><td valign="top"><a href="#to_raw-1">to_raw/1</a></td><td><code>Pfun</code>を通常のErlang関数(<code>function</code>型)に変換する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply-2"></a>

### apply/2 ###


<pre><code>
apply(Pfun::<a href="#type-portable_fun">portable_fun()</a>, Args::[term()]) -&gt; term()
</code></pre>
<br />


`Args`を引数として`Pfun`関数を呼び出す



```
  > F = pfun:bind(erlang, '+', 2, [5]).
  > 8 = pfun:apply(F, [3]).
```

<a name="bind-4"></a>

### bind/4 ###


<pre><code>
bind(Module::module(), Function::atom(), Arity::non_neg_integer(), BindArgs::[term()]) -&gt; <a href="#type-portable_fun">portable_fun()</a>
</code></pre>
<br />


完全修飾関数に対して引数の束縛(部分適用)を行う



[`lambda/2`](#lambda-2)とは異なりコンパイル時の`parse_transform`オプションの指定は不要



```
  > F = pfun:bind(erlang, element, 2, [2]).  % `fun erlang:element/2'の最初の引数を`2'に束縛する
  > b = pfun:apply(F, [{a, b, c}]).
```

<a name="is_portable_fun-1"></a>

### is_portable_fun/1 ###


<pre><code>
is_portable_fun(X::term()) -&gt; boolean()
</code></pre>
<br />

引数が`portable_fun`型のオブジェクトかどうかを判定する
<a name="lambda-2"></a>

### lambda/2 ###


<pre><code>
lambda(AnonymousFun::function(), Bindings::<a href="#type-bindings">bindings()</a>) -&gt; function()
</code></pre>
<br />


可搬な無名関数オブジェクトを生成する



注意: <br />
この関数は、ドキュメント目的でのみ用意されており、シェル上等から直接呼び出すことは想定されていない。



実際にコード内で使用する際には、コンパイルオプションに`{parse_transform, pfun_transform}`を指定する必要がある。 <br />
(`pfun_transform`により、本関数呼び出しはコンパイル時に別の実処理を行う内部関数に置換される)



```
  %%
  %% 使用例
  %%
  -module(example).
  -compile({parse_transform, pfun_transform}).  % parse_transformの設定が必須
  -export([hello_fun/1]).
  -spec hello_fun(atom()) -> fun (() -> {hello, atom()}).
  hello_fun(Name) ->
    PFun = pfun:lambda(fun () -> {hello, Name} end,
                       [{'Name', Name}]),  % 現バージョンでは自由変数の束縛は手動で行う必要がある
    pfun:to_raw(PFun).
```

<a name="to_raw-1"></a>

### to_raw/1 ###


<pre><code>
to_raw(Pfun::<a href="#type-portable_fun">portable_fun()</a>) -&gt; function()
</code></pre>
<br />


`Pfun`を通常のErlang関数(`function`型)に変換する



[`apply/2`](#apply-2)関数は、内部的には実行の度に本関数を呼び出しているので、
本関数で一度`function`型に変換し、それを持ち回した方が性能的には有利。



また、通常のErlang関数と同様に扱うことができるので、利便性もより高い。



その分、可搬性は(`portable_fun`型に比べて)若干損なわれるので注意が必要。<br />
ただし、変換後でも同じバージョンの`erl_eval`モジュールを使用しているノード間では可搬性が維持されるので、
単一ノード内での使用や、異なるバージョンのErlang/OTPの混在の無い環境での使用であれば、
これが問題となることはほぼないものと思われる。



```
  > F0 = pfun:bind(erlang, '+', 2, [4]).
  > F1 = pfun:to_raw(F0).
  > true = is_function(F1, 1).
  > 1234 = F1(1230).
```

