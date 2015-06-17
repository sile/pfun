pfun
====

複数のノード間やコードローディング間で可搬な関数オブジェクトを提供するためのライブラリ

__Portable Functions__の略

概要
----

通常のErlang関数オブジェクトの以下のような問題を解決することが目的:
- 1. 複数のコードローディングを跨いで生存することができない
  - 関数オブジェクトが属するモジュールが二回更新された場合は、そのオブジェクトを保持するプロセスがクラッシュする
     - 完全修飾形式(`fun Mod:Fun/Arity`)で参照された関数は例外
  - 参考: [ホットコードローディング時のプロセスクラッシュについて](http://qiita.com/sile/items/697f80db992819056127)
- 2. 複数のノード間で安全かつ気軽に関数オブジェクトを受け渡すことが難しい
  - 関数オブジェクトが属するモジュール(の特定バージョン)が、送信先のノードでもロードされている必要がある
- 上記問題は、どちらも無名関数の実体(バイトコード列)が特定のバージョンのモジュールインスタンスに紐付いていることが原因
  - そのバージョンのモジュールが破棄される(or 存在しない)場合は実行が不可能になる

`pfun`は、
- 特定のモジュールインスタンスに紐付かない関数オブジェクトを生成する手段を提供する
  - [例] 可搬な無名関数: `pfun:lambda(fun () -> io:format("hello") end, [])`
  - [例] 可搬な部分適用関数: `pfun:bind(erlang, '+', 2, [10])`
- 内部的には[erl_eval](http://www.erlang.org/doc/man/erl_eval.html)の仕組みを利用している
  - 関数オブジェクトを通常のデータ(ex. リストやタプル)によって表現された抽象構文木として保持する
     - これにより特定のモジュールインスタンスへの依存性がなくなる
  - 構文木をインタプリタ的に処理する必要があるため実行速度は遅い

利用方法
--------

ビルドツールには[rebar](https://github.com/rebar/rebar)を使用する想定。

利用アプリケーションの`rebar.config`に以下の設定項目を追加する:
```erlang
%%
%% in rebar.config
%%
{erl_opts,
  [
    {parse_transform, pfun_transform}  % `pfun:lambda/2`のために必要
  ]}.

{deps,
  [
    %% 依存ライブラリに追加
    {pfun, ".*", {git, "git://github.com/sile/pfun.git", {tag, "0.1.0"}}}
  ]}.
```

使用例
------

`pfun:bind/2`:
```erlang
%%
%% 完全修飾関数に対して、引数の部分適用を行った結果の関数オブジェクトが生成可能
%%

%% 関数オブジェクト生成
> F0 = pfun:bind(erlang, '+', 2, [10]). % 最初の引数を`10`に束縛する
> true = pfun:is_portable_fun(F0).

%% 実行: `pfun:apply/2`を使用する
> 15 = pfun:apply(F0, [5]).

%% 通常のErlang関数にも変換可能:
%% - これによって可搬性が大幅に損なわれることはない
%% - ただし注意すべき点もあるので、詳細は to_raw/1 のドキュメントを参照のこと
> F1 = pfun:to_raw(F0).
> true = is_function(F1).
> 15 = F1(5).
```

`pfun:lambda/2`:
```erlang
%%
%% 無名関数を可搬な関数オブジェクトに変換する。
%% `pfun:bind/2`とは異なりシェル上から直接実行することは出来ない。
%% (parse_transformの機能を使用しているため)
%%
-module(example).

-compile({parse_transform, pfun_transform}).  % parse_transformの設定が必須

-export([hello_fun/1]).

-spec hello_fun(atom()) -> pfun:portable_fun().
hello_fun(Name) ->
    pfun:lambda(fun () -> {hello, Name} end,
                [{'Name', Name}]).  % 現バージョンでは自由変数の束縛は手動で行う必要がある

%%
%% 実行
%%
> F = example:hello_fun(hoge).
> pfun:apply(F, []).
{hello, hoge}.
```

API
---

[EDocドキュメント](doc/README.md)を参照

既知の問題
----------

- 実行速度が遅い
- 実行中にエラーが発生した場合のスタックトレースが分かりにくくなる可能性がある
- 無名関数内にローカル関数の呼び出しがあると、実行時にエラーとなる

TODO
----
- 束縛変数を自動で検出・設定してくれる`pfun:lambda/1`を実装する
