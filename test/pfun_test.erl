%% Copyright (c) 2015, Takeru Ohta <phjgt308@gmail.com>
%%
-module(pfun_test).

-compile({parse_transform, pfun_transform}). % for `pfun:compile/2'

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
lambda_test_() ->
    [
     {"Create portable anonymous function",
      fun () ->
              Fun = pfun:lambda(fun () -> hello end, []),
              ?assert(pfun:is_portable_fun(Fun)),
              ?assertEqual(hello, pfun:apply(Fun, []))
      end},
     {"Create Closure",
      fun () ->
              X = 10,
              Fun = pfun:lambda(fun () -> 5 + X end,
                                [{'X', X}]), % bindings
              ?assertEqual(15, pfun:apply(Fun, []))
      end},
     {"Closure + Arguments",
      fun () ->
              Fun = pfun:lambda(fun (Y, Z) -> X + Y + Z end,
                                [{'X', 10}]),
              ?assertEqual(17, pfun:apply(Fun, [5, 2]))
      end},
     %% {"Named fun (since OTP17)",
     %%  fun () ->
     %%          Fun = pfun:lambda(
     %%                  fun Fib (0) -> 0;
     %%                      Fib (1) -> 1;
     %%                      Fib (N) -> Fib(N - 2) + Fib(N - 1)
     %%                  end,
     %%                  []),
     %%          ?assertEqual(2, pfun:apply(Fun, [3]))
     %%  end},
     {"Missing needed bindings",
      fun () ->
              _X = 10,
              ?assertError(
                 {unbound_var, '_X'},
                 pfun:lambda(fun () -> 5 + _X end, % NOTE: currently, pfun can't automatically bind depending variables
                             []))
      end}
    ].

to_raw_test_() ->
    [
     {"Convert to raw function",
      fun () ->
              PFun = pfun:lambda(fun (X) -> 5 + X end, []),
              ?assert(pfun:is_portable_fun(PFun)),

              RawFun = pfun:to_raw(PFun),
              ?assert(is_function(RawFun, 1)),
              ?assertNot(pfun:is_portable_fun(RawFun)),

              ?assertEqual(15, RawFun(10))
      end}
    ].

bind_test_() ->
    [
     {"Bind partial arguments (Partial application)",
      fun () ->
              Fun0 = pfun:bind(erlang, '+', 2, []),
              ?assertEqual(10+5, pfun:apply(Fun0, [10, 5])),

              Fun1 = pfun:bind(erlang, '+', 2, [5]),
              ?assertEqual(10+5, pfun:apply(Fun1, [10])),

              Fun2 = pfun:bind(erlang, '+', 2, [5, 10]),
              ?assertEqual(10+5, pfun:apply(Fun2, []))
      end},
     {"Too many arguments",
      fun () ->
              ?assertError({badarity, _}, pfun:bind(erlang, '+', 2, [1, 2, 3]))
      end},
     {"Undefined function",
      fun () ->
              Fun = pfun:bind(erlang, hoge, 0, []),
              ?assert(pfun:is_portable_fun(Fun)),
              ?assertError(undef, pfun:apply(Fun, [])) % execution time error
      end}
    ].
