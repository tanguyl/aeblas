-module(bench).
-compile(export_all).



clamp(V, Min, Max)->
  if V < Min -> Min; true -> if V > Max -> Max; true -> V end end.

is_in_range(V, Min, Max)->
  if V > Min andalso V < Max ->
    true;
  true ->
    false
  end.

n_for_ms(Fct, FctGen)->
  Bench = fun(N)->
      N_runs = 40,
      V = FctGen(N),
      Get_Milli = fun()-> {T,_} = timer:tc(Fct, [V,V]),  T/1000 end,
      Avg_run   = lists:foldl(fun(T,Acc) -> T/N_runs+Acc end, 0, [Get_Milli() || _ <- lists:seq(1,N_runs)]),
      Avg_run
  end,

  Find_milli = fun It(L)->
    N_runs = 40,
    if length(L) == N_runs ->
      Close_l    = lists:filter(fun({_, Ct})-> is_in_range(1/Ct,0.8, 1.2) end, L),
      Avg_close  = lists:foldl(fun({Cn,Ct}, Acc)->Acc+(Cn/(Ct*length(Close_l))) end, 0, Close_l),
      floor(Avg_close);
    true ->
      {Cur_n, Cur_t} = hd(L),
      Ratio = 1/Cur_t,
      New_n = floor(clamp(Ratio, 0.1, 10) * Cur_n),
      New_t = Bench(New_n),
      It([{New_n, New_t}|L])
    end
  end,
  Find_milli([{200, Bench(200)}]).


gen_n_list(Elem, N)->
     [Elem || _ <- lists:seq(1,N)].

add_e(X,Y)->
    lists:zipwith(fun(L,R)->R+L end, X, Y).

add_c(X, Y)->
    M = bit_size(X)/64,
    M = bit_size(Y)/64,
    aeblas:daxpy(floor(M), 1.0, X, 1, Y, 1).

wait_c(X,_)->
    aeblas:wait_c(X),
    X.

bench_n(F, N)->
    Times = lists:map(fun(_)->F()end, lists:seq(1,N)),
    Avg_time = lists:foldl(fun(T, Avg)-> T/N + Avg end, 0, Times),
    Avg_time.

bench_conc(Fun, Bs)->
    Bench = fun()->
        {T,_} = timer:tc(fun()->aeblas:zipwith_concurrent(Fun, Bs, Bs)end),
        T / 1000000
    end,
    bench_n(Bench,20).

bench_seq(Fun, Bs)->
    Bench = fun()->
        {T,_} = timer:tc(fun()->lists:zipwith(Fun, Bs, Bs)end),
        T / 1000000
    end,
    bench_n(Bench, 20).