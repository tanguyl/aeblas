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


bench_erl(Ns)->
  N_e = bench:n_for_ms(fun bench:add_e/2, fun(N)->lists:seq(1,N)end),
  V_e = lists:seq(1,N_e),

  Run_bench = fun(Bench)-> 
    lists:map(fun(N)->Bench(fun add_e/2, gen_n_list(V_e, N))end, Ns) 
  end,

  T_conc = Run_bench(fun bench_conc/2),
  T_seq  = Run_bench(fun bench_seq/2),
  {{concurrent, T_conc}, {sequential, T_seq}, {list_size, N_e}}.


zipwith_concurrent(F, L1, L2)->
  ParentPID = self(),
  N_elems   = length(L1),

  %Receive datasets...
  Worker_collector = 
  fun()->
    Collect =
    fun Iterate(Id, Acc) when Id == N_elems->
      lists:reverse(Acc);
    Iterate(Id, Acc)->
      receive {Id, Result} ->
        Iterate(Id+1, [Result|Acc])
      after 5000 -> 
          timeout
      end
    end,
    ParentPID ! Collect(0, [])
  end,
  % Launch collector
  CollectorPID = spawn(Worker_collector),
  
  % Launch datasets...
  Generate_work =
  fun It(_, [],[])->
      ok;
    It(I,[L1_h|L1_t], [L2_h|L2_t])->
      spawn(fun() -> CollectorPID ! {I, F(L1_h,L2_h)} end),
      It(I+1, L1_t, L2_t)
  end,
  % Launch generator
  Generate_work(0, L1, L2),

  receive Result -> Result
  after N_elems * 2000 ->
    timeout
  end.