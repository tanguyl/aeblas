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

n_for_millis(Fct, Time, FctGen)->
  Bench = fun(N)->
      V = FctGen(N),
      {T,_} = timer:tc(Fct, [V,V]), 
      T/1000
  end,

  N_runs = 100,
  Max_it = 1000,
  Find_milli = 
    fun It(_,L,Cur_i) when Cur_i == Max_it ->
      io:format("Could not converge in n_for_millis; found only ~w samples.~n", [length(L)]),
      nok;

    It(_,L, _) when length(L) == N_runs -> 
        Avg_close  = lists:foldl(fun({Cn,_}, Acc)->Acc+(Cn/(length(L))) end, 0, L),
        floor(Avg_close);

    It({Cur_n, Cur_t}, L, Cur_i)->
        Ratio = Time/Cur_t,
        New_n = floor(clamp(Ratio, 0.1, 10) * Cur_n),
        io:format("Cur_n ~w, ratio ~w ~n", [Cur_n, Ratio]),
        New_t = Bench(New_n),
        Valid = is_in_range(ratio,0.8, 1.2),
        New_i = Cur_i + 1,
        if Valid ->
          It({New_n, New_t}, [{New_n, New_t}|L], New_i);
        true->
          It({New_n, New_t}, L, New_i)
        end
  end,
  Find_milli({200, Bench(200)}, [], 0).


gen_n_list(Elem, N)->
     [Elem || _ <- lists:seq(1,N)].

add_e(X,Y)->
  add_e(X,Y,0,1000).

add_e(_,_,R,0)->
  R;
add_e(X,Y,_,I)->
  R = lists:zipwith(fun(L,R)->R+L end, X, Y),
  add_e(X,Y,R,I-1).

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
        {T,_} = timer:tc(fun()->prod_cons:zipwith_concurrent(Fun, Bs, Bs)end),
        T / 1000000
    end,
    bench_n(Bench,1).

bench_seq(Fun, Bs)->
    Bench = fun()->
        {T,_} = timer:tc(fun()->lists:zipwith(Fun, Bs, Bs)end),
        T / 1000000
    end,
    bench_n(Bench, 1).


bench_fct(F, Ns)->
  N_e = bench:n_for_millis(F, 10, fun(N)->lists:seq(1,N)end),
  io:format("Ne is ~w ~n", [N_e]),
  V_e = lists:seq(1,N_e),

  Run_bench = fun(Bench)->   
    lists:map(fun(N)->Bench(fun add_e/2, gen_n_list(V_e, N))end, Ns)
  end,

  T_conc = Run_bench(fun bench_conc/2),
  T_seq  = Run_bench(fun bench_seq/2),
  {{concurrent, T_conc}, {sequential, T_seq}, {list_size, N_e}, {n, Ns}}.


bench_e(Ns)->
  N_e = bench:n_for_millis(fun bench:add_e/2, 1, fun(N)->lists:seq(1,N)end),
  io:format("Ne is ~w ~n", [N_e]),
  V_e = lists:seq(1,N_e),

  Run_bench = fun(Bench)->   
    lists:map(fun(N)->Bench(fun add_e/2, gen_n_list(V_e, N))end, Ns)
  end,

  T_conc = Run_bench(fun bench_conc/2),
  T_seq  = Run_bench(fun bench_seq/2),
  {{concurrent, T_conc}, {sequential, T_seq}, {list_size, N_e}, {n, Ns}}.

bench_c(Ns)->
  N_c = bench:n_for_millis(fun bench:add_c/2, 1, fun(N)->aeblas:ltb(lists:seq(1,N))end),
  io:format("N_c is ~w ~n", [N_c]),
  V_c = aeblas:ltb(lists:seq(1,N_c)),

  Run_bench = fun(Bench)->   
    lists:map(fun(N)->Bench(fun add_c/2, gen_n_list(V_c, N))end, Ns)
  end,

  T_conc = Run_bench(fun bench_conc/2),
  T_seq  = Run_bench(fun bench_seq/2),
  {{concurrent, T_conc}, {sequential, T_seq}, {list_size, N_c}, {n, Ns}}.


zipwith_concurrent(F, L1, L2)->
  ParentPID = self(),
  N_elems   = length(L1),

  %Receive datasets...
  Worker_collector = 
  fun()->
    Collect =
    fun Iterate(Id, Acc) when Id == N_elems->
      ParentPID ! lists:reverse(Acc);
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