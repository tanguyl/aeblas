N = aeblas:n_for_milli_daxpy(),
B = aeblas:ltb(lists:seq(1,N)),

Gen_n_b = fun(Nbs)->
    Bs = [B || _ <- lists:seq(1,Nbs)]
end,

Daxpy = fun(X, Y)->
    M = bit_size(X)/64,
    M = bit_size(Y)/64,
    aeblas:daxpy(floor(M), 1.0, X, 1, Y, 1)
end.

Wait_erl = fun(X,_)->
    Init_time = erlang:monotonic_time(),
    Delay     = fun Loop() ->
        Delta = erlang:monotonic_time() - Init_time,
        Milli = erlang:convert_time_unit(Delta, native, millisecond),
        if Milli < 1 ->
            Loop();
        true->
            X
        end
    end,
    Delay()
end.

Wait_c = fun(X,_)->
    aeblas:wait_c(X),
    X
end.

Bench_n = fun(F, N)->
    Times = lists:map(fun(_)->F()end, lists:seq(1,N)),
    Avg_time = lists:foldl(fun(T, Avg)-> T/N + Avg end, 0, Times),
    Avg_time
end.

Bench_conc = fun(Fun, Bs)->
    Bench = fun()->
        {T,_} = timer:tc(fun()->aeblas:zipwith_concurrent(Fun, Bs, Bs)end),
        T / 1000000
    end,
    Bench_n(Bench,20)
end.

Bench_single= fun(Fun, Bs)->
    Bench = fun()->
        {T,_} = timer:tc(fun()->lists:zipwith(Fun, Bs, Bs)end),
        T / 1000000
    end,
    Bench_n(Bench, 20)
end.