N_e = bench:n_for_ms(fun bench:add_e/2, fun(N)->lists:seq(1,N)end).
V_e = lists:seq(1,N_e).

N_c = bench:n_for_ms(fun bench:add_c/2, fun(N)->aeblas:ltb(lists:seq(1,N))end).
V_c = aeblas:ltb(lists:seq(1,N_c)).