-module(aeblas).
-on_load(init/0).
-export([btl/1, ltb/1, vector/1, daxpy/6, wait_c/1, zipwith_concurrent/3, n_for_milli_daxpy/0]).

-record(vector,{content, n, stride}).
-type vector():: #vector{}.
-type num_seq()::   <<_:1, _:_*64>> | [number() , ...] | number().

init()->
  Dir = case code:priv_dir(aeblas) of
              {error, bad_name} ->
                  filename:join(
                    filename:dirname(
                      filename:dirname(
                        code:which(?MODULE))), "priv");
              D -> D
          end,
    SoName = filename:join(Dir, atom_to_list(?MODULE)),
    erlang:load_nif(SoName, 0).


-spec btl(num_seq()) -> [number()].
btl(B)->
  case B of
    L when is_list(L)   -> L;
    N when is_number(N) -> [N];
    _ -> [H || <<H:64/native-float>> <= B] 
  end.

-spec ltb(num_seq()) -> binary().
ltb(L)->
  case L of
    B when is_binary(B) -> B;
    N when is_number(N) -> <<N:64/native-float>>;
    _ -> << <<H:64/native-float>> || H <- L>>
  end.


-spec vector(num_seq()|vector()) -> vector().
vector(Content) ->
    case Content of
        #vector{} -> Content;
        _ ->
          Bin = ltb(Content),
          #vector{content=Bin, n=floor(bit_size(Bin)/64), stride=1}
    end.



% EXPERIMENTATIONS
% ----------------------------------------------------------------------------
clamp(V, Min, Max)->
  if V < Min -> Min; true -> if V > Max -> Max; true -> V end end.

in_range(V, Min, Max)->
  if V > Min andalso V < Max ->
    true;
  true ->
    false
  end.

n_for_milli_daxpy()->
  V = aeblas:ltb(lists:seq(1,10000000)),
  Bench = fun(N)->
      N_runs = 1000,
      Benched   = fun()-> aeblas:daxpy(N, 1.0, V,1,V,1) end,
      Get_Milli = fun()-> {T,_} = timer:tc(Benched),  T/1000 end,
      Avg_run   = lists:foldl(fun(T,Acc) -> T/N_runs+Acc end, 0, [Get_Milli() || _ <- lists:seq(1,N_runs)]),
      Avg_run
  end,

  Find_milli = fun It(Cur_n, Cur_t)->
    Ratio = 1/Cur_t,
    In_range = in_range(Ratio, 0.9, 1.1),
    if In_range ->
      Cur_n;
    true->
      New_n = floor(clamp(1/Cur_t, 0.1, 10) * Cur_n),
      It(New_n, Bench(New_n))
    end
  end,

  Start = 200,
  _ = Bench(Start), % Preheat
  Find_milli(Start, Bench(Start)).



add(#vector{content=Xb, n=N,stride=Sx}, #vector{content=Yb, n=N,stride=Sy})->
  daxpy_nif(N, 1.0, Xb, Sx, Yb, Sy).


daxpy(N, Alpha, X, Stride_x, Y, Stride_y)->
  N_x = bit_size(X)/64,
  N_y = bit_size(Y)/64,
  if N_x >= N andalso N_y >= N andalso N >= 0-> 
    daxpy_nif(N, float(Alpha), X, Stride_x, Y, Stride_y);
  true ->
    throw(badarg)
  end.

daxpy_nif(_,_,_,_,_,_)->
  nif_not_loaded.

wait_c(_)->
  nif_not_loaded.


zipwith_concurrent(F, L1, L2)->
  ParentPID = self(),
  Launch_Worker = fun(B1, B2)->
    spawn(fun()-> ParentPID ! {F(B1, B2), self()} end)
  end,
  Retrieve_work = fun(Pid)->
    receive {Result, Pid} ->
      Result
    end
  end, 

  Worker_pids = lists:zipwith(Launch_Worker, L1, L2), 
  lists:map(Retrieve_work, Worker_pids).

