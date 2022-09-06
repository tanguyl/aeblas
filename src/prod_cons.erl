-module(prod_cons).
-compile(export_all).

zipwith_concurrent(Fct, L1, L2)->
    Producer = fun(State)->
        case State of 
            {[],[]} ->
                {fun()-> finished end, {[],[]}};
            {P_l1, P_l2}->
                {
                    fun()->Fct(hd(P_l1), hd(P_l2))end,
                     {tl(P_l1), tl(P_l2)}
                }
        end
    end,

    MyPID = self(),
    PIDs = {spawn(fun()->producer(Producer, {L1,L2})end), spawn(fun()->collector(MyPID)end)},
    spawn(fun()->load_balance(10, PIDs) end),


    receive 
        {Result, _} -> Result
    after 1000*5*length(L1) ->
         timeout_zipwith_conc
    end.


producer(WorkStateGenerator, State)->
    producer(0, WorkStateGenerator, State).


producer(Id, WorkStateGenerator, State)->
    receive 
        {work, BalancerPID} ->
            {Work, NextState} = WorkStateGenerator(State),
            spawn(fun()-> BalancerPID ! {Id, Work(), self()} end),
            producer(Id+1, WorkStateGenerator, NextState);
        {quit, _}->
            ok;
        Msg ->
            io:format("Producer received unknown message: ~w~n", [Msg])
    after 1000*5 ->
        timeout_producer
    end.


load_balance(Max_active, PIDs)->
    load_balance(working, {0, Max_active}, PIDs).

load_balance(State, ProducerState, PIDs={ProducerPID, CollectorPID})->
    case ProducerState of 
        {0, _} when State == finishing ->
            CollectorPID ! {finished, self()},
            ProducerPID  ! {quit, self()},
            ok;
        {N, Max} when State == working  andalso N < Max->
            ProducerPID ! {work, self()},
            load_balance(State, {N+1, Max}, PIDs);

        {N, Max}  ->
            receive{_, finished, _} ->
                    load_balance(finishing, {N-1, Max}, PIDs);

                {Id, Work, _} ->
                    CollectorPID ! {Id, Work, self()},
                    load_balance(State, {N-1, Max}, PIDs);
                
                Msg ->
                    io:format("Load balance received unknown message: ~w~n", [Msg])
            after 1000*5 ->
                timeout_load_balance
            end
    end.


collector(DestPID)->
    collector(0, [], DestPID).
collector(Id, Acc, DestPID)->
    receive 
        {Id, Work, _} -> collector(Id+1, [Work|Acc], DestPID);
        {finished, _} -> DestPID ! {lists:reverse(Acc), self()}, ok
    after 1000*5 ->
        timeout_collector
    end,
    ok.



