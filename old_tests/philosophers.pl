:- use_module(actors).

% -module(philosophers).
% -export([dining/0]).

d :-
   /* debug(dispatch(send)),
    debug(dispatch(receive)),
    debug(dispatch(wakeup)),
    debug(dispatch(down)), */
    tmon,
    interactor,
    debug,
    dining.

sleep :- !.
sleep :-
    Time is random_float/10,
    sleep(Time).

doForks(ForkList) :-
    receive({
	{grabforks, {Left, Right}} ->
        subtract(ForkList, [Left,Right], ForkList1),
        doForks(ForkList1);
	{releaseforks, {Left, Right}} -> doForks([Left, Right| ForkList]);
	{available, {Left, Right}, Sender} ->
          (   member(Left, ForkList),
              member(Right, ForkList)
          ->  Bool = true
          ;   Bool = false
          ),
          Sender ! {areAvailable, Bool},
          doForks(ForkList);
	{die} -> format("Forks put away.~n")
    }).

areAvailable(Forks, Have) :-
    self(Self),
    forks ! {available, Forks, Self},
    receive({
		{areAvailable, false} -> Have = false;
		{areAvailable, true} -> Have = true
    }).


processWaitList([], false).
processWaitList([H|T], Result) :-
	{Client, Forks} = H,
        areAvailable(Forks, Have),
        (   Have == true
        ->  Client ! {served},
            Result = true
        ;   Have == false
        ->  processWaitList(T, Result)
        ).

doWaiter([], 0, 0, false) :-
	forks ! {die},
	format("Waiter is leaving.~n"),
	diningRoom ! {allgone}.
doWaiter(WaitList, ClientCount, EatingCount, Busy) :-
	receive({
		{waiting, Client} ->
			WaitList1 = [Client|WaitList],	%% add to waiting list
                        (   Busy == false,
                            EatingCount<2
                        ->  processWaitList(WaitList1, Busy1)
                        ;   Busy1 = Busy
                        ),
			doWaiter(WaitList1, ClientCount, EatingCount, Busy1);
		{eating, Client} ->
			subtract(WaitList, [Client], WaitList1),
                        EatingCount1 is EatingCount+1,
			doWaiter(WaitList1, ClientCount, EatingCount1, false);
		{finished} ->
                        processWaitList(WaitList, R1),
                        EatingCount1 is EatingCount-1,
			doWaiter(WaitList, ClientCount, EatingCount1, R1) ;
		{leaving} ->
                        ClientCount1 is ClientCount - 1,
		        flag(left_received, N, N+1),
			doWaiter(WaitList, ClientCount1, EatingCount, Busy)
        }).


philosopher(Name, _Forks, 0) :-
        format("~s is leaving.~n", [Name]),
	waiter ! {leaving},
        flag(left, N, N+1).
philosopher(Name, Forks, Cycle) :-
	self(Self),

	format("~s is thinking (cycle ~w).~n", [Name, Cycle]),
	sleep,

	format("~s is hungry (cycle ~w).~n", [Name, Cycle]),
	waiter ! {waiting, {Self, Forks}}, %%sit at table

	receive({
		{served}-> forks ! {grabforks, Forks},	%%grab forks
			waiter ! {eating, {Self, Forks}},	%%start eating
			format("~s is eating (cycle ~w).~n", [Name, Cycle])
        }),

	sleep,
	forks ! {releaseforks, Forks},					%% put forks down
	waiter ! {finished},

        Cycle1 is Cycle - 1,
	philosopher(Name, Forks, Cycle1).


dining :-	AllForks = [1, 2, 3, 4, 5],
		Clients = 5,
                self(Self),
		register(diningRoom, Self),

                spawn(doForks(AllForks), ForksPid),
		register(forks, ForksPid),
                spawn(doWaiter([], Clients, 0, false), WaiterPid),
		register(waiter, WaiterPid),
		Life_span = 20,
		spawn(philosopher('Aristotle', {5, 1}, Life_span), Aristotle),
		spawn(philosopher('Kant', {1, 2}, Life_span), Kant),
		spawn(philosopher('Spinoza', {2, 3}, Life_span), Spinoza),
		spawn(philosopher('Marx', {3, 4}, Life_span), Marx),
		spawn(philosopher('Russel', {4, 5}, Life_span), Russel),

                register(aristotle, Aristotle),
                register(kant, Kant),
                register(spinoza, Spinoza),
                register(marx, Marx),
                register(russel, Russel),

		receive({
			{allgone} -> format("Dining room closed.~n")

                }),
		unregister(diningRoom).
