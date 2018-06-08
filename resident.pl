/*
    This is the node-resident code available for the 
    philosophy section.
*/

p(X) :- q(X), r(X).

q(a).
q(b).
q(c).

r(b).
r(c).



/*
    This is the node-resident code available in this node. It is loaded
    in the module `web_prolog' using the following code:

    :- user:consult(resident).
*/

:- use_module(web_prolog).

ancestor_descendant(X, Y) :- parent_child(X, Y).
ancestor_descendant(X, Z) :- parent_child(X, Y), ancestor_descendant(Y, Z).

parent_child(X, Y) :- mother_child(X, Y).
parent_child(X, Y) :- father_child(X, Y).

mother_child(trude, sally).

father_child(tom, sally).
father_child(tom, erica).
father_child(mike, tom).

% Expermental for the Genealogist demo:

:- dynamic mother_child/2, father_child/2.

assert_mother_child(Mother, Child) :-
      assert(mother_child(Mother, Child)).

assert_father_child(Father, Child) :-
      assert(father_child(Father, Child)).
      
retract_mother_child(Mother, Child) :-
      retractall(mother_child(Mother, Child)).

retract_father_child(Father, Child) :-
      retractall(father_child(Father, Child)).



% Node-resident counting server:

count_server(Count0) :-
    Count is Count0 + 1,
    receive({
        count(From) ->
            From ! c(Count),
            count_server(Count)
    }).
    
:- spawn(count_server(0), Pid),
   register(counter, Pid).
   


% The core of a publish and subscribe service:

pubsub_service(Subscribers0) :-
    receive({
        publish(Message) ->
%            forall(member(Pid, Subscribers0), Pid ! msg{text:Message}), % Experimental - see also format.pl
            forall(member(Pid, Subscribers0), Pid ! msg(Message)),
            pubsub_service(Subscribers0);
        subscribe(Pid) ->
            pubsub_service([Pid|Subscribers0]);
        unsubscribe(Pid) ->
            (   select(Pid, Subscribers0, Subscribers)
            ->  pubsub_service(Subscribers)
            ;   pubsub_service(Subscribers0)
            )   
    }).

:- spawn(pubsub_service([]), Pid),
   register(pubsub_service, Pid).
   
 
  
sleep :-
    Time is random_float/10,
    sleep(Time).

doForks(ForkList) :-
    receive({
        {grabforks, {Left, Right}} ->
            subtract(ForkList, [Left,Right], ForkList1),
            doForks(ForkList1);
        {releaseforks, {Left, Right}} -> 
            doForks([Left, Right| ForkList]);
        {available, {Left, Right}, Sender} ->
            (   member(Left, ForkList),
                member(Right, ForkList)
            ->  Bool = true
            ;   Bool = false
            ),
            Sender ! {areAvailable, Bool},
            doForks(ForkList);
        {die} -> 
            io:format("Forks put away.")
    }).

areAvailable(Forks, Have) :-
    self(Self),
    forks ! {available, Forks, Self},
    receive({
        {areAvailable, false} ->
            Have = false;
        {areAvailable, true} -> 
            Have = true
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
    io:format("Waiter is leaving."),
    diningRoom ! {allgone}.
doWaiter(WaitList, ClientCount, EatingCount, Busy) :-
    receive({
        {waiting, Client} ->
            WaitList1 = [Client|WaitList], % add to waiting list
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
    io:format("~s is leaving.", [Name]),
    waiter ! {leaving},
    flag(left, N, N+1).
philosopher(Name, Forks, Cycle) :-
    self(Self),
    io:format("~s is thinking (cycle ~w).", [Name, Cycle]),
    sleep,
    io:format("~s is hungry (cycle ~w).", [Name, Cycle]),
    waiter ! {waiting, {Self, Forks}}, % sit at table
    receive({
        {served} -> 
            forks ! {grabforks, Forks}, % grab forks
            waiter ! {eating, {Self, Forks}}, % start eating
            io:format("~s is eating (cycle ~w).", [Name, Cycle])
    }),
    sleep,
    forks ! {releaseforks, Forks}, % put forks down
    waiter ! {finished},
    Cycle1 is Cycle - 1,
    philosopher(Name, Forks, Cycle1).

dining :-    
    AllForks = [1, 2, 3, 4, 5],
    Clients = 5,
    self(Self),
    register(diningRoom, Self),
    spawn(doForks(AllForks), ForksPid),
    register(forks, ForksPid),
    spawn(doWaiter([], Clients, 0, false), WaiterPid),
    register(waiter, WaiterPid),
    Life_span = 20,
    spawn(philosopher('Aristotle', {5, 1}, Life_span)),
    spawn(philosopher('Kant', {1, 2}, Life_span)),
    spawn(philosopher('Spinoza', {2, 3}, Life_span)),
    spawn(philosopher('Marx', {3, 4}, Life_span)),
    spawn(philosopher('Russel', {4, 5}, Life_span)),
    receive({
        {allgone} -> 
            io:format("Dining room closed.")
    }),
    unregister(diningRoom).
    
    
    

chatserver_loop(Guests) :-
    receive({
        enter(Pid, Nick) ->
            broadcast(Guests, entered(Nick)),
            chatserver_loop([guest(Pid, Nick)|Guests]);
        leave(Pid) ->
            (   select(guest(Pid, Nick), Guests, Rest)
            ->  broadcast(Rest, left(Nick)),
                chatserver_loop(Rest)
            ;   chatserver_loop(Guests)
            );
        say(Nick, Message) ->
            broadcast(Guests, said(Nick, Message)),
            chatserver_loop(Guests)
    }).

broadcast(Guests, Message) :-
    forall(member(guest(Pid, _), Guests), Pid ! Message).


?- spawn(chatserver_loop([]), Pid),
   register(chatserver, Pid).


/*
use_remote_module(URI, ImportList, Options) :-
    maplist(import(URI, Options), ImportList).

import(URI, Options, Functor1/Arity as Functor2) :- !,
    functor(Head1, Functor1, Arity),
    Head1 =.. [Functor1|Args],
    Head2 =.. [Functor2|Args],   
    assertz((Head2 :- rpc(URI, Head1, Options))).    
import(URI, Options, Functor/Arity) :-
    functor(Head, Functor, Arity),    
    assertz((Head :- rpc(URI, Head, Options))).
*/
