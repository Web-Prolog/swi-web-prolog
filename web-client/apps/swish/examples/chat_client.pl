
% Simple chat client built on top of pubsub service
%
% We would like to use register/2 and unregister/1 instead
% of dynamic db, but they are buggy at the moment.


:- dynamic chat_pid/1.	

chat(Alias) :-
	spawn(chat_client(Alias), Pid, [
		src_predicates([chat_client/1, chat_loop/2])
	]),
	assert(chat_pid(Pid)).
	
say(Message) :-
	chat_pid(Pid),
	Pid ! say(Message).
	
leave :-
	retract(chat_pid(Pid)),
	Pid ! leave.
	
	
chat_client(Alias) :-
	self(Self),
    pubsub_service ! subscribe(Self),
	pubsub_service ! publish(joined(Alias)),
	chat_loop(Alias, Self).
	
chat_loop(Alias, Self) :-
    receive({
	
	    % Messages to be sent to pubsub service
		say(Message) ->
			pubsub_service ! publish(message(Alias, Message)),
			chat_loop(Alias, Self);
		leave ->
			pubsub_service ! publish(left(Alias)),
			pubsub_service ! unsubscribe(Self);
			
	    % Messages to be received from pubsub service
        msg(joined(Guest)) ->
            io:format("~p joined", [Guest]),
            chat_loop(Alias, Self);
        msg(message(Guest, Message)) ->
            io:format("~p: ~p", [Guest, Message]),
            chat_loop(Alias, Self);
        msg(left(Guest)) ->
            io:format("~p left", [Guest]),
            chat_loop(Alias, Self)
    }).


/** Examples

chat(/* your alias here */).

say('Hello there!').

say('Goodbye!').

leave.

*/