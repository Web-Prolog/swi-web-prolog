% Reading and writing
% -------------------


hello_world :-
    pengine_output('Hello World!'),
    sleep(1),
    hello_world.


read_and_write :-
    pengine_input('Echo this term', Something),
    (   Something == stop
    ->  true
    ;   pengine_output(Something),
        read_and_write
    ).


/** Examples

hello_world.
read_and_write.

*/