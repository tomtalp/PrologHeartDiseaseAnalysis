:- dynamic(already_asked/1).

start:-
    write('Welcome to the Heart Disease Analysis expert system!'),nl,
    write("Please type your action -"),nl,
    tab(4), write('diagnose (to perform a diagnosis)'),nl,
    tab(4), write('help (to receive help about the program)'),nl,
    tab(4), write('quit (to exit)'),nl,
    repeat,
    write('>>> '),
    read(Action),

    % Change this part - 
    do(Action),
    Action == quit.

do(diagnose):-
    !, write('Performing diagnose!'), nl.    

do(help):-
    !, write("Here's some info about our system - blablabla") ,nl.    

do(quit).

do(Unknown):-
    write("Received an illegal operation `"), write(Unknown), write("`, please try again (type `help` for the manual)"),nl.


% What is the maximum difficulty on the ECG treadmil test?  (easy/medium/hard)
max_exer_difficulty(X):-
    ask_user(max_exer_difficulty, X, [easy, medium, hard]).

ask_user(Attr, Val, Options):-
    write("What's the value for `"), write(Attr), write('`? '),nl,
    write(Options), write(' : '), nl,
    read(UserAnswer),
    validate_answer(UserAnswer, Attr, Options),!,
    asserta(already_asked(Attr)).

% Received a valid answer
validate_answer(UserAnswer, Attr, Options):-
    member(UserAnswer, Options), !.

% Invalid answer, trying again
validate_answer(UserAnswer, Attr, Options):-
    write(UserAnswer), write(" is an invalid answer. Please try again."),nl,
    ask_user(Attr, Val, Options).



