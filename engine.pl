:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).

:- dynamic(already_asked/1).
:- dynamic(knowledge/2).

:- ensure_loaded(rules).

askable(blood_sugar).
askable(resting_bp).
askable(max_exer_difficulty).
askable(cp_only_exercise).
askable(cholesterol_level).
askable(gender).    


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
    !, write('Performing diagnose!'), 
    check_heart_risk.    

do(help):-
    !, write("Here's some info about our system - blablabla") ,nl.    

do(quit).

do(Unknown):-
    write("Received an illegal operation `"), write(Unknown), write("`, please try again (type `help` for the manual)"),nl.

safe_clear_all:-
    retractall(knowledge(_, _)),
    retractall(already_asked(_)),
    !.

safe_clear_all:- !.    

check_heart_risk:-
    safe_clear_all,
    is_true(knowledge(heart_risk, true)),!,
    write('The patient is in heart risk.'),nl.

check_heart_risk:-
    write('NO HEART RISK!'),nl.

human_readable_question(cp_only_exercise):-
    write('Is there chest pain only during exercise? (yes/no) : '), nl, !.

human_readable_question(max_exer_difficulty):-
    write('What is the maximum difficulty reached on the ECG treadmil test?  (easy/medium/hard) '), nl, !.    

human_readable_question(cholesterol_level):-
    write('What is the patients cholesterol level?  (low/normal/high) '), nl, !.   

human_readable_question(blood_sugar):-
    write('What is the patients blood sugar level?  (low/high) '), nl, !. 

human_readable_question(resting_bp):-
    write('What is the patients resting blood pressure?  (low/normal/high) '), nl, !. 

human_readable_question(gender):-
    write('What is the patients gender? (male/female) '), nl, !.       

get_multianswer_options(cp_only_exercise, [yes, no]).

get_multianswer_options(max_exer_difficulty, [easy, medium, hard]).

get_multianswer_options(cholesterol_level, [low, normal, high]).

get_multianswer_options(blood_sugar, [low, high]).

get_multianswer_options(resting_bp, [low, normal, high]).

get_multianswer_options(gender, [male, female]).

ask_user(Attr, Val, Options):-
    human_readable_question(Attr),
    read(UserAnswer),
    validate_answer(UserAnswer, Attr, Options),!,
    asserta(already_asked(Attr)), % Make sure we remember we asked
    Val = UserAnswer,
    asserta(knowledge(Attr, Val)). % Save the value


% Received a valid answer
validate_answer(UserAnswer, Attr, Options):-
    member(UserAnswer, Options), !.

% Invalid answer, trying again
validate_answer(UserAnswer, Attr, Options):-
    write(UserAnswer), write(" is an invalid answer. Please try again."),nl,
    ask_user(Attr, Val, Options).

is_true(knowledge(Attr1, Val1)):-
    explore(knowledge(Attr1, Val1)).

explore(knowledge(Attr1, Val1)):- 
    %write("Evaluating "), write(Attr1), write(" in regular exploration"),nl,
    knowledge(Attr1, Val1), !.

explore(knowledge(Attr1, Val1)):- 
    %write("Evaluating "), write(Attr1), write(" in askable exploration"),nl,
    askable(Attr1), 
    \+ knowledge(Attr1, _),
    \+ already_asked(Attr1),
    get_multianswer_options(Attr1, Options),
    ask_user(Attr1, Val, Options),
    Val == Val1. % Make sure the user answer is what we are exploring

explore(P1 and P2):-
    % write("Evaluating "), write(Attr1), write(" in AND exploration"),nl,
    explore(P1),
    explore(P2),!.

explore(P1 or P2):-
    (
        explore(P1)
        ;
        explore(P2)
    ), !.    

explore(P):-
    if Cond then P,
    explore(Cond).

