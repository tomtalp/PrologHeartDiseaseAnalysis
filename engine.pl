:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).
:- op(800, xfx, <=).

:- dynamic(already_asked/1).
:- dynamic(knowledge/2).

:- ensure_loaded(rules).

askable(blood_sugar).
askable(resting_bp).
askable(max_exer_difficulty).
askable(cp_only_exercise).
askable(cholesterol_level).
askable(gender).    

conc([],Ys,Ys).
conc([X|Xs],Ys,[X|Zs]):-conc(Xs,Ys,Zs).

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
    !, write('Performing diagnose!'),nl, 
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
    is_true(knowledge(heart_risk, HasRisk), Proof),!,
    write("Proof = "),nl,write(Proof),nl,
    print_diagnosis(HasRisk).

print_diagnosis(true):-
    !, write("The patient is in heart risk!"),nl.

print_diagnosis(false):-
    !, write("The patient doesn't have a heart risk!"),nl.    


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

ask_user(Attr, Val, Options, Proof, Trace):-
    human_readable_question(Attr),
    read(UserAnswer),
    process_answer(UserAnswer, Attr, Val, Options, Proof, Trace).

/**** WHY - putting on hold for now
process_answer(why, Attr, Val, Options, Proof, Trace):-
    !,
    display_rule_chain(Trace, 0),nl,
    %write('THIS IS THE WHY EXPLANATION!'),nl,
    ask_user(Attr, Val, Options, Proof, Trace).
*/
%process_answer(UserAnswer, Attr, Val, Options, Attr:Val <= was_told, _):-
process_answer(UserAnswer, Attr, Val, Options, Proof, _):-
    validate_answer(UserAnswer, Attr, Options),!,
    asserta(already_asked(Attr)), % Make sure we remember we asked
    Val = UserAnswer,
    Proof = Attr:Val,
    asserta(knowledge(Attr, Val)). % Save the value

% Received a valid answer
validate_answer(UserAnswer, Attr, Options):-
    member(UserAnswer, Options), !.    

% Invalid answer, trying again
validate_answer(UserAnswer, Attr, Options):-
    write(UserAnswer), write(" is an invalid answer. Please try again."),nl,
    ask_user(Attr, Val, Options, Proof, _).

is_true(knowledge(Attr1, Val1), Proof):-
    explore(knowledge(Attr1, Val1), Proof, []).

explore(knowledge(Attr1, Val1), Attr1:Val1, _):- 
    %write("Evaluating "), write(Attr1), write(" in regular exploration"),nl,
    knowledge(Attr1, Val1), !.

explore(knowledge(Attr1, Val1), Proof, Trace):- 
    %write("Evaluating "), write(Attr1), write(" in askable exploration"),nl,
    askable(Attr1), 
    \+ knowledge(Attr1, _),
    \+ already_asked(Attr1),
    get_multianswer_options(Attr1, Options),
    ask_user(Attr1, Val, Options, Proof, Trace),
    Val == Val1. % Make sure the user answer is what we are exploring

explore(P1 and P2, Proof1 and Proof2, Trace):-
    % write("Evaluating "), write(Attr1), write(" in AND exploration"),nl,
    explore(P1, Proof1, Trace1),
    explore(P2, Proof2, Trace2),
    conc(Trace1, Trace2, Trace),
    !.

explore(P1 or P2, Proof, Trace):-
    (
        explore(P1, Proof, Trace)
        ;
        explore(P2, Proof, Trace)
    ), !.    

explore(P, P <= CondProof, Trace):-
    if Cond then P,
    explore(Cond, CondProof, [if Cond then P | Trace]).

/**** WHY - putting on hold for now

display_rule_chain([], _).
display_rule_chain([if C then P | Rules], Indent):-
    nl, tab(Indent), write("To explore whether "), write(P), write(' using rule '),
    nl, tab(Indent), write(if C then P),
    NextIndent is Indent + 2,
    display_rule_chain(Rules, NextIndent).
*/    