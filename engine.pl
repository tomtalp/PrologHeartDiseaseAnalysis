/************************************************************************************
*
* engine.pl 
* Prolog Heart Disease Analyzer - OpenU final Prolog project. 
* Tom Talpir, 2019
*
* The inference engine - finds the rules that match the user answers, to make
* a diagnosis.
*
************************************************************************************/

:- ensure_loaded(utils).
:- ensure_loaded(rules).

askable(blood_sugar).
askable(resting_bp).
askable(max_exer_difficulty).
askable(cp_only_exercise).
askable(cholesterol_level).
askable(gender).      

check_heart_risk:-
    is_true(knowledge(heart_risk, HasRisk), Proof),
    nl,
    print_diagnosis(HasRisk),
    nl,
    print_diagnosis_proof(Proof),
    nl, nl.

print_diagnosis(true):-
    !, write("## The patient is in heart risk! ##"),nl.

print_diagnosis(false):-
    !, write("## The patient doesn't have a heart risk! ##"), nl.    

print_diagnosis_proof(knowledge(heart_risk, Diagnosis) <= Proof):-
    !,
    write("How was the diagnosis that `heart risk = "), write(Diagnosis), write("` reached? "),
    nl,
    print_diagnosis_proof(Proof).

print_diagnosis_proof(Proof1 and Proof2):-
    !, 
    print_diagnosis_proof(Proof1),
    print_diagnosis_proof(Proof2).

print_diagnosis_proof(Attr:Val):-
    !, 
    tab(4), human_readable_question(Attr), tab(8), write(Val), nl.    


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

%process_answer(UserAnswer, Attr, Val, Options, Attr:Val <= was_told, _):-
process_answer(UserAnswer, Attr, Val, Options, Proof, _):-
    validate_answer(UserAnswer, Attr, Options),!,
    asserta(already_asked(Attr)), % Make sure we remember we asked
    Val = UserAnswer,
    Proof = Attr:Val,
    asserta(knowledge(Attr, Val)). % Save the value

% Received a valid answer
validate_answer(UserAnswer, _, Options):-
    member(UserAnswer, Options), !.    

% Invalid answer, trying again
validate_answer(UserAnswer, Attr, Options):-
    write(UserAnswer), write(" is an invalid answer. Please try again."),nl,
    ask_user(Attr, _, Options, _, _).

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
