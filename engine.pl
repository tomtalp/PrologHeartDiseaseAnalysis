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

/*
    Define all our basic facts as "askable", so that the user will be
    asked about them.
*/
askable(blood_sugar).
askable(resting_bp).
askable(max_exer_difficulty).
askable(cp_only_exercise).
askable(cholesterol_level).
askable(gender).      

/*
    Start our inference process - check whether we can detect a heart risk
*/
check_heart_risk:-
    is_true(knowledge(heart_risk, HasRisk), Proof),
    nl,
    print_diagnosis(HasRisk),
    nl,
    print_diagnosis_proof(Proof),
    nl, nl.

/*
    Print the diagnosis results
*/
print_diagnosis(true):-
    !, write("## The patient is in heart risk! ##"),nl.

print_diagnosis(false):-
    !, write("## The patient doesn't have a heart risk! ##"), nl.    

/*
    Print the diagnosis proof
*/
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


/*
    Print the questions for the user in a nicely formatted way
*/
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

/*
    Get a list of possible answers for a given question
*/
get_multianswer_options(cp_only_exercise, [yes, no]).

get_multianswer_options(max_exer_difficulty, [easy, medium, hard]).

get_multianswer_options(cholesterol_level, [low, normal, high]).

get_multianswer_options(blood_sugar, [low, high]).

get_multianswer_options(resting_bp, [low, normal, high]).

get_multianswer_options(gender, [male, female]).

/*
    Ask the user to insert an answer for a given attribute -
    We first print the attribute as a human readable question, then read the answer and process it.
*/
ask_user(Attr, Val, Options, Proof, Trace):-
    human_readable_question(Attr),
    read(UserAnswer),
    process_answer(UserAnswer, Attr, Val, Options, Proof, Trace).

/*
    Process the user answer - check whether it's valid, and if it is - save it.
*/
process_answer(UserAnswer, Attr, Val, Options, Proof, _):-
    validate_answer(UserAnswer, Attr, Options),!,
    asserta(already_asked(Attr)), % Make sure we remember we asked
    Val = UserAnswer,
    Proof = Attr:Val,
    asserta(knowledge(Attr, Val)). % Save the value

/*
    Check if the answer from the user is valid (that is, it's a part of the possible answers for this question)
*/
validate_answer(UserAnswer, _, Options):-
    member(UserAnswer, Options), !.    

/*
    The answer the user entered is invalid, so we try asking again
*/
validate_answer(UserAnswer, Attr, Options):-
    write(UserAnswer), write(" is an invalid answer. Please try again."),nl,
    ask_user(Attr, _, Options, _, _).

/*
    Begin the exploration process - Receive the "knowledge" part we're trying to
    figure out (i.e. whether there's a heart risk), and begin exploring by backwards chaining.
*/
is_true(knowledge(Attr1, Val1), Proof):-
    explore(knowledge(Attr1, Val1), Proof, []).

/*
    Simple exploration - the attribute we're looking for is stated as an existing 
    knowledge piece
*/
explore(knowledge(Attr1, Val1), Attr1:Val1, _):- 
    knowledge(Attr1, Val1), !.

/*
    Askable exploration - the attribute we're looking for needs to be manually answered by 
    the user. 
    Before we ask the user we make sure we don't already have the answer
*/

explore(knowledge(Attr1, Val1), Proof, Trace):- 
    askable(Attr1), 
    \+ knowledge(Attr1, _),
    \+ already_asked(Attr1),
    get_multianswer_options(Attr1, Options),
    ask_user(Attr1, Val, Options, Proof, Trace),
    Val == Val1. % Make sure the user answer is what we are exploring

/*
    The question we're trying to figure is made up of 2 separate parts, combined with an AND operator.
    This means we need to make sure both are valid.
*/
explore(P1 and P2, Proof1 and Proof2, Trace):-
    explore(P1, Proof1, Trace1),
    explore(P2, Proof2, Trace2),
    conc(Trace1, Trace2, Trace),
    !.

/*
    The question we're trying to figure is made up of 2 separate parts, combined with an OR operator.
    This means we need to make sure that at least one is valid.
*/
explore(P1 or P2, Proof, Trace):-
    (
        explore(P1, Proof, Trace)
        ;
        explore(P2, Proof, Trace)
    ), !.    

/*
    The question we're trying to figure is made up of an IF/THEN rule - We'll check if the "IF"
    part is correct, and then we can derive that the "THEN" part is right.
*/
explore(P, P <= CondProof, Trace):-
    if Cond then P,
    explore(Cond, CondProof, [if Cond then P | Trace]).
