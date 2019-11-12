/************************************************************************************
*
* main.pl 
* Prolog Heart Disease Analyzer - OpenU final Prolog project. 
* Tom Talpir, 2019
*
* This is an expert system for analyzing heart risk. 
*
* Based on the UCI (University Of California, Irvine) heart disease dataset, the
* system builds a decision tree which helps creating IF/THEN Prolog rules.
*
* These rules allow us to create an `Expert System` that evaulates the heart risk
* for a patient.
*
************************************************************************************/

:- ensure_loaded(engine).
:- ensure_loaded(utils).

/*
    Start the analysis process
*/
start:-
    write('########################################################'),nl,
    write('# Welcome to the Heart Disease Analysis expert system! #'),nl,
    write('########################################################'),nl,
    repeat,
    print_menu_msg,
    nl,
    write('>>> '),
    read(Action),
    nl,
    execute_action(Action),
    Action == quit.

/*
    execute_action(Action)
    Receive an action from the user input and act accordingly
*/
execute_action(diagnose):-
    !, 
    write('### Starting a diagnosis'),
    nl, 
    safe_clear_all,
    check_heart_risk.    

execute_action(help):-
    !,
    print_help_msg,
    nl.    

execute_action(quit).

execute_action(Unknown):-
    write("Received an illegal operation `"), write(Unknown), write("`, please try again (type `help` for the manual)"),nl.


