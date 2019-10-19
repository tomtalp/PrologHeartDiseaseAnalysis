/************************************************************************************
*
* utils.pl 
* Prolog Heart Disease Analyzer - OpenU final Prolog project. 
* Tom Talpir, 2019
*
* Utility functions for hte Hear Disease Analyzer
*
************************************************************************************/

:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).
:- op(800, xfx, <=).

:- dynamic(already_asked/1).
:- dynamic(knowledge/2).

/*
    Print the menu message that's going to be repeated throughout the program.
*/
print_menu_msg:-
    write('Please type your action -'),nl,
    tab(4), write('diagnose (to perform a diagnosis)'),nl,
    tab(4), write('help (to receive help about the program)'),nl,
    tab(4), write('quit (to exit)'),nl.

/*
    Clear our database before performing a new diagnosis - this assures us that we're performing
    a new diagnosis without relying on previously gained information
*/
safe_clear_all:-
    retractall(knowledge(_, _)),
    retractall(already_asked(_)),
    !.

/*
    Nothing to clear (hence retractall fails), so just pass.
*/
safe_clear_all:- !.

/*
    Print a help message for the menu, explaining the program functionality
*/
print_help_msg:-
    write('Welcome to the Prolog Heart Disease Analysis expert system!'),
    nl,
    write('The system helps doctors perform a heart risk analysis for patients seeking a diagnosis, after experiencing chest'),
    nl,
    write('pain & going through several medical exams. The diagnosis requires answers to a few questions about the patient.'),
    nl,
    write('The result of the analysis is binary - did we detect a risk or not.'),
    nl,
    nl,
    nl,
    write('These are the questions that need to be answered to perform a diagnosis - '),
    nl,
    nl,
    tab(4), write('1. Is the chest pain experienced only during exercise, or is it constant?'),
    nl,
    tab(8), write('The answer should be yes or no, indicating whether the chest pain occurs only during exercise or not.'),
    nl,nl,
    tab(4), write('2. What is the maximum difficulty reached on the Cardiac stress test (ECG treadmil)?'),
    nl,
    tab(8), write('The difficulty ranges between easy, medium and hard - Easy means the patient did not reach high'),
    nl,
    tab(8), write('intensities during the treadmil test, while hard means the patient reached the highest intensity level.'),
    nl,nl,
    tab(4), write('3. Cholesterol level - The patients cholesterol level results in the blood test'),
    nl,
    tab(8), write('Cholesterol levels vary between low, normal and high.'),
    nl,nl,
    tab(4), write('4. Gender - Is the patient a male or a female?'),
    nl,nl,
    tab(4), write('5. Resting blood pressure - What is the patients blood pressure while resting?'),
    nl,
    tab(8), write('BP levels vary between low, normal and high.'),
    nl,nl,
    tab(4), write('6. Blood sugar levels - The concentration of glucose present in the blood test'),
    nl,
    tab(8), write('Blood sugar levels vary between low and high.'),

    nl, nl, write('-------------------------------------------------------').

/*
    Concatenate two lists into a third list
*/
conc([], Ys, Ys).

conc([X | Xs], Ys, [X | Zs]):-
    conc(Xs, Ys, Zs).