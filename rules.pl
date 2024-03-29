/************************************************************************************
*
* rules.pl 
* Prolog Heart Disease Analyzer - OpenU final Prolog project. 
* Tom Talpir, 2019
*
* This file contains the diagnosis rules specification - rules that
* specify what values are required to be diagnosed as having heart risk.
*
* These rules were built via the decision tree, that was based on
* the UCI training data.
*
************************************************************************************/


if 
    knowledge(cp_only_exercise, yes) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, female)
then 
    knowledge(heart_risk, true).

if 
    knowledge(cp_only_exercise, yes) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, male)
    and knowledge(cholesterol_level, low)
then 
    knowledge(heart_risk, false).    

if 
    knowledge(cp_only_exercise, yes) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, male)
    and knowledge(cholesterol_level, low)
then 
    knowledge(heart_risk, false).

if 
    knowledge(cp_only_exercise, yes) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, male)
    and knowledge(cholesterol_level, high)
then 
    knowledge(heart_risk, true).        

if 
    knowledge(cp_only_exercise, yes) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, male)
    and knowledge(cholesterol_level, normal)
    and (knowledge(resting_bp, low) or knowledge(resting_bp, normal))
then 
    knowledge(heart_risk, false).

if 
    knowledge(cp_only_exercise, yes) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, male)
    and knowledge(cholesterol_level, normal)
    and knowledge(resting_bp, high)
then 
    knowledge(heart_risk, true).        

if 
    knowledge(cp_only_exercise, yes) 
    and knowledge(max_exer_difficulty, medium) 
    and (knowledge(cholesterol_level, low) or knowledge(cholesterol_level, normal))
then 
    knowledge(heart_risk, false).        

if 
    knowledge(cp_only_exercise, yes) 
    and knowledge(max_exer_difficulty, medium) 
    and knowledge(cholesterol_level, high)
then 
    knowledge(heart_risk, true).            

if 
    knowledge(cp_only_exercise, yes) 
    and knowledge(max_exer_difficulty, hard) 
then 
    knowledge(heart_risk, false).                

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, female)
then 
    knowledge(heart_risk, true).       

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, male)
    and knowledge(blood_sugar, high)
then 
    knowledge(heart_risk, true).     

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, male)
    and knowledge(blood_sugar, low)
    and knowledge(resting_bp, high)
then 
    knowledge(heart_risk, true). 

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, male)
    and knowledge(blood_sugar, low)
    and knowledge(resting_bp, normal)
    and (knowledge(cholesterol_level, normal) or knowledge(cholesterol_level, high))
then 
    knowledge(heart_risk, true).    

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, male)
    and knowledge(blood_sugar, low)
    and knowledge(resting_bp, normal)
    and knowledge(cholesterol_level, low)
then 
    knowledge(heart_risk, false).  

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, male)
    and knowledge(blood_sugar, low)
    and knowledge(resting_bp, low)
    and (knowledge(cholesterol_level, low) or knowledge(cholesterol_level, normal))
then 
    knowledge(heart_risk, false).       

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, easy) 
    and knowledge(gender, male)
    and knowledge(blood_sugar, low)
    and knowledge(resting_bp, low)
    and knowledge(cholesterol_level, high)
then 
    knowledge(heart_risk, true).   

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, medium) 
    and knowledge(gender, male)
    and knowledge(resting_bp, low)
then 
    knowledge(heart_risk, false).  

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, medium) 
    and knowledge(gender, male)
    and knowledge(resting_bp, normal)
    and (knowledge(cholesterol_level, low) or knowledge(cholesterol_level, normal))
then 
    knowledge(heart_risk, false).      

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, medium) 
    and knowledge(gender, male)
    and knowledge(resting_bp, normal)
    and knowledge(cholesterol_level, high)
then 
    knowledge(heart_risk, true). 

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, medium) 
    and knowledge(gender, male)
    and knowledge(resting_bp, high)
    and knowledge(cholesterol_level, low)
then 
    knowledge(heart_risk, false).  

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, medium) 
    and knowledge(gender, male)
    and knowledge(resting_bp, high)
    and (knowledge(cholesterol_level, normal) or knowledge(cholesterol_level, high))
then 
    knowledge(heart_risk, true).

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, medium) 
    and knowledge(gender, female)
    and knowledge(resting_bp, low)
then 
    knowledge(heart_risk, false).

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, medium) 
    and knowledge(gender, female)
    and knowledge(resting_bp, high)
then 
    knowledge(heart_risk, true).  

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, medium) 
    and knowledge(gender, female)
    and knowledge(resting_bp, normal)
    and knowledge(blood_sugar, low)
then 
    knowledge(heart_risk, false).   

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, medium) 
    and knowledge(gender, female)
    and knowledge(resting_bp, normal)
    and knowledge(blood_sugar, high)
then 
    knowledge(heart_risk, true).                                                      

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, hard) 
    and (knowledge(cholesterol_level, low) or knowledge(cholesterol_level, normal))
then 
    knowledge(heart_risk, false).  

if 
    knowledge(cp_only_exercise, no) 
    and knowledge(max_exer_difficulty, hard) 
    and knowledge(cholesterol_level, high)
then 
    knowledge(heart_risk, true).      
    