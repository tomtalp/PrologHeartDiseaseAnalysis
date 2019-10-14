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
