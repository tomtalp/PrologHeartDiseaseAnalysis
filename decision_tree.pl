:- ensure_loaded(training_data).

/*
    Delete an element from the list
*/
del(X, [X | Rest], Rest):- !.
del(X, [Y | Rest0], [Y | Rest]):-
    del(X, Rest0, Rest).

/*
    Attributes in our training data
*/
attribute(gender, [male, female]).
attribute(resting_bp, [low, normal, high]).
attribute(cholesterol_level, [low, normal, high]).
attribute(blood_sugar, [low, high]).
attribute(cp_only_exercise, [yes, no]).
attribute(max_exer_difficulty, [easy, medium, hard]).

/*
    build_decision_tree(Tree)

    Build the decision tree - load all the examples, and look for the best splits in every branch.    
*/
build_decision_tree(Tree):-
    findall(example(Class, Obj), example(Class, Obj), Examples),
    findall(Att, attribute(Att, _), Attributes),
    build_decision_tree(Attributes, Examples, Tree),!.

/* No examples to learn from */
build_decision_tree(_, [], null):- !.

/* No attributes left */
build_decision_tree([], _, null):- !.

/* Only one class in our dataset */
build_decision_tree(_, [example(Class, _) | Examples], leaf(Class)):- 
    \+ (
        member(example(Class2, _), Examples),
        Class2 \== Class
    ), !.

/*
    Building the tree, an actual step - Select the best attribute for a split (via the Gini impurity metric)
    and recursively build the rest of the tree from the remaining attributes
*/
build_decision_tree(Attributes, Examples, tree(Attribute, SubTrees)):-
    choose_attribute(Attributes, Examples, Attribute), % Select the best attribute
    del(Attribute, Attributes, RemainingAttributes),
    attribute(Attribute, Values), % Get the possible values of the selected attribute
    build_decision_trees(Attribute, Values, RemainingAttributes, Examples, SubTrees).


/*
    build_decision_trees(Attribute, AttrValues, RemainingAttributes, Examples, SubTrees)
    An extension to the regular tree building operation - take the selected attribute for the
    split, take all different attribute values from the training set, and split into sub-trees
*/
build_decision_trees(_, [], _, _, []).

build_decision_trees(Att, [Val1 | Vals], RestAtts, Examples, [Val1 : Tree1 | Trees]):-
    get_subset_by_feature_query(Att = Val1, Examples, ExampleSubset),
    build_decision_tree(RestAtts, ExampleSubset, Tree1),
    build_decision_trees(Att, Vals, RestAtts, Examples, Trees).

/*
    check_clause(AttributeValues, SearchClauses)

    Verify that the attribute values list contains the search clause, and nothing that contradicts it
*/
check_clause(AttributeValues, SearchClauses):-
    \+ (
        member(Att = Val, SearchClauses),
        member(Att = ValX, AttributeValues),
        ValX \== Val
    ).

/*
    get_subset_by_feature_query(Feature = Value, Examples, ExampleSubset)

    Get all the training examples that match a given search clause (Feature = Value)
*/
get_subset_by_feature_query(Feature = Value, Examples, ExampleSubset):-
    findall(
        example(Class, Obj), 
        (
            member(example(Class, Obj), Examples),
            check_clause(Obj, [Feature = Value])
        ),
        ExampleSubset
    ).        


/*
    get_squared_sum_of_freqs(FrequenciesList, FrequenciesSum, SquaredSum)

    Receive a list representing frequencies, the sum of all frequencies, and return
    the squared sum of the probabilities. This will be subtracted from 1 to calc the Gini impurity metric
    Sum equation = (Freq_0 / FrequenciesSum)^2 + (Freq_1 / FrequenciesSum)^2 + ... + (Freq_n / FrequenciesSum)^2

    e.g. - 
        FrequenciesList = [ClassA/3, ClassB/2, ClassC/2],
        FrequenciesSum = 7,
        SquaredSum ---> (3/7)**2 + (2/7)**2 + (2/7)**2 ---> 0.3469
*/

get_squared_sum_of_freqs([], _, 0).
get_squared_sum_of_freqs([_/Freq | T], FrequenciesSum, Sum):-
    N is (Freq / FrequenciesSum) ** 2,
    get_squared_sum_of_freqs(T, FrequenciesSum, Sum1),
    Sum is Sum1 + N.

/*
    get_gini_impurity(Examples, FeatureName, FeatureValue, GiniImpurity)

    Calculate the Gini impurity value for a given feature split.
    We take all the training data with FeatureName = FeatureValue, calculate the frequencies
    of each class, then subtract the squared sum of the class probabilities from 1. 
    A more in depth explanation can be found in the project docs, page <TODO>

    e.g. - 
        Examples = [example(ClassA, _), example(ClassA, _), example(ClassB, _), ......],
        FeatureName = dummy_feature,
        FeatureValue = Blabla,
        GiniImpurity ---> 0.6531

    Running test - 
    findall(example(Class, Obj), example(Class, Obj), Examples), get_gini_impurity(Examples, size, small, GiniImpurity).
*/
get_gini_impurity(ExamplesSubset, SubsetLength, GiniImpurity):-
    count_freqs(ExamplesSubset, Frequencies),
    get_squared_sum_of_freqs(Frequencies, SubsetLength, Sum),
    GiniImpurity is 1 - Sum.

/*
    get_gini_impurity_for_feature(TrainingSet, FeatureName, FeatureGiniImpurity)

    Calculate the Gini impurity for an entire feature, by performing a weighted sum of the Gini impurity values 
    of every feature value for the given feature

    e.g. - 
        TrainingSet = [example(ClassA, _), example(ClassA, _), example(ClassB, _), ......],
        FeatureName = test_feature,
        FeatureGiniImpurity ---> 0.62516


    findall(example(Class, Obj), example(Class, Obj), TrainingSet), get_gini_impurity_for_feature(TrainingSet, size, Gini)
*/

get_gini_impurity_for_feature(TrainingSet, FeatureName, FeatureGiniImpurity):-
    attribute(FeatureName, FeatureValues),
    length(TrainingSet, TrainingSetSize),
    get_gini_impurities(TrainingSet, TrainingSetSize, FeatureName, FeatureValues, FeatureGiniImpurity).

/*
    get_gini_impurities(TrainingSet, TrainingSetSize, FeatureName, FeatureValues, ImpurityWeightedSum)

    Perform a sum of the Gini impurity values, for every possible value in our feature-value list.
    We go over all the values for the given feature, extract the training dataset with that value and
    calculate the Gini impurity for that single value. Then we perform a weighted sum for all impurities.
    Further explanation in the project document, page <TODO>
*/
get_gini_impurities(_, _, _, [], 0):- !.

get_gini_impurities(TrainingSet, TrainingSetSize, FeatureName, [FeatureValue | FeatureValueList], ImpurityWeightedSum):-
    /* Extract all training examples with the given value */
    get_subset_by_feature_query(FeatureName = FeatureValue, TrainingSet, TrainingSubset),
    
    /* Calculate the Gini impurity for this training set */
    length(TrainingSubset, SubsetSize),
    get_gini_impurity(TrainingSubset, SubsetSize, GiniImpurity),

    /* Recursively get the sum for all other feature values */
    get_gini_impurities(TrainingSet, TrainingSetSize, FeatureName, FeatureValueList, ImpurityWeightedSum1),

    /* Add up the Gini values according to their weight in the entire dataset */
    ImpurityWeightedSum is ImpurityWeightedSum1 + ((SubsetSize / TrainingSetSize) * GiniImpurity).
    

/*  
    count_freqs(Examples, Frequencies)
    Count frequencies for every class in a given example subset
    e.g. -
        Examples = [example(ClassA, _), example(ClassA, _), example(ClassB, _)],
        Frequencies ---> [ClassA/2, ClassB/1]
*/
count_freqs([], Res):-
    findall(Class/Freq, retract(freq(Class, Freq)), Res), !.

count_freqs([example(Class, _) | T], Res):-
    (
        (
            retract(freq(Class, N)),!,
            N1 is N+1,
            assert(freq(Class, N1))
        )
        ;
        assert(freq(Class, 1))
    ),
    count_freqs(T, Res).


/*
    choose_attribute(Atts, Examples, BestAtt)

    Select an attribute from the attributes, that has the minimum Impurity metric.
    `setof` will sort the results for us, so that the head of the list will contain the attribute with the
    lost Gini impurity metric
*/
choose_attribute(Atts, Examples, BestAtt):-
    setof(
        Impurity/Att,
        (
            member(Att, Atts), get_gini_impurity_for_feature(Examples, Att, Impurity)
        ),
        [_/BestAtt | _]
    ).
