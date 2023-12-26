
%%%% -*- Mode: Prolog -*-

%%%% oop.pl
%%%%
%%%% Tamburini Pietro 894628
%%%%

%%% color/2
%%%
%%% Returns the ANSI color code string for a given color.
%%% Succeeds when provided with a known color name.
color(red, "\033[31m").
color(green, "\033[32m").
color(blue, "\033[34m").
color(reset, "\033[0m").


%%% write_colored/2.
%%%
%%% Writes text in a specified color.
%%% Succeeds for any text and a defined color.
write_colored(Color, Text) :-
    color(Color, ColorCode),
    write(ColorCode),
    write(Text),
    color(reset, ResetCode),
    write(ResetCode).


%%% type_check/2.
%%%
%%% Checks if a value matches a specified type. Succeeds if the value
%%% matches the type or if the type is not specified.
%%% Handles 'integer' and 'float' types.
type_check(Value, Type) :- 
    (Type == integer -> integer(Value) ; true),
    (Type == float -> float(Value) ; true).


%%% def_class/2.
%%%
%%% Defines a new class with a given name and parent classes.
%%% Succeeds by asserting the class into the database.
%%% Requires the name of the class and a list of parent classes.
def_class(ClassName, Parents) :-
    % TODO: Check existence of Parents (Classes []) + not already exist
    assertz(class(ClassName, Parents, [])).


%%% def_class/3.
%%%
%%% Extends def_class/2 by also defining parts (fields and methods).
%%% Succeeds by asserting the class with its parts into the database.
def_class(ClassName, Parents, Parts) :-
    % TODO: Check existence of Parents (Classes []) + not already exist
    assertz(class(ClassName, Parents, Parts)),
    assert_parts(ClassName, Parts).


%%% assert_parts/2.
%%%
%%% Recursively asserts each part (field or method) of a class.
%%% Succeeds after asserting all parts.
assert_parts(_, []).
assert_parts(ClassName, [Part | Rest]) :-
    assert_part(ClassName, Part),
    assert_parts(ClassName, Rest).


%%% assert_part/2.
%%%
%%% Asserts a single part (field or method) of a class.
%%% For fields, it checks type compatibility if a type is given.
%%% Succeeds if the part is correctly asserted or fails if the
%%% type check for a field fails.
assert_part(ClassName, field(FieldName, Value)) :-
    assertz(class_field(ClassName, FieldName, Value, _)).
assert_part(ClassName, field(FieldName, Value, Type)) :-
    (   type_check(Value, Type)
    ->  assertz(class_field(ClassName, FieldName, Value, Type))
    ;   atomic_list_concat(
            ['Type check failed for field: ', FieldName],
            ErrorMessage
        ),
        write_colored(red, ErrorMessage),
        fail
    ).
assert_part(ClassName, method(MethodName, ArgList, Form)) :-
    assertz(class_method(ClassName, MethodName, ArgList, Form)). 


%%% remove_overridden/3.
%%%
%%% Removes attributes from a default set if they are overridden
%%% in a given field set.
%%% Succeeds by returning a list of attributes
remove_overridden([], _, []).
remove_overridden([DefAttr|DefAttrs], Fields, Result) :-
    DefAttr = (Name=_),
    (   memberchk(Name=_, Fields)
    ->  remove_overridden(DefAttrs, Fields, Result)
    ;   Result = [DefAttr|Others],
        remove_overridden(DefAttrs, Fields, Others)
    ).


%%% get_attributes/3.
%%%
%%% Retrieves attributes of a class.
%%% If 'IncludeType' is true, it includes attribute types.
%%% Succeeds by returning a list of attributes (with or without types)
%%% for the given class.
get_attributes(ClassName, Attributes, IncludeType) :-
    (   IncludeType
    ->  findall(
            Name:Type=Value,
            class_field(ClassName, Name, Value, Type),
            Attributes
        )
    ;   findall(
            Name=Value,
            class_field(ClassName, Name, Value, _),
            Attributes
        )
    ).


%%% get_methods/2.
%%%
%%% Retrieves methods of a class.
%%% Succeeds by returning a list of methods defined for the class.
get_methods(ClassName, Methods) :-
    findall(
        [Name, Args, Value],
        class_method(ClassName, Name, Args, Value),
        Methods
    ).


%%% get_methods_from_classes/2.
%%% get_methods_from_classes_helper/3.
%%%
%%% Retrieves methods from a list of classes, ensuring uniqueness.
%%% Succeeds with a list of unique methods.
get_methods_from_classes(ClassList, AllAttrs) :-
    get_methods_from_classes_helper(ClassList, [], AllAttrs).
get_methods_from_classes_helper([], Acc, Acc).
get_methods_from_classes_helper([ClassName|Others], Acc, AllAttrs) :-
    get_methods(ClassName, Attributes),
    add_unique_methods(Attributes, Acc, NewAcc),
    get_methods_from_classes_helper(Others, NewAcc, AllAttrs).


%%% add_unique_methods/3.
%%%
%%% Adds unique methods to an accumulator list, avoiding duplicates.
%%% Succeeds with an updated list of unique methods.
add_unique_methods([], Acc, Acc).
add_unique_methods([Attr|Attrs], Acc, NewAcc) :-
    Attr = [Name, Args, _],
    memberchk([Name, Args, _], Acc),
    add_unique_methods(Attrs, Acc, NewAcc).
add_unique_methods([Attr|Attrs], Acc, NewAcc) :-
    Attr = [Name, Args, _],
    \+ memberchk([Name, Args, _], Acc),
    add_unique_methods(Attrs, [Attr|Acc], NewAcc).


%%% get_fields_from_classes/3.
%%%
%%% Retrieves attributes from a list of classes, considering 'Type'.
%%% Succeeds with a list of unique attributes.
get_fields_from_classes(ClassList, AllAttrs, Type) :-
    get_attrs_from_classes_helper(ClassList, [], AllAttrs, Type).
get_attrs_from_classes_helper([], Acc, Acc, _).
get_attrs_from_classes_helper([ClassName|Others],Acc,AllAttrs,Type) :-
    get_attributes(ClassName, Attributes, Type),
    add_unique_attributes(Attributes, Acc, NewAcc),
    get_attrs_from_classes_helper(Others, NewAcc, AllAttrs, Type).


%%% add_unique_attributes/3.
%%%
%%% Adds unique attributes to an accumulator list, avoiding duplicates
%%% Succeeds with an updated list of unique attributes.
add_unique_attributes([], Acc, Acc).
add_unique_attributes([Attr|Attrs], Acc, NewAcc) :-
    attribute_name(Attr, AttrName),
    memberchk(AttrName=_, Acc),
    add_unique_attributes(Attrs, Acc, NewAcc).
add_unique_attributes([Attr|Attrs], Acc, NewAcc) :-
    attribute_name(Attr, AttrName),
    \+ memberchk(AttrName=_, Acc),
    add_unique_attributes(Attrs, [Attr|Acc], NewAcc).


%%% attribute_name/2.
%%%
%%% Extracts the name of an attribute from its representation.
%%% Succeeds with the name of the attribute.
attribute_name(Name=_Value, Name).
attribute_name(Name:_Type=_Value, Name).


%%% classes_chain/2.
%%%
%%% Constructs a list of a class and its superclasses.
%%% Succeeds with a list starting from the class up to its
%%% highest ancestor.
classes_chain(ClassName, Result) :-
    find_superclasses(ClassName, [ClassName], ReversedResult),
    reverse(ReversedResult, Result).


%%% find_superclasses/3.
%%%
%%% Recursively finds all superclasses of a given class.
%%% Succeeds with an accumulated list of superclasses.
find_superclasses(ClassName, Accumulator, Result) :-
    class(ClassName, SuperClasses, _),
    process_superclasses(SuperClasses, Accumulator, Result).


%%% process_superclasses/3.
%%%
%%% Processes each superclass, avoiding circular references.
%%% Succeeds with a list of unique superclasses.
process_superclasses([], Acc, Acc).
process_superclasses([SuperClass|SuperClasses], Acc, Result) :-
    \+ member(SuperClass, Acc),
    find_superclasses(SuperClass, [SuperClass|Acc], PartialResult),
    process_superclasses(SuperClasses, PartialResult, Result).
process_superclasses([SuperClass|SuperClasses], Acc, Result) :-
    member(SuperClass, Acc),
    process_superclasses(SuperClasses, Acc, Result).


%%% check_fields_type/2.
%%%
%%% Checks each field in the provided list against the default
%%% attributes with types.
%%% Fails with an error message if a type mismatch is found.
check_fields_type([], _).
check_fields_type([Field|Rest], DefaultFieldsTyped) :-
    Field = (Name=Value),
    memberchk(Name:Type=_, DefaultFieldsTyped),
    (   type_check(Value, Type)
    ->  check_fields_type(Rest, DefaultFieldsTyped)
    ;   format(
            string(FormattedMessage),
            'Type check failed for ~w. value: ~w, expected type ~w',
            [Name, Value, Type]
        ),
        write_colored(red, FormattedMessage),
        fail
    ).


%%% make/2, make/3.
%%%
%%% Creates an instance of a class with optional specified fields.
%%% Validates the fields against the class definition,
%%% ensuring type correctness.
%%% Succeeds by asserting the instance into the database.
make(InstanceName, ClassName) :-
    make(InstanceName, ClassName, []).

make(InstanceName, ClassName, Params) :-
    classes_chain(ClassName, ChainedClasses),
    get_fields_from_classes(
        ChainedClasses,
        DefaultFieldsTyped,
        true
    ),
    !,
    check_fields_type(Params, DefaultFieldsTyped),
    get_fields_from_classes(ChainedClasses, DefaultFields, false),
    remove_overridden(DefaultFields, Params, UpdatedFields),
    append(Params, UpdatedFields, FinalFields),
    get_methods_from_classes(ChainedClasses, Methods),
    forall(
        member([Name, Args, Method], Methods),
        add_method(InstanceName, Name, Args, Method)
    ),
    InstanceTerm = instance(ClassName, FinalFields),
    (   atom(InstanceName)
        ->  (   clause(instance(InstanceName, _, _), true)
            ->  fail
            ;   assertz(
                    instance(InstanceName, ClassName, FinalFields)
                )
            )
        ;   var(InstanceName)
        ->  InstanceName = InstanceTerm,
            (   clause(instance(InstanceName, _, _), true)
            ->  fail
            ;   assertz(
                    instance(InstanceName, ClassName, FinalFields)
                )
            )
        ;   InstanceName = InstanceTerm
    ).


%%% add_method/3.
%%%
%%% Adds a method of an instance.
%%% Constructs the method predicate and asserts it into the database.
%%% Handles the method body using 'add_method_helper'.
add_method(InstanceName, MethodName, Args, Actions) :-
    Predicate =.. [MethodName, InstanceName | Args],
    add_method_helper(InstanceName, Actions, FinalActions),
    assertz((Predicate :- FinalActions)).


%%% add_method_helper/2.
%%%
%%% Helps in constructing the method body for 'add_method'.
%%% It modifies the method's actions to include the instance
%%% name when necessary.
add_method_helper(InstanceName, Method, FinalMethod) :-
    term_to_list(Method, MethodAsListReversed),
    reverse(MethodAsList,MethodAsListReversed),
    maplist(
        replace_this_in_predicate(InstanceName),
        MethodAsList,
        FinalMethodAsList
    ),
    list_to_term(FinalMethodAsList, FinalMethod).


%%% replace_this_in_predicate/2.
%%%
%%% Replaces occurrences of 'this' with the instance name
%%% in a predicate.
%%% Used in method definitions to refer to the current instance.
replace_this_in_predicate(InstanceName, Predicate, FinalPredicate) :-
    (   is_list(Predicate)
    ->  Lista = Predicate
    ;   Predicate =.. Lista
    ),
    maplist(replace_this_in_value(InstanceName), Lista, ResultList),
    (   is_list(Predicate)
    ->  FinalPredicate = ResultList
    ;   FinalPredicate =.. ResultList
    ).


%%% replace_this_in_value/2.
%%%
%%% Helper for 'replace_this_in_predicate'.
%%% Replaces 'this' with the instance name in a value,
%%% recursively handling complex structures.
replace_this_in_value(InstanceName, Value, Result) :-
    (   atom_or_string(Value)
    ->  (   Value = this
        ->  Result = InstanceName
        ;   Result = Value
        )
    ;   (   var(Value)
        ->  Result = Value
        ;   replace_this_in_predicate(InstanceName, Value, Result)
        )
    ).


%%% atom_or_string/1.
%%%
%%% Checks if a value is either an atom or a string.
atom_or_string(Value) :-
    (   atom(Value)
    ;   string(Value)
    ).


%%% term_to_list/2.
%%%
%%% Converts a complex term into a list of terms.
%%% Used for processing method bodies in 'add_method_helper'.
term_to_list(Term, List) :-
    term_to_list_helper(Term, [], List).


%%% term_to_list_helper/3.
%%%
%%% Recursive helper for 'term_to_list'.
%%% Breaks down complex terms into their constituent parts.
term_to_list_helper((Term1, Term2), Accumulator, List) :-
    !,
    term_to_list_helper(Term1, Accumulator, NewAccumulator),
    term_to_list_helper(Term2, NewAccumulator, List).
term_to_list_helper(Term, Accumulator, [Term|Accumulator]).


%%% list_to_term/2.
%%%
%%% Converts a list of terms back into a complex term.
%%% Reverse of 'term_to_list', used in 'add_method_helper'.
list_to_term(List, Term) :-
    list_to_term_helper(List, Term).


%%% list_to_term_helper/3.
%%%
%%% Recursive helper for 'list_to_term'.
%%% Revert list into complex terms.
list_to_term_helper([LastTerm], LastTerm) :- !.
list_to_term_helper([Term1 | Rest], (Term1, CompoundTerm)) :-
    list_to_term_helper(Rest, CompoundTerm).


%%% is_class/1.
%%%
%%% Checks if a given name corresponds to a defined class.
is_class(ClassName) :- class(ClassName, _, _).


%%% is_instance/1, is_instance/2.
%%%
%%% Checks if a given term is an instance of a class.
%%% 'is_instance/2' additionally checks if it's
%%% an instance of a specific class.
is_instance(Instance) :- instance(Instance, _, _).
is_instance(Instance, ClassName) :-
    instance(Instance, InstanceClass, _),
    is_subclass(InstanceClass, ClassName).


%%% is_subclass/2.
%%%
%%% Determines if one class is a subclass of another.
%%% Checks direct and indirect (transitive) subclass relationships.
is_subclass(Class, Class).
is_subclass(SubClass, Class) :-
    class(SubClass, SuperClasses, _),
    member(Class, SuperClasses).
is_subclass(SubClass, Class) :-
    class(SubClass, SuperClasses, _),
    member(SuperClass, SuperClasses),
    is_subclass(SuperClass, Class).


%%% inst/2.
%%%
%%% Retrieves an instance's details given its name.
%%% Useful for querying instances in the database.
inst(InstanceName, Instance) :-
    atom(InstanceName),
    instance(InstanceName, ClassName, Params),
    Instance = instance(InstanceName, ClassName, Params).


%%% field/3.
%%%
%%% Retrieves the value of a specified field from an instance.
field(Instance, AttributeName, Value) :-
    instance(Instance, _, Attributes),
    member(AttributeName=Value, Attributes).


%%% fieldx/3.
%%%
%%% Extended version of 'field/3'.
%%% Allows accessing nested fields within an instance
%%% or its related instances.
fieldx(Instance, [FieldName|FieldNames], Result) :-
    field(Instance, FieldName, IntermediateResult),
    fieldx_recursive(IntermediateResult, FieldNames, Result).


%%% fieldx_recursive/3.
%%%
%%% Recursive helper for 'fieldx'.
%%% Traverses through nested fields to retrieve the final value.
fieldx_recursive(Intermediate, [], Intermediate).
fieldx_recursive(Intermediate, [FieldName|FieldNames], Result) :-
    field(Intermediate, FieldName, NextIntermediate),
    fieldx_recursive(NextIntermediate, FieldNames, Result).


% Initialization
:- initialization(init).

init :-
    write_colored(green, '\nIl file oop.pl è stato caricato correttamente.\n'), nl,
    def_class(person, [], [ field(name, 'Eve'), field(age, 21, integer)]),
    def_class(
        student,
        [person],
        [
            field(name, 'Eva Lu Ator'),
            field(university, 'Berkeley'),
            method(
                talk,
                [],
                (
                    write('My name is '),
                    field(this, name, N),
                    writeln(N),
                    write('My age is '),
                    field(this, age, A),
                    writeln(A)
                )
            )
        ]
    ),
    def_class(
        studente_bicocca,
        [student],
        [
            field('university', 'UNIMIB'),
            method(
                to_string,
                [ResultingString],
                with_output_to(
                        string(ResultingString),
                        (
                            field(this, name, N),
                            field(this, university, U),
                            format('#<~w student ~w>', [U, N])
                        )
                    )
                
            ),
            method(
                talk,
                [],
                (
                    write('Mi chiamo '),
                    field(this, name, N),
                    writeln(N),
                    writeln('e studio alla Bicocca.')
                )
            )
        ]
    ),
    def_class(
        obj,
        [person],
        [
            field(other, A)
        ]
    ),
    make(p, person, [age=99, name="custom_name"]),
    make(s, student),
    make(sb, studente_bicocca, [name="ssss", age=999]),
    make(obj1, obj, [other=obj2]),
    make(obj2, obj, [name=obj3]),
    make(obj3, obj, [name='ok']).

    
% fieldx(obj1, [other, name, name], R).