
% ANSI color codes                                              % Utilities:
color(red, "\033[31m").
color(green, "\033[32m").
color(blue, "\033[34m").
color(reset, "\033[0m").

% function for printing colored text
write_colored(Color, Text) :-
    color(Color, ColorCode),
    write(ColorCode),
    write(Text),
    color(reset, ResetCode),
    write(ResetCode).

clear :- shell('clear').    % per sistemi Unix/Linux
cls :- shell('cls').        % per Windows

% Implementazione dei controlli di tipo
type_check(Value, Type) :- 
    (Type == integer -> integer(Value) ; true),
    (Type == float -> float(Value) ; true).

% END utilities



def_class(ClassName, Parents) :-
    % TODO: Check Esistenza Parents (Classe/i []) is_class
    assertz(class(ClassName, Parents, [])).

def_class(ClassName, Parents, Parts) :-
    assertz(class(ClassName, Parents, Parts)),
    assert_parts(ClassName, Parts).

assert_parts(_, []).                                            % Caso def_class/2 || def_class/3 con Parts []
assert_parts(ClassName, [Part | Rest]) :-                       % Funzione ricorsiva per aggiungere parts (sia field che method)
    assert_part(ClassName, Part),
    assert_parts(ClassName, Rest).

assert_part(ClassName, field(FieldName, Value)) :-
    assertz(class_field(ClassName, FieldName, Value, _)).

assert_part(ClassName, field(FieldName, Value, Type)) :-
    (   type_check(Value, Type)
    ->  assertz(class_field(ClassName, FieldName, Value, Type))
    ;   atomic_list_concat(['Type check failed for field: ', FieldName], ErrorMessage),
        write_colored(red, ErrorMessage),
        fail  % Type Check Failed
    ).

assert_part(ClassName, method(MethodName, ArgList, Form)) :-    % assert method
    assertz(class_method(ClassName, MethodName, ArgList, Form)).      



remove_overridden([], _, []).
remove_overridden([DefAttr|DefAttrs], Fields, Result) :-
    DefAttr = (Name=_),
    (   memberchk(Name=_, Fields)
    ->  remove_overridden(DefAttrs, Fields, Result)
    ;   Result = [DefAttr|Rest],
        remove_overridden(DefAttrs, Fields, Rest)
    ).


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

get_methods(ClassName, Methods) :-
    findall(
        [Name, Args, Value],
        class_method(ClassName, Name, Args, Value),
        Methods
    ).


get_methods_from_classes(ClassList, AllAttributes) :-
    get_methods_from_classes_helper(ClassList, [], AllAttributes).
% Helper per iterare attraverso la lista di classi
get_methods_from_classes_helper([], Accumulator, Accumulator).
get_methods_from_classes_helper([ClassName|RestClasses], Accumulator, AllAttributes) :-
    get_methods(ClassName, Attributes),
    add_unique_methods(Attributes, Accumulator, NewAccumulator),
    get_methods_from_classes_helper(RestClasses, NewAccumulator, AllAttributes).

% Aggiunge metodi all'accumulatore solo se non sono già presenti
add_unique_methods([], Accumulator, Accumulator).
add_unique_methods([Attr|Attrs], Accumulator, NewAccumulator) :-
    Attr = [Name, Args, _],
    memberchk([Name, Args, _], Accumulator), % Verifica se il metodo è già presente
    add_unique_methods(Attrs, Accumulator, NewAccumulator).
add_unique_methods([Attr|Attrs], Accumulator, NewAccumulator) :-
    Attr = [Name, Args, _],
    \+ memberchk([Name, Args, _], Accumulator),
    add_unique_methods(Attrs, [Attr|Accumulator], NewAccumulator).

% Estrae il nome del metodo da un'espressione attributo-valore (o attributo-tipo-valore)
methods_name(Name=_Value, Name).
methods_name(Name:_Type=_Value, Name).




get_attributes_from_classes(ClassList, AllAttributes, IncludeType) :-
    get_attributes_from_classes_helper(ClassList, [], AllAttributes, IncludeType).

% Helper per iterare attraverso la lista di classi
get_attributes_from_classes_helper([], Accumulator, Accumulator, _).
get_attributes_from_classes_helper([ClassName|RestClasses], Accumulator, AllAttributes, IncludeType) :-
    get_attributes(ClassName, Attributes, IncludeType),
    add_unique_attributes(Attributes, Accumulator, NewAccumulator),
    get_attributes_from_classes_helper(RestClasses, NewAccumulator, AllAttributes, IncludeType).

% Aggiunge attributi all'accumulatore solo se non sono già presenti
add_unique_attributes([], Accumulator, Accumulator).
add_unique_attributes([Attr|Attrs], Accumulator, NewAccumulator) :-
    attribute_name(Attr, AttrName),
    memberchk(AttrName=_, Accumulator), % Verifica se l'attributo è già presente
    add_unique_attributes(Attrs, Accumulator, NewAccumulator).
add_unique_attributes([Attr|Attrs], Accumulator, NewAccumulator) :-
    attribute_name(Attr, AttrName),
    \+ memberchk(AttrName=_, Accumulator),
    add_unique_attributes(Attrs, [Attr|Accumulator], NewAccumulator).

% Estrae il nome dell'attributo da un'espressione attributo-valore (o attributo-tipo-valore)
attribute_name(Name=_Value, Name).
attribute_name(Name:_Type=_Value, Name).



classes_chain(ClassName, Result) :-
    find_superclasses(ClassName, [ClassName], ReversedResult),
    reverse(ReversedResult, Result).

find_superclasses(ClassName, Accumulator, Result) :-
    % Ottiene le superclassi della classe corrente
    class(ClassName, SuperClasses, _),
    % Processa ogni superclasse
    process_superclasses(SuperClasses, Accumulator, Result).

% Processa ogni superclasse, accumulando i risultati
process_superclasses([], Accumulator, Accumulator).
process_superclasses([SuperClass|SuperClasses], Accumulator, Result) :-
    % Verifica che la superclasse non sia già stata inclusa
    \+ member(SuperClass, Accumulator),
    % Aggiunge la superclasse all'accumulatore
    find_superclasses(SuperClass, [SuperClass|Accumulator], PartialResult),
    % Continua con le restanti superclassi
    process_superclasses(SuperClasses, PartialResult, Result).
process_superclasses([SuperClass|SuperClasses], Accumulator, Result) :-
    % Se la superclasse è già stata inclusa, salta
    member(SuperClass, Accumulator),
    process_superclasses(SuperClasses, Accumulator, Result).


check_each_field([], _).
check_each_field([Field|Rest], DefaultAttributesTyped) :-
    Field = (Name=Value),
    memberchk(Name:Type=_, DefaultAttributesTyped),
    (   type_check(Value, Type)
    ->  check_each_field(Rest, DefaultAttributesTyped)
    ;   format("Type check failed for ~w with value ~w, expected type ~w~n", [Name, Value, Type]),
        fail
    ).
    

make(InstanceName, ClassName) :-
    make(InstanceName, ClassName, []).

make(InstanceName, ClassName, Fields) :-
    % verificare non ci siano Fields illegali e che il type corrisponda a quanto dichiarato in class(...).
    classes_chain(ClassName, ChainedClasses),
    get_attributes_from_classes(ChainedClasses, DefaultAttributesTyped, true),
    % Typing check

    !,  % Taglio per evitare il backtracking
    check_each_field(Fields, DefaultAttributesTyped),

    % sovrascrivere lista DefaultAttributes con tutti i Fields
    get_attributes_from_classes(ChainedClasses, DefaultAttributes, false),
    remove_overridden(DefaultAttributes, Fields, UpdatedDefaults),
    append(Fields, UpdatedDefaults, FinalAttributes),

 
    get_methods_from_classes(ChainedClasses, Methods),

    forall(
        member([Name, Args, Method], Methods),
        add_method(InstanceName, Name, Args, Method)
    ),

    InstanceTerm = instance(ClassName, FinalAttributes),
    (   atom(InstanceName)
        ->  (   clause(instance(InstanceName, _, _), true)
            ->  fail  % L'istanza esiste già, fallisce.
            ;   assertz(instance(InstanceName, ClassName, FinalAttributes))  % Crea l'istanza.
            )
        ;   var(InstanceName)
        ->  InstanceName = InstanceTerm,
            (   clause(instance(InstanceName, _, _), true)
            ->  fail  % L'istanza esiste già, fallisce.
            ;   assertz(instance(InstanceName, ClassName, FinalAttributes))  % Crea l'istanza.
            )
        ;   InstanceName = InstanceTerm
    ).


% Aggiungi un nuovo metodo alla base di conoscenzako
add_method(InstanceName, MethodName, Args, Actions) :-
    % Costruisci il termine del predicato
    Predicate =.. [MethodName, InstanceName | Args],
    % Costruisci il corpo del predicato (azione da eseguire)
    add_method_helper(InstanceName, Actions, FinalActions),
    assertz((Predicate :- FinalActions)).


add_method_helper(InstanceName, Method, FinalMethod) :-
    % Modifico il metodo in array di istruzioni
    term_to_list(Method, MethodAsListReversed),
    reverse(MethodAsList,MethodAsListReversed),
    % Per ogni istruzione in MethodAsList eseguo la replace_this_by_list
    maplist(replace_this_in_predicate(InstanceName), MethodAsList, FinalMethodAsList),
    % Rimodifico la MethodAsList come Method.
    list_to_term(FinalMethodAsList, FinalMethod).


replace_this_in_predicate(InstanceName, Predicate, FinalPredicate) :-
    % Predicato -> Lista se non già lista.
    (   is_list(Predicate)
    ->  Lista = Predicate
    ;   Predicate =.. Lista
    ),
    % Sostituisco
    maplist(replace_this(InstanceName), Lista, ResultList),
    (   is_list(Predicate)
    ->  FinalPredicate = ResultList
    ;   FinalPredicate =.. ResultList
    ).
    % Faccio ritornare Lista a predicato.


replace_this(InstanceName, Value, Result) :-
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

atom_or_string(Value) :-
(   atom(Value)
;   string(Value)
).

% Predicato principale per convertire un termine complesso in una lista di termini
term_to_list(Term, List) :-
    term_to_list_helper(Term, [], List).

% Helper ricorsivo per gestire la conversione
term_to_list_helper((Term1, Term2), Accumulator, List) :-
    !,  % Taglio per evitare il backtracking
    term_to_list_helper(Term1, Accumulator, NewAccumulator),
    term_to_list_helper(Term2, NewAccumulator, List).
term_to_list_helper(Term, Accumulator, [Term|Accumulator]).

% Predicato principale per convertire una lista di termini in un termine complesso
list_to_term(List, Term) :-
    list_to_term_helper(List, Term).

% Helper ricorsivo per costruire il termine complesso dalla lista
list_to_term_helper([LastTerm], LastTerm) :- !.  % Caso base: l'ultimo termine nella lista
list_to_term_helper([Term1 | Rest], (Term1, CompoundTerm)) :-
    list_to_term_helper(Rest, CompoundTerm).



    
is_class(ClassName) :-
    class(ClassName, _, _).


is_instance(Instance) :-
    instance(Instance, _, _).

is_instance(Value, ClassName) :-
    instance(Value, InstanceClass, _),
    is_subclass(InstanceClass, ClassName).

is_subclass(Class, Class).
is_subclass(SubClass, Class) :-
    class(SubClass, SuperClasses, _),
    member(Class, SuperClasses).
is_subclass(SubClass, Class) :-
    class(SubClass, SuperClasses, _),
    member(SuperClass, SuperClasses),
    is_subclass(SuperClass, Class).


inst(InstanceName, Instance) :-
    atom(InstanceName),
    instance(InstanceName, ClassName, Params),
    Instance = instance(InstanceName, ClassName, Params).



field(Instance, AttributeName, Value) :-
    instance(Instance, _, Attributes),
    member(AttributeName=Value, Attributes).

% fieldx predicate extracts a value from an instance by traversing a chain of fields
fieldx(Instance, [FieldName|FieldNames], Result) :-
    field(Instance, FieldName, IntermediateResult),
    fieldx_recursive(IntermediateResult, FieldNames, Result).

% Recursive helper for fieldx to handle multiple fields
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
% field(I1, s1, V1),
% field(V1, s2, V2),
% field(V2, s3, R),
% fieldx(I1, [s1, s2, s3], R).