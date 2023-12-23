
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

% Implementazione dei controlli di tipo
type_check(Value, Type) :- 
    (Type == integer -> integer(Value) ; true),
    (Type == float -> float(Value) ; true).




  % END utilities




def_class(ClassName, Parents) :-
    % TODO: Check Esistenza Parents (Classe/i []) is_class
    assertz(class(ClassName, Parents, [])).

def_class(ClassName, Parents, Parts) :-

    % se mi definisce classe con parents:
    %   se definisce un field della sottoclasse:
    %     se (field typed && sott_field typed):
    %         verificare congruenza name:type
    %             se uguali: aggiugere field in [field(name, value, type)]
    %             se non uguali: errore (incogruenza type stesso name tra classe e sottoclasse.), fail.
    %     se (field typed && sott_field untype) || (field untyped && sott_field untyped):
    %         aggiugere field in [field(name, value, type?)]
    %     se (field untyped && sott_field typed):
    %         fail. (incogruenza type. Scenario non accettato)
    %   se definisce un method della sottoclasse:
    %       se (name == sott_name && args.len() == sott_args.len())
    %           fail.
    %       else
    %           aggiugere method in [method(name, args, form)]

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

remove_overridden([], _, []).
remove_overridden([DefAttr|DefAttrs], Fields, Result) :-
    DefAttr = (Name=_),
    (   memberchk(Name=_, Fields)
    ->  remove_overridden(DefAttrs, Fields, Result)
    ;   Result = [DefAttr|Rest],
        remove_overridden(DefAttrs, Fields, Rest)
    ).
        

make(InstanceName, ClassName) :-
    make(InstanceName, ClassName, []).

make(InstanceName, ClassName, Fields) :-
    
    % verificare non ci siano Fields illegali e che il type corrisponda a quanto dichiarato in class(...), altrimenti false.
    get_attributes(ClassName, DefaultAttributesTyped, true),
    forall(
        (member(Field, Fields), Field = (Name=Value)),
        (memberchk(Name:Type=_, DefaultAttributesTyped), type_check(Value, Type))
    ),

    % sovrascrivere lista DefaultAttributes con tutti i Fields
    get_attributes(ClassName, DefaultAttributes, false),
    remove_overridden(DefaultAttributes, Fields, UpdatedDefaults),
    append(Fields, UpdatedDefaults, FinalAttributes),

    InstanceTerm = instance(ClassName, FinalAttributes),
    (   atom(InstanceName)
    ->  (   clause(instance(InstanceName, _, _), true)
        ->  write_colored(red, 'Instance already exist'),
            fail
        ;   assertz(instance(InstanceName, ClassName, FinalAttributes))
        )
    ;   var(InstanceName)
    ->  InstanceName = InstanceTerm,
        (   clause(instance(InstanceName, _, _), true)
        ->  write_colored(red, 'Instance already exist'),
            fail
        ;   assertz(instance(InstanceName, ClassName, FinalAttributes))
        )
    ;   InstanceName = InstanceTerm
    ).
    



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


% Initialization
:- initialization(init).

init :-
    write_colored(green, '\nIl file oop.pl è stato caricato correttamente.\n'), nl,
    def_class(person, [], [field(age, 21, integer), field(name, "default")]),
    def_class(student, [person], [field(studentID, 1, integer)]),
    make(mario, student),
    make(marco, person),
    make(nuovo, person, [age=18]).