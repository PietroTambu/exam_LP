% ANSI color codes
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

% Definition of the def_class predicate
def_class(ClassName, Parents) :-
    assertz(class(ClassName, Parents, [])).

def_class(ClassName, Parents, Parts) :-
    assertz(class(ClassName, Parents, Parts)).

% Helper for defining fields and methods
def_field(FieldName, Value) :-
    field(FieldName, Value, _).

def_field(FieldName, Value, Type) :-
    field(FieldName, Value, Type).

def_method(MethodName, ArgList, Form) :-
    method(MethodName, ArgList, Form).

% Basic definitions for field and method
field(FieldName, Value, Type) :-
    assertz(field_definition(FieldName, Value, Type)).

method(MethodName, ArgList, Form) :-
    assertz(method_definition(MethodName, ArgList, Form)).

% Initialization
:- initialization(init).

init :-
    write_colored(green, '\nIl file oop.pl è stato caricato correttamente.\n'), nl.