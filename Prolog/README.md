

### Title

#### def_class:

```prolog
% Class person
def_class(
    ClassName: atom,
    Parents: atom[],
    Parts: any[]
)
```
```prolog
def_class(
    person,
    [],
    [
        field(age, 21 integer),
        field(name, "default")
    ]
)

% base conoscenza:

class(person, [], [field(age, 21 integer), field(name, "default")]).

% class_field(ClassName, FieldName, ValueName, ValueType)
class_field(person, age, 21, integer).
class_field(person, name, "default", _).


% Class student:
def_class(
    studente,
    [person],
    [
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
).

% person class ...
class(studente, [person], [method(talk, [], (write('My name is '), field(this, name, A), writeln(A), write('My age is '), field(this, age, B), writeln(B)))]).

% class_method:
class_method(
    studente,
    talk,
    [],
    (
        write('My name is '),
        field(this, name, A),
        writeln(A),
        write('My age is '),
        field(this, age, B), writeln(B)
    )
).

```

#### Make

```prolog
    make(p, person, [age=20, name="persona1"]),
    make(s, student).
```

