Prolog project E1P OOP

Author:
    Tamburini Pietro 894628


OOΠ in Prolog

documentation:


def_class/2:
def_class/3:

 -  A field type check will be performed during class definition.
 -  In methods use 'this' as alias for future instance value.

    usage:
        def_class(
            <class_name>,                               # compulsory
            [<super_classes_names>],                    # compulsory
            [                                           # optional
                <field(<name>, <value>, <type?>)>
                <method(<name>, <args>, <form>)>
            ]
        )

    example:
        def_class(
            person,
            [],
            [
                field(name, 'person_name'),
                field(age, 18, integer)
            ]
        ),
        def_class(
            student,
            [person],
            [
                field(name, 'student_name'),            # override
                field(age, 21, integer),                # override
                field(university, 'university_name'),
                method(
                    talk,
                    [],
                    (
                        write('My name is '),
                        field(this, name, N),
                        writeln(N),
                        write('I am '),
                        field(this, age, A),
                        write(A),
                        writeln(' years old'),
                        write('I attend '),
                        field(this, university, U),
                        write(U),
                        writeln(' university')
                    )
                )
            ]
        ).

make/2:
make/3:

 -  creates a new instance of a class
 -  fields of the class that are not specified will be instantiated
    with the default value.
 -  A field type check will be performed during class instantiation.
 -  Invalid field names will cause a failure.

    usage:
        make(
            <instance_name>                             # compulsory
            <class_name>                                # compulsory
            [                                           # optional
                attr1Name=value1,
                attr2Name=value2
            ]
        )

    example:
        make(p, person),
        make(p1, person, [name='override_default_name']),
        make(
            s,
            student,
            [
                name='override_person_default_field',
                university='override_student_default_field'
            ]
        ),
        make(fail, person, [age='12']).  % fail: '12' is not integer

is_class/1:

 -  checks that the symbol passed is a defined class

    usage:
        is_class(<class_name>)
    
    example:
        is_class(student).
        true .

        is_class(not_exist)
        false .

is_instance/1:
is_instance/2:

 -  checks whether the passed atom is an instance of a class.
 -  instance/2 checks whether the instance has <class_name>
    as a superclass

    usage:
        is_instance(
            <value>,                                    # compulsory
            <class_name>                                # optional
        )
    
    example:
        is_instance(s).

        is_instance(s, person).

inst/2:

 -  retrieves an instance given the name by which it was created.
    
    usage:
        inst(
            <instance_name>,
            <instance>
        )

    example:
        instance(p, A).
        A = instance(p, person, [age=18, name="person_name"])

        instance(p, instance(p, person, [age=18, name="person_name"]))
        true .

field/3:

 -  extracts the value of a field from an instance of a class.

    usage:
        field(
            <instance>,
            <field_name>,
            <result>
        )
    
    example:
        field(p, name, R).
        R = "person_name".

        field(s, name, R).
        R = "person_name".

        field(not_exist, name, R).
        false .

fieldx/3:

 -  extracts the value of a field from an instance of a class
    by traversing a chain of attributes.

    usage:
        fieldx(
            <instance>,
            [<field_name>, ...],
            <result>
        )
    
    example:
        make(c1, class_name, [other=c2]).
        make(c2, class_name, [other=p]).
        make(p, person, [name="person_name"]).

        % c1.other = c2
        % c2.other = p
        % p.name = "person_name"

        fieldx(c1, [other, other, name], R).
        R = "person_name".