Prolog project E1P OOP

Autore:
    Tamburini Pietro 894628



OOΠ in Prolog

documentation:


def_class/2:
def_class/3:

 -  All specified fields will override superclass fields with
    the same name.
 -  A field type check will be performed during class definition.
 -  Default values for unspecified fields will be used.

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
    ?-  def_class(
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
 -  Invalid field names will cause instantiation to fail.

    usage:
        make(
            <instance_name>                             # compulsory
            <class_name>                                # compulsory
            [                                           # optional
                <field(<name>, <value>, <type?>)>
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

