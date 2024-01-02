Common Lissp project E1P

Author:
    Tamburini Pietro 894628


OOΛ in Common Lisp

documentation:


def-class:

 -  

    usage:
        (def-class <class-name> <parents> <part>*)
    
    example:
        (def-class 'person nil '(fields (name "person_name") (age 18 integer)))

        (def-class 'student '(person)
            '(fields
                ('name "student_name")
                (university "university_name" string))
            '(methods
                (talk (&optional (out *standard-output*)) 
                    (format out "My name is ~A~%My age is ~D~%"
                    (field this 'name)
                    (field this 'age)))))


make:

 -  
    usage:
        (make <class-name> [<field-name> <value>]*)
    
    example:
        (make 'person 'name "person name")
        (make 'student 'name "student_name" 'age 20)


is-class:

 -  Predicate

    usage:
        (is-class <class-name>)

    example:
        (is-class 'person)                                      # return T
        (is-class 'doesntexist)                                 # return NIL


is-instance:
 -   Predicate
    
    usage:
        (is-instance <value> [<class-name>])
    
    example:
        (defparameter studente (make 'student 'name "student_name" 'age 20))

        (is-instance studente '(person))                        # return T
        (is-instance studente '(student person))                # return NIL

field:
 -  

    usage:
        (field <instance> <field-name>)
    
    example:
        (defparameter studente (make 'student 'name "student_name" 'age 20))
        (field studente 'age)                                   # return 20

field*:
 -  
    usage:
        (field* <instance> [<field-name>])
    
    example:
        (defparameter p1 (make 'person 'name 'p2))
        (defparameter p2 (make 'person 'name 'p3))
        (defparameter p3 (make 'person 'name "p3_name"))

        (field* p1 '(name name name))