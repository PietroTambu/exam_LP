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

% Initialization
:- initialization(init).

init :-
    write_colored(green, '\nIl file oop.pl è stato caricato correttamente.\n'), nl.