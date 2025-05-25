:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)).

/* Startposition */
i_am_at(zelle).

/* Wege zwischen den Orten */
path(zelle, e, gang).
path(gang, w, zelle).
path(gang, n, hof).
path(hof, s, gang).
path(hof, e, dschungel).
path(hof, w, keller).
path(keller, e, hof).

/* Gegenstände */
at(schluessel, zelle).
at(tagebuch, hof).
at(messer, keller).

/* Beschreibung der Orte */
describe(zelle) :- 
    write('Du bist in einer dunklen Zelle. Es riecht nach Schimmel. Vielleicht ist hier irgendwo ein Schluessel.'), nl.

describe(gang) :- 
    write('Ein enger Gang mit feuchten Waenden. Richtung Norden hoerst du Schritte.'), nl.

describe(hof) :- 
    write('Der Hof ist weit, aber eine Wache steht in der Nähe. Hier liegt etwas...'), nl.

describe(dschungel) :- 
    write('Du hast den Dschungel erreicht. Freiheit?'), nl.

describe(keller) :- 
    write('Ein dunkler Keller, modrig und kalt. Ein Messer liegt auf dem Boden.'), nl.

/* Befehle für Richtungen */
n :- go(n).
s :- go(s).
e :- go(e).
w :- go(w).

/* Bewegung mit Ereignis */
go(Direction) :-
    i_am_at(Here),
    path(Here, Direction, There),
    can_go(Here, Direction, There),
    retract(i_am_at(Here)),
    assert(i_am_at(There)),
    look, !.

go(_) :-
    write('Du kannst dort nicht hingehen.'), nl.

/* Tür mit Schloss zwischen gang und hof */
can_go(gang, n, hof) :-
    holding(schluessel), !.

can_go(gang, n, hof) :-
    write('Die Tür zum Hof ist verschlossen. Du brauchst einen Schluessel.'), nl,
    fail.

/* Nur mit Tagebuch kann man in den Dschungel */
can_go(hof, e, dschungel) :-
    holding(tagebuch),
    write('Mit dem Tagebuch in der Tasche rennst du in den Dschungel. Die Wahrheit lebt weiter...'), nl,
    finish, !.

can_go(hof, e, dschungel) :-
    write('Du kannst nicht fliehen ohne Beweise. Das Tagebuch fehlt dir.'), nl,
    fail.

can_go(_, _, _) :- !.

/* Gegenstände nehmen */
take(X) :-
    holding(X),
    write('Du hast das schon.'), nl, !.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    retract(at(X, Place)),
    assert(holding(X)),
    write('OK, du hast es.'), nl, !.

take(_) :-
    write('Das ist hier nicht.'), nl.

/* Gegenstände ablegen */
drop(X) :-
    holding(X),
    i_am_at(Place),
    retract(holding(X)),
    assert(at(X, Place)),
    write('OK, abgelegt.'), nl, !.

drop(_) :-
    write('Du hast das nicht.'), nl.

/* Inventar anzeigen */
inventory :-
    findall(X, holding(X), Items),
    ( Items = [] -> write('Du hast nichts bei dir.'), nl
    ; write('Du hast dabei: '), write(Items), nl ).

/* Umgebung ansehen */
look :-
    i_am_at(Place),
    describe(Place),
    notice_objects_at(Place),
    nl.

/* Objekte anzeigen */
notice_objects_at(Place) :-
    at(X, Place),
    write('Hier liegt: '), write(X), nl,
    fail.

notice_objects_at(_).

/* Anleitung */
instructions :-
    nl,
    write('Du befindest dich in einem kolonialen Gefangenenlager.'), nl,
    write('Ziel: Entkomme mit Beweisen über die Grausamkeit.'), nl,
    write('Befehle:'), nl,
    write('start.             -- Spiel starten'), nl,
    write('n. s. e. w.        -- Gehe Richtung Norden, Sueden, etc.'), nl,
    write('take(X).           -- Nimm Objekt X'), nl,
    write('drop(X).           -- Lege Objekt X ab'), nl,
    write('look.              -- Umgebung ansehen'), nl,
    write('inventory.         -- Zeige dein Inventar'), nl,
    write('instructions.      -- Anleitung anzeigen'), nl,
    write('halt.              -- Spiel beenden'), nl, nl.

/* Spiel starten */
start :-
    retractall(at(_, _)),
    retractall(i_am_at(_)),
    retractall(holding(_)),
    assert(i_am_at(zelle)),
    assert(at(schluessel, zelle)),
    assert(at(tagebuch, hof)),
    assert(at(messer, keller)),
    instructions,
    look.

/* Spielende */
finish :-
    nl,
    write('--- Das Spiel ist vorbei. Gib "halt." ein, um zu beenden. ---'), nl.

