% --- Spielzustand & Inventar ---
:- dynamic location/1, inventory/1, alive/1, energy/1, has_signal/1, has_knife/1, watch_time/1, found_sister/1, talked_guard/1, anger_guard/1.
:- discontiguous location/1.
:- discontiguous look/0.
:- discontiguous go/1.
:- discontiguous talk/1.

% Startwerte
alive(yes).
energy(10).  % Energie, die durch Essen und Ruhe steigt/sinkt
location(camp).
inventory([]).
has_signal(no).
has_knife(no).
watch_time(day).
found_sister(no).
talked_guard(no).
anger_guard(no).

path(prison_cell, hallway).
path(hallway, prison_cell).

path(hallway, guard_room).
path(guard_room, hallway).

path(hallway, village).
path(village, hallway).

path(village, forest).
path(forest, village).

path(forest, river).
path(river, forest).

% --- Anweisungen ---
instructions :-
    nl,
    write('Willkommen bei "Kiala: Flucht aus dem Kongo"'), nl,
    write('Du bist Kiala, ein junger Gefangener in einem Zwangsarbeitslager.'), nl,
    write('Deine Aufgabe ist es, zu ueberleben, zu fliehen, deine Schwester zu finden und den Weg nach Cabinda zu schaffen.'), nl,
    write('Gib Befehle in Prolog ein, z.B.'), nl,
    write('  start.           -- Spiel starten'), nl,
    write('  look.            -- Umgebung beschreiben'), nl,
    write('  go(Direction).   -- Richtung gehen (north, south, east, west)'), nl,
    write('  take(Item).      -- Objekt aufnehmen'), nl,
    write('  use(Item).       -- Objekt benutzen'), nl,
    write('  rest.            -- Ruhen und Energie wiederherstellen'), nl,
    write('  inventory.       -- Inventar anzeigen'), nl,
    write('  status.          -- Status anzeigen (Energie, Ort, usw.)'), nl,
    write('  instructions.            -- Anleitung anzeigen'), nl,
    write('  quit.            -- Spiel beenden'), nl,
    nl.

% Startbefehl
start :-
    instructions,
    write('Die Sonne steht hoch ueber dem Lager. Die Hitze lastet schwer auf dir.'), nl,
    write('Dein Ziel: Flucht und Ueberleben.'), nl,
    write('Gib "look." ein, um deine Umgebung zu erkunden.'), nl,
    retractall(location(_)),
    assert(location(camp)),
    retractall(alive(_)),
    assert(alive(yes)),
    retractall(energy(_)),
    assert(energy(10)),
    retractall(inventory(_)),
    assert(inventory([])),
    retractall(has_signal(_)),
    assert(has_signal(no)),
    retractall(has_knife(_)),
    assert(has_knife(no)),
    retractall(watch_time(_)),
    assert(watch_time(day)),
    retractall(found_sister(_)),
    assert(found_sister(no)),
    retractall(talked_guard(_)),
    assert(talked_guard(no)),
    retractall(anger_guard(_)),
    assert(anger_guard(no)).

% Inventar anzeigen
inventory :-
    inventory(List),
    write('Inventar: '), write(List), nl.

% Status anzeigen
status :-
    location(Loc),
    energy(E),
    alive(Alive),
    has_signal(Signal),
    has_knife(Knife),
    watch_time(WT),
    found_sister(FS),
    write('Ort: '), write(Loc), nl,
    write('Energie: '), write(E), nl,
    write('Lebendig: '), write(Alive), nl,
    write('Signalgeraet: '), write(Signal), nl,
    write('Messer: '), write(Knife), nl,
    write('Tag/Nacht: '), write(WT), nl,
    write('Schwester gefunden: '), write(FS), nl.


check_end :-
    alive(no),
    write('Deine Reise endet hier im Tod.'), nl,
    write('Schlechtes Ende.'), nl,
    end_game.

check_end :-
    alive(yes),
    location(cabinda),
    found_sister(yes),
    has_signal(yes),
    write('Du hast Cabinda erreicht, deine Schwester gefunden und das Signalgeraet.'), nl,
    write('Du sendest einen Hilferuf, der gerettet wird! Du schreibst spaeter ein Buch ueber deine Flucht.'), nl,
    write('Glueckliches Ende!'), nl,
    end_game.

check_end :-
    alive(yes),
    location(cabinda),
    found_sister(no),
    write('Du hast Cabinda erreicht, aber deine Schwester nicht gefunden.'), nl,
    write('Deine Flucht ist offen, aber dein Herz bleibt schwer.'), nl,
    write('Neutrales Ende.'), nl,
    end_game.

check_end :-
    alive(yes),
    % Beispiel Recapture: Wenn Energie zu niedrig im Lager oder anderes
    location(camp),
    energy(E),
    E =< 1,
    write('Du wurdest von den Wachen im Lager wieder gefangen genommen.'), nl,
    write('Schlechtes Ende: Recapture.'), nl,
    end_game.

check_end.  % Wenn keine Endbedingung erfüllt, weiterspielen

% Spiel beenden
end_game :-
    write('Das Spiel ist hier beendet. Moechtest du neu starten? (yes/no)'), nl,
    read(Answer),
    ( Answer = yes -> start
    ; Answer = no -> quit
    ; write('Bitte yes oder no eingeben.'), nl, end_game).


talk(sister) :-
    location(village),
    alive(yes),
    ( found_sister(no) ->
        write('Du findest deine Schwester! Sie lebt versteckt im Dorf.'), nl,
        retract(found_sister(no)),
        assert(found_sister(yes)),
        write('Deine Hoffnung waechst.'), nl
    ; write('Du hast deine Schwester schon gefunden.'), nl
    ).

talk(guard) :-
    location(food_area),
    alive(yes),
    anger_guard(no),
    ( talked_guard(no) ->
        write('Der Waechter mustert dich misstrauisch. Du kannst etwas sagen:'), nl,
        write('1. "Bitte lass mich gehen!"'), nl,
        write('2. "Du bist ein schrecklicher Waechter."'), nl,
        write('Waehle 1 oder 2:'), nl,
        read(Choice),
        handle_guard_talk(Choice)

    ; write('Der Waechter beobachtet dich aufmerksam und schweigt.'), nl
    ).

talk(guard) :-
    anger_guard(yes),
    write('Der Waechter ist wuetend und greift dich an!'), nl,
    fight.

talk(_) :-
    write('Mit dieser Person kannst du nicht sprechen.'), nl.


% Behandlung der Gesprächsauswahl
handle_guard_talk(1) :-
    write('Der Waechter schmunzelt und sagt: "Das wuenschst du dir wohl!"'), nl,
    retract(talked_guard(no)),
    assert(talked_guard(yes)).

handle_guard_talk(2) :-
    write('Der Waechter wird wuetend ueber deine Beleidigung und greift an!'), nl,
    retract(anger_guard(no)),
    assert(anger_guard(yes)),
    fight.


look :-
    location(river),
    alive(yes),
    write('Du bist am Fluss. Das Wasser ist klar, aber gefaehrlich.'), nl,
    write('Du kannst:'), nl,
    write(' - fish.       -- Versuche zu fischen und Nahrung zu erhalten'), nl,
    write(' - go(east). -- Zurueck in den Dschungel'), nl,
    write(' - go(village). -- Erkundige das Dorf'), nl,
    write(' - go(cabinda). -- Versuche die Flucht ueber den Fluss'), nl.

fish :-
    alive(yes),
    energy(E),
    E =< 7,
    gain_energy(3),
    write('Du faengst etwas Fisch und isst ihn.'), nl,
    advance_watch.

fish :-
    alive(yes),
    energy(E),
    E > 7,
    write('Du bist nicht hungrig genug zum Fischen.'), nl.

fish :-
    alive(no),
    write('Du bist tot und kannst nicht fischen.'), nl.

go(cabinda) :-
    alive(yes),
    has_signal(yes),
    found_sister(yes),
    retract(location(river)),
    assert(location(cabinda)),
    write('Du ueberquerst den Fluss und erreichst Cabinda. Die Freiheit ist greifbar!'), nl,
    check_end.

go(cabinda) :-
    alive(yes),
    (has_signal(no); found_sister(no)),
    write('Du kannst nicht direkt nach Cabinda fliehen, du brauchst deine Schwester und das Signalgeraet!'), nl.

go(village) :-
    alive(yes),
    retract(location(river)),
    assert(location(village)),
    write('Du gehst zurueck ins Dorf.'), nl,
    advance_watch.

go(river) :-
    alive(yes),
    retract(location(village)),
    assert(location(river)),
    write('Du gehst zurueck zum Fluss.'), nl,
    advance_watch.

% Umgebung beschreiben
look :-
    location(camp), !,
    write('Du bist im Zwangsarbeitslager. Die Wachposten sind ueberall.'), nl,
    write('Du kannst:'), nl,
    write(' - go(north). -- Richtung Norden zum Lagerausgang'), nl,
    write(' - go(east).  -- Richtung Osten zum Schlafbereich'), nl,
    write(' - go(west).  -- Richtung Westen zur Essensausgabe'), nl,
    write(' - rest.     -- Versuch zu ruhen (Risiko von Patrouillen)'), nl.

look :-
    location(north_path), !,
    write('Du bist auf einem schmalen Pfad im Dschungel, Richtung Freiheit.'), nl,
    write('Du kannst:'), nl,
    write(' - go(south). -- Zurueck zum Lager'), nl,
    write(' - go(north). -- Weiter in den Dschungel'), nl,
    write(' - search.    -- Umgebung durchsuchen'), nl.

look :-
    location(sleep_area), !,
    write('Du bist im Schlafbereich des Lagers. Einige Gefangene ruhen.'), nl,
    write('Du kannst:'), nl,
    write(' - go(west). -- Zurueck zum Lagerzentrum'), nl,
    write(' - search.   -- Suche nach nuetzlichen Gegenstaenden'), nl.

look :-
    location(food_area), !,
    write('Du stehst bei der Essensausgabe. Es riecht nach Maisbrei und Fisch.'), nl,
    write('Du kannst:'), nl,
    write(' - go(east). -- Zurueck zum Lagerzentrum'), nl,
    write(' - talk(guard). -- Mit einem Wachmann sprechen'), nl.

look :-
    location(deep_jungle), !,
    write('Du bist tief im Dschungel. Die Geraeusche der Wildnis umgeben dich.'), nl,
    write('Du kannst:'), nl,

    write(' - go(south). -- Zurueck zum Pfad'), nl,
    write(' - go(north). -- Der Weg in den Dschungel der in ein dunkles Gebiet zu fuehren scheint.'), nl,
    write(' - go(west). -- Ein schmaler Pfad fuehrt zum Fluss.'), nl,
    write(' - search.    -- Suche nach essbaren Pflanzen oder Insekten'), nl.

look :-
    location(hidden_cave), !,
    write('Du hast eine versteckte Hoehle gefunden. Es ist dunkel und feucht.'), nl,
    write('Du kannst:'), nl,
    write(' - go(south). -- Zurueck in den Dschungel'), nl,
    write(' - search.  -- Untersuche die Hoehle'), nl.

look :-
    location(cabinda), !,
    write('Du hast Cabinda erreicht, die Freiheit ist zum Greifen nah.'), nl,
    write('Du hast es geschafft!'), nl,
    write('-> Spiel gewonnen!'), nl.

look :-
    location(village), !,
    write('Du bist im Dorf. Es gibt einfache Holzhuetten und einige Dorfbewohner, die ihren taeglichen Aufgaben nachgehen. Darunter erkennst du eine Frau dessen Gesicht kaum erkennbar ist. Es ist deine Schwester!'), nl,
    write(' - talk(sister). -- Du kannst mit ihr reden'), nl,
    write(' - go(river). -- Zurueck zum Fluss'), nl.

look :-
    write('Hier gibt es nichts Interessantes.'), nl.

% Richtungswechsel
go(Direction) :-
    alive(yes),
    location(camp),
    ( Direction = north -> (
        retract(location(camp)),
        assert(location(north_path)),
        write('Du gehst Richtung Norden zum Dschungelpfad.'), nl,
        advance_watch
      );
      Direction = east -> (
        retract(location(camp)),
        assert(location(sleep_area)),
        write('Du gehst zum Schlafbereich des Lagers.'), nl,
        advance_watch
      );
      Direction = west -> (
        retract(location(camp)),
        assert(location(food_area)),
        write('Du gehst zur Essensausgabe.'), nl,
        advance_watch
      );
      write('Du kannst dort nicht hingehen.'), nl
    ).

go(Direction) :-
    alive(yes),
    location(north_path),
    ( Direction = north -> (
        retract(location(north_path)),
        assert(location(deep_jungle)),
        write('Du dringtst tiefer in den Dschungel ein.'), nl,
        advance_watch
      );
      Direction = south -> (
        retract(location(north_path)),
        assert(location(camp)),
        write('Du kehrst zurueck zum Lager.'), nl,
        advance_watch
      );
      write('Dort kannst du nicht hingehen.'), nl
    ).

go(Direction) :-
    alive(yes),
    location(sleep_area),
    ( Direction = west -> (
        retract(location(sleep_area)),
        assert(location(camp)),
        write('Du gehst zurueck zum Lagerzentrum.'), nl,
        advance_watch
      );
      write('Dort kannst du nicht hingehen.'), nl
    ).

go(Direction) :-
    alive(yes),
    location(food_area),
    ( Direction = east -> (
        retract(location(food_area)),
        assert(location(camp)),
        write('Du gehst zurueck zum Lagerzentrum.'), nl,
        advance_watch
      );
      write('Dort kannst du nicht hingehen.'), nl
    ).

go(Direction) :-
    alive(yes),
    location(deep_jungle),
    ( Direction = south -> (
        retract(location(deep_jungle)),
        assert(location(north_path)),
        write('Du gehst zurueck zum Pfad.'), nl,
        advance_watch
      );
      Direction = north -> (
        retract(location(deep_jungle)),
        assert(location(hidden_cave)),
        write('Du findest einen Ausgang zur versteckten Hoehle.'), nl,
        advance_watch
      );
      Direction = west -> (
        retract(location(deep_jungle)),
        assert(location(river)),
        write('Du gehst zum Fluss.'), nl,
        advance_watch
      );
      write('Dort kannst du nicht hingehen.'), nl
    ).

go(Direction) :-
    alive(yes),
    location(hidden_cave),
    ( Direction = south -> (
        retract(location(hidden_cave)),
        assert(location(deep_jungle)),
        write('Du verlaesst die Hoehle zurueck in den Dschungel.'), nl,
        advance_watch
      );
      write('Dort kannst du nicht hingehen.'), nl
    ).

go(Direction) :-
    alive(yes),
    location(river),
    ( Direction = east -> (
        retract(location(river)),
        assert(location(deep_jungle)),
        write('Du kehrst zurueck in den Dschungel.'), nl,
        advance_watch
      );
      write('Dort kannst du nicht hingehen.'), nl
    ).

go(_) :-
    alive(no),
    write('Du bist tot. Das Spiel ist vorbei.'), nl.

% Ruhen (rest)
rest :-
    alive(yes),
    energy(E),
    watch_time(day),
    NewE is min(10, E + 3),
    retract(energy(E)),
    assert(energy(NewE)),
    write('Du ruhst dich aus und fuehlst dich etwas erholter.'), nl,
    advance_watch.

rest :-
    alive(yes),
    energy(E),
    watch_time(night),
    write('Nachts ist ruhen riskant. Du koenntest von Wachen entdeckt werden!'), nl,
    write('Wagen willst du es trotzdem? (yes/no)'), nl,
    read(Answer),
    ( Answer = yes -> (
        maybe_patrol,
        energy(E),
        NewE is min(10, E + 3),
        retract(energy(E)),
        assert(energy(NewE)),
        write('Du ruhst dich aus, trotz der Gefahr.'), nl
    ); Answer = no -> write('Du entscheidest dich, nicht zu ruhen.'), nl
    ; write('Ungueltige Eingabe.'), nl, rest).

rest :-
    alive(no),
    write('Du bist tot. Ruhen ist nicht mehr moeglich.'), nl.

% Suche (search)
search :-
    location(sleep_area),
    alive(yes),
    ( has_knife(no) ->
        write('Du findest ein altes, rostiges Messer unter den Brettern. Es koennte hilfreich sein.'), nl,
        retract(has_knife(no)),
        assert(has_knife(yes)),
        take(knife)
    ; write('Du findest nichts Neues.'), nl
    ),
    advance_watch.

search :-
    location(food_area),
    alive(yes),
    write('Du sprichst mit einem Wachmann und erfaehrst von einer Schwachstelle im Lagerzaun.'), nl,
    advance_watch.

search :-
    location(north_path),
    alive(yes),
    write('Du findest einige essbare Beeren und trinkst etwas Wasser aus einem Bach.'), nl,
    gain_energy(2),
    advance_watch.

search :-
    location(deep_jungle),
    alive(yes),
    write('Du findest einige Insekten und wurmaehnliche Tiere. Obwohl ekelig, essen sie dich weniger hungrig.'), nl,
    gain_energy(3),
    advance_watch.

search :-
    location(hidden_cave),
    alive(yes),
    ( has_signal(no) ->
        write('Du findest ein altes, aber funktionsfaehiges Signalgeraet! Dieses koennte deine Rettung sein.'), nl,
        retract(has_signal(no)),
        assert(has_signal(yes)),
        take(signal_device)
    ; write('Die Hoehle ist leer.'), nl
    ),
    advance_watch.

search :-
    alive(no),
    write('Du bist tot und kannst nichts suchen.'), nl.

search :-
    write('Hier gibt es nichts zu suchen.'), nl.

% Inventar ergänzen
take(Item) :-
    inventory(List),
    \+ member(Item, List),
    retract(inventory(List)),
    assert(inventory([Item|List])).

% Energie erhöhen
gain_energy(Amount) :-
    energy(E),
    NewE is min(10, E + Amount),
    retract(energy(E)),
    assert(energy(NewE)),
    write('Deine Energie steigt auf '), write(NewE), write('.'), nl.

% Energie verringern
lose_energy(Amount) :-
    energy(E),
    NewE is E - Amount,
    (NewE =< 0 ->
        write('Du bist erschoepft und stirbst vor Erschoepfung.'), nl,
        die
    ;
        retract(energy(E)),
        assert(energy(NewE)),
        write('Deine Energie sinkt auf '), write(NewE), write('.'), nl
    ).

% Wachen Patrouille check
maybe_patrol :-
    random(1, 5, Chance),
    (Chance =< 2 -> (
        write('Eine Wache entdeckt dich! Du musst kaempfen!'), nl,
        fight
    ); write('Du ruhst dich ungestoert.'), nl).

% Kampfmechanik
fight :-
    has_knife(HasKnife),
    ( HasKnife = yes ->
        write('Du kaempfst mit deinem Messer und besiegst die Wache!'), nl,
        lose_energy(2)
    ;
        energy(E),
        ( E > 6 ->
            write('Du hast genug Energie und besiegst den Waechter!'), nl,
            retract(alive(yes)),
            assert(alive(no)),
            retract(anger_guard(yes)),
            assert(anger_guard(no))
        ;
            write('Du hast keine Waffe und nicht genug Energie. Der Waechter besiegt dich!'), nl,
            quit
        )
    ).

% Weiter mit der Zeit (Tag/Nacht-Wechsel)
advance_watch :-
    watch_time(day),
    retract(watch_time(day)),
    assert(watch_time(night)),
    write('Die Nacht bricht herein. Sei vorsichtig.'), nl.

advance_watch :-
    watch_time(night),
    retract(watch_time(night)),
    assert(watch_time(day)),
    write('Die Sonne geht auf. Ein neuer Tag beginnt.'), nl.

% Todesszenario
die :-
    retract(alive(yes)),
    assert(alive(no)),
    write('Du bist gestorben.'), nl,
    write('Spiel vorbei. Nutze halt. um zu beenden.'), nl.

% quit command
quit :-
    write('Spiel wird beendet.'), nl,
    halt.

