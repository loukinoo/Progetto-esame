%%%% Bagini    Luca    923969

%%%% nfsa.pl
%%%% Compilatore da RE a nsfa

%%% Gestisco i casi base di RE

%%% func_res/1
%%% Stabilisco le funzioni riservate delle RE 
%%% per non vederle come compound semplici
func_res(c).
func_res(a).
func_res(z).
func_res(o).
func_res('[|]').


%%% is_regex/1
%%% Stabilisco se un input è RE o no
is_regex([]) :- !.
is_regex([X | Y]) :- is_regex(X), is_regex(Y), !.
is_regex(RE) :- atomic(RE), !.
is_regex(RE) :- compound(RE), functor(RE, F, _), \+ func_res(F), !.
is_regex(RE) :- RE =.. [c | List], is_regex(List), !.
is_regex(RE) :- RE =.. [a | List], is_regex(List), !.
is_regex(z(RE)) :- is_regex(RE), !.
is_regex(o(RE)) :- is_regex(RE), !.


%%% nfsa_compile_regex/2
%%% Predicato effettivamente utilizzato nella pratica
nfsa_compile_regex(FA_Id, RE) :- 
    % is_regex(RE),
    gensym(q, Iniziale),
    gensym(q, Finale),
    assertz(nfsa_init(FA_Id, Iniziale)),
    assertz(nfsa_final(FA_Id, Finale)),
    nfsa_compile_regex(FA_Id, RE, Iniziale, Finale),
    !.


%%% nfsa_compile_regex/4
%%% Per gestire in maniera ricorsiva la RE in input creando l'automa
%%% Caso base: considero la RE vuota come epsilon
nfsa_compile_regex(FA_Id, [], Iniziale, Finale) :-
    assertz(nfsa_delta(FA_Id, Iniziale, "", Finale)).
%%% Lista vista come concatenazione
nfsa_compile_regex(FA_Id, [X | Y], Iniziale, Finale) :-
    gensym(q, Medio),
    nfsa_compile_regex(FA_Id, X, Iniziale, Medio),
    nfsa_compile_regex(FA_Id, Y, Medio, Finale).
%%% Caso base: simbolo atomico
nfsa_compile_regex(FA_Id, RE, Iniziale, Finale) :-
    atomic(RE),
    assertz(nfsa_delta(FA_Id, Iniziale, RE, Finale)).
%%% Caso base: simbolo compound diverso dai riservati
nfsa_compile_regex(FA_Id, RE, Iniziale, Finale) :-
    compound(RE),
    functor(RE, F, _),
    \+ func_res(F),
    assertz(nfsa_delta(FA_Id, Iniziale, RE, Finale)).
%%% Caso ricorsivo: concatenazione di RE
nfsa_compile_regex(FA_Id, RE, Iniziale, Finale) :-
    RE =.. [c | List],
    nfsa_compile_regex(FA_Id, List, Iniziale, Finale).
%%% Caso ricorsivo: somma di RE
nfsa_compile_regex(FA_Id, RE, Iniziale, Finale) :-
    RE =.. [a | List],
    compile_sum(FA_Id, List, Iniziale, Finale).
%%% Caso ricorsivo: chiusura di Kleene
nfsa_compile_regex(FA_Id, z(RE), Iniziale, Finale) :-
    gensym(q, Med1),
    gensym(q, Med2),
    assertz(nfsa_delta(FA_Id, Iniziale, "", Med1)),
    assertz(nfsa_delta(FA_Id, Med2, "", Med1)),
    assertz(nfsa_delta(FA_Id, Med2, "", Finale)),
    assertz(nfsa_delta(FA_Id, Iniziale, "", Finale)),
    nfsa_compile_regex(FA_Id, RE, Med1, Med2).
%%% Caso ricorsivo: RE+
nfsa_compile_regex(FA_Id, o(RE), Iniziale, Finale) :-
    gensym(q, Med1),
    gensym(q, Med2),
    assertz(nfsa_delta(FA_Id, Iniziale, "", Med1)),
    assertz(nfsa_delta(FA_Id, Med2, "", Med1)),
    assertz(nfsa_delta(FA_Id, Med2, "", Finale)),
    nfsa_compile_regex(FA_Id, RE, Med1, Med2).


%%% compile_sum/4
%%% Predicato per distinguere lista generata
%%% da concatenazione rispetto a quella di somma
%%% Per non mandare in errore: a fine lista termina
compile_sum(_, [], _, _).
%%% Gestione effettiva della somma di RE
compile_sum(FA_Id, [X | Y], Iniziale, Finale) :-
    gensym(q, Med1),
    gensym(q, Med2),
    assertz(nfsa_delta(FA_Id, Iniziale, "", Med1)),
    assertz(nfsa_delta(FA_Id, Med2, "", Finale)),
    nfsa_compile_regex(FA_Id, X, Med1, Med2),
    compile_sum(FA_Id, Y, Iniziale, Finale).


%%% nfsa_recognize/2
%%% Predicato effettivamente utilizzato nella pratica
nfsa_recognize(FA_Id, Input) :-
    % is_regex(Input),
    nfsa_init(FA_Id, Q),
    nfsa_recognize(FA_Id, Input, Q),
    !.


%%% nfsa_recognize/3
%%% Per gestire in maniera ricorsiva l'input di un automa
nfsa_recognize(FA_Id, [], Q) :-
    nfsa_final(FA_Id, Q).
nfsa_recognize(FA_Id, [], Q) :-
    nfsa_delta(FA_Id, Q, "", P),
    nfsa_recognize(FA_Id, [], P).
nfsa_recognize(FA_Id, [X | Y], Q) :-
    nfsa_delta(FA_Id, Q, "", P),
    nfsa_recognize(FA_Id, [X | Y], P).
nfsa_recognize(FA_Id, [X | Y], Q) :-
    nfsa_delta(FA_Id, Q, X, P),
    nfsa_recognize(FA_Id, Y, P).


%%% nfsa_delete_all/0
%%% Cancella tutti gli automi salvati in memoria
nfsa_delete_all() :-
    retractall(nfsa_delta(_, _, _, _)),
    retractall(nfsa_init(_, _)),
    retractall(nfsa_final(_, _)).


%%% nfsa_delete/1
%%% Cancella dalla memoria l'automa con FA_Id specificato
nfsa_delete(FA_Id) :-
    retractall(nfsa_delta(FA_Id, _, _, _)),
    retractall(nfsa_init(FA_Id, _)),
    retractall(nfsa_final(FA_Id, _)).


%%% Metodi dinamici utilizzati con assertz
%%% nfsa_delta(FA_Id, Stato1, Simbolo, Stato2)
%%% Rappresenta la funzione di transizione dell'automa
:- dynamic nfsa_delta/4.
%%% nfsa_init(FA_Id, Stato)
%%% Rappresenta lo stato iniziale dell'automa
:- dynamic nfsa_init/2.
%%% nfsa_final(FA_Id, Stato)
%%% Rappresenta lo stato finale dell'automa
:- dynamic nfsa_final/2.



%%%% Fine del file nfsa.pl