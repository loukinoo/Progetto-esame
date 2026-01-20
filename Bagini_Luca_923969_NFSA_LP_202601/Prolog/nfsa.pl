%%%% Bagini    Luca    923969

%%%% nfsa.pl
%%%% Compilatore da RE a nsfa

%%% func_res/1
%%% Stabilisco le funzioni riservate delle RE
func_res(c).
func_res(a).
func_res(z).
func_res(o).

%%% is_regex/1
is_regex(RE) :- atomic(RE), !.
is_regex(RE) :- compound(RE), functor(RE, F, _), \+ func_res(F), !.



%%%% Fine del file nfsa.pl