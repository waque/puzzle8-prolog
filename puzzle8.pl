
				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				%%%                                         %%%
				%%%        Funcoes auxiliares               %%%
				%%%                                         %%%
				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% escreve/1 e um predicado auxiliar de transformacao/2
%%% a primeira regra permite escrever uma configuracao

escreve([A, B, C, D, E, F, G, H, I]) :- nl, escreve(A), escreve(B), escreve(C), nl,
										escreve(D), escreve(E), escreve(F), nl,
                                        escreve(G), escreve(H), escreve(I), nl, nl.

escreve(S) :- S = 0, write('   ').
escreve(S) :- S < 10, write(' '), write(S), write(' ').
									
%%% transformacao/2
%%% transformacao(C1, C2) em que C1 e C2 sao configuracoes representadas por listas	
								
transformacao([A, B, C, D, E, F, G, H, I], 
              [J, K, L, M, N, O, P, Q, R]) :-
											write('Transformacao desejada:'), nl, 
											escreve(A), escreve(B), escreve(C),  
											write('    '), 
											escreve(J), escreve(K), escreve(L),nl, 
											escreve(D), escreve(E), escreve(F), 
											write(' -> '), 
											escreve(M), escreve(N), escreve(O), nl,
											escreve(G), escreve(H), escreve(I), 
											write('    '), 
											escreve(P), escreve(Q), escreve(R), nl.
	

	

%%% escreve_solucao/1				
escreve_solucao([(M, P) | []]) :- write('mova a peca '), 
                                  write(P), 
                                  traduz(M, Mp), 
                                  write(Mp),
                                  write('.'),
                                  nl.

escreve_solucao([(M, P) | R]) :- write('mova a peca '), 
                                 write(P), 
                                 traduz(M, Mp), 
                                 write(Mp),
                                 nl, 
                                 escreve_solucao(R).

traduz(c, ' para cima').
traduz(b, ' para baixo').
traduz(e, ' para a esquerda').
traduz(d, ' para a direita').


				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				%%%                                         %%%
				%%%       	 Procura Manual                 %%%
				%%%                                         %%%
				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				
				
%%% read_index/3

read_index([X|_],X, 0):-!.
read_index([_|Y],X, I):-
						I_1 is I-1,
						read_index(Y, X, I_1).
%%% index_of/3
index_of([N|_], N, 0):-!.
index_of([_|Y], N, C):-
						index_of(Y, N, R),
						C is R+1.
%%% change_pos/4
change_pos([E|Tail], [_|Tail],0,E).
change_pos([Head|Rest],[Head|Tail],I,E):-
										I1 is I-1,
										change_pos(Rest,Tail,I1,E).
%%% trade_pos/4
trade_pos(Tab3, Tab1, I1, I2):- read_index(Tab1, E1, I1),
								read_index(Tab1, E2, I2),
								change_pos(Tab2, Tab1, I1, E2),
								change_pos(Tab3, Tab2, I2, E1).

mov_c(Tab1, P, TabF):- index_of(Tab1, 0, I0), I0 < 6, IE is I0+3, read_index(Tab1,P, IE), trade_pos(Tab1, TabF, I0, IE), !.
mov_b(Tab1, P, TabF):- index_of(Tab1, 0, I0), I0 > 2, IE is I0-3, read_index(Tab1,P, IE), trade_pos(Tab1, TabF, I0, IE), !.
mov_d(Tab1, P, TabF):- index_of(Tab1, 0, I0), I0 =\= 0, I0 =\= 3, I0 =\= 6, IE is I0-1, read_index(Tab1,P, IE), trade_pos(Tab1, TabF, I0, IE), !.
mov_e(Tab1, P, TabF):- index_of(Tab1, 0, I0), I0 =\= 2, I0 =\= 5, I0 =\= 8, IE is I0+1, read_index(Tab1,P, IE), trade_pos(Tab1, TabF, I0, IE), !.

mov_legal(C1, c, P, C2):- mov_c(C2, P, C1). %C1 e o final
mov_legal(C1, b, P, C2):- mov_b(C2, P, C1).
mov_legal(C1, e, P, C2):- mov_e(C2, P, C1).
mov_legal(C1, d, P, C2):- mov_d(C2, P, C1).

%%% resolve/2
resolve(Tab1, Tab_final):-
							writeln('Qual o seu movimento?'),
							read(S),
							mov_legal(Tab_out, S, _, Tab1),
							escreve(Tab_out),
							resolve_m(Tab_out, Tab_final).
resolve(Tab1, Tab_final):-
							writeln('Movimento ilegal'),
							resolve_m(Tab1, Tab_final).

resolve_m(Tab1, Tab1):- write('Parabens!'), !.
resolve_m(Tab1, Tab_final):-
							resolve(Tab1, Tab_final).
	
resolve_manual(Tab1, Tab_final):- transformacao(Tab1, Tab_final), resolve_m(Tab1, Tab_final).

				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				%%%                                         %%%
				%%%        		Procura Cega                %%%
				%%%                                         %%%
				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exists_config(Tab,[Tab|_]).
exists_config(X,[_|Y]) :- exists_config(X,Y).


%%% blind/4
blind(C1, Data, M, Final):-
							mov_legal(Tab, M, P, C1),
							not(exists_config(Tab,Data)),
							escreve_solucao([(M, P) | []]),
							sel(Tab, Final, [Tab|Data]).
%%% sel/3
sel(C1, C1, _):-
				resolve_c(C1, C1), !.
sel(C1, C2, Data):-
				blind(C1, Data, _, C2), !.

resolve_c(C2, C2):- !.
resolve_c(C1, C2):-
					sel(C1, C2, [C1]), !.
	
resolve_cego(C1, C2):-	transformacao(C1, C2), resolve_c(C1, C2), !.

trade_tab_list([A, B, C, D, E, F, G, H, I|C1], [A1, B1, C1, D1, E1, F1, G1, H1, I1|_], C1_F, C2_F):-
	C1_F = [A1, B1, C1, D1, E1, F1, G1, H1, I1|C1], C2_F = [A, B, C, D, E, F, G, H, I|C1].

delete_tab_list([_, _, _, _, _, _, _, _, _|C1], C1_F):-
														C1_F = C1.

				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				%%%                                         %%%
				%%%        		Procura informada           %%%
				%%%                                         %%%
				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

				
%%% calcula_hamming/3				
calcula_hamming([], [], 8).
calcula_hamming([A|C1], [A|C2], N):-
									A =\= 0,
									calcula_hamming(C1, C2, N1), !,
									N is N1 - 1.
calcula_hamming([_|C1], [_|C2], N):-
									calcula_hamming(C1, C2, N1), !,
									N is N1+0.

									
									
				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				%%%                                         %%%
				%%%     		   STRUCT      		        %%%
				%%%                                         %%%
				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				
				
node(C, F, G, H, M, data_node(C, F, G, H, M)).

node_config(data_node(C, _, _, _, _), C).
node_function(data_node(_, F, _, _, _), F).
node_g(data_node(_, _, G, _, _), G).
node_hamming(data_node(_, _, _, H, _), H).
node_moves(data_node(_, _, _, _, M), M).


%%% remove_from_list/3
remove_from_list(Acc, [], Output):- Output = Acc, !.
remove_from_list(Tabs, [B|Data], Output):- delete(Tabs, B, Acc), remove_from_list(Acc, Data, Output).


%%% remove_Acc/4
remove_Acc([], _, Acc, Output):- Output = Acc, !.
remove_Acc([A|Data], List, Acc, Output):- node_config(A, C), (C = List -> remove_Acc(Data, List, Acc, Output); remove_Acc(Data, List, [A|Acc], Output)), !.


%%% compress_tabs_from_nodes/3
compress_tabs_from_nodes([], Acc, Output):- Output = Acc, !.
compress_tabs_from_nodes([A|List], Acc, Output):- node_config(A, C), compress_tabs_from_nodes(List, [C|Acc], Output).


%%% gen_nodes/4
%%% funcao que gera e filtra os novos nodes
gen_nodes(C1, S, Blocked_nodes, Free_nodes):-
											findall(Out_put, mov_legal(Out_put, _, _, C1), Poss),
											compress_tabs_from_nodes(Blocked_nodes, [], Blocked),
											remove_from_list(Poss, Blocked, Poss_aux),
											compress_tabs_from_nodes(Free_nodes, [], Free),
											remove_from_list(Poss_aux, Free, S).

											
%%% add_to_free/6
%%% funcao que adiciona os novos nodes livres											
add_to_free(_, [], _, _, Free, Output):- Output = Free, !.
add_to_free(Acc, [A|B], C2, G, Free, Output):-
											calcula_hamming(A, C2, H),
											F is G + H,
											node_moves(Acc, Prev_moves),
											node(A, F, G, H, [A|Prev_moves], New_node),
											add_to_free(Acc, B, C2, G, [New_node|Free], Output).

											
%%% sel_menor/5									
sel_menor([], _, Min_Tab, Output):- Output = Min_Tab, !.
sel_menor([N|Data], Min_aux, Min_Tab, Output):-
											node_function(N, F),
											(F<Min_aux -> sel_menor(Data, F, N, Output);
											sel_menor(Data, Min_aux, Min_Tab, Output)).

											
%%% gen_options/5					
gen_options(C1, C2, G, Free, Blocked):- 
										node_config(C1, Node_tab),
										gen_nodes(Node_tab, New_tabs, Blocked, Free),
										G1 is G + 1,
										add_to_free(C1, New_tabs, C2, G1, Free, New_free),
										selec_h(C1, C2, G1, New_free, Blocked).

										
%%% selec_h/5
%%% seleciona o node com F menor				
%%% Actualiza C1, Free, Blocked e Moves
selec_h(C1, C2, G, Free, Blocked):- 
									node_config(C1, Node_tab),
									remove_Acc(Free, Node_tab, [], New_free),
									sel_menor(New_free, 9999, [], Node_H_menor),
									solve_informada(Node_H_menor, C2, G, New_free, [C1|Blocked]).

									
%%% solve_informada/5									
solve_informada(C1, C2, G, Free, Blocked):-
											node_config(C1, C), (C = C2 -> node_moves(C1, M), 
											reverse_list(M,Final_moves,[]), 
											print_output(Final_moves), !;
											gen_options(C1, C2, G, Free, Blocked)), !.

											
%%% resolve_info_h/2											
resolve_info_h(Tab, C2):-
						transformacao(Tab, C2),
						node(Tab, 0, 0, 0, [Tab], C1),
						solve_informada(C1, C2, 0, [], []), !.


%%% reverse_list/3						
reverse_list([],Z,Z).
reverse_list([H|T],Z,Acc):- reverse_list(T,Z,[H|Acc]).
	
	
%%% print_output/1	
print_output([_|[]]):- !.
print_output([A,B|C]):- mov_legal(A, M, P, B), 
						troca(M, New_M), 
						escreve_solucao([(New_M, P) | []]),
						print_output([B|C]), !.
%%% fix
troca('d', 'e').
troca('e', 'd').
troca('c', 'b').
troca('b', 'c'). 


				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				%%%                                         %%%
				%%%     		   BONUS      		        %%%
				%%%                                         %%%
				%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				
%%% list_min/2
list_min([L|Ls], Min) :- list_min(Ls, L, Min).
list_min([], Min, Min).
list_min([L|Ls], Min0, Min) :-	Min1 is min(L, Min0),
    				list_min(Ls, Min1, Min).


%%% find_index/3
find_index(L,N,I) :- find_index(L, N ,I, 1).
find_index([Head|_],Head ,I,I).
find_index([_|Tail],N,I,C):- 	C1 is C + 1,
    				find_index(Tail,N,I,C1).


%%% calcula_inversos/2					
calcula_inversos(L,Invs):- 	delete(L,0,L1),
    				calcula_inversos(L1,0,Invs).
calcula_inversos([],Invs,Invs).
calcula_inversos(L1,C,Invs):- 	list_min(L1,Min),
    				find_index(L1,Min,I),
    				C1 is C + I-1,
					delete(L1,Min,L2),
    				calcula_inversos(L2,C1,Invs).
					
									
%%% transformacao_possivel/2					
transformacao_possivel(L1,L2):- calcula_inversos(L1,Invs1),
    					calcula_inversos(L2,Invs2),
    					Soma is Invs1+Invs2,
    					Soma mod 2 =:= 0.






