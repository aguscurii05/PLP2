%Ejercicio 1

padre(juan,carlos).
padre(juan,luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

hijo(H,P) :- padre(P,H).

hermano(H1,H2):-padre(P,H1),padre(P,H2),H1\=H2.

descendiente(Des,Anc):-padre(Anc,Des).
descendiente(Des,Anc):-padre(Anc,X),descendiente(Des,X).

ancestro(X, X).
ancestro(X, Y) :-  padre(X, Z),ancestro(Z, Y).

%Ejercicio 3

natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
menorOIgual(X,X) :- natural(X).

%Ejercicio 4

juntar([],L,L).
juntar([H|T],L,[H|T2]):-juntar(T,L,T2).

%Ejercicio 5

%i

last(L,H):- append(_,[H],L).

%ii

reverse([],[]).
reverse([H|T],L):-reverse(T,T2),append(T2,[H],L).

%iii

%prefijo([],[]).
prefijo(P,L):-append(P,_,L).


%iv

%sufijo([],[]).
sufijo(S,L):-append(_,S,L).

%v
sublista(Sub,L):-prefijo(P,L),sufijo(S,L),append(P,Sub,L2),append(L2,S,L). 

%vi

pertenece(E,L):-prefijo(P,L),sufijo(S,L), append(P,[E],L1),append(L1,S,L).


%Ejercicio 6

aplanar([],[]).
aplanar([H|T],Res):-H\=[_|_],aplanar(T,Tr),append([H],Tr,Res).
aplanar([H|T],Res):-H=[_|_],aplanar(H,Hr),aplanar(T,Tr),append(Hr,Tr,Res).

%Ejercicio 7

%i
interseccion([],_,[]).
interseccion([H|T],L,[H|I]):-member(H,L),interseccion(T,L,I).
interseccion([H|T],L,I):-not(member(H,L)),interseccion(T,L,I).


partir(0,L,[],L).
partir(N,[H|T],[H|L1],L2):-N2 is N-1,partir(N2,T,L1,L2),append(L1,L2,T).

%Podria definirse como:

%partir(+N,+L,?L1,?L2).
%partir(+N,?L,+L1,+L2).

%ii
borrar([],_,[]).
borrar([H|T],H,L):-borrar(T,H,L).
borrar([H|T],X,Res):-X\=H,borrar(T,X,L),append([H],L,Res).

%iii

sacarDup([],[]).
sacarDup([X],[X]).
sacarDup([H|T],[H|Res]):-borrar(T,H,L),sacarDup(L,Res).

%iv

%permutacion([],[]).
%permutacion([H|T],L):-member(H,L),borrar(T,H,L1),borrar(L,H,L2),permutacion(L1,L2).

permutacion([],[]).

%v
reparto(L,1,[L]).
reparto(L,N,[P|Ls]):-N>=1,append(P,S,L),N2 is N-1,reparto(S,N2,Ls).

%vi
repartoSinVacias([H|T],[[H|T]]).
repartoSinVacias(L,[P|Ls]):-append(P,S,L),P\=[],repartoSinVacias(S,Ls).
%Ejercicio 9

%Duda

%Ejercicio 11

vacio(nil).

raiz(bin(_,R,_),R).

max(I,D,I):-I>=D.
max(I,D,D):-D>I.

altura(nil,0).
altura(bin(I,_,D),Res):- altura(I,AltI),altura(D,AltD),max(AltI,AltD,M),Res is 1+M.

cantNodos(nil,0).
cantNodos(bin(I,_,D),Res):- cantNodos(I,Ci),cantNodos(D,Cd),Res is 1+Cd+Ci.

%Ejercicio 12

%i

inorder(nil,[]).
inorder(bin(I,R,D),Res):-inorder(I,InI),inorder(D,InD),append(InI,[R],Subres),append(Subres,InD,Res).

%ii

arbolConInorder([],nil).
arbolConInorder(L,bin(InI,R,InD)):-append(I,[R|D],L),arbolConInorder(I,InI),arbolConInorder(D,InD).

%iii
listaMenor([],_).
listaMenor([H|T],N):-H=<N,listaMenor(T,N).

listaMayor([],_).
listaMayor([H|T],N):-H>=N,listaMayor(T,N).

aBB(nil).
aBB(bin(I,R,D)):- inorder(I,LI),inorder(D,LD),aBB(I),aBB(D),listaMenor(LI,R),listaMayor(LD,R).

%Ejercicio 13

desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).


coprimos(X,Y):- desde(1,X),between(1,X,Y),Res is gcd(X,Y),Res=:=1.

%Ejercicio 14

