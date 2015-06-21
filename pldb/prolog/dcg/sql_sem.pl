
:- op(900,fy,?).
:- op(900,xfy,?).

nothing ? Phrase --> [].
just(Sem) ? Phrase --> call(Phrase,Sem).

*(Phrase,Things) --> seqmap(Phrase,Things).
clist(Phrase,Things) --> seqmap_with_sep(",",Phrase,Things).

