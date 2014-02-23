(* ::Package:: *)

Unprotect@Interval; Interval[{}]=Interval[]; Protect@Interval;
p=FileNameJoin[{NotebookDirectory[], "2014ny.txt"}];
d=ToExpression@Import[p,"CSV"]

f[]=False
f[m_,n_]=m<=A<n
i=f@@@#&/@d

Clear[sched];events={};Clear[pairs];Clear[conflicts];
With[{l=#},(AppendTo[events,#];sched[#]=First[l])&/@Rest[#]]&/@i//Flatten


(*?sched
?events*)

(*MyAnd[n___ /; Length[{n}] == 1]:=True
MyAnd[n___]:=And[n]*)


FalseQ=#===False&;
PairwiseDisjointQ[]=True;
PairwiseDisjointQ[l__]:=
	PairwiseDisjointQ[l]=
		And@@
			FalseQ/@
				Reduce/@
					And@@@
						Subsets[{l},{2}];
	
{{0<=A<1},{False},{0<=A<1,False},{1<=A<2,1<=A<2,2<=A<3},{0<=A<1,1<=A<2,2<=A<3,False}};
PairwiseDisjointQ@@@%


(Not@FalseQ@#&)[True]


pairs[n_]:=pairs[n]=DeleteCases[Map[sched,Subsets[events,{n}],{2}],False,2]
conflicts[n_]:=Monitor[
	(prog[n]=0;conflicts[n]=(prog[n]++;(*##->*)PairwiseDisjointQ@@##)&/@pairs[n]),
	n->ProgressIndicator[prog[n],{0,Length[pairs[n]]}]
]
Pfalse=Count[#,False]/Length[#]&;
falses2[c_]:=Select[c,!#[[2]]&] (* use with -> only *)
Pfalse2=Count[#[[2]]&/@#,False]/Length[#]&;


Pfalse@conflicts[1]
Pfalse@conflicts[2]
Pfalse@conflicts[3]
Pfalse@conflicts[4]
Pfalse@conflicts[5]


(#->Pfalse@conflicts@#)&/@Range[0,8]//N (*// RuntimeTools`Profile*)


DiscretePlot[{syoa[[n+1]], bern[[n+1]], Style[nats[[n+1]],Orange]},{n,1,6},PlotRange->{0,1}]


nats={0., 0., 0.055336, 0.161491, 0.305025, 0.466195, 0.623585}
bern={0., 0., 0.057971, 0.168972, 0.318182, 0.483954, 0.643258}
syoa={0., 0., 0.055336, 0.161491, 0.305025, 0.466195, 0.623585}


pairs[3]


Reverse[Select[#,Not@FalseQ@#&]&/@pairs[3],2]//Sort


DeleteDuplicates[{1<=A<2,1<=A<2}]


a=pairs[3]
DeleteCases[a,False,2]
PairwiseDisjointQ@@a[[1]]
PairwiseDisjointQ@@DeleteDuplicates@a[[1]]


PairwiseDisjointQ[{}]
