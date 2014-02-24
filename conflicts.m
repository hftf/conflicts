(* ::Package:: *)

list={"2014ny", "2014nats", "2014wa"};

f[]=False;
f[m_,n_]=m<=A<n;

Clear[sched];Clear[events];Clear[pairs];Clear[conflicts];

Function[file,
	events[file]={};
	fullpath[file]=FileNameJoin[{NotebookDirectory[], "schedules/", file <> ".txt"}];
	table[file]=ToExpression@Import[fullpath[file],"CSV"];

	inequalities[file]=f@@@#&/@(table[file]);

	Flatten@
		With[{l=#},
			(AppendTo[events[file],#];sched[file][#]=First[l])& /@ Rest[#]
		]&/@
			inequalities[file]

]/@list


FalseQ=#===False&;
PairwiseDisjointQ[]=True;
PairwiseDisjointQ[l__]:=
	PairwiseDisjointQ[l]=
		And@@
			FalseQ/@
				Reduce/@
					And@@@
						Subsets[{l},{2}];


pairs[n_]:=pairs[n]=DeleteCases[Map[sched,Subsets[events,{n}],{2}],False,2]
conflicts[n_]:=Monitor[
	(prog[n]=0;conflicts[n]=(prog[n]++;(*##->*)PairwiseDisjointQ@@##)&/@pairs[n]),
	n->ProgressIndicator[prog[n],{0,Length[pairs[n]]}]
]
Pfalse=Count[#,False]/Length[#]&;
falses2[c_]:=Select[c,!#[[2]]&] (* use with -> only *)
Pfalse2=Count[#[[2]]&/@#,False]/Length[#]&;


(#->Pfalse@conflicts@#)&/@Range[0,8]//N (*// RuntimeTools`Profile*)


DiscretePlot[{syoa[[n+1]], bern[[n+1]], Style[nats[[n+1]],Orange]},{n,1,6},PlotRange->{0,1}]


nats={0., 0., 0.055336, 0.161491, 0.305025, 0.466195, 0.623585}
bern={0., 0., 0.057971, 0.168972, 0.318182, 0.483954, 0.643258}
syoa={0., 0., 0.055336, 0.161491, 0.305025, 0.466195, 0.623585}


Reverse[Select[#,Not@FalseQ@#&]&/@pairs[3]//Tally,2]//Sort//Reverse
