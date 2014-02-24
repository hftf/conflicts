(* ::Package:: *)

Clear[sched];Clear[events];Clear[pairs];Clear[conflicts];


list={"2014nats", "2014wa", "2014nats-nowalkins"};
ns=Range[0,9];

f[]=False;
f[m_,n_]=m<=A<n;

FalseQ=#===False&;
PairwiseDisjointQ[]=True;
PairwiseDisjointQ[l__]:=
	PairwiseDisjointQ[l]=
		And@@
			FalseQ/@
				Reduce/@
					And@@@
						Subsets[{l},{2}];

pairs[file_,n_]:=pairs[file,n]=DeleteCases[Map[sched[file],Subsets[events[file],{n}],{2}],False,2];
conflicts[file_,n_]:=Monitor[
	(prog[file,n]=0;conflicts[file,n]=(prog[file,n]++;(*##->*)PairwiseDisjointQ@@##)&/@pairs[file,n]),
	n->ProgressIndicator[prog[file,n],{0,Length[pairs[file,n]]}]
];
Pfalse=Count[#,False]/Length[#]&;
falses2[c_]:=Select[c,!#[[2]]&]; (* use with -> only *)
Pfalse2=Count[#[[2]]&/@#,False]/Length[#]&;

distros=Function[file,
	events[file]={};
	fullpath[file]=FileNameJoin[{NotebookDirectory[], "schedules/", file <> ".txt"}];
	table[file]=ToExpression@Import[fullpath[file],"CSV"];

	inequalities[file]=f@@@#&/@(table[file]);

	Flatten@
		With[{l=#},
			(AppendTo[events[file],#];sched[file][#]=First[l])& /@ Rest[#]
		]&/@
			inequalities[file];

	Pfalse@
		conflicts[file,#]&/@
			ns
]/@list;

Column[{
	TableForm[N[distros,2],TableHeadings->{list,ns}],
	ListLinePlot[
		distros,
		PlotRange->{0,1},PlotLegends->list,PlotStyle->Thick,AxesStyle->Thick,ImageSize->Medium
	]
}]
Export[FileNameJoin[{NotebookDirectory[], "conflicts.pdf"}],%]


Reverse[Select[#,Not@FalseQ@#&]&/@pairs[3]//Tally,2]//Sort//Reverse
