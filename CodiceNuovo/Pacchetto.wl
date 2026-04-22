(* ::Package:: *)

(* :Title: Hangman-Geoguesser *)
(* :Context: Gioco dell'impiccato dove devo indovinare il paese e successivamente la capitale *)
(* :Author: Billie AI-Lish+ *)
(* :Summary: a preliminary version of the ComplexMap package *)
(* :Copyright: BA 2026 *)
(* :Package Version: 3 *)
(* :Mathematica Version: 14.3 *)
(* :History: last modified 22/04/2026 *)
(* :Keywords: programming style, local variables *)
(* :Sources: biblio *)
(* :Limitations: this is a preliminary version, for educational purposes only. *)
(* :Discussion: *)
(* :Requirements: *)
(* :Warning: Documentare TUTTO il codice *)

BeginPackage["HangmanGame`"];

GeneraInterfaccia::usage = "GeneraInterfaccia[]
	Funzione che permette di generare un'interfaccia interattiva e dinamica, la quale richiama
	le altre funzionalit\[AGrave] del gioco.";


Begin["`Private`"];

(* === DIZIONARIO GEOGRAFICO: Nazioni e Capitali === *)
dizionarioGeografia = <|
  "cina" -> "pechino", "cuba" -> "l'avana", "cile" -> "santiago", "peru" -> "lima",
  "iran" -> "teheran", "iraq" -> "baghdad", "mali" -> "bamako", "nepal" -> "kathmandu",
  "siria" -> "damasco", "india" -> "nuova delhi", "qatar" -> "doha", "yemen" -> "sana'a",
  "italia" -> "roma", "spagna" -> "madrid", "francia" -> "parigi", "svezia" -> "stoccolma",
  "olanda" -> "amsterdam", "belgio" -> "bruxelles", "albania" -> "tirana", "croazia" -> "zagabria",
  "canada" -> "ottawa", "messico" -> "citta del messico", "brasile" -> "brasilia", "russia" -> "mosca",
  "giappone" -> "tokyo", "germania" -> "berlino", "argentina" -> "buenos aires",
  "australia" -> "canberra", "indonesia" -> "giacarta", "portogallo" -> "lisbona",
  "inghilterra" -> "londra", "madagascar" -> "antananarivo", "groenlandia" -> "nuuk",
  "colombia" -> "bogota", "norvegia" -> "oslo", "finlandia" -> "helsinki",
  "grecia" -> "atene", "egitto" -> "il cairo", "marocco" -> "rabat", "thailandia" -> "bangkok"
|>;


(* Implementazione della funzione GeneraEsericizio *)
GeneraEsericizio[ gamemode_:1, seed_:Automatic ] := Module[ 
{ 
	wordlist, (* Lista delle possibili parole *)
	wordlen, (* Lunghezza della parola *)
	word, (* Parola da indovinare *)
	stato, (* Vettore di trattini bassi *)
	errors={}, (* Lista di errori *)
	score=0 (* Punteggio *)
}, 
	(* Imposta la lunghezza delle possibili parole in base alla difficolt\[AGrave] selezionata *)
	If[ gamemode === 1, wordlen = {1,5} ]; (* 1 = facile *)
	If[ gamemode === 2, wordlen = {6,8} ]; (* 2 = media *)
	If[ gamemode === 3, wordlen = {9,15} ]; (* 3 = difficile *)
	
	(* Filtra il dizionario geografico per nazioni della lunghezza desiderata *)
	wordlist = Select[Keys[dizionarioGeografia], Between[wordlen][StringLength[#]] &];
	
	(* Fallback di sicurezza se nessuna parola soddisfa i requisiti *)
	If[Length[wordlist] == 0, wordlist = Keys[dizionarioGeografia]]; 
	
	(* Imposta il seed per la selezione pseudo-casuale *)
	SeedRandom[seed];
	
	(* Estrae una parola casuale e la scompone in caratteri *)
	word = Characters[RandomChoice[wordlist]];
	
	(* Inizializzazione dello stato di gioco con trattini bassi per ogni lettera *)
	stato = InizializzaStato[word];
	
	(* Ritorna il vettore lettere (in minuscolo), lo stato, la lista vuota di errori e punteggio a zero *)
	{ToLowerCase[word], stato, errors, score}
];


(* Mostra la selezione in un pop-up *)
MostraSoluzione[soluzione_List] := Module[
{
	stringa (* Soluzione da mostrare *)
},
	(* Concatenazione della lista di caratteri in una stringa da mostrare all'utente *)
	stringa = StringJoin[soluzione];
	
	(* Messaggio da mostrare all'utente attraverso un pop-up *)
	MessageDialog[
		Panel[
			Row[{"La parola da indovinare era: ", Style[stringa, Bold]}],
			Alignment -> Center
		]
	]
];


(* Fornisce un suggerimento e penalizza il punteggio *)
Suggerimento[word_List, stato_List, errors_List, score_Integer, gameMode_Integer] := Module[
{
	suggerimento, (* Una tra le lettere mancanti *)
	lettereMancanti, (* Lista di lettere mancanti senza ripetizioni *)
	newScore (* Punteggio aggiornato con la penalit\[AGrave] *)
},
	(* Calcola le lettere mancanti senza ripetizioni *)
	lettereMancanti = DeleteDuplicates[Complement[word, stato]];
	
	(* Scelta casuale di una lettera mancante *)
	suggerimento = RandomChoice[lettereMancanti];
	
	(* Penalit\[AGrave]: -5 punti per ogni livello di difficolt\[AGrave]  *)
	newScore = score - 5 * gameMode;
	
	(* Applica il suggerimento aggiornando lo stato senza cambiare punteggio *)
	AggiornaStato[word, stato, suggerimento, newScore, 0, errors]
];


(* Reinizializza lo stato di gioco *)
Pulisci[stato_List] := Module[
{
	newState, (* Vettore di trattini bassi quanti la lunghezza della parola *)
	errors = {}, (* Errori *)
	score = 0 (* Punteggio *)
},
	(* Reinizializza lo stato *)
	newState = InizializzaStato[stato];
	
	(* Ritorna il nuovo stato e la lista vuota degli errori *)
	{newState, errors, score} 
];


(* Funzioni ausiliarie *)
(* Verifica la presenza di caratteri accentati o apostrofi *)
HaCaratteriNonAmmessiQ[s_] := Module[ 
{ 
	accenti = {"\[AGrave]", "\[EGrave]", "\[IGrave]", "\[OGrave]", "\[UGrave]", "\[AAcute]", "\[EAcute]", "\[IAcute]", "\[OAcute]", "\[UAcute]"} (* Vettore delle lettere accentate presenti nella lingua italiana *)
},
	StringContainsQ[s, Alternatives @@ accenti] || StringContainsQ[s, "'"]
];

(* Crea un array di trattini bassi della lunghezza della parola *)
InizializzaStato[word_List] := ConstantArray["_", Length[word]]

(* Aggiorna lo stato in base al tentativo dell'utente *)
AggiornaStato[word_List, currentState_List, guess_, score_Integer, gameMode_, errors_List:{}] := Module[
{
	newState, (* Nuovo vettore che indica le lettere indovinate o trattini bassi *)
	newErrors, (* Nuovo vettore con le lettere non presenti nella parola da indovinare *)
	newScore (* Punteggio aggiornato in caso la lettera \[EGrave] stata indovinata *)
},

	If[MemberQ[word, guess],
		(* Lettera corretta: aggiorno currentState e punteggio *)
		newState = MapThread[If[#2 == guess, guess, #1] &, {currentState, word}];
		newErrors = errors;
		newScore = score + 10 * gameMode, (* In caso di lettera corretta per suggerimento gameMode = 0 *)
		
		(* Lettera sbagliata: mantengo lo stato e aggiungo l'errore *)
		newState = currentState;
		newErrors = Append[errors, guess];
		newScore = score
	];
	
	(* Ritorna un vettore con lo stato, gli errori e il punteggio aggiornati *)
	{newState, newErrors, newScore}
];

(* Salva il record di punteggio in un file JSON *)
SalvaRecord[nome_String, punteggio_Integer, file_:"score.json"] := Module[
{
	record, (* Record da memorizzare *)
	datiEsistenti = {}, (* Record memorizzati precedentemente *)
	nuovoContenuto (* Lista dei 10 migliori record ordinati in base al punteggio *)
},
	record = {"nome" -> nome, "punteggio" -> punteggio};
	datiEsistenti = RecuperaClassifica[];
	nuovoContenuto = Append[datiEsistenti, record];
	nuovoContenuto = Take[Reverse@SortBy[nuovoContenuto, #[[2,2]] &], UpTo[10]];
	Export[file, nuovoContenuto, "JSON"];
];

(* Carica e ordina la classifica dal file JSON *)
RecuperaClassifica[file_:"score.json"] := Module [
{
	classifica = {}
},
	If[FileExistsQ[file], 
	classifica = Import[file, "JSON"]; 
	If[!ListQ[classifica], classifica = {}], 
	classifica = {} 
	];
	classifica = Reverse@SortBy[classifica, #[[2,2]] &] 
];

(* Interfaccia grafica per mostrare la classifica *)
MostraClassificaGUI[score_Integer] := DynamicModule[
{
	nome = "", 
	classifica = {}, 
	punteggioSalvato = False, 
	file = "score.json" 
},
	SetDirectory[NotebookDirectory[]];
	classifica = RecuperaClassifica[];

	CreateDialog[
		Framed[
			Dynamic[
				Column[{
					Style["Classifica", Bold, 16],
						Grid[
							Prepend[
								MapIndexed[{#2[[1]], #[[1,2]], #[[2,2]]} &, classifica],
								{"#", "Nome", "Punteggio"}
							],
						Frame -> All,
						Alignment -> Center
						],
					If[
						!punteggioSalvato,
						Column[{
	                            "Inserisci il tuo nome:",
	                            InputField[Dynamic[nome], String, FieldSize -> 20],
		                            Button[
		                                 "Salva Punteggio",
										Module[{nomeVal = nome},
		                                    SalvaRecord[nomeVal, score];
		                                    classifica = RecuperaClassifica[];
		                                    punteggioSalvato = True;
		                                 ],
		                                 Enabled -> Dynamic[StringLength[nome] > 0]
		                            ]
	                        }],
	                        Button["Chiudi", DialogReturn[]]
					]
				}, Spacings -> 1.5]
			],
			FrameMargins -> 20,
			FrameStyle -> None,
			RoundingRadius -> 10,
			Background -> LightGray
		],
		WindowTitle->"Classifica"
	]
];


(* Disegna l'impiccato in base al numero di errori *)
DisegnaImpiccato[n_] := Graphics[
  {
    Thick,
    Line[{{0, 0}, {3, 0}}], 
    Line[{{1.5, 0}, {1.5, 5}}], 
    Line[{{1.5, 5}, {3, 5}}], 
    Line[{{3, 5}, {3, 4.5}}], 

    If[n >= 1, Circle[{3, 4}, 0.5], Nothing], 
    If[n >= 2, Line[{{3, 3.5}, {3, 2.5}}], Nothing], 
    If[n >= 3, Line[{{3, 3.3}, {2.7, 3}}], Nothing], 
    If[n >= 4, Line[{{3, 3.3}, {3.3, 3}}], Nothing], 
    If[n >= 5, Line[{{3, 2.5}, {2.7, 2}}], Nothing], 
    If[n >= 6, Line[{{3, 2.5}, {3.3, 2}}], Nothing] 
  },
  PlotRange -> {{0, 4}, {0, 6}}, ImageSize -> 200
]


(* righe della tastiera con relativo offset *)
righeTastiera = {
  {0, {"Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"}},   
  {1, {"A", "S", "D", "F", "G", "H", "J", "K", "L"}},        
  {2, {"Z", "X", "C", "V", "B", "N", "M"}}              
};


(* Genera l'interfaccia complete per il gioco *)
GeneraInterfaccia[] := DynamicModule[
  {
    seed, 
    seedError = "", 
    fase = "selezione", 
    gamemode = 1, 
    parola, stato, errori, score, 
    letteraUtente = "", 
    messaggio = "", 
    maxErrori = 6, 
    classificaMostrata = False,
	(* Variabili per la fase Bonus *)
	rispostaBonus = "",
	faseBonusCompletata = False,
	messaggioBonus = ""
  },

  Dynamic[
    Switch[fase, 

      "selezione", 
      Column[{
        Style["\|01f3afSeleziona la difficolt\[AGrave]", Bold, 16], 
        RadioButtonBar[Dynamic[gamemode], {1 -> "Facile", 2 -> "Media", 3 -> "Difficile"}], 
        Row[{"Seed (opzionale): ", InputField[
				Dynamic[seed, ({seed, seedError} = If[StringMatchQ[#, DigitCharacter ..] || StringMatchQ[#, ""], {#, ""}, {seed, "\:26a0\:fe0f Inserire solo numeri naturali (0, 1, 2, ...)."}]) &],  
				String, 
				FieldHint->"Inserire un numero naturale", 
				ContinuousAction->True 
				]}], 
        Dynamic[
			If[seedError != "",
				Style[seedError, Red, Italic], 
				"" 
			]
        ],
        Button["Inizia partita", 
          Module[{},
            If[StringQ[seed] && seed != "", seed = ToExpression[seed], seed = Automatic]; 
            {parola, stato, errori, score} = GeneraEsericizio[gamemode, seed]; 
            fase = "gioco"; 
            letteraUtente = ""; messaggio = ""; classificaMostrata = False;
			(* Resetta le variabili della fase bonus *)
			rispostaBonus = ""; faseBonusCompletata = False; messaggioBonus = "";
          ],
          ImageSize -> {200, 60}, 
          BaseStyle -> {FontSize -> 16, Bold} 
        ]
      }, Spacings -> 2], 

      "gioco", 
      Column[{
        Style["Gioco dell'impiccato", Bold, 20],
        Dynamic[Row[{"Punteggio: ", Style[score, Blue, Bold]}]], 
        Dynamic[Row[Riffle[If[# === "_", Style[" _ ", Gray], Style[#]] & /@ stato, " "]]], 
        
        Row[{
          Button["\|01f4a1Suggerimento", 
            {stato, errori, score} = Suggerimento[parola, stato, errori, score, gamemode], 
			Enabled -> MemberQ[stato, "_"] && Length[errori] < maxErrori
          ],
          Spacer[20], 
          Button["\|01f50eMostra soluzione",
           MostraSoluzione[parola] 
           {stato = parola, fase = "selezione", letteraUtente = ""; messaggio = ""; seedError = ""}
           ]
        }],

        Dynamic[Row[{"Lettere sbagliate: ", StringJoin[Riffle[errori, ", "]]}]], 
        Dynamic[Row[{"Errori: ", Length[errori], "/", maxErrori}]], 
        Dynamic[Style[messaggio, Blue]], 
        Dynamic[DisegnaImpiccato[Length[errori]]], 
		Dynamic[
			Column[
				Map[
					Row[
						Join[
							{Spacer[#[[1]]*25]}, 
							Table[
								With[{l = lettera}, 
									Button[
										l, 
										Module[{guess = ToLowerCase[StringTrim[l]]}, 
											{stato, errori, score} = AggiornaStato[parola, stato, guess, score, gamemode, errori]; 
											messaggio = If[MemberQ[parola, guess], "Lettera corretta!", "Lettera sbagliata!"]; 
										], 
										Enabled -> !MemberQ[Join[stato, errori], ToLowerCase[l]] &&
											MemberQ[stato, "_"] &&
											Length[errori] < maxErrori,
										Background->Which[
											MemberQ[stato, ToLowerCase[l]], LightGreen,
											MemberQ[errori, ToLowerCase[l]], LightRed],
										ImageSize -> {40, 40}
									]
								],
								{lettera, #[[2]]} 
							]
						],
						Spacer[5] 
					] &,
					righeTastiera
				], 
				Spacings -> 1 
			]
		],
        Dynamic[
        If[
            stato === parola || Length[errori] >= maxErrori, 
            Column[{
              If[stato === parola, 
                (* GESTIONE VITTORIA E DOMANDA BONUS *)
				If[!faseBonusCompletata,
					Column[{
						Style["Hai vinto!", Green, Bold, 16],
						Style["Domanda Bonus (+50 punti extra!):", Purple, Bold],
						Row[{"Qual \[EGrave] la capitale di ", Capitalize[StringJoin[parola]], "? "}],
						Row[{
							InputField[Dynamic[rispostaBonus], String, FieldSize -> 15],
							Button["Conferma",
								If[ToLowerCase[StringTrim[rispostaBonus]] === dizionarioGeografia[StringJoin[parola]],
									score = score + 50;
									messaggioBonus = "Risposta corretta! Hai guadagnato 50 punti.",
									messaggioBonus = "Sbagliato! La risposta corretta era: " <> Capitalize[dizionarioGeografia[StringJoin[parola]]]
								];
								faseBonusCompletata = True;
							]
						}]
					}, Alignment -> Center],

					(* SE LA FASE BONUS E' FINITA, MOSTRA CLASSIFICA *)
					Column[{
						Style[messaggioBonus, If[StringContainsQ[messaggioBonus, "corretta"], Green, Red], Bold],
						Module[{},
							If[!classificaMostrata, 
								classificaMostrata = True;
								MostraClassificaGUI[score]; 
							];        
							""
						]
					}, Alignment -> Center]
				],

                (* GESTIONE SCONFITTA *)
				Column[{
					Module[{},
						If[!classificaMostrata, 
							classificaMostrata = True;
							MostraClassificaGUI[score]; 
						];        
						Style["Hai perso! La nazione era: " <> Capitalize[StringJoin[parola]], Red, Bold]
					]
				}, Alignment -> Center]
              ],
              Button["\|01f9fdPulisci",
                {stato, errori, score} = Pulisci[parola];
				faseBonusCompletata = False; rispostaBonus = ""; messaggioBonus = "";
              ]
            }],
            ""        
          ]
        ],
        Button["\|01f504Nuova partita", 
          Module[{},
            fase = "selezione"; 
            letteraUtente = ""; messaggio = ""; seedError = "";
			rispostaBonus = ""; faseBonusCompletata = False; messaggioBonus = "";
          ]
        ]
      }]
    ]
  ]
];


End[];


EndPackage[]





