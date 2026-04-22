(* ::Package:: *)

(* ::Package:: *)

(* :Title: HangmanGame *)
(* :Context: HangmanGame` *)
(* :Author: AN, LV, CN *)
(* :Summary: funzioni utilizzate nell'interfaccia hangman.nb *)
(* :Copyright: AN, LV, CN 2025 *)
(* :Package Version: 9 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 17/05/2025 *)
(* :Sources: bblio *)
(* :Limitations: educational purposes *)
(* :Discussion: *)
(* :Requirements: *)

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
	stringa = StringJoin[soluzione];
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
	suggerimento, 
	lettereMancanti, 
	newScore 
},
	lettereMancanti = DeleteDuplicates[Complement[word, stato]];
	suggerimento = RandomChoice[lettereMancanti];
	newScore = score - 5 * gameMode;
	AggiornaStato[word, stato, suggerimento, newScore, 0, errors]
];


(* Reinizializza lo stato di gioco *)
Pulisci[stato_List] := Module[
{
	newState, 
	errors = {}, 
	score = 0 
},
	newState = InizializzaStato[stato];
	{newState, errors, score} 
];


(* Funzioni ausiliarie *)
HaCaratteriNonAmmessiQ[s_] := Module[ 
{ 
	accenti = {"\[AGrave]", "\[EGrave]", "\[IGrave]", "\[OGrave]", "\[UGrave]", "\[AAcute]", "\[EAcute]", "\[IAcute]", "\[OAcute]", "\[UAcute]"} 
},
	StringContainsQ[s, Alternatives @@ accenti] || StringContainsQ[s, "'"]
];

InizializzaStato[word_List] := ConstantArray["_", Length[word]]

AggiornaStato[word_List, currentState_List, guess_, score_Integer, gameMode_, errors_List:{}] := Module[
{
	newState, 
	newErrors, 
	newScore 
},
	If[MemberQ[word, guess],
		newState = MapThread[If[#2 == guess, guess, #1] &, {currentState, word}];
		newErrors = errors;
		newScore = score + 10 * gameMode, 
		
		newState = currentState;
		newErrors = Append[errors, guess];
		newScore = score
	];
	{newState, newErrors, newScore}
];

(* ================= GESTIONE CLASSIFICA E DATI ================= *)

(* Salva il record controllando l'esistenza del nome *)
SalvaRecord[nome_String, punteggio_Integer, file_:"score.json"] := Module[
{
	datiEsistenti = {}, 
	nuovoContenuto,
	punteggioMax = punteggio
},
	datiEsistenti = RecuperaClassifica[file];
	
	(* Cerca se il nome esiste gi\[AGrave] e aggiorna il punteggio massimo storico *)
	Map[
		If[Lookup[#, "nome", ""] == nome, 
			punteggioMax = Max[punteggioMax, Lookup[#, "punteggio", 0]]
		]&, 
		datiEsistenti
	];
	
	(* Rimuove tutte le vecchie occorrenze di quel nome in modo blindato *)
	datiEsistenti = DeleteCases[datiEsistenti, x_ /; Lookup[x, "nome", ""] == nome];
  
	(* Aggiunge il record aggiornato *)
	nuovoContenuto = Append[datiEsistenti, {"nome" -> nome, "punteggio" -> punteggioMax}];

	(* Ordina in modo decrescente e prende i primi 50 *)
	nuovoContenuto = Reverse[SortBy[nuovoContenuto, Lookup[#, "punteggio", 0] &]];
	nuovoContenuto = Take[nuovoContenuto, UpTo[50]]; 
	
	(* Sovrascrivi il file JSON *)
	Export[file, nuovoContenuto, "JSON"];
];

(* Carica e ordina la classifica dal file JSON in modo sicuro *)
RecuperaClassifica[file_:"score.json"] := Module [
{
	classifica = {} 
},
	If[FileExistsQ[file], 
		classifica = Quiet[Import[file, "JSON"]]; 
		If[!ListQ[classifica], classifica = {}]
	];
	
	(* Ordina in ordine decrescente in modo sicuro *)
	classifica = Reverse[SortBy[classifica, Lookup[#, "punteggio", 0] &]];
	classifica
];

(* Interfaccia grafica infallibile (Macchina a Stati) *)
MostraClassificaGUI[score_Integer] := DynamicModule[
{
	nomeUtente = "", 
	datiClassifica = {}, 
	faseClassifica = 1, (* 1 = Inserimento, 2 = Conferma, 3 = Salvato *)
	file = "score.json" 
},
	(* Imposta la directory ma non si blocca se c'e' un errore *)
	Quiet[SetDirectory[NotebookDirectory[]]];
	datiClassifica = RecuperaClassifica[];

	CreateDialog[
		Framed[
			Dynamic[ (* Il Dynamic avvolge tutto e reagisce ai cambiamenti di 'faseClassifica' *)
				Column[{
					Style["Classifica Globale", Bold, 16],
					
						(* Tabella con barra di scorrimento automatica *)
						Pane[
							Grid[
								Prepend[
									MapIndexed[{#2[[1]], Lookup[#, "nome", "N/A"], Lookup[#, "punteggio", 0]} &, datiClassifica],
									{"#", "Nome", "Punteggio"}
								],
							Frame -> All,
							Alignment -> Center
							],
							{Automatic, 200}, 
							Scrollbars -> Automatic
						],
						
					Spacer[10],

					(* Macchina a stati per l'interazione *)
					Switch[faseClassifica,
						
						1, (* Fase di Inserimento *)
						Column[{
		                        "Inserisci il tuo nome:",
								(* L'opzione ContinuousAction aggiorna in tempo reale per sbloccare il bottone *)
		                        InputField[Dynamic[nomeUtente], String, FieldSize -> 20, ContinuousAction -> True],
		                        Button["Salva Punteggio",
									 faseClassifica = 2;, (* Passa alla fase 2 *)
		                             Enabled -> Dynamic[StringLength[nomeUtente] > 0]
		                        ]
		                }, Alignment -> Center],
		                    
						2, (* Fase di Conferma Inline *)
						Column[{
								Style["Sei sicuro che '" <> nomeUtente <> "' sia corretto?", Darker[Red], Bold],
								Row[{
									Button["S\[IGrave], Salva",
										Module[{nomeVal = nomeUtente},
											SalvaRecord[nomeVal, score];
											datiClassifica = RecuperaClassifica[]; (* Ricarica i dati per mostrarli *)
											faseClassifica = 3; (* Passa alla fase 3 *)
										],
										Background -> LightGreen
									],
									Spacer[10],
									Button["No, Modifica", faseClassifica = 1;, Background -> LightRed]
								}]
						}, Alignment -> Center],
						
						3, (* Fase Finale Salvata *)
						Column[{
							Style["Punteggio salvato con successo!", Darker[Green], Bold],
							Spacer[5],
	                        Button["Chiudi Finestra", DialogReturn[]]
						}, Alignment -> Center]
					]

				}, Spacings -> 1.5, Alignment -> Center]
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
    messaggio = "", 
    maxErrori = 6, 
	(* Variabili per la fase Bonus *)
	opzioniBonus = {}, 
	faseBonusCompletata = False,
	messaggioBonus = ""
  },

  Dynamic[
    Switch[fase, 

      "selezione", 
      Column[{
        Style["\|01f3af Seleziona la difficolt\[AGrave]", Bold, 16], 
        RadioButtonBar[Dynamic[gamemode], {1 -> "Facile", 2 -> "Media", 3 -> "Difficile"}], 
        Row[{"Seed (opzionale): ", InputField[
				Dynamic[seed, ({seed, seedError} = If[StringMatchQ[#, DigitCharacter ..] || StringMatchQ[#, ""], {#, ""}, {seed, "\:26a0\:fe0f Inserire solo numeri naturali (0, 1, 2, ...)."}]) &],  
				String, 
				FieldHint->"Inserire un numero naturale", 
				ContinuousAction->True 
				]}], 
        Dynamic[
			If[seedError != "", Style[seedError, Red, Italic], ""]
        ],
        Button["Inizia partita", 
          Module[{},
            If[StringQ[seed] && seed != "", seed = ToExpression[seed], seed = Automatic]; 
            {parola, stato, errori, score} = GeneraEsericizio[gamemode, seed]; 
            fase = "gioco"; 
            messaggio = "";
			opzioniBonus = {}; faseBonusCompletata = False; messaggioBonus = "";
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
          Button["\|01f4a1 Suggerimento", 
            {stato, errori, score} = Suggerimento[parola, stato, errori, score, gamemode], 
			Enabled -> MemberQ[stato, "_"] && Length[errori] < maxErrori
          ],
          Spacer[20], 
          Button["\|01f50e Mostra soluzione",
           MostraSoluzione[parola] 
           {stato = parola, fase = "selezione", messaggio = ""; seedError = ""}
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
					
					(* Genera 4 opzioni random per il Bonus (1 corretta, 3 sbagliate) *)
					If[opzioniBonus === {},
						Module[{corretta = dizionarioGeografia[StringJoin[parola]], sbagliate},
							sbagliate = RandomSample[DeleteCases[Values[dizionarioGeografia], corretta], 3];
							opzioniBonus = RandomSample[Join[{corretta}, sbagliate]];
						]
					];

					Column[{
						Style["Hai vinto!", Green, Bold, 16],
						Style["Domanda Bonus (+50 punti extra!):", Purple, Bold],
						Row[{"Qual \[EGrave] la capitale di ", Capitalize[StringJoin[parola]], "? "}],
						
						(* Generazione bottoni Bonus a scelta multipla *)
						Row[Riffle[
							Button[Capitalize[#],
								If[# === dizionarioGeografia[StringJoin[parola]],
									score = score + 50; 
									messaggioBonus = "Risposta corretta! Hai guadagnato 50 punti extra.",
									messaggioBonus = "Sbagliato! La risposta corretta era: " <> Capitalize[dizionarioGeografia[StringJoin[parola]]]
								];
								faseBonusCompletata = True;
							] & /@ opzioniBonus, 
							Spacer[10] 
						]]
					}, Alignment -> Center],

					(* SE LA FASE BONUS E' FINITA, PULSANTE SICURO PER CLASSIFICA *)
					Column[{
						Style[messaggioBonus, If[StringContainsQ[messaggioBonus, "corretta"], Green, Red], Bold],
						Spacer[10],
						Button["\|01f3c6 Salva e Mostra Classifica", MostraClassificaGUI[score], ImageSize -> {250, 40}, Background -> LightBlue]
					}, Alignment -> Center]
				],

                (* GESTIONE SCONFITTA *)
				Column[{
					Style["Hai perso! La nazione era: " <> Capitalize[StringJoin[parola]], Red, Bold],
					Spacer[10],
					Button["\|01f3c6 Salva e Mostra Classifica", MostraClassificaGUI[score], ImageSize -> {250, 40}, Background -> LightBlue]
				}, Alignment -> Center]
              ],
			  Spacer[15],
              Button["\|01f9fd Pulisci Campi",
                {stato, errori, score} = Pulisci[parola];
				opzioniBonus = {}; faseBonusCompletata = False; messaggioBonus = "";
              ]
            }],
            ""        
          ]
        ],
        Button["\|01f504 Nuova partita", 
          Module[{},
            fase = "selezione"; 
            messaggio = ""; seedError = "";
			opzioniBonus = {}; faseBonusCompletata = False; messaggioBonus = "";
          ]
        ]
      }]
    ]
  ]
];

End[];

EndPackage[]
