(* ::Package:: *)

(* ::Package:: *)
(**)


(* :Title:            Geographic Hangman                          *)
(* :Context:          Gioco dell'impiccato geografico             *)
(* :Author:           Gli impiccati                          *)
(* :Summary:          Interfaccia interattiva per indovinare       *)
(*                    nazioni e capitali del mondo                 *)
(* :Copyright:        BA 2026                                     *)
(* :Package Version:  4                                           *)
(* :Mathematica Ver.: 14.3                                        *)
(* :History:          last modified 10/05/2026                    *)
(* :Keywords:         DynamicModule, interfaccia, gioco           *)
(* :Discussion: *)
(* :Requirements: *)
(* :Limitations:
*)



(* BeginPackage dichiara il contesto pubblico del package.
   Tutte le funzioni definite qui saranno accessibili dall'esterno
   con il nome HangmanGame`NomeFunzione *)
BeginPackage["HangmanGame`"];

(* Stringa di utilizzo pubblica: appare quando l'utente chiama ?GeneraInterfaccia *)
GeneraInterfaccia::usage = "GeneraInterfaccia[]
	Funzione che permette di generare un'interfaccia interattiva e dinamica, la quale richiama
	le altre funzionalit\[AGrave] del gioco.";


(* Begin["`Private`"] apre il contesto privato del package.
   Tutte le funzioni definite da qui in poi sono interne e non accessibili
   direttamente dall'utente finale \[LongDash] non compaiono nell'autocompletamento *)
Begin["`Private`"];

(* ============================================================== *)
(* DIZIONARIO GEOGRAFICO                                          *)
(* Association Mathematica: mappa chiave->valore                  *)
(* Chiave  = nome della nazione (stringa in minuscolo)            *)
(* Valore  = nome della capitale (stringa in minuscolo)           *)
(* Usato sia per estrarre le parole da indovinare                 *)
(* sia per la domanda bonus sulla capitale                        *)
(* ============================================================== *)
dizionarioGeografia = <|
  "afghanistan" -> "kabul", "albania" -> "tirana", "algeria" -> "algeri", "andorra" -> "andorra la vella", 
  "angola" -> "luanda", "antigua e barbuda" -> "saint john's", "arabia saudita" -> "riyad", "argentina" -> "buenos aires", 
  "armenia" -> "erevan", "australia" -> "canberra", "austria" -> "vienna", "azerbaigian" -> "baku", 
  "bahamas" -> "nassau", "bahrein" -> "manama", "bangladesh" -> "dacca", "barbados" -> "bridgetown", 
  "belgio" -> "bruxelles", "belize" -> "belmopan", "benin" -> "porto-novo", "bhutan" -> "thimphu", 
  "bielorussia" -> "minsk", "birmania" -> "naypyidaw", "bolivia" -> "sucre", "bosnia ed erzegovina" -> "sarajevo", 
  "botswana" -> "gaborone", "brasile" -> "brasilia", "brunei" -> "bandar seri begawan", "bulgaria" -> "sofia", 
  "burkina faso" -> "ouagadougou", "burundi" -> "citega", "cambogia" -> "phnom penh", "camerun" -> "yaounde", 
  "canada" -> "ottawa", "capo verde" -> "praia", "repubblica ceca" -> "praga", "repubblica centrafricana" -> "bangui", 
  "ciad" -> "n'djamena", "cile" -> "santiago", "cina" -> "pechino", "cipro" -> "nicosia", 
  "colombia" -> "bogota", "comore" -> "moroni", "corea del nord" -> "pyongyang", "corea del sud" -> "seul", 
  "costa d'avorio" -> "yamoussoukro", "costa rica" -> "san jose", "croazia" -> "zagabria", "cuba" -> "l'avana", 
  "danimarca" -> "copenaghen", "dominica" -> "roseau", "ecuador" -> "quito", "egitto" -> "il cairo", 
  "el salvador" -> "san salvador", "emirati arabi uniti" -> "abu dhabi", "eritrea" -> "asmara", "estonia" -> "tallinn", 
  "etiopia" -> "addis abeba", "figi" -> "suva", "filippine" -> "manila", "finlandia" -> "helsinki", 
  "francia" -> "parigi", "gabon" -> "libreville", "gambia" -> "banjul", "georgia" -> "tbilisi", 
  "germania" -> "berlino", "ghana" -> "accra", "giamaica" -> "kingston", "giappone" -> "tokyo", 
  "gibuti" -> "gibuti", "giordania" -> "amman", "grecia" -> "atene", "grenada" -> "saint george's", 
  "guatemala" -> "citta del guatemala", "guinea" -> "conakry", "guinea-bissau" -> "bissau", "guinea equatoriale" -> "malabo", 
  "guyana" -> "georgetown", "haiti" -> "port-au-prince", "honduras" -> "tegucigalpa", "india" -> "nuova delhi", 
  "indonesia" -> "giacarta", "iran" -> "teheran", "iraq" -> "baghdad", "irlanda" -> "dublino", 
  "islanda" -> "reykjavik", "israele" -> "gerusalemme", "italia" -> "roma", "kazakistan" -> "astana", 
  "kenya" -> "nairobi", "kirghizistan" -> "bisceek", "kiribati" -> "tarawa sud", "kuwait" -> "kuwait city", 
  "laos" -> "vientiane", "lesotho" -> "maseru", "letonia" -> "riga", "libano" -> "beirut", 
  "liberia" -> "monrovia", "libia" -> "tripoli", "liechtenstein" -> "vaduz", "lituania" -> "vilnius", 
  "lussemburgo" -> "lussemburgo", "madagascar" -> "antananarivo", "malawi" -> "lilongwe", "malesia" -> "kuala lumpur", 
  "maldive" -> "male", "mali" -> "bamako", "malta" -> "la valletta", "marocco" -> "rabat", 
  "isole marshall" -> "majuro", "mauritania" -> "nouakchott", "mauritius" -> "port louis", "messico" -> "citta del messico", 
  "micronesia" -> "palikir", "moldavia" -> "chisinau", "monaco" -> "monaco", "mongolia" -> "ulaanbaatar", 
  "montenegro" -> "podgorica", "mozambico" -> "maputo", "namibia" -> "windhoek", "nauru" -> "yaren", 
  "nepal" -> "kathmandu", "nicaragua" -> "managua", "niger" -> "niamey", "nigeria" -> "abuja", 
  "norvegia" -> "oslo", "nuova zelanda" -> "wellington", "oman" -> "mascate", "paesi bassi" -> "amsterdam", 
  "pakistan" -> "islamabad", "palau" -> "ngerulmud", "panama" -> "panama", "papua nuova guinea" -> "port moresby", 
  "paraguay" -> "asuncion", "peru" -> "lima", "polonia" -> "varsavia", "portogallo" -> "lisbona", 
  "qatar" -> "doha", "regno unito" -> "londra", "romania" -> "bucarest", "russia" -> "mosca", 
  "rwanda" -> "kigali", "saint kitts e nevis" -> "basseterre", "santa lucia" -> "castries", "saint vincent e grenadine" -> "kingstown", 
  "isole salomone" -> "honiara", "samoa" -> "apia", "san marino" -> "san marino", "sao tome e principe" -> "sao tome", 
  "senegal" -> "dakar", "serbia" -> "belgrado", "seychelles" -> "victoria", "sierra leone" -> "freetown", 
  "singapore" -> "singapore", "siria" -> "damasco", "slovacchia" -> "bratislava", "slovenia" -> "lubiana", 
  "somalia" -> "mogadiscio", "spagna" -> "madrid", "sri lanka" -> "sri jayawardenepura kotte", "stati uniti" -> "washington", 
  "sudafrica" -> "pretoria", "sudan" -> "khartum", "sudan del sud" -> "juba", "suriname" -> "paramaribo", 
  "svezia" -> "stoccolma", "svizzera" -> "berna", "swaziland" -> "mbabane", "tagikistan" -> "dushanbe", 
  "taiwan" -> "taipei", "tanzania" -> "dodoma", "thailandia" -> "bangkok", "timor est" -> "dili", 
  "togo" -> "lome", "tonga" -> "nuku'alofa", "trinidad e tobago" -> "port of spain", "tunisia" -> "tunisi", 
  "turchia" -> "ankara", "turkmenistan" -> "ashgabat", "tuvalu" -> "funafuti", "ucraina" -> "kiev", 
  "uganda" -> "kampala", "ungheria" -> "budapest", "uruguay" -> "montevideo", "uzbekistan" -> "tashkent", 
  "vanuatu" -> "port vila", "vaticano" -> "citta del vaticano", "venezuela" -> "caracas", "vietnam" -> "hanoi", 
  "yemen" -> "sana'a", "zambia" -> "lusaka", "zimbabwe" -> "harare"
|>;


(* Implementazione della funzione GeneraEsericizio *)
(* ============================================================== *)
(* GeneraEsericizio                                               *)
(* Genera una nuova partita selezionando una parola dal           *)
(* dizionario in base alla difficolt\[AGrave] e al seed forniti          *)
(*                                                                *)
(* Parametri:                                                     *)
(*   gamemode : 1 (facile), 2 (media), 3 (difficile)             *)
(*              default = 1 se non specificato                    *)
(*   seed     : intero per la selezione deterministica            *)
(*              default = Automatic (casuale ad ogni esecuzione)  *)
(*                                                                *)
(* Restituisce: {parola, stato, errori, score}                    *)
(*   parola   = lista di caratteri in minuscolo es. {"i","t",...} *)
(*   stato    = lista di "_" della stessa lunghezza               *)
(*   errori   = lista vuota {}                                    *)
(*   score    = 0                                                 *)
(* ============================================================== *)
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
	If[ gamemode === 1, wordlen = {1,5} ]; (* facile: parole corte, max 5 caratteri *)
	If[ gamemode === 2, wordlen = {6,8} ]; (* media: parole di lunghezza media *)
	If[ gamemode === 3, wordlen = {9,15} ]; (* difficile: parole lunghe *)
	
	(* Filtra il dizionario geografico per nazioni della lunghezza desiderata *)
	wordlist = Select[Keys[dizionarioGeografia], Between[wordlen][StringLength[#]] &];
	
	(* Fallback di sicurezza se nessuna parola soddisfa i requisiti *)
	If[Length[wordlist] == 0, wordlist = Keys[dizionarioGeografia]];
	
	(* SeedRandom fissa il generatore casuale: con lo stesso seed
	   RandomChoice selezioner\[AGrave] sempre la stessa parola *)
	SeedRandom[seed];
	
	(* RandomChoice sceglie una nazione casuale dalla wordlist filtrata.
	   Characters la scompone in lista di caratteri: "cina" -> {"c","i","n","a"} *)
	word = Characters[RandomChoice[wordlist]];
	
	(* Inizializzazione dello stato di gioco con trattini bassi per ogni lettera *)
	stato = InizializzaStato[word];
	
	(* Ritorna il vettore lettere (in minuscolo), lo stato, la lista vuota di errori e punteggio a zero *)
	{ToLowerCase[word], stato, errors, score}
];


(* ============================================================== *)
(* MostraSoluzione                                                *)
(* Mostra la parola completa in un dialog modale quando           *)
(* il giocatore sceglie di arrendersi                             *)
(*                                                                *)
(* Parametri:                                                     *)
(*   soluzione : lista di caratteri (es. {"c","i","n","a"})       *)
(* ============================================================== *)
MostraSoluzione[soluzione_List] := Module[
{
    stringa (* Stringa ricostruita dalla lista di caratteri *)
}, 
 
    (* StringJoin riunisce la lista di caratteri in un'unica stringa *)
	stringa = StringJoin[soluzione];
	
	(* MessageDialog apre una finestra modale con il messaggio.
	   Panel aggiunge un bordo e centra il contenuto.
	   Style[stringa, Bold] mette la soluzione in grassetto *)
	MessageDialog[
		Panel[
			Row[{"La parola da indovinare era: ", Style[stringa, Bold]}],
			Alignment -> Center
		]
	]
];


(* ============================================================== *)
(* Suggerimento                                                   *)
(* Rivela una lettera mancante casuale e penalizza il punteggio   *)
(*                                                                *)
(* Parametri:                                                     *)
(*   word      : lista di caratteri della parola da indovinare    *)
(*   stato     : lista dello stato corrente (lettere o "_")       *)
(*   errors    : lista delle lettere sbagliate finora             *)
(*   score     : punteggio corrente                               *)
(*   gameMode  : difficolta' corrente (1/2/3)                     *)
(*                                                                *)
(* Restituisce: {newStato, newErrors, newScore} tramite           *)
(*              AggiornaStato                                     *)
(* ============================================================== *)
Suggerimento[word_List, stato_List, errors_List, score_Integer, gameMode_Integer] := Module[

{  suggerimento,  (* Lettera scelta come suggerimento *)
   lettereMancanti, (* Lettere della parola non ancora indovinate *)
   newScore    (* Punteggio dopo la penalit\[AGrave] *)
   },

(* Complement trova le lettere presenti in word ma non ancora in stato.
	   DeleteDuplicates evita che lettere ripetute contino due volte *)
	lettereMancanti = DeleteDuplicates[Complement[word, stato]];
	
	(* RandomChoice sceglie una lettera casuale tra quelle ancora da scoprire *)
	suggerimento = RandomChoice[lettereMancanti];
	
	(* Penalit\[AGrave]: 5 punti * difficolt\[AGrave] (piu' difficile = penalit\[AGrave] maggiore) *)
	newScore = score - 5 * gameMode;
	
	(* Delega l'aggiornamento effettivo dello stato ad AggiornaStato.
	   gameMode=0 perche' non va aggiunto punteggio per la lettera suggerita *)
	AggiornaStato[word, stato, suggerimento, newScore, 0, errors]
];


(* ============================================================== *)
(* Pulisci                                                        *)
(* Reinizializza lo stato di gioco per la parola corrente,        *)
(* azzerando errori e punteggio \[LongDash] utile per ricominciare          *)
(* la stessa parola senza cambiarla                               *)
(*                                                                *)
(* Parametri:                                                     *)
(*   stato : lista di caratteri (la parola corrente)              *)
(*                                                                *)
(* Restituisce: {newState, {}, 0}                                 *)
(* ============================================================== *)
Pulisci[stato_List] := Module[
{ 
  newState,  (* Nuovo stato con tutti "_" *)
  errors = {},  (* Errori azzerati *)
  score = 0     (* Punteggio azzerato *)
  },
    
    (* InizializzaStato crea una lista di "_" della stessa lunghezza di stato *)
	newState = InizializzaStato[stato];
	
	(* Restituisce la tripla {stato_pulito, errori_vuoti, punteggio_zero} *)
	{newState, errors, score} 
];


(* ============================================================== *)
(* HaCaratteriNonAmmessiQ                                         *)
(* Predicato: restituisce True se la stringa s contiene           *)
(* caratteri accentati o apostrofi, che non sono sulla tastiera   *)
(* standard e causerebbero problemi nell'input                    *)
(*                                                                *)
(* Parametri:                                                     *)
(*   s : stringa da controllare                                   *)
(*                                                                *)
(* Restituisce: True / False                                      *)
(* ============================================================== *)
HaCaratteriNonAmmessiQ[s_] := Module[ 
{ 
    (* Lista di tutti i caratteri accentati da controllare *)
	accenti = {"\[AGrave]", "\[EGrave]", "\[IGrave]", "\[OGrave]", "\[UGrave]", "\[AAcute]", "\[EAcute]", "\[IAcute]", "\[OAcute]", "\[UAcute]"} 
},

    (* Alternatives @@ accenti costruisce un pattern OR tra tutti gli accenti.
	   StringContainsQ torna True se almeno uno e' presente nella stringa.
	   || include anche l'apostrofo come carattere non ammesso *)
	StringContainsQ[s, Alternatives @@ accenti] || StringContainsQ[s, "'"]
];



(* ============================================================== *)
(* InizializzaStato                                               *)
(* Crea lo stato iniziale del gioco: una lista di "_"             *)
(* lunga quanto la parola da indovinare.                          *)
(* Gli spazi vengono rivelati subito come " " invece di "_"       *)
(* cos\[IGrave] l'utente non \[EGrave] ingannato da trattini nelle posizioni    *)
(* in cui si trovano gli spazi nelle nazioni composte             *)
(* es. "stati uniti" -> {"_","_","_","_","_"," ","_","_","_","_","_","_"} *)
(*                                                                *)
(* Parametri:                                                     *)
(*   word : lista di caratteri                                    *)
(*                                                                *)
(* Restituisce: lista di "_" e " " es. {"_","_"," ","_","_"}     *)
(* ============================================================== *)
InizializzaStato[word_List] := Map[If[# === " ", " ", "_"] &, word]


(* ============================================================== *)
(* AggiornaStato                                                  *)
(* Aggiorna lo stato del gioco dopo che il giocatore ha           *)
(* inserito una lettera (guess)                                   *)
(*                                                                *)
(* Parametri:                                                     *)
(*   word         : lista di caratteri della parola completa      *)
(*   currentState : lista dello stato attuale (lettere o "_")     *)
(*   guess        : lettera inserita dal giocatore                *)
(*   score        : punteggio corrente                            *)
(*   gameMode     : difficolta' (usata per il bonus punteggio)    *)
(*   errors       : lista lettere sbagliate (default vuoto)       *)
(*                                                                *)
(* Restituisce: {newState, newErrors, newScore}                   *)
(* ============================================================== *)
AggiornaStato[word_List, currentState_List, guess_, score_Integer, gameMode_, errors_List:{}] := Module[

{ 
   newState,   (* Stato aggiornato dopo la mossa *)
   newErrors,  (* Lista errori aggiornata *)
   newScore    (* Punteggio aggiornato *)
 },
	If[MemberQ[word, guess],
	    (* LETTERA CORRETTA: MapThread scorre in parallelo currentState e word.
        Per ogni posizione: se il carattere di word coincide con guess,
        sostituisce "_" con la lettera; altrimenti lascia invariato.
        Gli spazi sono gi\[AGrave] rivelati da InizializzaStato e non vengono
        mai inseriti come guess dall'utente *)
		newState = MapThread[If[#2 == guess, guess, #1] &, {currentState, word}];
		newErrors = errors;                 (* Nessun nuovo errore *)
		newScore = score + 10 * gameMode,   (* Bonus: 10 punti * difficolta' *)
		
		(* LETTERA SBAGLIATA: lo stato non cambia *)
		newState = currentState;
		newErrors = Append[errors, guess];  (* Aggiunge la lettera alla lista errori *)
		newScore = score                    (* Punteggio invariato *) 
	];
	{newState, newErrors, newScore}
];


(* ============================================================== *)
(* GESTIONE CLASSIFICA E DATI                                     *)
(* ============================================================== *)

(* ============================================================== *)
(* SalvaRecord                                                    *)
(* Salva il punteggio di un giocatore nel file JSON della         *)
(* classifica. Se il nome esiste gi\[AGrave], conserva il punteggio      *)
(* massimo tra quello storico e quello nuovo                       *)
(*                                                                *)
(* Parametri:                                                     *)
(*   nome      : stringa con il nickname del giocatore            *)
(*   punteggio : punteggio intero da salvare                      *)
(*   file      : percorso del file JSON (default "score.json")    *)
(* ============================================================== *)

SalvaRecord[nome_String, punteggio_Integer, file_:"score.json"] := Module[
{
   datiEsistenti = {},       (* Lista dei record gi\[AGrave] presenti nel file *)
   nuovoContenuto,           (* Lista aggiornata da riscrivere sul file *)
   punteggioMax = punteggio  (* Punteggio massimo da conservare *)
   },
   
    (* Carica la classifica attuale dal file JSON *)
	datiEsistenti = RecuperaClassifica[file];
	
	(* Cerca se il nome esiste gi\[AGrave]: se si', aggiorna punteggioMax
	   con il valore piu' alto tra quello storico e quello nuovo.
	   Lookup[#, "nome", ""] accede al campo "nome" di ogni record Association *)
	Map[
		If[Lookup[#, "nome", ""] == nome, 
			punteggioMax = Max[punteggioMax, Lookup[#, "punteggio", 0]]
		]&, 
		datiEsistenti
	];
	
	(* DeleteCases rimuove tutti i record con quel nome dalla lista.
	   Il pattern x_ /; condizione filtra gli elementi che soddisfano la condizione *)
	datiEsistenti = DeleteCases[datiEsistenti, x_ /; Lookup[x, "nome", ""] == nome];
	
	(* Aggiunge il record aggiornato in fondo alla lista *)
	nuovoContenuto = Append[datiEsistenti, {"nome" -> nome, "punteggio" -> punteggioMax}];
	
	(* SortBy ordina per punteggio crescente, Reverse inverte in decrescente.
	   Take[..., UpTo[50]] mantiene al massimo i primi 50 record *)
	nuovoContenuto = Reverse[SortBy[nuovoContenuto, Lookup[#, "punteggio", 0] &]];
	nuovoContenuto = Take[nuovoContenuto, UpTo[50]];
	
	(* Export sovrascrive il file JSON con la classifica aggiornata *)
	Export[file, nuovoContenuto, "JSON"];
];



(* ============================================================== *)
(* RecuperaClassifica                                             *)
(* Carica la classifica dal file JSON e la restituisce            *)
(* ordinata in modo decrescente per punteggio                     *)
(*                                                                *)
(* Parametri:                                                     *)
(*   file : percorso del file JSON (default "score.json")         *)
(*                                                                *)
(* Restituisce: lista di Association {nome, punteggio} ordinata   *)
(* ============================================================== *)
RecuperaClassifica[file_:"score.json"] := Module[
{
   classifica = {}  (* Lista vuota come valore di fallback *)
   
   },
   (* FileExistsQ controlla che il file esista prima di provare a leggerlo.
	   Quiet sopprime eventuali messaggi di errore di Import.
	   Se Import fallisce o restituisce qualcosa che non \[EGrave] una lista,
	   classifica resta {} per non bloccare l'interfaccia *)
	If[FileExistsQ[file], 
		classifica = Quiet[Import[file, "JSON"]]; 
		If[!ListQ[classifica], classifica = {}]  (* Fallback: lista vuota se il JSON e' corrotto *)
	];
	(* Ordina in ordine decrescente per punteggio prima di restituire *)
	classifica = Reverse[SortBy[classifica, Lookup[#, "punteggio", 0] &]];
	classifica
];


(* ============================================================== *)
(* MostraClassificaGUI                                            *)
(* Interfaccia grafica per la classifica \[LongDash] macchina a 3 stati:    *)
(*   Stato 1: inserimento nome                                    *)
(*   Stato 2: conferma nome                                       *)
(*   Stato 3: punteggio salvato con successo                      *)
(*                                                                *)
(* Usa DynamicModule per mantenere le variabili locali tra        *)
(* un'interazione e l'altra all'interno del dialog.               *)
(* Due Dynamic separati con TrackedSymbols evitano il problema    *)
(* del contesto mangled tipico dei package Mathematica:           *)
(*   - Il primo Dynamic aggiorna solo la tabella (datiClassifica) *)
(*   - Il secondo Dynamic aggiorna solo i controlli (faseClass.)  *)
(*                                                                *)
(* Parametri:                                                     *)
(*   score : punteggio intero da mostrare e salvare               *)
(* ============================================================== *)
MostraClassificaGUI[score_Integer] := DynamicModule[
{
	nomeUtente = "",     (* Nome inserito dall'utente nell'InputField *)
	datiClassifica = {}, (* Lista dei record caricata dal file JSON *)
	faseClassifica = 1,  (* Stato corrente: 1 = Inserimento, 2 = Conferma, 3 = Salvato *)
	file = "score.json"  (* Percorso del file della classifica *)
},

    (* SetDirectory punta alla cartella del notebook corrente,
	   cos\[IGrave] score.json viene cercato e scritto nella stessa cartella.
	   Quiet evita messaggi se SetDirectory fallisce *)
	Quiet[SetDirectory[NotebookDirectory[]]];
	
	(* Carica i dati esistenti all'apertura del dialog *)
	datiClassifica = RecuperaClassifica[];
	
	(* CreateDialog apre una finestra modale separata dal notebook *)
	CreateDialog[
	    (* Framed aggiunge bordo arrotondato e sfondo colorato al dialog *)
		Framed[
			Column[{
				Style["Classifica Globale", Bold, 20],
				
				(* ---- TABELLA CLASSIFICA ---- *)
				(* Dynamic con TrackedSymbols:{datiClassifica}:
				   rivaluta SOLO quando datiClassifica cambia (dopo il salvataggio),
				   non ad ogni cambio di faseClassifica \[LongDash] evita problemi di contesto *)
				
			Dynamic[
			 (* Pane crea un contenitore a dimensione fissa con scrollbar:
					   {Automatic, 200} = larghezza automatica, altezza max 200px *)
				Pane[
				  (* Grid costruisce la tabella con intestazioni e dati.
						   Prepend aggiunge la riga header {"#","Nome","Punteggio"} in cima.
						   MapIndexed mappa ogni record aggiungendo l'indice #2[[1]] come numero di riga.
						   Lookup accede ai campi "nome" e "punteggio" di ogni Association in modo sicuro
						   (terzo argomento = valore di fallback se il campo manca) *)
					Grid[
						Prepend[
							MapIndexed[{  Style[#2[[1]], 16],  Style[Lookup[#, "nome", "N/A"], 16],  Style[Lookup[#, "punteggio", 0], Bold, 16]
							} &, datiClassifica],
							{Style["#", Bold, 16], Style["Nome", Bold, 16], Style["Punteggio", Bold, 16]}
						],
						Frame -> All,        (* Bordo per tutte le celle *)
						Alignment -> Center  (* Testo centrato *)
					],
					{Automatic, 250}, 
					Scrollbars -> Automatic
				],
				
				TrackedSymbols :> {datiClassifica}  (* si aggiorna solo quando datiClassifica cambia *)
				
			],	
					
				Spacer[10],   (* Spazio verticale tra tabella e controlli *)

				(* ---- CONTROLLI A STATI ---- *)
				(* Dynamic con TrackedSymbols:{faseClassifica}:
				   rivaluta SOLO quando faseClassifica cambia (1->2->3),
				   non quando datiClassifica viene aggiornato \[LongDash] i due Dynamic
				   sono indipendenti e non si interferiscono *)
				Dynamic[
				  (* Switch seleziona il blocco di controlli in base alla fase corrente *)
					Switch[faseClassifica,
					
					   (* ---- FASE 1: INSERIMENTO NOME ---- *)
						
						1, 
						Column[{
							"Inserisci il tuo nome:",
							(* InputField con ContinuousAction -> True:
							   aggiorna Dynamic[nomeUtente] ad ogni tasto premuto,
							   necessario per abilitare il bottone in tempo reale *)
							InputField[Dynamic[nomeUtente], String, FieldSize -> 25, ContinuousAction -> True],
							
							(* Enabled -> Dynamic[...]: il bottone e' cliccabile
							   solo se il campo nome non e' vuoto *)
							Button["Salva Punteggio",
								faseClassifica = 2,
								Enabled -> Dynamic[StringLength[nomeUtente] > 0]
							]
						}, Alignment -> Center, BaseStyle->"Subsection"],
						
						(* ---- FASE 2: CONFERMA NOME ---- *)
		                    
						2, 
						Column[{
						   (* Mostra il nome inserito per la conferma.
							   <> \[EGrave] l'operatore di concatenazione stringhe in Mathematica *)
							Style["Sei sicuro che '" <> nomeUtente <> "' sia corretto?", Darker[Red], Bold, 16],
							Spacer[10],
							Row[{
							  (* "Si, Salva": salva il record, ricarica la classifica
								   e avanza alla fase 3 *)
								Button["Si, Salva",
									Module[{nomeVal = nomeUtente},
									    (* nomeVal cattura il valore corrente di nomeUtente
										   in modo sicuro con Module, evitando side effects *)
										SalvaRecord[nomeVal, score];
										(* Ricarica i dati dal file \[LongDash] aggiorner\[AGrave] la tabella
										   tramite il Dynamic con TrackedSymbols:{datiClassifica} *)
										datiClassifica = RecuperaClassifica[]; 
										faseClassifica = 3 (* Passa alla fase 3 *)
									],
									Background -> RGBColor[0.3, 0.7, 0.9]
								],
								Spacer[10],
								(* "No, Modifica": torna alla fase 1 senza salvare *)
								Button["No, Modifica", faseClassifica = 1, Background -> Red]
							}]
						}, Alignment -> Center],
						
						(* ---- FASE 3: SALVATAGGIO COMPLETATO ---- *)
						
						3,  
						Column[{
						  (* Messaggio di conferma in verde *)
							Style["Punteggio salvato con successo!", Darker[Green], Bold, 18],
							Spacer[5],
							(* DialogReturn[] chiude il dialog restituendo Null *)
							Button["Chiudi Finestra", DialogReturn[]]
						}, Alignment -> Center]
					],
					TrackedSymbols :> {faseClassifica}  (* Aggiorna solo se faseClassifica cambia *)
				]

			}, Spacings -> 1.5, Alignment -> Center],
			FrameMargins -> 20,   (* Padding interno *)
			FrameStyle -> None,
			RoundingRadius -> 10,
			Background -> RGBColor[0.1, 0.4, 0.9]
		],
		WindowTitle -> "Classifica"    (* Titolo della finestra del dialog *)
		
	]
];


(* ============================================================== *)
(* DisegnaImpiccato                                               *)
(* Restituisce un oggetto Graphics con il disegno dell'impiccato  *)
(* aggiornato al numero di errori commessi                        *)
(*                                                                *)
(* Parametri:                                                     *)
(*   n : numero di errori correnti (0..6)                         *)
(*                                                                *)
(* La figura viene costruita progressivamente:                    *)
(*   n=0: solo la forca (base + palo + traversa + cappio)         *)
(*   n=1: testa                                                   *)
(*   n=2: corpo                                                   *)
(*   n=3: braccio sinistro                                        *)
(*   n=4: braccio destro                                          *)
(*   n=5: gamba sinistra                                          *)
(*   n=6: gamba destra (impiccato completo = partita persa)       *)
(* ============================================================== *)
DisegnaImpiccato[n_] := Graphics[
  {
    White,Thick,  (* Spessore delle linee *)
    
    (* Forca: base orizzontale, palo verticale, traversa, cappio *)
    Line[{{0, 0}, {3, 0}}], 
    Line[{{1.5, 0}, {1.5, 5}}], 
    Line[{{1.5, 5}, {3, 5}}], 
    Line[{{3, 5}, {3, 4.5}}], 
    
    (* Ogni parte del corpo appare solo se n raggiunge la soglia.
       Nothing e' un elemento grafico vuoto che non modifica il disegno *)
    If[n >= 1, Circle[{3, 4}, 0.5], Nothing],         (* Testa: cerchio *)
    If[n >= 2, Line[{{3, 3.5}, {3, 2.5}}], Nothing],  (* Corpo: segmento verticale *)
    If[n >= 3, Line[{{3, 3.3}, {2.7, 3}}], Nothing],  (* Braccio sinistro *)
    If[n >= 4, Line[{{3, 3.3}, {3.3, 3}}], Nothing],  (* Braccio destro *)
    If[n >= 5, Line[{{3, 2.5}, {2.7, 2}}], Nothing],  (* Gamba sinistra *)
    If[n >= 6, Line[{{3, 2.5}, {3.3, 2}}], Nothing]   (* Gamba destra *)
  },
  PlotRange -> {{0, 4}, {0, 6}},  (* Estensione degli assi del piano grafico *)
  ImageSize -> 200                (* Dimensione in pixel dell'immagine *)
]


(* ============================================================== *)
(* righeTastiera                                                  *)
(* Lista che definisce le 3 righe della tastiera QWERTY           *)
(* Ogni elemento \[EGrave] {offset, {lettere}}:                          *)
(*   offset = indentazione della riga in unita' Spacer            *)
(*   lettere = lista di stringhe da mostrare come bottoni         *)
(* ============================================================== *)
righeTastiera = {
  {0, {"Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"}},   
  {1, {"A", "S", "D", "F", "G", "H", "J", "K", "L"}},        
  {2, {"Z", "X", "C", "V", "B", "N", "M"}}              
};


(* ============================================================== *)
(* GeneraInterfaccia                                              *)
(* Funzione principale: genera l'interfaccia interattiva          *)
(* completa del gioco usando DynamicModule.                       *)
(*                                                                *)
(* DynamicModule mantiene le variabili di stato locali e          *)
(* persistenti per tutta la durata dell'interfaccia:              *)
(*   seed, seedError   : gestione del seed opzionale              *)
(*   fase              : schermata corrente ("selezione"/"gioco") *)
(*   gamemode          : difficolta' selezionata (1/2/3)          *)
(*   parola            : lista di caratteri della parola corrente *)
(*   stato             : stato corrente (lettere indovinate o "_")*)
(*   errori            : lista lettere sbagliate                  *)
(*   score             : punteggio corrente                       *)
(*   messaggio         : feedback testuale all'utente             *)
(*   maxErrori         : numero massimo di errori consentiti      *)
(*   opzioniBonus      : 4 opzioni per la domanda bonus           *)
(*   faseBonusCompletata: True dopo che il bonus e' stato giocato *)
(*   messaggioBonus    : feedback della domanda bonus             *)
(* ============================================================== *)
GeneraInterfaccia[] := DynamicModule[
  {
    seed,                           (* Seed inserito dall'utente (stringa o Automatic) *)
    seedError = "",                 (* Messaggio di errore se il seed non \[EGrave] valido *)
    fase = "selezione",             (* Schermata iniziale *)
    gamemode = 1,                   (* Difficolt\[AGrave] default: facile *)
    parola, stato, errori, score,   (* Variabili di gioco inizializzate da GeneraEsericizio *)
    messaggio = "",                 (* Feedback dopo ogni lettera inserita *)
    maxErrori = 6,                  (* Massimo 6 errori prima di perdere *)
    opzioniBonus = {},              (* Lista delle 4 opzioni della domanda bonus *)
    faseBonusCompletata = False,    (* Flag: True quando il bonus \[EGrave] gi\[AGrave] stato risposto *)
    messaggioBonus = ""             (* Feedback della risposta al bonus *)
  },

 (* Dynamic avvolge Switch: rivaluta l'intera UI ogni volta
     che una variabile del DynamicModule cambia.
     Switch seleziona la schermata da mostrare in base a "fase" *)
  Dynamic[
    Switch[fase, 

      (* ========================================================= *)
      (* SCHERMATA SELEZIONE                                        *)
      (* Permette di scegliere difficolt\[AGrave] e seed prima di giocare *)
      (* ========================================================= *)
      "selezione", 
      Column[{
        Style["Seleziona la difficolt\[AGrave]", Bold, White, 20], 
        
        (* RadioButtonBar crea i bottoni radio collegati a gamemode.
           Dynamic[gamemode] aggiorna la variabile al click *)
        Row[{
         RadioButton[Dynamic[gamemode], 1], Style[" Facile  ", White, 16],
         RadioButton[Dynamic[gamemode], 2], Style[" Media   ", White, 16],
         RadioButton[Dynamic[gamemode], 3], Style[" Difficile", White, 16]
           }], 
        
        (* Campo seed con validazione in tempo reale.
           La funzione di setter ({seed, seedError} = If[...]) viene chiamata
           ad ogni tasto grazie a ContinuousAction -> True.
           StringMatchQ[#, DigitCharacter..] accetta solo cifre decimali.
           StringMatchQ[#, ""] accetta il campo vuoto (seed non specificato) *)
        Row[{Style["Seed (opzionale): ",White], InputField[
            Dynamic[seed, ({seed, seedError} = If[
              StringMatchQ[#, DigitCharacter ..] || StringMatchQ[#, ""],
              {#, ""},     (* Input valido: aggiorna seed, azzera errore *)
              {seed, "Inserire solo numeri naturali (0, 1, 2, ...)."}   (* Input non valido: mostra errore *)
            ]) &],  
            String,   (* Tipo di dato accettato *)
            FieldHint -> "Inserire un numero naturale",  (* Testo placeholder *)
            ContinuousAction -> True  (* Valida ad ogni tasto *)
        ]}], 
        
        (* Mostra il messaggio di errore solo se seedError non \[EGrave] vuoto *)
        Dynamic[If[seedError != "", Style[seedError, Red, Italic], ""]],
        
        (* Bottone Inizia partita:
           converte seed da stringa a intero se necessario,
           poi genera l'esercizio e cambia fase a "gioco" *)
        Button["Inizia partita", 
          Module[{},
          
           (* Se seed \[EGrave] una stringa non vuota, convertila in intero;
               altrimenti usa Automatic per una selezione casuale *)
            If[StringQ[seed] && seed != "", seed = ToExpression[seed], seed = Automatic]; 
            
            (* GeneraEsericizio restituisce {parola, stato, errori, score}
               che vengono assegnati alle rispettive variabili del DynamicModule *)
            {parola, stato, errori, score} = GeneraEsericizio[gamemode, seed]; 
            fase = "gioco";   (* Passa alla schermata di gioco *)
            messaggio = "";   (* Azzera il messaggio precedente *)
            
            (* Azzera tutte le variabili del bonus per la nuova partita *)
            opzioniBonus = {}; faseBonusCompletata = False; messaggioBonus = "";
          ],
          ImageSize -> {150, 60},                    (* Dimensione bottone *)
          BaseStyle -> {FontSize -> 16, Bold, White}, (* Stile testo bottone *)
          Background -> RGBColor[0.1, 0.4, 0.9]      (* Colore sfondo bottone *)
        ]
      }, Spacings -> 2], (* Spaziatura verticale tra gli elementi *)


      (* ========================================================= *)
      (* SCHERMATA GIOCO                                            *)
      (* Mostra la parola, la tastiera, l'impiccato e gestisce     *)
      (* vittoria, sconfitta e domanda bonus                       *)
      (* ========================================================= *)
      "gioco", 
      Column[{
        Style["Gioco dell'impiccato", Bold, 28, White],
        
        Spacer[10],

        (* Punteggio aggiornato in tempo reale grazie a Dynamic *)
        Dynamic[Row[{Style["Punteggio: ", White, 14], Style[score, Blue, Bold, 14]}]], 
        
        Spacer[10],

        (* Parola da indovinare: Dynamic rende la riga reattiva.
           Map (con /@) applica la funzione ad ogni elemento di stato.
           Se l'elemento \[EGrave] "_" mostra un trattino grigio grande,
           altrimenti mostra la lettera in nero grassetto grande.
           Riffle inserisce uno spazio " " tra ogni elemento della lista *)
        Dynamic[Row[Riffle[
          Which[
            # === " ", Style["   ", Bold, 28],  (* spazio: mostra vuoto *)
            # === "_", Style[" _ ", Gray, Bold, 28],  (* da indovinare *)
            True,      Style[#, Bold, 28, White]             (* lettera indovinata *)
           ] & /@ stato,
           " "
        ]]], 
        
        Spacer[10],
        
        (* Riga con i due bottoni azione principali *)
        Row[{
          (* Suggerimento: rivela una lettera, penalizza il punteggio.
             Disabilitato se non ci sono pi\[UGrave] lettere da indovinare
             o se gli errori hanno raggiunto il massimo *)
          Button["Suggerimento", 
            {stato, errori, score} = Suggerimento[parola, stato, errori, score, gamemode], 
            Enabled -> MemberQ[stato, "_"] && Length[errori] < maxErrori,
            BaseStyle -> {White, Bold, FontSize -> 13},
            Background -> RGBColor[0.8, 0.5, 0.0], (* Arancione *)
            ImageSize -> {140, 38}
          ],
          Spacer[20], 
          (* Mostra soluzione: rivela la parola e torna alla selezione.
             Il ; separa MostraSoluzione dal reset delle variabili *)
          Button["Mostra soluzione",
            MostraSoluzione[parola];
            (* Reset contestuale: ripristina parola, schermata, messaggi *)
            {stato, fase, messaggio, seedError} = {parola, "selezione", "", ""},
            BaseStyle -> {White, Bold, FontSize -> 13},
            Background -> RGBColor[0.4, 0.1, 0.6], (* Viola *)
            ImageSize -> {160, 38}
          ]
        }],
        

        (* Lettere sbagliate: Riffle inserisce ", " tra gli elementi di errori.
           StringJoin ricostruisce la stringa dalla lista *)
        Dynamic[Row[{Style["Lettere sbagliate: ", White],Style[StringJoin[Riffle[errori, ", "]], Red, Bold]}]],
      
        (* Contatore errori con massimo consentito *)
        Dynamic[Row[{Style["Errori: ", White], Style[Length[errori], Red, Bold], Style["/", White], maxErrori}]],
        
        (* Messaggio di feedback dopo ogni lettera (corretto/sbagliato) *)
        Dynamic[Style[messaggio, RGBColor[0.1, 0.4, 0.9], Bold]], 

        (* Disegno impiccato: Dynamic lo aggiorna ad ogni errore *)
        Dynamic[DisegnaImpiccato[Length[errori]]], 
        
        Spacer[10],

        (* ---- TASTIERA ---- *)
        (* Dynamic rivaluta la tastiera dopo ogni lettera premuta
           per aggiornare i colori (blu/verde/rosso) dei tasti *)
        Dynamic[
          Column[
           (* Map scorre le 3 righe di righeTastiera *)
            Map[
             (* Per ogni riga costruisce una Row di bottoni *)
              Row[
                Join[
                  (* Spacer[offset*25] crea l'indentazione della riga *)
                  {Spacer[#[[1]]*25]}, 
                  (* Table genera i bottoni per ogni lettera della riga *)
                  Table[
                    (* With[{l=lettera},...] cattura il valore corrente di lettera
                       nel momento in cui il bottone viene creato (closure).
                       Senza With, tutti i bottoni catturerebbero la stessa lettera
                       (l'ultima del ciclo Table) *)
                    With[{l = lettera}, 
                      Button[
                        Style[l, White, Bold, FontSize -> 16],
                        (* Azione al click: converte la lettera in minuscolo e aggiorna lo stato *)
                        Module[{guess = ToLowerCase[StringTrim[l]]}, 
                          {stato, errori, score} = AggiornaStato[parola, stato, guess, score, gamemode, errori]; 
                          messaggio = If[MemberQ[parola, guess], "Lettera corretta!", "Lettera sbagliata!"]; 
                        ], 
                        (* Il bottone \[EGrave] disabilitato se:
                           - la lettera \[EGrave] gi\[AGrave] stata indovinata (in stato)
                           - la lettera \[EGrave] gi\[AGrave] stata sbagliata (in errori)
                           - non ci sono pi\[UGrave] lettere da trovare
                           - gli errori hanno raggiunto il massimo *)
                        Enabled -> !MemberQ[Join[stato, errori], ToLowerCase[l]] &&
                          MemberQ[stato, "_"] &&
                          Length[errori] < maxErrori,
                        (* Colore dinamico del tasto:
                           Which valuta le condizioni in ordine e usa la prima vera *)
                        Background -> Which[
                          MemberQ[stato, ToLowerCase[l]], RGBColor[0.2, 0.7, 0.3],     (* Verde: lettera indovinata *)
                          MemberQ[errori, ToLowerCase[l]], RGBColor[0.75, 0.15, 0.15], (* Rosso: lettera sbagliata *)
                          True, RGBColor[0.15, 0.35, 0.75]                             (* Blu: lettera non ancora usata *)
                        ],
                        ImageSize -> {48, 48} (* Dimensione bottone tastiera *)
                      ]
                    ],
                    {lettera, #[[2]]}  (* Itera sulle lettere della riga corrente *)
                  ]
                ],
                Spacer[4]  (* Spaziatura orizzontale tra i bottoni *)
              ] &,
              righeTastiera  (* Lista delle 3 righe della tastiera *)
            ], 
            Spacings -> 0.8  (* Spaziatura verticale tra le righe *)
          ]
        ],

        (* ---- GESTIONE FINE PARTITA ---- *)
        (* Dynamic controlla ad ogni aggiornamento se la partita \[EGrave] finita:
           - vittoria: stato === parola (tutte le lettere indovinate)
           - sconfitta: Length[errori] >= maxErrori (troppi errori) *)
        Dynamic[
          If[stato === parola || Length[errori] >= maxErrori, 
            Column[{
              If[stato === parola, 

                (* GESTIONE VITTORIA E DOMANDA BONUS *)
                If[!faseBonusCompletata,
                  (* Genera 4 opzioni random per il Bonus (1 corretta, 3 sbagliate) *)
                  If[opzioniBonus === {},
                    Module[{corretta = dizionarioGeografia[StringJoin[parola]], sbagliate},
                      (* Recupera la capitale corretta dal dizionario *)
                      (* RandomSample[lista, 3] sceglie 3 capitali sbagliate casuali
                         DeleteCases rimuove la risposta corretta dalle opzioni sbagliate *)
                      sbagliate = RandomSample[DeleteCases[Values[dizionarioGeografia], corretta], 3];
                      (* RandomSample mescola le 4 opzioni per posizione casuale *)
                      opzioniBonus = RandomSample[Join[{corretta}, sbagliate]];
                    ]
                  ];
                  (* Mostra la domanda bonus con i 4 bottoni risposta *)
                  Column[{
                    Spacer[10],
                    Style["Hai vinto!", Green, Bold, 18],
                    Style["Domanda Bonus (+50 punti extra!):", Purple, Bold, 18],
                    Spacer[10],
                    (* Capitalize porta la prima lettera in maiuscolo per la visualizzazione *)
                    Row[{Style["Qual \[EGrave] la capitale di ", White, 16],Style[Capitalize[StringJoin[parola]], White, Bold, 16],Style["?", White, 16]
                    }],
                    
                    (* Generazione bottoni Bonus a scelta multipla *)
                    Row[Riffle[
                      Button[Capitalize[#],
                        If[# === dizionarioGeografia[StringJoin[parola]],
                          score = score + 50;  (* Risposta corretta: +50 punti *)
                          messaggioBonus = "Risposta corretta! Hai guadagnato 50 punti extra.",
                          (* Risposta sbagliata: mostra la capitale corretta *)
                          messaggioBonus = "Sbagliato! La risposta corretta era: " <> Capitalize[dizionarioGeografia[StringJoin[parola]]]
                        ];
                        faseBonusCompletata = True; (* Impedisce di rispondere di nuovo *)
                      ] & /@ opzioniBonus, 
                      Spacer[10] 
                    ]]
                  }, Alignment -> Center],

                  (* Bonus completato: mostra feedback e bottone classifica *)
                  Column[{
                    Spacer[10],
                    (* Colore del feedback in base alla correttezza della risposta *)
                    Style[messaggioBonus, If[StringContainsQ[messaggioBonus, "punti extra"], Green, Red], Bold],
                    Spacer[10],
                    (* SessionSubmit lancia MostraClassificaGUI in un thread separato,
                       fuori dal contesto Dynamic \[LongDash] evita problemi di esecuzione bloccante *)
                    Button["Salva e Mostra Classifica",
                      SessionSubmit[MostraClassificaGUI[score]],
                      BaseStyle -> {White, Bold, FontSize -> 13},
                      Background -> RGBColor[0.1, 0.4, 0.9],
                      ImageSize -> {240, 42}
                    ]
                  }, Alignment -> Center]
                ],

                (* SCONFITTA: mostra la nazione e il bottone classifica *)
                Column[{
                  Spacer[10],
                  Style["Hai perso! La nazione era: " <> Capitalize[StringJoin[parola]], Red, Bold],
                  Spacer[10],
                  Button["Salva e Mostra Classifica",
                    SessionSubmit[MostraClassificaGUI[score]],
                    BaseStyle -> {White, Bold, FontSize -> 13},
                    Background -> RGBColor[0.1, 0.4, 0.9],
                    ImageSize -> {240, 42}
                  ]
                }, Alignment -> Center]
              ],

              Spacer[25],
              (* Pulisci Campi: reimposta stato ed errori per la parola corrente
                 senza cambiarla \[LongDash] utile per riprovare dopo aver visto la soluzione *)
              Button["Pulisci Campi",
                {stato, errori, score} = Pulisci[parola];
                opzioniBonus = {}; faseBonusCompletata = False; messaggioBonus = "";,
                BaseStyle -> {White, Bold, FontSize -> 13},
                Background -> RGBColor[0.2, 0.5, 0.7],
                ImageSize -> {150, 38}
              ]
            }],
            ""        (* Stringa vuota: non mostra nulla se la partita \[EGrave] ancora in corso *)
          ]
        ],

        (* Nuova partita: torna alla schermata di selezione e azzera tutto *)
        Button["Nuova partita", 
          Module[{},
            fase = "selezione"; 
            messaggio = ""; seedError = "";
            opzioniBonus = {}; faseBonusCompletata = False; messaggioBonus = "";
          ],
          BaseStyle -> {White, Bold, FontSize -> 13},
          Background -> RGBColor[0.3, 0.3, 0.8],
          ImageSize -> {150, 40}
        ]

      }]  (* Fine Column schermata gioco *)
    ]     (* Fine Switch *)
  ]       (* Fine Dynamic *)
];        (* Fine DynamicModule *)


(* End[] chiude il contesto privato  *)
End[];

(* EndPackage[] ripristina il contesto precedente al BeginPackage. *)
EndPackage[]
