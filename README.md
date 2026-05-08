geoGuesser per Mc

TODO:
- Controllo dei nomi (magari con lo spazio)
- Controllo del seed (ad esempio se uno mette un seed troppo grande, negativo, una lettera ecc...)
- Quando qualcuno deve salvare la classifica, il nome viene tagliato

Attualmente l'inserimento del seed funziona così:
- blocca qualsiasi inserimento diverso da numeri naturali con un messaggio di avviso (anche numeri negativi)
- SeedRandom[seed] inizializza il generatore di numeri casuali di Mathematica con un valore fisso (quello che diamo). Qualsiasi intero positivo funziona, anche molto grande.
- il valore che diamo verrà usato per generare una nazione casuale dalla wordlist e una volta che diamo questo valore, la nazione assegnata sarà sempre la stessa per quel valore (se restiamo nella stessa difficoltà). Quindi con seed inserito RandomChoice restituisce sempre la stessa parola.
Seed specificato —> parola deterministica. Questo vale su qualsiasi pc e so.
- se non inseriamo nessun seed -> SeedRandom[Automatic] Automatic è il default quindi Mathematica usa il timestamp corrente come sorgente di casualità quindi -> Nazione diversa ad ogni esecuzione.
- Il seed è legato alla combinazione seed + difficoltà. Lo stesso seed con difficoltà diversa da parole diverse perché wordlist cambia e RandomChoice opera su una lista diversa.
