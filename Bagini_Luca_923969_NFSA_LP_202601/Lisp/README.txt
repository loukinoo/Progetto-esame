Bagini  Luca    923969
Come per il programma Prolog e come specificato nei
commenti del programma, la lista vuota viene considerata 
come corrispondente ad epsilon e non al linguaggio vuoto.

L'automa è rappresentato come una lista
(:NFSA iniziale finale transitions), dove:
  - iniziale e finale sono lo stato iniziale e finale 
    dell'automa ottenuti con gensym 
  - transitions è una lista che contiene le transizioni
    dell'automa viste come liste (stato-partenza 
    simbolo-di-transizione stato-destinazione)