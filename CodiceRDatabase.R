
install.packages("plasmaLite")
remove.packages("a")

#PULIZIA GLOBAL ENVIORMENT
rm(list = ls())

#OPERAZIONI PRELIMINARI
#------------------------------------------------

library("dplyr")
library("viridis")
library("viridisLite")
library("tidyverse")
library("ggplot2")
library("plotly")
library("tidyverse")
library("plotrix")

dipartimento=read.csv("./dipartimento.csv", sep=",")
dipendente=read.csv("./dipendente.csv" , sep=",")
lavoro=read.csv("./lavoro.csv" , sep=",")
progetto=read.csv("./progetto.csv" , sep=",")
#ufficio=read.csv("./ufficio.csv" , sep=",")
#View(ufficio)
View(progetto)
View(lavoro)
View(dipendente)
View(dipartimento)

#Aggiunta colonna nome dipartimento e binding di nome e numero

nomedipartimenti = c("Produzione","Testing","Logistica","Amministrazione","Commercio","Relazioni pubbliche","Legale","Risorse umane","Ricerca e sviluppo")
dipendente$nomedipartimenti <- nomedipartimenti

dipendente$nomedipartimenti[dipendente$dipartimento == 1] <- "Produzione" 
dipendente$nomedipartimenti[dipendente$dipartimento == 2] <- "Testing" 
dipendente$nomedipartimenti[dipendente$dipartimento == 3] <- "Logistica" 
dipendente$nomedipartimenti[dipendente$dipartimento == 4] <- "Amministrazione"
dipendente$nomedipartimenti[dipendente$dipartimento == 5] <- "Commercio"
dipendente$nomedipartimenti[dipendente$dipartimento == 6] <- "Relazioni pubbliche"
dipendente$nomedipartimenti[dipendente$dipartimento == 7] <- "Legale"
dipendente$nomedipartimenti[dipendente$dipartimento == 8] <- "Risorse umane"
dipendente$nomedipartimenti[dipendente$dipartimento == 9] <- "Ricerca e sviluppo"

#Rimozione '' da nome dipartimento 

dipartimento$nome[dipartimento$nome == "'Produzione'"] <- "Produzione"
dipartimento$nome[dipartimento$nome == "'Testing'"] <- "Testing"
dipartimento$nome[dipartimento$nome == "'Logistica'"] <- "Logistica"
dipartimento$nome[dipartimento$nome == "'Amministrazione'"] <- "Amministrazione"
dipartimento$nome[dipartimento$nome == "'Commercio'"] <- "Commercio"
dipartimento$nome[dipartimento$nome == "'Relazioni pubbliche'"] <- "Relazioni pubbliche"
dipartimento$nome[dipartimento$nome == "'Legale'"] <- "Legale"
dipartimento$nome[dipartimento$nome == "'Risorse umane'"] <- "Risorse umane"
dipartimento$nome[dipartimento$nome == "'Ricerca e sviluppo'"] <- "Ricerca e sviluppo"

#Fill caselle vuote/spazi con NA
lavoro[lavoro == "" | lavoro == " "] <- NA 

#------------------------------------------------
#FUNZIONI DI PROVA

#Restituisce tutti i dati di un dipartimento inserendo il nome
dipartimento %>% filter(str_detect(nome, "Produzione")) 

#restituisce il budget di un dipartimento inserendo il nome
numero = readline("Inserisci il numero di dipartimento di cui vuoi sapere il budget: ");
#numero = as.string(nomedipartimento);
dipartimento %>% filter(str_detect(numero_dipartimento, numero))%>% 
  select(nome, budget)

#Restituisce il numero dell'impiegato con il lavoro "Addetto ai macchinari"
lavoro %>% filter(str_detect(tipo_lavoro, 'Addetto ai macchinari'))%>%
  select(impiegato)

#Restituisce il dipartimento con più budget
dipartimento %>% 
  select(3) %>%
  top_n(1)

#------------------------------------------------
#GRAFICI/TABELLE

#PIEGRAPH BUDGET DIPARTIMENTI
BUDG <- dipartimento$budget
LBL1 <- dipartimento$nome
pie(BUDG,
      labels=LBL1,
      explode=0.4,
      col=hcl.colors(length(BUDG),"plasma"),
      labelcex=1,
      main="Grafico a torta del budget a disposizione dei dipartimenti ")

#Calcolo numero dipendenti per dipartimento + percentuale dipendenti e percentuale budget con relativi 2 grafici


DIPNUM=dipendente %>% count(nomedipartimenti, sort = TRUE)
BUDG=dipartimento %>% select(1,2,3)
colnames(BUDG)[1] <- "num"
colnames(BUDG)[2] <- "dipartimento"
colnames(DIPNUM)[1] <- "dipartimento"
GRAPH2 <- data.frame(DIPNUM %>%
                        full_join(BUDG, by = "dipartimento"))
GRAPH2 <- GRAPH2 %>% mutate (rapportobudget = (round(budget/sum(budget) * 100, 2)))
GRAPH2 <- GRAPH2 %>% mutate (rapportodipendenti = (round(n/sum(n) * 100, 2)))

#GRAFICO BUDGET DIPARTIMENTI
BUDGETGRAPH <- ggplot(GRAPH2, aes(x=num, y=rapportobudget, fill=dipartimento, stat="identity")) +
  geom_col(color="dimgrey") +
  geom_text(aes(label=paste0(rapportobudget, "%")), vjust=-1, color="dimgrey", size=3.5)+
  theme_minimal()+
  labs(
    title="Percentuale budget",
    subtitle = "Divisione per numero di dipartimento",
    x = "Numero dipartimento",
    y = "Percentuale"
  )+
  scale_fill_manual(values=c(plasma(9)))+
  scale_x_discrete(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9),labels=c("1", "2", "3","4", "5", "6","7", "8", "9"))
BUDGETGRAPH +
  ylim(0, 100) 

#GRAFICO DIPENDENTI DIPARTIMENTI
DEPENDENTGRAPH <- ggplot(GRAPH2, aes(x=dipartimento, y=rapportodipendenti, fill=dipartimento, stat="identity")) +
  geom_col(color="dimgrey") +
  geom_text(aes(label=paste0(rapportodipendenti, "%")), vjust=-1, color="dimgrey", size=3.5)+
  theme_minimal() +
labs(
    title="Percentuale dipendenti",
    subtitle = "Divisione per numero di dipartimento",
    x = "Numero dipartimento",
    y = "Percentuale"
  )+
  scale_fill_manual(values=c(plasma(9)))+
  scale_x_discrete(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9),labels=c("1", "2", "3","4", "5", "6","7", "8", "9"))
DEPENDENTGRAPH +
  ylim(0, 100)


#lavoro, droppa righe dove numero impiegato non si ripete
#prendi stipendio_lavoro max e min, mostra lavoratore con percorso migliore o maggior aumento di paga? (in minor tempo?)

#linea costo progetti con media? + analisi percentuale

#Grafico con numero di dipendenti attuali distributi su range di stipendi


EMPLOYEESALARIES <- data.frame(ID= c(1:7), Range = c("1600-1800","1800-2000", "2000-2200", "2200-2400", "2400-2600", "2600-2800", "2800+"),
                                NumeroDipendenti = c(
  nrow(subset(lavoro[is.na(lavoro$data_fine), ], stipendio_lavoro >= "1600" & stipendio_lavoro < "1800")),
  nrow(subset(lavoro[is.na(lavoro$data_fine), ], stipendio_lavoro >= "1800" & stipendio_lavoro < "2000")),
  nrow(subset(lavoro[is.na(lavoro$data_fine), ], stipendio_lavoro >= "2000" & stipendio_lavoro < "2200")),
  nrow(subset(lavoro[is.na(lavoro$data_fine), ], stipendio_lavoro >= "2200" & stipendio_lavoro < "2400")),
  nrow(subset(lavoro[is.na(lavoro$data_fine), ], stipendio_lavoro >= "2400" & stipendio_lavoro < "2600")),
  nrow(subset(lavoro[is.na(lavoro$data_fine), ], stipendio_lavoro >= "2600" & stipendio_lavoro <= "2800")),
  nrow(subset(lavoro[is.na(lavoro$data_fine), ], stipendio_lavoro > "2800"))))

SALARYGRAPH <- ggplot(EMPLOYEESALARIES, aes(x=Range, y=NumeroDipendenti, fill=Range)) +
  geom_col(color="dimgrey") +
  theme_minimal() +
  labs(
    title="Distribuzione stipendi dell'azienda",
    x = "Range stipendi",
    y = "Numero dipendenti"
  )+
  geom_text(data = EMPLOYEESALARIES, label = paste(LBL2), size = 4, fontface = "bold", nudge_y = 10)+
  scale_fill_manual(values=c(plasma(7)))
SALARYGRAPH +
theme(legend.position = "none")
 
#GRAFICO BOXPLOT (senza outlier)

EMPLOYEESALARIES2 <- lavoro %>% mutate(range = case_when(
                                  stipendio_lavoro >= "1600" & stipendio_lavoro < "2000" ~ '1600-2000',
                                  stipendio_lavoro >= "2000" & stipendio_lavoro < "2400" ~ '2000-2400',
                                  stipendio_lavoro >= "2400" & stipendio_lavoro < "2800" ~ '2400-2800',
                                  stipendio_lavoro >= "2800" ~ '2800+'))

EMPLOYEESALARIES2 <- EMPLOYEESALARIES2[!complete.cases(EMPLOYEESALARIES2), ]

SALARYGRAPH2 <- ggplot(EMPLOYEESALARIES2, aes(x=stipendio_lavoro, y=range,  fill=range)) + 
  geom_boxplot(
    
    color="red4",
    #Outlier aestetichs, eliminato con xlim
    #outlier.colour="blue",outlier.fill="red", outlier.size=6,outlier.shape = 8,outlier.stroke = 0.5, coef = 4,
  )+
  scale_fill_manual(values=c(plasma(7)))+
  labs(
    title="Distribuzione stipendi dell'azienda",
    subtitle = "Divisione per numero di dipartimento",
    x = "Stipendio",
    y = "Range di stipendi"
  )
  
SALARYGRAPH2+
  xlim(1500, 3700)+
  theme(legend.position = "none")

#GRAFICO VIOLIN (senza outlier)

EMPLOYEESALARIES3 <- lavoro %>% mutate(range = case_when(
  stipendio_lavoro >= "1600" & stipendio_lavoro < "1800" ~ ' ',
  stipendio_lavoro >= "1800" & stipendio_lavoro < "2000" ~ ' ',
  stipendio_lavoro >= "2000" & stipendio_lavoro < "2200" ~ ' ',
  stipendio_lavoro >= "2200" & stipendio_lavoro < "2400" ~ ' ',
  stipendio_lavoro >= "2400" & stipendio_lavoro < "2600" ~ ' ',
  stipendio_lavoro >= "2600" & stipendio_lavoro < "2800" ~ ' ',
  stipendio_lavoro >= "2800" ~ ' '))

colnames(dipendente)[1] <- "id"
EMPLOYEESALARIES3 <- data.frame(EMPLOYEESALARIES3 %>%
                       right_join(dipendente, by = "id"))
EMPLOYEESALARIES3 <- EMPLOYEESALARIES3[!complete.cases(EMPLOYEESALARIES3), ]

SALARYGRAPH3 <- ggplot(EMPLOYEESALARIES3, aes(x=genere, y=stipendio_lavoro, fill="red")) + 
  geom_violin(
    draw_quantiles=c(0.25, 0.5, 0.75),
    scale = "count",
  )+
  scale_fill_manual(values=c("#414487FF"))+
  scale_x_discrete(0)+
  labs(
    title="Distribuzione stipendi dell'azienda",
    y = "Stipendio"
  )
SALARYGRAPH3 +
  ylim(1500, 3700)+
  theme(legend.position = "none", panel.background = element_rect(fill = "white"))

