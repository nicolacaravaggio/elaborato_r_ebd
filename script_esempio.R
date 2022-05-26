# ==============================================================================
# FINAL LECTURE
# May 23-25, 2022
# ------------------------------------------------------------------------------
# Università degli Studi Roma Tre, Economia e Big Data
# ==============================================================================

# Puliamo il nostro environment prima di iniziare (sempre consigliato)
rm(list = ls())

# Working Directory
# ------------------------------------------------------------------------------
# Settiamo la working directory
if (Sys.getenv('USERNAME')=='Nicola'){
  setwd("C:/Users/Nicola Caravaggio/OneDrive/Research/Corsi Roma Tre/Statistica (Mortera)/Prova in R/Esempio")
} else if (Sys.getenv('USERNAME')=='cilu')
{Sys.getenv('USERNAME')=='cilu'
  setwd("C:/Users/NickS/OneDrive/Research/Corsi Roma Tre/Statistica (Mortera)/Prova in R/Esempio")
} else {choose.dir()}

# La working directory è dove sono ubicati, sul vostro computer, i file da importare
# Possiamo selezionarla anche dal menù andando su Session -> Set Working Directory -> Choose Directory...


# Importazione del dataset
# ------------------------------------------------------------------------------
# Importare un file in formato Excel (.xlsx)
library(readxl)
df <- read_excel("dati_istat.xlsx", sheet = "Dati ISTAT", col_names = TRUE)
# NOTA: Ricorda che il file non deve essere aperto sul computer quando lo si importa

# Importare un file in formavo di testo (.csv)
df <- read.csv("dati_istat.csv", header = TRUE, sep = ',')
# NOTA: Potete avere il file anche contemporaneamente aperto

# Lista dei nomi delle variabili (colonne)
colnames(df)

# Qualora volessimo rinominare alcune variabili
names(df)[names(df) == "emigrazione_ospedaliera"] <- "emigr_osp"
names(df)[names(df) == "emigr_osp"] <- "emigrazione_ospedaliera"

# Statistiche descrittive "veloci"
summary(df)

# Per vedere la classe (e.g., character, boolean, integer, numeric, float, etc.) di ciascuna variabile
lapply(df, class)
# Character: stringhe
# Boolean: solo TRUE and FALSE
# Integer: solo numeri interi
# Numeric: anche numeri decimali
# Float: insieme dei numeri relativi

# Istogrammi
# ------------------------------------------------------------------------------
# Possiamo rappresentare l'istogramma associato a ciascuna delle variabili

# Reddito
hist(df$reddito, 
     freq = TRUE, 
     main = "Istogramma variabile Reddito", 
     xlab = "",
     col = "darkseagreen")
# NOTA: Scrivendo "freq = TRUE" stiamo rappresentando sull'asse verticale le frequenze assolute
# Se volessimo, invece, optare per la densità di frequenza, bisogna scrivere "freq = FALSE"
# Possiamo aggiungere sul grafico anche delle linee che si riferiscono a:
# Media
abline(v = mean(df$reddito), col = "red", lwd = 2)
# Mediana
abline(v = median(df$reddito), col = "blue", lwd = 2)
# Primo quartile
abline(v = quantile(df$reddito, 0.25), col = "blue", lty = 2, lwd = 2)
# Terzo quartile
abline(v = quantile(df$reddito, 0.75), col = "blue", lty = 2, lwd = 2)
# RICORDA: Quando la media è maggiore della mediana vi è asimmetria positiva; viceversa,
# quando la media è inferiore della mediana vi è asimmetria negativa

# Disuguaglianza (con densità di frequenza)
hist(df$disuguaglianza, 
     freq = FALSE, 
     main = "Istogramma variabile Disuguaglianza", 
     xlab = "",
     col = "darkseagreen")
abline(v = mean(df$disuguaglianza), col = "red", lwd = 2)
abline(v = median(df$disuguaglianza), col = "blue", lwd = 2)
abline(v = quantile(df$disuguaglianza, 0.25), col = "blue", lty = 2, lwd = 2)
abline(v = quantile(df$disuguaglianza, 0.75), col = "blue", lty = 2, lwd = 2)

# Qualità dell'aria
hist(df$qualita_aria, 
     freq = TRUE, 
     main = "Istogramma variabile Qualità dell'aria", 
     xlab = "",
     col = "darkseagreen")
abline(v = mean(df$qualita_aria), col = "red", lwd = 2)
abline(v = median(df$qualita_aria), col = "blue", lwd = 2)
abline(v = quantile(df$qualita_aria, 0.25), col = "blue", lty = 2, lwd = 2)
abline(v = quantile(df$qualita_aria, 0.75), col = "blue", lty = 2, lwd = 2)

# Medici
hist(df$medici, 
     freq = TRUE, 
     main = "Istogramma variabile Medici", 
     xlab = "",
     col = "darkseagreen")
abline(v = mean(df$medici), col = "red", lwd = 2)
abline(v = median(df$medici), col = "blue", lwd = 2)
abline(v = quantile(df$medici, 0.25), col = "blue", lty = 2, lwd = 2)
abline(v = quantile(df$medici, 0.75), col = "blue", lty = 2, lwd = 2)

# Emigrazione Ospedaliera
hist(df$emigrazione_ospedaliera, 
     freq = TRUE, 
     main = "Istogramma variabile Emigrazione Ospedaliera", 
     xlab = "",
     col = "darkseagreen")
abline(v = mean(df$emigrazione_ospedaliera), col = "red", lwd = 2)
abline(v = median(df$emigrazione_ospedaliera), col = "blue", lwd = 2)
abline(v = quantile(df$emigrazione_ospedaliera, 0.25), col = "blue", lty = 2, lwd = 2)
abline(v = quantile(df$emigrazione_ospedaliera, 0.75), col = "blue", lty = 2, lwd = 2)
# EXTRA: Aggiunta dei label per le varie linee aggiunte
# Media
text((mean(df$emigrazione_ospedaliera)+1), 10, round(mean(df$emigrazione_ospedaliera),2), srt = 90, pos = 2, col = "red")
# Mediana
text((median(df$emigrazione_ospedaliera)+1), 10, round(median(df$emigrazione_ospedaliera),2), srt = 90, pos = 2, col = "blue")

# EXTRA: Qualora volessimo realizzare l'istogramma con ggplot2
varlab <- df$reddito
library(ggplot2)
ggplot(df, aes(x = reddito)) + 
  geom_histogram(bins = 8, color = "black", fill = "darkseagreen") + 
  geom_vline(aes(xintercept = mean(reddito)),
             color = "red", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = median(reddito)),
             color = "blue", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = quantile(reddito, 0.25)),
             color = "blue", linetype = "dotted", size = 1) +
  geom_vline(aes(xintercept = quantile(reddito, 0.75)),
             color = "blue", linetype = "dotted", size = 1) +
  ggtitle("Istogramma variabile Reddito") +
  theme(plot.title = element_text(hjust = 0.5)) + # Per centrare il titolo
  xlab("") + 
  ylab("Frequency") +
  geom_text(aes(mean(varlab), 10), label = round(mean(varlab),2), hjust = 1, vjust = 1.5, angle = 90, col = "red") +
  geom_text(aes(median(varlab), 10), label = round(median(varlab),2), hjust = 1, vjust = 1.5, angle = 90, col = "blue")

# Per rimuovere oggetti nell'environment non più necessari
rm(varlab)


# Raggruppamenti e grafici a barre
# ------------------------------------------------------------------------------
# Le unità statistiche possono essere raggruppate nelle cinque macroaree della colonna "area"
unique(df$area)
# Pertanto, l'indagine potrebbe disaggregarsi nelle cinque macroaree

# Reddito medio suddiviso per macroarea
mean(df[df$area == "Nord-Ovest",c("reddito")])
mean(df[df$area == "Nord-Est",c("reddito")])
mean(df[df$area == "Centro",c("reddito")])
mean(df[df$area == "Sud",c("reddito")])
mean(df[df$area == "Isole",c("reddito")])

# Sarebbe forse più comodo realizzare una nuova tabella con i dati raggruppati per macroaree
# Pertanto, attraverso un loop (for) realizzeremo una tabella contentente:
# i nomi delle varie aree e per ciascuna di esse la media di ogni variabile
df_area <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 7))
mynames <- c("area","unita",c(names(df)[-1]))
names(df_area) <- paste(mynames)
var_area <- unique(df$area)
for (var in var_area) {
  dfl <- df[df$area == var,]
  df_area = rbind(df_area,c(var,
                            nrow(dfl),
                            round(mean(dfl$reddito),2),
                            round(mean(dfl$disuguaglianza),2),
                            round(mean(dfl$qualita_aria),2),
                            round(mean(dfl$medici),2),
                            round(mean(dfl$emigrazione_ospedaliera),2)
                            ))
  df_area$unita <- as.integer(df_area$unita)
  df_area$reddito <- as.numeric(df_area$reddito)
  df_area$disuguaglianza <- as.numeric(df_area$disuguaglianza)
  df_area$qualita_aria <- as.numeric(df_area$qualita_aria)
  df_area$medici <- as.numeric(df_area$medici)
  df_area$emigrazione_ospedaliera <- as.numeric(df_area$emigrazione_ospedaliera)
}
df_area <- df_area[-1,]

# Quanto mostrato con l'uso del loop può essere fatto anche con il pacchetto dyplr
# facendo ricorso alla funzione "group_by" e all'uso dell'operatore pipe %>%
library(dplyr)
df_area_v2 <- 
  df %>%
  group_by(area) %>%
  summarise(
    unita = n(),
    reddito = mean(reddito),
    disuguaglianza = mean(disuguaglianza),
    qualita_aria = mean(qualita_aria),
    medici = mean(medici),
    emigrazione_ospedaliera = mean(emigrazione_ospedaliera)
  )

# Barplot del reddito medio suddiviso nelle cinque macroaree
barplot(reddito ~ area, 
        data = df_area,
        main = "Barplot Reddito per macroarea",
        xlab = "Reddito",
        ylab = "Area",
        col = "moccasin")

# EXTRA: Realizziamo un barplot anche con il pacchetto ggplot2
library(ggplot2)
ggplot(data = df_area, aes(x = area, y = disuguaglianza)) +
  geom_bar(stat = "identity", color = "black", fill = "moccasin") +
  ggtitle("Barplot Disuguaglianza per macroarea") +
  xlab("Area") +
  ylab("Disuguaglianza")


# Raggruppamenti e boxplot
# ------------------------------------------------------------------------------
# Considerando le cinque macroaree, possiamo realizzare dei boxplot
# Questi ci forniranno informazioni circa la distribuzione delle nostre variabili
# in ciascuna delle cinque macroaree in cui raggruppiamo le unità statistiche

# Boxplot della variabile reddito nelle cinque aree
boxplot(reddito ~ area, data = df)
boxplot(reddito ~ area_4, data = df)

# L'area denominata "isole" con solo due unità statistiche non si presta benissimo in un boxplot
# Generiamo dunque una nuova colonna chiamata "area_4" dove mettiamo assieme: Sud e Isole (Mezzogiorno)
df$area_4 = ifelse(df$area == "Isole" | df$area == "Sud", "Mezzogiorno", df$area)

# Generiamo nuovamente il boxplot precedente (con qualche aggiunta estetica)
boxplot(reddito ~ area_4, 
        data = df,
        main = "Boxplot variabile Reddito",
        xlab = "Macroaree",
        ylab = "Reddito",
        col = c("palegoldenrod","lightcyan3","sandybrown","plum2"), 
        outcol = "red")
# Aggiungiamo le medie
data_means <- aggregate(df$reddito,         
                        list(df$area_4),
                        mean)
points(x = 1:nrow(data_means),                           
       y = data_means$x,
       col = "green",
       pch = 16) 

# EXTRA: Realizziamo un boxplot anche con ggplot2
library(ggplot2)
ggplot(
  df,
  aes(x = area_4, 
      y = disuguaglianza,
      fill = area_4)) + 
  geom_boxplot(outlier.colour = "red", color = "black") +
  stat_summary(fun = mean, 
               geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..),
               width = .75, 
               linetype = "dashed", 
               color = 'red',
               size = 0.8) +
  labs(title = "Boxplot variabile Disuguaglianza",
       x = "Gruppi di comuni", 
       y = "Disuguaglianza"
  ) + 
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#9fde8c"))
# NOTA: qui per coloarare abbiamo utilizzato i codici esadecimali

# EXTRA: Aggiunta del barplot anche riguardante il totale delle unità
df_tot <- df
df_tot$area_4 = "Totale"
df_tot <- rbind(df,df_tot)

# EXTRA: con ggplot2
library(ggplot2)
ggplot(
  df_tot,
  aes(x = area_4, 
      y = disuguaglianza,
      fill = area_4)) + 
  geom_boxplot(outlier.colour = "red", color = "black") +
  stat_summary(fun = mean, 
               geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..),
               width = .75, 
               linetype = "dashed", 
               color = 'red',
               size = 0.8) +
  labs(title = "Boxplot variabile Disuguaglianza",
       x = "Gruppi di comuni", 
       y = "Disuguaglianza"
  ) + 
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#9fde8c", "#b68fc2", "#999999"))

# EXTRA: La stessa cosa, fatta però con boxplot
boxplot(reddito ~ area_4, 
        data = df_tot,
        main = "Boxplot variabile Reddito",
        xlab = "Macroaree",
        ylab = "Reddito",
        col = c("palegoldenrod", "lightcyan3", "sandybrown", "plum2", "#999999"), 
        outcol = "red")
# Aggiungiamo le medie
data_means <- aggregate(df_tot$reddito,         
                        list(df_tot$area_4),
                        mean)
points(x = 1:nrow(data_means),                           
       y = data_means$x,
       col = "green",
       pch = 16) 

# Si nota come in tutti e 4 gruppi (reddito) la variabile mostri asimmetria di tipo positivo
# Infatti, la media è sempre maggiore della mediana

# Concentriamoci sul gruppo che presente alcuni outlier (Mezzogiorno)

# Deviazione standard (scarto quadratico medio): 
# 1) Campionaria
sd(df[df$area_4 == "Mezzogiorno", c("reddito")])
# 2) Popolazione
p.var = function(a){sqrt( sum((a - mean(a))^2) / length(a) )}
p.var(df[df$area_4 == "Mezzogiorno", c("reddito")])

# Seppur la presenza di asimmetria positiva sia chiara (anche per via degli outlier)
# Vi sono indici che ci aiutano a capire con che genere di asimmetria si abbia a che fare

# Indice di asimmetria (basata sulla differenza interquartile)
p.as = function(a){
  unname(
    ((quantile(a, c(.75)) - quantile(a, c(.5))) - (quantile(a, c(.5)) - quantile(a, c(.25)))) /
      ((quantile(a, c(.75)) - quantile(a, c(.5))) + (quantile(a, c(.5)) - quantile(a, c(.25))))
  )
}
# NOTA: il comando "unname" serve unicamente a mostrare il risultano
p.as(df[df$area_4 == "Mezzogiorno", c("reddito")])

# Indice di asimmetria di Fisher
p.asf = function(a){ 
  (sum(((a - mean(a)) / (sqrt(sum((a - mean(a))^2) / length(a))))^3)) / length(a)
}
p.asf(df[df$area_4 == "Mezzogiorno", c("reddito")])

# Individuazione dei dati anomali/outlier
# Per farlo dobbiamo prima identificare il massimo e il minimo
lim.inf = function(a){
  unname(quantile(a, c(.25))) - 1.5*(unname(quantile(a, c(.75)) - quantile(a, c(.25))))
}
liminf <- lim.inf(df[df$area_4 == "Mezzogiorno", c("reddito")])
lim.sup = function(a){
  unname(quantile(a, c(.75))) + 1.5*(unname(quantile(a, c(.75)) - quantile(a, c(.25))))
}
limsup <- lim.sup(df[df$area_4 == "Mezzogiorno", c("reddito")])

# Identifichiamo le unità statistiche con reddito superiore al limite
df[df$reddito > limsup & df$area_4 == "Mezzogiorno",]
# Identifichiamo le unità statistiche con reddito inferiore al limite
df[df$reddito < liminf & df$area_4 == "Mezzogiorno",]


# Correlazione
# ------------------------------------------------------------------------------
# Proviamo a vedere se tra le variabili vi sia qualche sorta di concordanza/discordanza
# Facendo ricorso alla covarianza
# NOTA: Qui consideriamo le unità statistiche come provenienti da un campione;
# pertanto, si farà uso della covarianza campionaria 

# Reddito -vs- Disuguaglianza

# Covarianza
cov(df$reddito,df$disuguaglianza)

# EXTRA: Covarianza (statistica descrittiva, popolazione)
p.cov = function(a, b){
  sum((a - mean(a)) * (b - mean(b))) / length(a)}
p.cov(df$reddito, df$disuguaglianza) 

# Coefficiente di correlazione di Bravais-Pearson
cor(df$reddito,df$disuguaglianza)
# Concludiamo che vi è una più che discreta relazione negativa (discordanza) tra reddito e disuguaglianza
# A valori alti di reddito sono associati valori bassi di disuguaglianza
# A valori bassi di reddito sono associati valori alti di disuguaglianza

# EXTRA: Coefficiente di correlazione di Bravais-Pearson (fatto "a mano")
p.cor = function(a, b){
  (sum((a - mean(a)) * (b - mean(b))) / length(a)) /
    sqrt((sum((a - mean(a))^2) / length(a)) * (sum((b - mean(b))^2) / length(b)))
}
p.cor(df$reddito, df$disuguaglianza)

# Reddito -vs- Qualità dell'aria

# Covarianza
cov(df$reddito,df$qualita_aria)
# Coefficiente di correlazione di Bravais-Pearson
cor(df$reddito,df$qualita_aria)
# Concludiamo che vi è una più che discreta relazione positiva (concordanza) tra reddito e qualità dell'aria
# A valori alti di reddito sono associati valori alti di qualità dell'aria
# A valori bassi di reddito sono associati valori bassi di qualità dell'aria

# Medici -vs- Emigrazione ospedaliera
# Covarianza
cov(df$medici,df$emigrazione_ospedaliera)
# Coefficiente di correlazione di Bravais-Pearson
cor(df$medici,df$emigrazione_ospedaliera)
# Concludiamo che vi è una bassa relazione negativia (discordanza) tra numero di medici
# e percentuale di emigrazione ospedaliera
# A valori alti di medici sono associati valori bassi di emigrazione ospedaliera
# A valori bassi di medici sono associati valori alti di emigrazione ospedaliera

# Reddito -vs- Emigrazione ospedaliera
# Covarianza
cov(df$reddito,df$emigrazione_ospedaliera)
# Coefficiente di correlazione di Bravais-Pearson
cor(df$reddito,df$emigrazione_ospedaliera)
# Concludiamo che vi è una discreta relazione negativia (discordanza) tra reddito
# e percentuale di emigrazione ospedaliera
# A valori alti di reddito sono associati valori bassi di emigrazione ospedaliera
# A valori bassi di reddito sono associati valori alti di emigrazione ospedaliera


# Scatterplot
# ------------------------------------------------------------------------------
# Possiamo studiare graficamente, per mezzo di uno scatterplot, la relazione tra due variabili

# Semplice scatter plot di Reddito (X) -vs- Disuguaglianza (Y)
plot(~ reddito + disuguaglianza, 
     data = df,
     main = "Scatter plot di Reddito (X) e Disuguaglianza (Y)")

# EXTRA: Possiamo rendere il grafico più accattivante
# Coloreremo con un diverso colore le nostre coppie di osservazioni (Xi, Yi)
# a seconda del gruppo di appartenenza (area_4)
# Scegliamo determinati colori che vorremo utilizzare
colors <- c("#de9b52", "#57de52", "#52c7de", "#de52a6") 

plot(~ reddito + disuguaglianza, 
     pch = 19,
     col = colors[factor(area_4)],
     data = df,
     main = "Scatter plot di Reddito (X) e Disuguaglianza (Y)")
# Aggiungiamo la legenda
legend("topright",
       legend = levels(factor(df$area_4)),
       pch = 19,
       col = colors[factor(levels(factor(df$area_4)))])
# Aggiungiamo la retta di regressione lineare
abline(lm(df$disuguaglianza ~ df$reddito), col = "red")

# Semplice scatter plot di Reddito (X) -vs- Qualità dell'aria (Y)
plot(~ reddito + qualita_aria, 
     data = df,
     main = "Scatter plot di Reddito (X) e Qualità dell'aria (Y)")
abline(lm(df$qualita_aria ~ df$reddito), col = "red")

# Semplice scatter plot di Medici (X) -vs- Emigrazione Ospedaliera (Y)
plot(~ medici + emigrazione_ospedaliera, 
     data = df,
     main = "Scatter plot di Medici (X) ed Emigrazione Ospedaliera (Y)")
abline(lm(df$emigrazione_ospedaliera ~ df$medici), col = "red")

# Semplice scatter plot di Reddito (X) -vs- Emigrazione Ospedaliera (Y)
plot(~ reddito + emigrazione_ospedaliera, 
     data = df,
     main = "Scatter plot di Reddito (X) ed Emigrazione Ospedaliera (Y)")
abline(lm(df$emigrazione_ospedaliera ~ df$reddito), col = "red")

# Possiamo realizzare anche delle Scatterplot Matrix
pairs(~ reddito + qualita_aria + disuguaglianza + medici + emigrazione_ospedaliera, data = df,
      main = "Simple Scatterplot Matrix")

# EXTRA: scatter plot con ggplot2
model <- lm(emigrazione_ospedaliera ~ reddito, data = df)
ggplot(df, aes(x = reddito, y = emigrazione_ospedaliera, color = area_4)) +
  geom_point(aes(x = reddito, y = emigrazione_ospedaliera), size = 3) +
  geom_abline(intercept = model[["coefficients"]][["(Intercept)"]], slope = model[["coefficients"]][["reddito"]], 
              color = "red", linetype = "solid", size = 0.9, alpha = 0.4) +
  labs(title = "Scatter plot di Reddito (X) ed Emigrazione Ospedaliera (Y)",
            y = "Emigrazione ospedaliera",
            x = "Reddito") +
  scale_color_manual(values=c('#999999','#E69F00', '#382f9c', "#7edeb9"), name = "Legenda")


# Regressione
# ------------------------------------------------------------------------------
# Ora andremo a stimare una funzione lineare tra due variabili
# Per comodità selezioniamo una X e una Y

# Regressione del reddito (X) sulla disuguaglianza (Y)
# X: variabile esplicativa / indipendente
# Y: variabile dipendente / risposta
X <- df$reddito
Y <- df$disuguaglianza

mydata <- as.data.frame(cbind(X,Y))

# Media campionaria di X
meanX <- mean(X)
# Media campionaria di Y
meanY <- mean(Y)
# Varianza campionaria di X
varX <- var(X)
# Varianza campionaria di Y
varY <- var(Y)
# Covarianza tra X e Y
varXY <- var(X,Y)

# Stima di Beta_1 (coefficiente angolare della retta)
beta1 <- varXY/varX
# Stima di Beta_0 (intercetta)
beta0 <- meanY - beta1*meanX

# Scriviamo la retta di regressione
paste0("Y = ",round(beta0,5),ifelse(beta1<0," "," +"),round(beta1,5),"X")

# Aggiungo al dataframe i valori di Y stimati dal modello
mydata$pY <- beta0 + beta1*(mydata$X)

# Calcolo i residui del modello
mydata$res <- (mydata$Y - mydata$pY)
sum(mydata$res)
# Verifico se la somma dei residui sia prossima a zero

# Calcolo lo scarto (al quadrato) tra valori predetti e valori di Y medi
# Ci serve per calcolare SSR (Somma dei quadrati della regressione - SSR)
# La varianza attribuita al modello
mydata$ssrY <- (mydata$pY - meanY)^2
# Calcolo lo scarto dei quadrati dei veri valori di Y rispetto alla media
# Ci serve per calcolare SST (Somma dei quadrati totale - SST)
# La varianza dei valori di Y rispetto alla loro media
mydata$sstY <- (mydata$Y - meanY)^2

# Calcoliamo R^2 (coefficiente di determinazione)
r2 <- sum(mydata$ssrY) / sum(mydata$sstY)
r2
# Indice di correlazione di Bravais-Pearson 
# Covarianza tra X e Y diviso il prodotto della deviazione standard di X e Y
r <- cor(X,Y)
# Se eleviamo al quadrato otteniamo proprio l'R^2
r^2

# Per effettuare il test sul coefficiente beta1 abbiamo bisogno di calcolare...

# Varianza del modello SSE (Somma dei quadrati degli errori - SSE)
mydata$sseY <- (mydata$Y - mydata$pY)^2
var_e <- sum(mydata$sseY) / (length(mydata$Y) - 2)
var_e

# Varianza di Beta1
var_beta1 <- var_e / (varX*(length(mydata$X) - 1))
var_beta1
# Standard error di Beta1
se_beta1 <- sqrt(var_beta1)
se_beta1

# Test d'ipotesi su Beta1
# H0 : Beta1 = 0
# H1 : Beta1 != 0

# Statistica test
t_oss <- (beta1 - 0) / sqrt(var_beta1)
t_oss

# Troviamo il valore critico di t (livello di significativià 5%)
ifelse(beta1 < 0,
       paste0("-",round(qt(p = .05/2, df = (length(mydata$Y) - 2), lower.tail = FALSE),5)),
       paste0("+",round(qt(p = .05/2, df = (length(mydata$Y) - 2), lower.tail = FALSE),5))
       )
  
# EXTRA: troviamo i valori critici per 1%, 5%, 10%
out <- ifelse(beta1 < 0,
         paste("\n",
               "Sig. 10%: -",round(qt(p = .1/2, df = (length(mydata$Y) - 2), lower.tail = FALSE),5),"\n",
               "Sig.  5%: -",round(qt(p = .05/2, df = (length(mydata$Y) - 2), lower.tail = FALSE),5),"\n",
               "Sig.  1%: -",round(qt(p = .01/2, df = (length(mydata$Y) - 2), lower.tail = FALSE),5)),
         paste("\n",
               "Sig. 10%: ",round(qt(p = .1/2, df = (length(mydata$Y) - 2), lower.tail = FALSE),5),"\n",
               "Sig.  5%: ",round(qt(p = .05/2, df = (length(mydata$Y) - 2), lower.tail = FALSE),5),"\n",
               "Sig.  1%: ",round(qt(p = .01/2, df = (length(mydata$Y) - 2), lower.tail = FALSE),5))
         )
cat(out)
# Notiamo che sino all'1% la statistica test (toss) è superiore/inferiore a quella critica
# Questo significa che cadiamo nella zona di riufiuto e quindi rifiutiamo l'ipotesi nulla di indipendenza

# Stimiamo la retta di regressione tramite la funzione "lm"
model <- lm(df$disuguaglianza ~ df$reddito)
model <- lm(disuguaglianza ~ reddito, data = df)

model <- lm(Y ~ X)
summary(model)
# All'aumento del reddito medio di 1 euro diminuisce di 0.00024 l'indice di disuguaglianza

# Meglio esprimere il reddito in modo diverso dividendolo per 1,000 euro
# Pertanto, una variazione incrementale di 1 unità della X equivarrà all'aumento
# del reddito medio di 1000 euro
mydata$X1000 <- mydata$X / 1000
model <- lm(Y ~ X1000, data = mydata)
summary(model)
# Il nostro R'2 è discreto e il coefficiente della nostra X statisticamente significativo all'1%
# All'aumentare del reddito medio di 1,000 euro diminuisce di 0.24 il valore dell'indice di disuguaglianza

# Regressione del reddito (X) sulla qualità dell'aria (Y)
X <- df$reddito
Y <- df$qualita_aria

mydata <- as.data.frame(cbind(X,Y))

# Stima del modello di regressione
model <- lm(Y ~ X)
summary(model)
# Cambiando l'unità di misura della X
mydata$X1000 <- mydata$X / 1000
model <- lm(Y ~ X1000, data = mydata)
summary(model)
# Il nostro R'2 è discreto e il coefficiente della nostra X statisticamente altamente significativo (< 0.1%)
# All'aumentare del reddito medio di 1,000 euro aumenta di 4.427 il valore della qualità dell'aria


# Regressione del numero di medici (X) sull'emigrazione ospedaliera (Y)
X <- df$medici
Y <- df$emigrazione_ospedaliera

mydata <- as.data.frame(cbind(X,Y))

# Stima del modello di regressione
model <- lm(Y ~ X)
summary(model)
# Si può notare un bassissimo R^2 nonché la non significatività della X
# Questo significa che il test d'ipotesi su beta1 
# con ipotesi nulla - H0 - che il valore di beta1 è pari a zero non può essere rifiutata


# Regressione del reddito (X) sull'emigrazione ospedaliera (Y)
X <- df$reddito
Y <- df$emigrazione_ospedaliera

mydata <- as.data.frame(cbind(X,Y))

# Stima del modello di regressione
model <- lm(Y ~ X)
summary(model)
# Cambiando l'unità di misura della X
mydata$X1000 <- mydata$X / 1000
model <- lm(Y ~ X1000, data = mydata)
summary(model)
# L'R^2 non è particolarmente elevato seppur la variabile esplicativa sia statisticamente significativa
# Questo vuol dire che rifiutiamo l'ipotesi nulla di assenza di relazione lineare tra X e Y
# con un livello di significatività del 5%; pertanto, se andassimo ad effettuare il test
# con un livello di significatività più basso, non si potrebbe rifiutare H0


# EXTRA: Mappe cloropletica
# ------------------------------------------------------------------------------
# Realizzazione di una mappa cloropletica in R con uno dei pacchetti più performanti: leaflet

# Importazione Shape file
# https://www.istat.it/it/archivio/222527
library(rgdal)
ita_map <- readOGR('Shape file Italia/RipGeo01012020_g', 
                       'RipGeo01012020_g_WGS84', stringsAsFactors = FALSE)

# Convertiamo i file importati in una struttura dataframe che contenga le geometrie che dovremo rappresentare
library(sf)
ita_map_sf  <- st_as_sf(ita_map)

# Merge: possiamo farlo con il pacchetto dplyr
library(dplyr)
map_area = left_join(ita_map_sf, df_area, by = c("DEN_RIP" = "area"))
# Oppure con la funzione merge (ma qui bisogna avere il medesimo nome nelle colonne dei due dataset)
df_area2 <- df_area
names(df_area2)[names(df_area2) == "area"] <- "DEN_RIP"
map_area = merge(ita_map_sf, df_area2, by = c("DEN_RIP")) 

# Generiamo il nostro database completo con le coordinate geografiche
st_map_area = st_transform(map_area, crs = 4326)

# Rappresentiamo la mappa del reddito nelle 5 macroaree

# Palette
library(leaflet)
palette = colorBin("YlGnBu", domain = st_map_area$reddito, na.color = "grey", bins = 4, alpha = 0.9)

# Setting dei labels (in JAVA)
labels <- sprintf(
  "<b>Area</b>: %s <br/>
   <b>Reddito</b>: %s <br/> 
   <b>Disuguaglianza</b>: %s <br/> 
   <b>Qualita' aria</b>: %s <br/>",
  st_map_area$DEN_RIP,
  st_map_area$reddito,
  st_map_area$disuguaglianza,
  st_map_area$qualita_aria
) %>% lapply(htmltools::HTML)

# Map
mymap <- st_map_area %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  setView(lat = 42.293598, lng = 12.220722, zoom = 6) %>%
  addPolygons( 
    data = st_map_area,
    fillColor = ~palette(reddito),
    weight = 0.8,
    opacity = 0.8,
    color = "grey",
    smoothFactor = 0.1,
    fillOpacity = 0.3,
    highlight = highlightOptions(
      weight = 2,
      color = "orange",
      fillOpacity = 0.5,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
    addLegend("bottomright", 
              data = st_map_area, 
              pal = palette, 
              values = ~reddito,
              title = "Reddito (2020)",
              labFormat = labelFormat(prefix = "Eur. "),
              opacity = 0.5
              )
mymap


# EXTRA: Boxplot multipli per gruppi
# ------------------------------------------------------------------------------
# Proviamo ad effettuare un boxplot suddiviso per aree geografiche ma dove affiancheremo:
# - tasso di occupazione maschile
# - tasso di occupazione femminile

df_add <- df <- read.csv("dati_istat_add.csv", header = TRUE, sep = ',')
df_add$area_4 = ifelse(df_add$area == "Isole" | df_add$area == "Sud", "Mezzogiorno", df_add$area)

# Dobbiamo attuare un po' di modifiche all'attuale struttura dei dati
# E' preferibile creare dei dataset ad hoc per ciascun grafico che si vuole realizzare

# Aggiunta del barplot anche riguardante il totale delle unità
df_add_tot <- df_add
df_add_tot$area_4 = "Totale"
df_add_tot <- rbind(df_add,df_add_tot)

df_add_tot_m <- df_add_tot[,c("area_4","tasso_occupazione_m")]
df_add_tot_f <- df_add_tot[,c("area_4","tasso_occupazione_f")]
df_add_tot_m$genere <- "Uomini"
df_add_tot_f$genere <- "Donne"

names(df_add_tot_m)[names(df_add_tot_m) == "tasso_occupazione_m"] <- "tasso_occupazione"
names(df_add_tot_f)[names(df_add_tot_f) == "tasso_occupazione_f"] <- "tasso_occupazione"

df_add_tot <- rbind(df_add_tot_m, df_add_tot_f)

# Generiamo nuovamente il boxplot precedente (con qualche aggiunta estetica)
boxplot(tasso_occupazione ~ genere + area_4, 
        data = df_add_tot,
        main = "Boxplot: Tasso di Occupazione per genere (20-64)",
        xlab = "Macroaree",
        ylab = "Tasso di Occupazione (20-64)",
        cex.axis = 0.8,
        col = c("thistle3","skyblue3","thistle3","skyblue3","thistle3","skyblue3","thistle3","skyblue3","thistle4","skyblue4"), 
        outcol = "red"
        )
# Aggiungiamo le medie
data_means <- aggregate(df_add_tot$tasso_occupazione,         
                        list(df_add_tot$genere,df_add_tot$area_4),
                        mean)
points(x = 1:nrow(data_means),                           
       y = data_means$x,
       col = "green",
       pch = 16) 

# Con ggplot2
library(ggplot2)
ggplot(
  df_add_tot,
  aes(x = area_4, 
      y = tasso_occupazione,
      fill = genere)) + 
  geom_boxplot(outlier.colour = "red", color = "black") +
  stat_summary(fun = mean, 
               geom = "point", 
               size = 2,
               shape = 19,
               position = position_dodge(0.75),
               color = 'green') +
  labs(title = "Boxplot variabile Disuguaglianza",
       x = "Gruppi di comuni", 
       y = "Disuguaglianza"
  ) + 
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("thistle3","skyblue3"))


# ------------------------------------------------------------------------------
# Link utili
# StackOverflow: https://stackoverflow.com/
# R-bloggers: https://www.r-bloggers.com/
# Colors: https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
# R plot pch symbols: http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r