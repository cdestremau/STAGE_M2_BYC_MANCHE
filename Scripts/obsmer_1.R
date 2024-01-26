
library(ggplot2)

library(data.table) #more efficient management of large data sets

library(RVAideMemoire) #used for post-hoc tests in exploratory analyses

library(stringr) # handling character string format in R

library(randomForest)

library(RRF) #regularized random forest




OBSMER_BYC_2021<-read.csv2("CAPTURES.csv", fileEncoding="ISO-8859-3")
setDT(OBSMER_BYC_2021)
OBSMER_BYC_2021=OBSMER_BYC_2021[ESPECE=="Delphinus delphis",]


strat<-read.csv2("STRATEGIE.csv", fileEncoding="ISO-8859-3")
setDT(strat)


OBSMER_BYC_2021=merge(OBSMER_BYC_2021, strat[,.(ID_VRAC,ID_OP)], by="ID_VRAC", all.x = T, all.y=F)


operation<-read.csv2("OPERATION_PECHE.csv", fileEncoding="ISO-8859-3")
setDT(operation)

OBSMER_BYC_2021=merge(OBSMER_BYC_2021, operation[,.(ID_MAREE,ID_OP,ZONE)], by="ID_OP", all.x = T, all.y=F)
OBSMER_BYC_2021=OBSMER_BYC_2021[ZONE %in% c("27.7.e","27.7.d"),]


anguille <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Anguilla anguilla",]

murre <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Uria aalge",]

dauphin <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Delphinus delphis",]

marsouin <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Phocoena phocoena",]

thresher <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Alopias vulpinus",]

p_gris <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Halichoerus grypus",]

r_bleu <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Prionace glauca",]

r_autres <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Carcharhiniformes",]

r_taupe <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE =="Lamna nasus",]

squalidae <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Squalidae",]

p_roussette <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE =="Scyliorhinus",]

s_dogfish <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Squalus acanthias",]

#emissoles <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Mustelus",]

#r_ha <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Galeorhinus galeus",]

#emi_lisse <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Mustelus mustelus" ,]

requins <- rbind(r_bleu, r_autres, r_taupe, squalidae, p_roussette, s_dogfish, thresher)

sp_byc <- rbind (requins, anguille, murre, dauphin, marsouin)



species_counts <- table(OBSMER_BYC_2021$ESPECE)
sp <- data.table(species_counts)
species_counts_df <- as.data.frame(species_counts)

# Rename the columns and remove the row names
colnames(species_counts_df) <- c("Species", "Frequency")
rownames(species_counts_df) <- NULL

# Display the summary
print(species_counts_df)


filtered_species <- subset(species_counts_df, Frequency >= 0 & Frequency <= 4)

# Visualize as a barplot
barplot <- barplot(filtered_species$Frequency, names.arg = filtered_species$Species, 
        main = "Species Frequency > 20", xlab = "Species", ylab = "Frequency")


setDT(requins)
requins_table <- table(requins$ESPECE)
sp_byc <- table(sp_byc$ESPECE)


# Load the ggplot2 package
library(ggplot2)

# Create a data frame for ggplot
data <- data.frame(
  category = names(requins_table),
  count = as.numeric(requins_table),
  percentage = percentages
)

# Create a pie chart with counts and percentages
ggplot(data, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = sprintf("%d\n%.1f%%", count, percentages)), position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(title = "Pie Chart with Counts and Percentages")


####################

percentages <- prop.table(sp_byc) * 100

sp_byc_table <- data.frame(
  category = names(sp_byc),
  count = as.numeric(sp_byc),
  percentage = percentages
)


ggplot(sp_byc_table, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = sprintf("%d\n%.1f%%", count, percentages)), position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(title = "Pie Chart with Counts and Percentages")


#another way of proceeding is to select all marees of vessels fishing at least in part in the bay of biscay (areas 27.8.a/27.8.b), using vessels ID present in the data_vessel dataset (see fleet definition works)




#all data from 2003 to 2023: 160 declarations


marees<-read.csv2("MAREE.csv", fileEncoding="ISO-8859-3")

setDT(marees)

OBSMER_BYC_2021=marees[ID_MAREE %in% OBSMER_BYC_2021[,ID_MAREE],]


OBSMER_BYC_2021=OBSMER_BYC_2021[VALID_PROGRAMME=="Données valides",] # all the OBSMER data in 2023, are not validated yet




#description of fishing marees implying accidental bycatch:



table(OBSMER_BYC_2021$ANNEE) #few data between 2011 and 2014, no data before 2008 (although the program started in 2003?)


table(OBSMER_BYC_2021$ANNEE)/table(marees$ANNEE)[names(table(marees$ANNEE)) %in% names(table(OBSMER_BYC_2021$ANNEE))] 



t.test(OBSMER_BYC_2021$LONGUEUR,marees$LONGUEUR) #tendency of vessels a bit longer in case of dolphin bycatch ?
t.test(as.numeric(OBSMER_BYC_2021$PUISSANCE),as.numeric(marees$PUISSANCE))





#precise comparisons to only vessels operating in the Bay of Biscay?


OBSMER_BYC_2021=merge(OBSMER_BYC_2021, operation[ID_MAREE %in% OBSMER_BYC_2021$ID_MAREE,], by="ID_MAREE", all = T)
OBSMER_BYC_2021=merge(OBSMER_BYC_2021, strat[ID_OP %in% OBSMER_BYC_2021$ID_OP,], by="ID_OP", all = T)




capture<-read.csv2("CAPTURES.csv", fileEncoding="ISO-8859-3")
setDT(capture)

OBSMER_BYC_2021=merge(OBSMER_BYC_2021, capture[ID_VRAC %in% OBSMER_BYC_2021$ID_VRAC,], by="ID_VRAC", all = T)






mesure<-read.csv2("MESURES.csv", fileEncoding="ISO-8859-3")

setDT(mesure)

OBSMER_BYC_2021[, `:=`(TYPE.x = NULL , TYPE.y = NULL)]

OBSMER_BYC_2021=merge(OBSMER_BYC_2021, mesure[ID_CAPTURE %in% OBSMER_BYC_2021$ID_CAPTURE,], by="ID_CAPTURE", all = T)





#Distinguishing individuals measured separately (these individuals are currently not differentiated):

setorder(OBSMER_BYC_2021, CRITERE)


u=c()
for (i in unique(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",]$ID_CAPTURE)) {
  u=c(u,max(c(table(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",][ID_CAPTURE==i,]$CRITERE),1)))}
unique(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",]$ID_CAPTURE)[u>1] #problematic captures



OBSMER_BYC_2021$ID_IND=NA_character_





for (i in 1:length(u)) {
  for (j in 1:u[i]) {
    OBSMER_BYC_2021[ESPECE=="Delphinus delphis",][ID_CAPTURE==unique(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",]$ID_CAPTURE)[i],][which(!duplicated(CRITERE))+(j-1),]$ID_IND=
      rep(
        paste(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",][ID_CAPTURE==unique(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",]$ID_CAPTURE)[i],]$ID_CAPTURE[1],j, sep="_"),
        length(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",][ID_CAPTURE==unique(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",]$ID_CAPTURE)[i],][which(!duplicated(CRITERE))+(j-1),]$ID_IND)
      )
  }
}


setorder(OBSMER_BYC_2021, ANNEE, ID_NAVIRE, ID_MAREE, ID_OP, ID_CAPTURE, ID_IND)





#Attribution of number of captured dolphin per fishing sequence (incomplete aggregation presently):


for (i in OBSMER_BYC_2021[ESPECE=="Delphinus delphis",]$ID_CAPTURE) {
  OBSMER_BYC_2021[ESPECE=="Delphinus delphis",][ID_CAPTURE==i,][is.na(NOMBRE.x),]$NOMBRE.x=
    rep(sum(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",][ID_CAPTURE==i & !duplicated(ID_IND),]$NOMBRE.y), 
        length(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",][ID_CAPTURE==i,][is.na(NOMBRE.x),]$NOMBRE.x)
    )
}






OBSMER_BYC_2021$NOMBRE=NA_integer_



for (i in OBSMER_BYC_2021[ESPECE=="Delphinus delphis",]$ID_OP) {
  OBSMER_BYC_2021[ESPECE=="Delphinus delphis",][ID_OP==i,]$NOMBRE=
    rep(sum(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",][ID_OP==i & !duplicated(ID_CAPTURE),]$NOMBRE.x), 
        length(OBSMER_BYC_2021[ESPECE=="Delphinus delphis",][ID_OP==i,]$NOMBRE)
    )
}









