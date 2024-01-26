
####################################################################
####################################################################
####################################################################
#                 ID species of concern 


library(ggplot2)

library(data.table) #more efficient management of large data sets

library(RVAideMemoire) #used for post-hoc tests in exploratory analyses

library(stringr) # handling character string format in R


OBSMER_BYC_2021<-read.csv2("CAPTURES.csv", fileEncoding="ISO-8859-3")
setDT(OBSMER_BYC_2021)
#OBSMER_BYC_2021=OBSMER_BYC_2021[ESPECE=="Delphinus delphis",]


#Add strategy data
strat<-read.csv2("STRATEGIE.csv", fileEncoding="ISO-8859-3")
setDT(strat)

OBSMER_BYC_2021 <- subset(OBSMER_BYC_2021, select = -c(ID_DETAIL, CAT, ESPECE_COM, TAUX_ECH, POIDS_REF, WEIGHT_RTP, PRODUCT_DESTINATION, CONVERSION_COEFFICIENT, ALL_TAUX_ECH))

#Combine the two datasets and sort them via the ID_VRAC variable
OBSMER_BYC_2021=merge(OBSMER_BYC_2021, strat[,.(ID_VRAC,ID_OP)], by="ID_VRAC", all.x = T, all.y=F)

#Add operation data
operation<-read.csv2("OPERATION_PECHE.csv", fileEncoding="ISO-8859-3")
setDT(operation)

operation <- subset(operation, select = c(ID_OP, DATE_FIN, DUREE_OP, RECTANGLE, PROF_FOND, PROF_ENGIN, ID_MAREE,ENGIN, METIER1, LONG_DEB_OP,LAT_DEB_OP, ESPECE_CIBLE, MAILL, EFFORT, ID_OP,ZONE))
setDT(operation)

#Repeat merge 
OBSMER_BYC_2021=merge(OBSMER_BYC_2021, operation[,.(ID_MAREE,ENGIN, METIER1, LONG_DEB_OP,LAT_DEB_OP, ESPECE_CIBLE, MAILL, EFFORT, ID_OP,ZONE)], by="ID_OP", all.x = T, all.y=F)


OBSMER_BYC_2021=merge(OBSMER_BYC_2021, operation[,.(DATE_FIN, DUREE_OP, RECTANGLE, PROF_FOND, PROF_ENGIN, ID_MAREE,ENGIN, METIER1, LONG_DEB_OP,LAT_DEB_OP, ESPECE_CIBLE, MAILL, EFFORT, ID_OP,ZONE)], by="ID_OP", all.x = T, all.y=F)



#select data in the region of interest, here english channel
OBSMER_BYC_2021=OBSMER_BYC_2021[ZONE %in% c("27.7.e","27.7.d"),]











#selection of species interest

#Cormoran huppe N = 12
#Phoque commun (Phoca vitulina) N = 2

h_seal <-
g_seal <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Halichoerus grypus",]
murre <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Uria aalge",]
dauphin <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Delphinus delphis",]
marsouin <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Phocoena phocoena",]

#birds 
# Ala torda 1
#  Alcidae 1
#  Phalacrocorax aristotelis 12 
#  Phalacrocorax carbo 4
#  Morus bassanus 3
#  Gavia stellata  1
#  Gavia immer 1
#  Larus marinus 1
#  Larus argentatus 6

bird_byc <- rbind(
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Alca torda", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Uria aalge", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Alcidae", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Phalacrocorax aristotelis", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Phalacrocorax carbo", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Morus bassanus", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Gavia stellata", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Gavia immer", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Phalacrocorax aristotelis", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Larus marinus", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Larus argentatus", ]
)

bird_byc$NOMBRE[is.na(bird_byc$NOMBRE)] <- 1


#requins
thresher <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Alopias vulpinus",]
p_gris <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Halichoerus grypus",]
r_bleu <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Prionace glauca",]
  

#Visualization of the four selected species omitting the shark vector :(

library(ggplot2)

sp_byc <- rbind(g_seal, murre, dauphin, marsouin)
sp_byc <- sp_byc[,7]
sp_byc <- table(sp_byc)
sp_byc <- data.frame(sp_byc)
#sp_byc_R <- rbind(sp_byc, requins)

sp_byc$NOMBRE[is.na(sp_byc$NOMBRE)] <- 1

##########################################################################

sp_byc <- rbind(
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Halichoerus grypus", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Delphinus delphis", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Phocoena phocoena", ],
  OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Phoca vitulina", ],
  bird_byc
)



mesure <-read.csv2("MESURES.csv", fileEncoding="ISO-8859-3")
setDT(mesure)

sp_byc=merge(sp_byc, mesure[,.(NOMBRE)], by="ID_CAPTURE", all.x = T, all.y=F)

sp_byc=merge(sp_byc, operation[,.(DATE_FIN, DUREE_OP, RECTANGLE, PROF_FOND, PROF_ENGIN)], by="ID_OP", all.x = T, all.y=F)

OBSMER_BYC_2021=merge(OBSMER_BYC_2021, operation[,.(DATE_FIN, DUREE_OP, RECTANGLE, PROF_FOND, PROF_ENGIN, ID_MAREE,ENGIN, METIER1, LONG_DEB_OP,LAT_DEB_OP, ESPECE_CIBLE, MAILL, EFFORT, ID_OP,ZONE)], by="ID_OP", all.x = T, all.y=F)


sp_byc1 <- sp_byc1[, c("ESPECE", "ZONE")]

sp_byc2 <- table(sp_byc1$ESPECE)
sp_byc2 <- as.data.frame.table(sp_byc2)
sp_byc2 <- sp_byc2[sp_byc2$Freq != 0, ]

ggplot(sp_byc2, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Species Bycatch per Zone", x = "Species", y = "# of bycatch event") +
  scale_y_continuous(breaks=seq(0,32,5)) +
  theme_minimal()



ggplot(sp_byc1, aes(x = ESPECE, y = NOMBRE, fill = ZONE)) +
  geom_col(position = "dodge") +
  labs(title = "Total individuals per species and zone", x = "Species", y = "# of Individuals") +
  theme_bw() +
  scale_fill_manual(values = c("27.7.e" = "grey23", "27.7.d" = "lightgrey")) 




sp_byc<-read.csv("sp_byc.csv", header = T, sep = ",")


write.csv(mm_byc, file = "D:/Data/FREE2_OBSMER_serie/mm_byc.csv", row.names = FALSE)

#barplot of species of concern based on # of rows it shows up 
#warning : noticed after the column 'number', so proportions are not representative of actual data here

ggplot(sp_byc, aes(x = ESPECE, y = Freq, fill = ESPECE)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of bycatch per species", x = "Species", y = "Count") +
  theme_minimal()


ggplot(sp_byc, aes(x = ESPECE, y = NOMBRE)) +
     geom_bar(stat = "count", position = "dodge") +
     labs(title = "Count of bycatch per species", x = "Species", y = "Count") +
     theme_minimal()


ggplot(sp_byc, aes(x = ESPECE, y = NOMBRE, fill = ESPECE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bycatch count per species", x = "SPECIES", y = "# OF INDIVIDUALS") +
  theme_bw()+
  scale_fill_manual(values=c("skyblue", "darkseagreen", "khaki", "red4", "rosybrown"))+
  scale_y_continuous(breaks=seq(0,12,2))


##################distribution in time?

index <- grepl("2018", sp_byc$DATE_FIN, fixed = TRUE)

# Select rows based on the logical index
selected_rows <- sp_byc[index, ]

# Print the selected rows
print(selected_rows)

years <- 2007:2023

# Create a new column 'Year' with NA
sp_byc$Year <- NA

# Loop through each year and update the 'Year' column
for (year in years) {
  sp_byc$Year[grepl(as.character(year), sp_byc$DATE_FIN, fixed = TRUE)] <- as.character(year)
}

print(sp_byc)

sp_byc$Year <- ifelse(grepl("2018", sp_byc$DATE_FIN, fixed = TRUE), "2018", NA)

# Print the updated data frame
print(sp_byc)


years_to_keep <- c("2023", "2022", "2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007")



ggplot(sp_byc, aes(x = Year, fill = ESPECE)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Species Count per Year", x = "Year", y = "Count") +
  theme_minimal()# Use rainbow palette  theme_minimal()# Use rainbow palette  theme_minimal()




sp_23 <- sp_byc[sp_byc$Year == "2023",]

d_byc <- sp_byc[sp_byc$ESPECE == "Delphinus delphis",]

d_byc <- table(d_byc$ESPECE, d_byc$Year)
d_byc <- as.data.frame.table(d_byc)

ggplot(d_byc, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Species Bycatch per Year", x = "Year", y = "Number of Dolphins") +
  theme_minimal()






################N individual per species in entire dataset

species_counts <- table(OBSMER_BYC_2021$ESPECE)
sp <- data.table(species_counts)

colnames(sp) <- c("Species", "Count")
rownames(sp) <- NULL

#What if i want to know the species based on the times it was caught
filtered_species <- subset(sp, Count >= 14 & Count <= 29)

# Visualize as a barplot
barplot <- barplot(filtered_species$Count, names.arg = filtered_species$Species, 
        main = "Species Frequency > 20", xlab = "Species", ylab = "Frequency")

############################################################
############################################################
############################################################
#           ENGIN/ESPECE_CIBLE : DELPHINUS DELPHIS    


DAUPHIN =OBSMER_BYC_2021[ESPECE=="Delphinus delphis",]

#Keep the data of interest : ESPECE & ESPECE_CIBLE 
#Get a count each time a combo shows up

E_C <- DAUPHIN[,c("ENGIN","ESPECE_CIBLE")]
E_C <- data.frame(E_C)
E_C <- table(E_C)
E_C <- data.table(E_C)


#We end up having all the possible combos, most don't show up in the data so we remove them

E_C <- subset(E_C, N!=0)

#Visualization 

E_C$ENGIN <- as.factor(E_C$ENGIN)
E_C$ESPECE_CIBLE<- as.factor(E_C$ESPECE_CIBLE)
  
  library(ggplot2)

ggplot(E_C, aes(x = ENGIN, y = N, fill = ESPECE_CIBLE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Plot of ESPECE_CIBLE by ENGIN",
       x = "ENGIN",
       y = "Count")  

#BSS has highest value

############################################################
############################################################
############################################################
#           ENGIN/ESPECE_CIBLE : URIA AALGE

MURRE =OBSMER_BYC_2021[ESPECE=="Uria aalge",]

#Keep the data of interest : ESPECE & ESPECE_CIBLE 
#Get a count each time a combo shows up
U <- MURRE[,c("ENGIN","ESPECE_CIBLE")]
U <- data.frame(table(U))

#We end up having all the possible combos, most don't show up in the data so we remove them
U <- subset(U, Freq!=0)

#Visualization 

U$ENGIN <- as.factor(U$ENGIN)
U$ESPECE_CIBLE<- as.factor(U$ESPECE_CIBLE)
  
  library(ggplot2)

ggplot(U, aes(x = ENGIN, y = Freq, fill = ESPECE_CIBLE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Plot of ESPECE_CIBLE by ENGIN",
       x = "ENGIN",
       y = "Count")  



############################################################
############################################################
############################################################
#           ENGIN/ESPECE_CIBLE : PHOCOENA PHOCOENA

marsouin =OBSMER_BYC_2021[ESPECE=="Phocoena phocoena",]

#Keep the data of interest : ESPECE & ESPECE_CIBLE 
#Get a count each time a combo shows up
M <- marsouin[,c("ENGIN","ESPECE_CIBLE")]
M <- data.frame(table(M))

#We end up having all the possible combos, most don't show up in the data so we remove them
M <- subset(M, Freq!=0)


#Visualization 

M$ENGIN <- as.factor(M$ENGIN)
M$ESPECE_CIBLE<- as.factor(M$ESPECE_CIBLE)
  
  library(ggplot2)

ggplot(M, aes(x = ENGIN, y = Freq, fill = ESPECE_CIBLE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Plot of ESPECE_CIBLE by ENGIN",
       x = "ENGIN",
       y = "Count")  

ENGIN <- data.frame(table(M$ENGIN))



############################################################
############################################################
############################################################
################            MAP             ################             

dauphin <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Delphinus delphis"]
murre <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Uria aalge",]
marsouin <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Phocoena phocoena",]
anguille <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Anguilla anguilla",]

install.packages("maps")
install.packages("mapdata")

library(maps)   # package qui permet de créer des cartes
library(mapdata)

manche <- map("world", xlim=c(-8,4), ylim=c(47,51), col = "gray90", boundary = T, fill = T)

# On rajoute ensuite les axes et une échelle
map.axes(cex.axis=1)
map.scale(3, 41.5, ratio=FALSE, relwidth=0.10, cex=0.8)

#add data points 

dauphin_loc <- points(dauphin$LONG_DEB_OP, dauphin$LAT_DEB_OP, pch = 3, col = "black")
marsouin_loc <- points(marsouin$LONG_DEB_OP, marsouin$LAT_DEB_OP, pch = 21, col = "blue")
murre_loc <- points(murre$LONG_DEB_OP, murre$LAT_DEB_OP, pch = 20, col = "red")
anguille_loc <- points(anguille$LONG_DEB_OP, anguille$LAT_DEB_OP, pch = 23, col = "orange")

legend(x="bottomright", legend=c("Dauphins","Marsouins", "Murre", "Anguille"), col=c("black","blue", "red", "orange"), pch=c(3,21, 20, 23), cex = 0.8)























########################################################
########################################################
########################################################
# Visualization of fishing effort per metier per species
#Warning : sample sizes aren't equal

dauphin$NOMBRE[is.na(dauphin$NOMBRE)] <- 1
marsouin$NOMBRE[is.na(marsouin$NOMBRE)] <- 1
murre$NOMBRE[is.na(murre$NOMBRE)] <- 1


d_effort <- dauphin[, c("METIER1", "EFFORT")]
m_effort <- marsouin[, c("METIER1", "EFFORT")]
u_effort <- murre[, c("METIER1", "EFFORT")]

mean_effort <- tapply(d_effort$EFFORT, d_effort$METIER1, mean, na.rm = TRUE)
mean_effort <- data.frame(METIER1 = names(result), Mean_EFFORT = unname(result))
mean_effort <- na.omit(result_df)

m_m_effort <- tapply(m_effort$EFFORT, m_effort$METIER1, mean, na.rm = TRUE)
m_m_effort <- data.frame(m_m_effort)

u_u_effort <- tapply(u_effort$EFFORT, u_effort$METIER1, mean, na.rm = TRUE)
u_u_effort <- data.frame(u_u_effort)

effort <- cbind(mean_effort, m_m_effort, u_u_effort)
colnames(effort) <- c("Métiers", "Dauphin", "Marsouin", "Uria")


sum_effort <- as.numeric()
sum_total <- as.numeric()
sum_perc <- as.numeric()

col_sum <- c("Dauphin", "Marsouin", "Uria")

for (col in col_sum) {
  sum_effort <- c(sum_effort, sum(as.numeric(effort[[col]], na.rm = TRUE)))
sum_effort <-(sum_effort)
}

sum_effort <- data.frame(sum_effort, row.names = c("Dauphin", "Marsouin", "Murre"))

print(sum_effort)

### Plot for effort per metier for dolphin bycatch

ggplot(mean_effort, aes(x = METIER1, y = Mean_EFFORT)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +  # You can use geom_col() as well
  labs(title = "Mean EFFORT per METIER1", x = "METIER1", y = "Mean EFFORT") +
  theme_bw()

#### Plot mean effort per métier per bycatch sp

library(reshape2)
melted_data <- melt(effort, id.vars = "Métiers")


#regular barplot

ggplot(melted_data, aes(x = Métiers, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean fishing effort per métier depending on sp bycatchr", x = "Métier", y = "Mean Effort") +
  scale_fill_manual(values = c("Dauphin" = "blue", "Marsouin" = "green", "Uria" = "red"))

#proportion 

ggplot(melted_data, aes(x = Métiers, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Mean fishing effort per métier depending on sp bycatch", x = "Métier", y = "Mean Effort") +
  scale_fill_manual(values = c("Dauphin" = "blue", "Marsouin" = "green", "Uria" = "red"))























