rm(list=ls(all=TRUE))

Data <- read.table("C:/Users/Luciano/Documents/Analisis publicaciones/HM Olrog/Database.txt", header = TRUE)
Data
class(Data)

# CREATION OF FACTORS

Data$Sex <- factor(Data$Sex, labels=c("Females","Males"))
Data$Category <- factor(Data$Category, labels=c("Adults","Dead chicks"))

# EXPLORATORY ANALYSIS
unique(Data$Category) 
dim(Data)
class(Data$Category)
class(Data$Sex)
class(Data$ID)


# BIOCHEM VS. METALS ADULTS

colnames(Data)
with(Data, is.factor(Category))

select <- Data$Category == "Adults"  # Creates a vector called select which has the same length as the variable "Cateory" with values that are TRUE if Category = Adult and FALSE otherwise

adults <- Data[select,]  # Select those rows of data for which "select" = TRUE and store the data in "adults" 
adults
dim(adults)
colnames(adults)
summary(adults)

# HISTOGRAM PLOTS

with(adults, hist(Cd))
with(adults, hist(Pb))
with(adults, hist(Cu))
with(adults, hist(Zn))
with(adults, hist(Cr))
with(adults, hist(Ni))
with(adults, hist(Mn))
with(adults, hist(Fe))

# COEFFICIENTS OF VARIATION FOR METAL DATA IN ADULTS

(MEAN_Cd <- with(adults,mean(Cd)))
(SD_Cd <- with(adults,sd(Cd)))
(CV_Cd <- (SD_Cd/MEAN_Cd))

(MEAN_Pb <- with(adults,mean(Pb)))
(SD_Pb <- with(adults,sd(Pb)))
(CV_Pb <- (SD_Pb/MEAN_Pb))

(MEAN_Cu <- with(adults,mean(Cu)))
(SD_Cu <- with(adults,sd(Cu)))
(CV_Cu <- (SD_Cu/MEAN_Cu))

(MEAN_Zn <- with(adults,mean(Zn)))
(SD_Zn <- with(adults,sd(Zn)))
(CV_Zn <- (SD_Zn/MEAN_Zn))

(MEAN_Cr <- with(adults,mean(Cr)))
(SD_Cr <- with(adults,sd(Cr)))
(CV_Cr <- (SD_Cr/MEAN_Cr))

(MEAN_Ni <- with(adults,mean(Ni)))
(SD_Ni <- with(adults,sd(Ni)))
(CV_Ni <- (SD_Ni/MEAN_Ni))

(MEAN_Mn <- with(adults,mean(Mn)))
(SD_Mn <- with(adults,sd(Mn)))
(CV_Mn <- (SD_Mn/MEAN_Mn))

(MEAN_Fe <- with(adults,mean(Fe)))
(SD_Fe <- with(adults,sd(Fe)))
(CV_Fe <- (SD_Fe/MEAN_Fe))


######################################################

# POCHA'S WAY TO USE SUBSET OF DATA

model.1 <- lm(Data[Data$Category=="Adult","PCV"]~Data[Data$Category=="Adult","Cd"])
summary(model.1)

adults=Data[Data$Category=="Adult",]
adults

model.2 <- lm(adultos$PCV~adultos$Cd)
summary(model.2)

######################################################

# EXPLORATORY ANALYSIS METALS

plot.new()
op <- par(mfrow = c(3,3), mar = c(1,2.5,2,2))

with(adults, boxplot(Cd, main = "Cd"))
with(adults, boxplot(Pb, main = "Pb"))
with(adults, boxplot(Cu, main = "Cu"))
with(adults, boxplot(Zn, main = "Zn"))
with(adults, boxplot(Cr, main = "Cr"))
with(adults, boxplot(Ni, main = "Ni"))
with(adults, boxplot(Mn, main = "Mn"))
with(adults, boxplot(Fe, main = "Fe"))

with(adults, boxplot(Cd, main = "Cd"))
with(adults, boxplot(Pb, main = "Pb"))
with(adults, boxplot(Cu, main = "Cu"))
with(adults, boxplot(Zn, main = "Zn"))
with(adults, boxplot(Cr, main = "Cr"))
with(adults, boxplot(Ni, main = "Ni"))
with(adults, boxplot(Mn, main = "Mn"))
with(adults, boxplot(Fe, main = "Fe"))


# PCV vs. metals

plot(adults$Cd, adults$PCV)
abline(lm(PCV ~ Cd, data=adults))
modelo.3 <- lm(PCV ~ Cd, data=adults)
summary(modelo.3)
op <- par(mfrow = c(2,2), mar = c(5,4,1,2))
plot(modelo.3)


plot(adults$Pb, adults$PCV)
abline(lm(PCV ~ Pb, data=adults))
modelo.4 <- lm(PCV ~ Pb, data=adults)
summary(modelo.4)

plot(adults$Cu, adults$PCV)
abline(lm(PCV ~ Cu, data=adults))
modelo.5 <- lm(PCV ~ Cu, data=adults)
summary(modelo.5)

plot(adults$Zn, adults$PCV)
abline(lm(PCV ~ Zn, data=adults))
modelo.6 <- lm(PCV ~ Zn, data=adults)
summary(modelo.6)

plot(adults$Cr, adults$PCV)
abline(lm(PCV ~ Cr, data=adults))
modelo.7 <- lm(PCV ~ Cr, data=adults)
summary(modelo.7)

plot(adults$Ni, adults$PCV)
abline(lm(PCV ~ Ni, data=adults))
modelo.8 <- lm(PCV ~ Ni, data=adults)
summary(modelo.8)

plot(adults$Mn, adults$PCV)
abline(lm(PCV ~ Mn, data=adults))
modelo.9 <- lm(PCV ~ Mn, data=adults)
summary(modelo.9)

plot(adults$Cr, adults$PCV)
abline(lm(PCV ~ Cr, data=adults))
modelo.10 <- lm(PCV ~ Cr, data=adults)
summary(modelo.10)

##########################################################

# TP vs. metals

plot(adults$Cd, adults$Prot)
abline(lm(Prot ~ Cd, data=adults))
modelo.11 <- lm(Prot ~ Cd, data=adults)
summary(modelo.11)

plot(adults$Pb, adults$Prot)
abline(lm(PCV ~ Prot, data=adults))
modelo.12 <- lm(Prot ~ Pb, data=adults)
summary(modelo.12)

plot(adults$Cu, adults$Prot)
abline(lm(Prot ~ Cu, data=adults))
modelo.13 <- lm(Prot ~ Cu, data=adults)
summary(modelo.13)

plot(adults$Zn, adults$Prot)
abline(lm(Prot ~ Zn, data=adults))
modelo.14 <- lm(Prot ~ Zn, data=adults)
summary(modelo.14)

plot(adults$Cr, adults$Prot)
abline(lm(Prot ~ Cr, data=adults))
modelo.15 <- lm(Prot ~ Cr, data=adults)
summary(modelo.15)

plot(adults$Ni, adults$Prot)
abline(lm(Prot ~ Ni, data=adults))
modelo.16 <- lm(Prot ~ Ni, data=adults)
summary(modelo.16)

plot(adults$Mn, adults$Prot)
abline(lm(Prot ~ Mn, data=adults))
modelo.17 <- lm(Prot ~ Mn, data=adults)
summary(modelo.17)
op <- par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(modelo.17)

plot(adults$Cr, adults$Prot)
abline(lm(Prot ~ Cr, data=adults))
modelo.18 <- lm(Prot ~ Cr, data=adults)
summary(modelo.18)

#############################################################################

# SEX EFFECT

# To extract the data from Category == adult and Sex == males (including NAs) 

adult.males <- Data[Data$Category == "Adult" & Data$Sex == 1,]  # Da errores por los NAs en Sex creo
adult.males 

# Extraccion paso a paso 
adults <- Data[Data$Category == "Adult" & Data$Sex == 1,] 
Data[adults, ]

adult.males <- adults[adults$Sex == 1,]  # Da errores por los NAs en Sex creo 
adult.males
 

# R frendo
selection.males <- which(Data$Category == "Adult" & Data$Sex == 1) 
Data[selection.males, ]

dim(selection.males)  # Dimentionless (NULL)


adultos_sexados <- Data[!is.na(Data$Sex),]
adultos_sexados

dim(adultos_sexados)  # It has dimention, so I should work with this!!

only_males <- adultos_sexados[adultos_sexados$Sex == "Males",]
only_males
summary(only_males)

sapply(only_males[,12:19], FUN = sd)

only_females <- adultos_sexados[adultos_sexados$Sex == "Females",] 
only_females
summary(only_females)

sapply(only_females[,12:19], FUN = sd)

               
###############################################################################################

# In-depth exploration of Mn vs. Prot (solo adultos sexados)

modelo.19 <- lm(Prot ~ Mn, data=adultos_sexados)
summary(modelo.19)

##############################################

# Cómo extraigo una base de datos que contenga solo "Mn" de Adultos de ambos Sexos para luego comparar diferencias entre Sexos con t-test?

# Pido toda la base de datos para Adultos sexados

adultos_sexados <- Data[!is.na(Data$Sex),]
adultos_sexados
dim(adultos_sexados)

# Subset Females
females <- adultos_sexados[adultos_sexados$Sex == 0,]
females
dim(females)

# Subset Males
males <- adultos_sexados[adultos_sexados$Sex == 1,]
males
dim(males)

# Armo una sub-base con Mn para Males
Mn_males = males[,c(18)]
Mn_males

# Armo una sub-base con Mn para Females
Mn_females = females[,c(18)]
Mn_females

# Las dimensiones son distintas (numeros de filas) con lo que no se puede poner como cols distitnas, a menos que quieras que te llene de NAs, que despues seran dificiles de trabajar
dim(males)
dim(females)

# Entonces te lo puse en una misma base en una sola col
datos.n = rbind(only_males, only_females)
datos.n

t.test(Cd ~ Sex, data = datos.n)
t.test(Pb ~ Sex, data = datos.n)
t.test(Cu ~ Sex, data = datos.n)
t.test(Zn ~ Sex, data = datos.n)
t.test(Cr ~ Sex, data = datos.n)
t.test(Ni ~ Sex, data = datos.n)
t.test(Mn ~ Sex, data = adultos_sexados)
t.test(Fe ~ Sex, data = datos.n)

wilcox.test(Cd ~ Sex, data = datos.n)
wilcox.test(Pb ~ Sex, data = datos.n)
wilcox.test(Cu ~ Sex, data = datos.n)
wilcox.test(Zn ~ Sex, data = datos.n)
wilcox.test(Cr ~ Sex, data = datos.n)
wilcox.test(Ni ~ Sex, data = datos.n)
wilcox.test(Mn ~ Sex, data = adultos_sexados)
wilcox.test(Fe ~ Sex, data = datos.n)

# PLOT METALS BY SEX

summary(datos.n[datos.n$Sex=="Male",])
summary(datos.n[datos.n$Sex=="Female",])

plot.new()
par(mfrow=c(2,2), mar = c(2,2.5,2,2), bty = "l")
boxplot(Cd ~ Sex, data = adultos_sexados)
boxplot(Pb ~ Sex, data = adultos_sexados)
boxplot(Cu ~ Sex, data = adultos_sexados)
boxplot(Zn ~ Sex, data = adultos_sexados)

grep("grey",colors())

colors()[grep("grey",colors())]

barplot(rep(1,8), yaxt="n", col=1:8)
barplot(rep(1,8), yaxt="n", col=palette())

0:8/8
palette(red(0:8 / 8))
palette()
barplot(rep(1,10),col=1:10, yaxt="n")
palette()

plot.new()
par(mar = c(3,4.2,2,2), bty = "l")
plot(Cr ~ Sex, las = 1, ylab = "Cr concentration (ug g-1 d.w.)", xlab = "", cex.lab = 1, col = 68, cex.axis = 1, font.main = 4, data = adultos_sexados)

minor.tick(ny=2, nx=FALSE, tick.ratio=0.5)
text(locator(1), "P=0.01", font = 1, cex = 1) 

boxplot(Ni ~ Sex, data = datos.n)
boxplot(Mn ~ Sex, data = datos.n)
boxplot(Fe ~ Sex, data = datos.n)


############################################################################################

# EXPLORATION OF METAL CONCENTRATION AS A FUNCTION OF SIZE

cor(datos.n[, 4:8], method = "spearman", use = "complete.obs")

cor(Pb,Cd, use = "complete.obs")


###########################################################################################

op <- par(mfrow = c(2,1), mar = c(2.5,5,2,2))

plot(Cd ~ Beak, data = adults)
abline(lm(Cd ~ Beak, data = adults))
lm_1 <- lm(Cd ~ Beak, data = adults)
summary(lm_1)

plot(Cu ~ Beak, data = adults)
abline(lm(Cu ~ Beak, data = adults))
lm_2 <- lm(Cu ~ Beak, data = adults)
summary(lm_2)


plot(Zn ~ Beak, data = adults)
abline(lm(Zn ~ Beak, data = adults))
lm_3 <- lm(Zn ~ Beak, data = adults)
summary(lm_3)

plot(Cr ~ Beak, data = adults)
abline(lm(Cr ~ Beak, data = adults))
lm_4 <- lm(Cr ~ Beak, data = adults)
summary(lm_4)

plot(Ni ~ Beak, data = adults)
abline(lm(Ni ~ Beak, data = adults))
lm_5 <- lm(Ni ~ Beak, data = adults)
summary(lm_5)

plot(Mn ~ Beak, data = adults)
abline(lm(Mn ~ Beak, data = adults))
lm_6 <- lm(Mn ~ Beak, data = adults)
summary(lm_6)

plot(Fe ~ Beak, data = adults)
abline(lm(Fe ~ Beak, data = adults))
lm_7 <- lm(Fe ~ Beak, data = adults)
summary(lm_7)


############################################################################################

plot(Cd ~ Head, data = adults)
abline(lm(Cd ~ Head, data = adults))
lm_8 <- lm(Cd ~ Head, data = adults)
summary(lm_8)

plot(Cu ~ Head, data = adults)
abline(lm(Cu ~ Head, data = adults))
lm_9 <- lm(Cu ~ Head, data = adults)
summary(lm_9)

plot(Zn ~ Head, data = adults)
abline(lm(Zn ~ Head, data = adults))
lm_10 <- lm(Zn ~ Head, data = adults)
summary(lm_10)

plot(Cr ~ Head, data = adults)
abline(lm(Cr ~ Head, data = adults))
lm_11 <- lm(Cr ~ Head, data = adults)
summary(lm_11)

plot(Ni ~ Head, data = adults)
abline(lm(Ni ~ Head, data = adults))
lm_12 <- lm(Ni ~ Head, data = adults)
summary(lm_12)

plot(Mn ~ Head, data = adults)
abline(lm(Mn ~ Head, data = adults))
lm_13 <- lm(Mn ~ Head, data = adults)
summary(lm_13)

plot(Fe ~ Head, data = adults)
abline(lm(Fe ~ Head, data = adults))
lm_14 <- lm(Fe ~ Head, data = adults)
summary(lm_14)

############################################################################################

plot(Cd ~ Tibiotarsus, data = adults)
abline(lm(Cd ~ Tibiotarsus, data = adults))
lm_15 <- lm(Cd ~ Tibiotarsus, data = adults)
summary(lm_15)

plot(Cu ~ Tibiotarsus, data = adults)
abline(lm(Cu ~ Tibiotarsus, data = adults))
lm_16 <- lm(Cu ~ Tibiotarsus, data = adults)
summary(lm_16)

plot(Zn ~ Tibiotarsus, data = adults)
abline(lm(Zn ~ Tibiotarsus, data = adults))
lm_17 <- lm(Zn ~ Tibiotarsus, data = adults)
summary(lm_17)

plot(Cr ~ Tibiotarsus, data = adults)
abline(lm(Cr ~ Tibiotarsus, data = adults))
lm_18 <- lm(Cr ~ Tibiotarsus, data = adults)
summary(lm_18)

plot(Ni ~ Tibiotarsus, data = adults)
abline(lm(Ni ~ Tibiotarsus, data = adults))
lm_19 <- lm(Ni ~ Tibiotarsus, data = adults)
summary(lm_19)

plot(Ni ~ Tibiotarsus, data = adultos_sexados)
abline(lm(Ni ~ Tibiotarsus, data = adultos_sexados))
lm_19A <- lm(Ni ~ Tibiotarsus*Sex, data = adultos_sexados)
summary(lm_19A)

lm_19B <- lm(Ni ~ Tibiotarsus+Sex, data = adultos_sexados)
summary(lm_19B)

plot(Ni ~ Sex, data = adultos_sexados)
lm_19B <- lm(Ni ~ Sex, data = adultos_sexados)
summary(lm_19B)


plot(Mn ~ Tibiotarsus, data = adults)
abline(lm(Mn ~ Tibiotarsus, data = adults))
lm_20 <- lm(Mn ~ Tibiotarsus, data = adults)
summary(lm_20)

plot(Mn ~ Tibiotarsus, data = adultos_sexados)
abline(lm(Mn ~ Tibiotarsus, data = adultos_sexados))
lm_20A <- lm(Mn ~ Tibiotarsus*Sex, data = adultos_sexados)
summary(lm_20A)

lm_20B <- lm(Mn ~ Tibiotarsus+Sex, data = adultos_sexados)
summary(lm_20B)


plot(Fe ~ Tibiotarsus, data = adults)
abline(lm(Fe ~ Tibiotarsus, data = adults))
lm_21 <- lm(Fe ~ Tibiotarsus, data = adults)
summary(lm_21)

############################################################################################

plot(Cd ~ Wing, data = adults)
abline(lm(Cd ~ Wing, data = adults))
lm_22 <- lm(Cd ~ Wing, data = adults)
summary(lm_22)

plot(Cu ~ Wing, data = adults)
abline(lm(Cu ~ Wing, data = adults))
lm_23 <- lm(Cu ~ Wing, data = adults)
summary(lm_23)

plot(Zn ~ Wing, data = adults)
abline(lm(Zn ~ Wing, data = adults))
lm_24 <- lm(Zn ~ Wing, data = adults)
summary(lm_24)

plot(Cr ~ Wing, data = adults)
abline(lm(Cr ~ Wing, data = adults))
lm_25 <- lm(Cr ~ Wing, data = adults)
summary(lm_25)

plot(Ni ~ Wing, data = adults)
abline(lm(Ni ~ Wing, data = adults))
lm_26 <- lm(Ni ~ Wing, data = adults)
summary(lm_26)

plot(Mn ~ Wing, data = adults)
abline(lm(Mn ~ Wing, data = adults))
lm_27 <- lm(Mn ~ Wing, data = adults)
summary(lm_27)

plot(Fe ~ Wing, data = adults)
abline(lm(Fe ~ Wing, data = adults))
lm_28 <- lm(Fe ~ Wing, data = adults)
summary(lm_28)

##############################################################################################

plot(Cd ~ Weight, data = adults)
abline(lm(Cd ~ Weight, data = adults))
lm_29 <- lm(Cd ~ Weight, data = adults)
summary(lm_29)


plot(Cu ~ Weight, data = adults)
abline(lm(Cu ~ Weight, data = adults))
lm_30 <- lm(Cu ~ Weight, data = adults)
summary(lm_30)


plot(Zn ~ Weight, data = adults)
abline(lm(Zn ~ Weight, data = adults))
lm_31 <- lm(Zn ~ Weight, data = adults)
summary(lm_31)

plot(Cr ~ Weight, data = adults)
abline(lm(Cr ~ Weight, data = adults))
lm_32 <- lm(Cr ~ Weight, data = adults)
summary(lm_32)

plot(Ni ~ Weight, data = adults)
abline(lm(Ni ~ Weight, data = adults))
lm_33 <- lm(Ni ~ Weight, data = adults)
summary(lm_33)

plot(Mn ~ Weight, data = adults)
abline(lm(Mn ~ Weight, data = adults))
lm_34 <- lm(Mn ~ Weight, data = adults)
summary(lm_34)

plot(Fe ~ Weight, data = adults)
abline(lm(Fe ~ Weight, data = adults))
lm_35 <- lm(Fe ~ Weight, data = adults)
summary(lm_35)

##############################################################################################
##############################################################################################

# COMPARISON BETWEEN FEATHER TYPES IN DEAD CHICKS

# ---1. Extraer Dead Chicks

dead_chicks <- Data[Data$Category=="Dead chicks",] 
dead_chicks
class(dead_chicks)

# Basic stats

cover_feathers <- dead_chicks[dead_chicks$Feather=="Cover",]
cover_feathers  
summary(cover_feathers$Mn)  # Para no tener que repetir para cada matal, hacer...

summary(dead_chicks[dead_chicks$Feather=="Cover",])

# Coefficient of variation for metals in cover feathers dead chicks

(MEAN_Cd <- with(cover_feathers,mean(Cd)))
(SD_Cd <- with(cover_feathers,sd(Cd)))
(CV_Cd <- (SD_Cd/MEAN_Cd))

(MEAN_Pb <- with(cover_feathers,mean(Pb)))
(SD_Pb <- with(cover_feathers,sd(Pb)))
(CV_Pb <- (SD_Pb/MEAN_Pb))

(MEAN_Cu <- with(cover_feathers,mean(Cu)))
(SD_Cu <- with(cover_feathers,sd(Cu)))
(CV_Cu <- (SD_Cu/MEAN_Cu))

(MEAN_Zn <- with(cover_feathers,mean(Zn)))
(SD_Zn <- with(cover_feathers,sd(Zn)))
(CV_Zn <- (SD_Zn/MEAN_Zn))

(MEAN_Cr <- with(cover_feathers,mean(Cr)))
(SD_Cr <- with(cover_feathers,sd(Cr)))
(CV_Cr <- (SD_Cr/MEAN_Cr))

(MEAN_Ni <- with(cover_feathers,mean(Ni)))
(SD_Ni <- with(cover_feathers,sd(Ni)))
(CV_Ni <- (SD_Ni/MEAN_Ni))

(MEAN_Mn <- with(cover_feathers,mean(Mn)))  # NAs >> error
(SD_Mn <- with(cover_feathers,sd(Mn)))
(CV_Mn <- (SD_Mn/MEAN_Mn))

# The following example shows how to select those listwise nonmissing cases
a1 <- !is.na(cover_feathers$Mn);a1    # TRUE if nonmissing    

(MEAN_Mn <- with(cover_feathers, mean(Mn[!is.na(Mn)])))
(SD_Mn <- with(cover_feathers, sd(Mn[!is.na(Mn)])))
(CV_Mn <- (SD_Mn/MEAN_Mn))

a2 <- !is.na(cover_feathers$Fe);a2    # TRUE if nonmissing
(MEAN_Fe <- with(cover_feathers, mean(Fe[!is.na(Fe)])))
(SD_Fe <- with(cover_feathers, sd(Fe[!is.na(Fe)])))
(CV_Fe <- (SD_Fe/MEAN_Fe))

#############################################################################

# Coefficient of variation for metals in cover feathers dead chicks

prim <- dead_chicks[dead_chicks$Feather=="Primary",]
prim

summary(dead_chicks[dead_chicks$Feather=="Primary",])

(MEAN_Cd <- with(prim,mean(Cd)))
(SD_Cd <- with(prim,sd(Cd)))
(CV_Cd1 <- (SD_Cd/MEAN_Cd))

(MEAN_Pb <- with(prim,mean(Pb)))
(SD_Pb <- with(prim,sd(Pb)))
(CV_Pb1 <- (SD_Cd/MEAN_Pb))

(MEAN_Cu <- with(prim,mean(Cu)))
(SD_Cu <- with(prim,sd(Cu)))
(CV_Cu1 <- (SD_Cu/MEAN_Cu))

(MEAN_Zn <- with(prim,mean(Zn)))
(SD_Zn <- with(prim,sd(Zn)))
(CV_Zn1 <- (SD_Zn/MEAN_Zn))

(MEAN_Cr1 <- with(prim,mean(Cr)))
(SD_Cr1 <- with(prim,sd(Cr)))
(CV_Cr1 <- (SD_Cr1/MEAN_Cr1))

(MEAN_Ni <- with(prim,mean(Ni)))
(SD_Ni <- with(prim,sd(Ni)))
(CV_Ni1 <- (SD_Cd/MEAN_Ni))

# The following example shows how to select those listwise nonmissing cases
a3 <- !is.na(prim$Mn);a3    # TRUE if nonmissing    

(MEAN_Mn <- with(prim, mean(Mn[!is.na(Mn)])))
(SD_Mn <- with(prim, sd(Mn[!is.na(Mn)])))
(CV_Mn1 <- (SD_Mn/MEAN_Mn))

a4 <- !is.na(prim$Fe);a4    # TRUE if nonmissing
(MEAN_Fe <- with(prim, mean(Fe[!is.na(Fe)])))
(SD_Fe <- with(prim, sd(Fe[!is.na(Fe)])))
(CV_Fe1 <- (SD_Fe/MEAN_Fe))


#############################################################################

# Comparison metal level between feather types
t.test(Cd ~ Feather, data = dead_chicks)
t.test(Pb ~ Feather, data = dead_chicks)
t.test(Cu ~ Feather, data = dead_chicks)
t.test(Zn ~ Feather, data = dead_chicks)
t.test(Cr ~ Feather, data = dead_chicks)
t.test(Ni ~ Feather, data = dead_chicks)
t.test(Mn ~ Feather, data = dead_chicks)
t.test(Fe ~ Feather, data = dead_chicks)

# EXTRACT ONLY DATA FROM DEAD CHICKS TO RUN WILCOXON

dead_chicks <- Data[Data$Category=="Dead_chick",] 
dead_chicks

paired <- dead_chicks[c(1:11,16:18,22,29,41,42:58), c(11,12:19)]
paired
class(paired)


wilcox.test(Cd ~ Feather, data = dead_chicks)
wilcox.test(Pb ~ Feather, data = dead_chicks)
wilcox.test(Cu ~ Feather, data = dead_chicks)
wilcox.test(Zn ~ Feather, data = dead_chicks)
wilcox.test(Cr ~ Feather, data = dead_chicks)
wilcox.test(Ni ~ Feather, data = dead_chicks)
wilcox.test(Mn ~ Feather, data = dead_chicks)
wilcox.test(Fe ~ Feather, data = dead_chicks)

#######################################################################################

# COMPARISON BETWEEN ADULT PRIMARY AND CHICK PRIMARY 

primaries <- Data[Feather=="Primary",]
primaries

dim(primaries)

primaries$Category <- factor(primaries$Category, labels=c("Adults","Dead chicks"))

t.test(Cd ~ Category, data = primaries)
t.test(Pb ~ Category, data = primaries)
t.test(Cu ~ Category, data = primaries)
t.test(Zn ~ Category, data = primaries)
t.test(Cr ~ Category, data = primaries)
t.test(Ni ~ Category, data = primaries)
t.test(Mn ~ Category, data = primaries)
t.test(Fe ~ Category, data = primaries)

plot.new()
op <- par(mfrow = c(2,2), mar = c(2.5,5,2,2), bty = "l")

plot(Cd ~ Category, col = "grey", las = 1, xlab = "", ylab = "", cex.lab = 1.4, cex.axis = 1.4, data = primaries)
minor.tick(ny=2, nx=FALSE, tick.ratio=0.5)
text(locator(1), "P<0.01", font = 1, cex = 1.2)
mtext("Cd (ug g-1)", 2, line = 3.5, cex = 1.4)

plot(Pb ~ Category, col = "grey", las = 1, xlab = "", ylab = "", cex.lab = 1.4, cex.axis = 1.4, data = primaries)
minor.tick(ny=2, nx=FALSE, tick.ratio=0.5)
text(locator(1), "P>0.05", font = 1, cex = 1.2)
mtext("Pb (ug g-1)", 2, line = 3.5, cex = 1.4)

plot(Cu ~ Category, col = "grey", las = 1, xlab = "", ylab = "", cex.lab = 1.4, cex.axis = 1.4, data = primaries)
minor.tick(ny=2, nx=FALSE, tick.ratio=0.5)
text(locator(1), "P<0.01", font = 1, cex = 1.2)
mtext("Cu (ug g-1)", 2, line = 3.5, cex = 1.4)

plot(Zn ~ Category, col = "grey", las = 1, xlab = "", ylab = "", cex.lab = 1.4, cex.axis = 1.4, data = primaries)
minor.tick(ny=2, nx=FALSE, tick.ratio=0.5)
text(locator(1), "P>0.05", font = 1, cex = 1.2)
mtext("Zn (ug g-1)", 2, line = 3.5, cex = 1.4)


plot.new()
op <- par(mfrow = c(2,2), mar = c(2.5,5,2,2), bty = "l")

plot(Cr ~ Category, col = "grey", las = 1, xlab = "", ylab = "", cex.lab = 1.4, cex.axis = 1.4, data = primaries)
minor.tick(ny=2, nx=FALSE, tick.ratio=0.5)
text(locator(1), "P<0.01", font = 1, cex = 1.2)
mtext("Cr (ug g-1)", 2, line = 3.5, cex = 1.4)

plot(Ni ~ Category, col = "grey", las = 1, xlab = "", ylab = "", cex.lab = 1.4, cex.axis = 1.4, data = primaries)
minor.tick(ny=2, nx=FALSE, tick.ratio=0.5)
text(locator(1), "P<0.01", font = 1, cex = 1.2)
mtext("Ni (ug g-1)", 2, line = 3.5, cex = 1.4)

plot(Mn ~ Category, col = "grey", las = 1, xlab = "", ylab = "", cex.lab = 1.4, cex.axis = 1.4, data = primaries)
minor.tick(ny=2, nx=FALSE, tick.ratio=0.5)
text(locator(1), "P<0.01", font = 1, cex = 1.2)
mtext("Mn (ug g-1)", 2, line = 3.5, cex = 1.4)

plot(Fe ~ Category, col = "grey", las = 1, xlab = "", ylab = "", cex.lab = 1.4, cex.axis = 1.4, data = primaries)
minor.tick(ny=2, nx=FALSE, tick.ratio=0.5)
text(locator(1), "P>0.05", font = 1, cex = 1.2)
mtext("Fe (ug g-1)", 2, line = 3.5, cex = 1.4)


# SUMMARY ADULT PRIMARIES AND CHICK PRIMARIES

summary(primaries[primaries$Category=="Adults",])
summary(primaries[primaries$Category=="Dead chicks",])      

desv.est = sd(only_cover[only_cover$Category=="Adult",])
desv.est

##############################################################################################

# SIZE COMPARISON BETWEENS SEXES

plot.new()
op <- par(mfrow = c(3,2), mar = c(2.5,4.5,2,2), bty = "l")
boxplot(Beak ~ Sex, ylab = "Beak", data = adults)
boxplot(Head ~ Sex, ylab = "Head", data = adults)
boxplot(Tibiotarsus ~ Sex, ylab = "Tibiotarsus", data = adults)
boxplot(Wing ~ Sex, ylab = " Wing", data = adults)
boxplot(Weight ~ Sex, ylab = "Weight", data = adults)


