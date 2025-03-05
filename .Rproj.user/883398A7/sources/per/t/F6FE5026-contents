#######################################
# Estudi Univariant
# Archiu del paquet titanic built-in de R
#######################################

# Instal·lar el paquet
install.packages("titanic")

# Carregar les dades
library(titanic)
data("titanic_train")

# Filtrar dades rellevants
titanic_clean <- na.omit(titanic_train[, c("Survived", "Age", "Fare", "Sex", "Pclass")])

paste(titanic_clean)

#######################################
#Descripció de les dades

# Survived = binàri 0 o 1
# Age = numéric edat en anys
# Fare = numéric preu del billet
# Sex = no numèric M/F
# Pclass = numéric ordinal eg classe 1, 2 o 3
#######################################

#######################################

# Estudi univariant sobre el preu del tíquet dels supervivents

#######################################

titanic_supervivents <- subset(titanic_clean, Survived == 1)

# Moda 

modaSuperVivents <- function(x){
  freq <- table(x)
  moda <- names(freq)[which.max(freq)]
  return(moda)
}

moda <- modaSuperVivents(titanic_supervivents$Fare)
print(paste("Moda del preu dels bitllets entre supervivents:", moda))

titanic_supervivents$Fare_Group <- cut(titanic_supervivents$Fare, 
                               breaks = c(0, 20, 40, 60, 100, 600),
                               labels = c("0-20", "20-40", "40-60", "60-100", "100+"))

moda_grup_fare <- modaSuperVivents(titanic_supervivents$Fare_Group)
print(paste("Interval modal del preu entre supervivents:", moda_grup_fare))

# Mitjana

print(paste("Mitjana dels preus dels billets entre supervivents:", 
            round(mean(titanic_supervivents$Fare), 2)))


#
