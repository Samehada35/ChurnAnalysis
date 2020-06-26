library(ggplot2)
churn.data1.original <- read.table(file="./data/DATA1_churn_analysis.csv", header=T, sep = ",")
churn.data2.original <- read.table(file="./data/DATA2_churn_analysis.csv", header=T, sep = ",")

# 1. Statistiques Descriptives

dim(churn.data1.original)
#DATA1_churn_analysis contient 2000 observations de 14 variables (2000x14)

dim(churn.data2.original)
#DATA1_churn_analysis contient 3333 observations de 21 variables (2000x14)

#Oui il existe des valeurs manquantes, il n'existe pas de valeurs constantes

plot(churn.data1.original$Class)
#Pour DATA1, il y'a 50% des individus qui ont churné

plot(churn.data2.original$churn)
#Pour DATA1, il y'a environ 15% des individus qui ont churné

#Proportion churn vs non churn pour les variables catégorielles de Data 1
ggplot(churn.data1.original, aes(x=Class, fill=aug_user_type)) + geom_bar(position = "dodge")
ggplot(churn.data1.original, aes(x=Class, fill=sep_user_type)) + geom_bar(position = "dodge")
ggplot(churn.data1.original, aes(x=Class, fill=aug_fav_a)) + geom_bar(position = "dodge")
ggplot(churn.data1.original, aes(x=Class, fill=sep_fav_a)) + geom_bar(position = "dodge")

#Proportion churn vs non churn pour les variables catégorielles de Data 2
ggplot(churn.data2.original, aes(x=churn, fill=state)) + geom_bar(position = "dodge")
ggplot(churn.data2.original, aes(x=churn, fill=phone.number)) + geom_bar(position = "dodge")
ggplot(churn.data2.original, aes(x=churn, fill=international.plan)) + geom_bar(position = "dodge")
ggplot(churn.data2.original, aes(x=churn, fill=voice.mail.plan)) + geom_bar(position = "dodge")

#Proportion churn vs non churn pour les variables numériques de Data 1
par(mfrow=c(1,2))
hist(x=churn.data1.original$network_age[which(churn.data1.original$Class=="Churned")])
hist(x=churn.data1.original$network_age[which(churn.data1.original$Class=="Active")])

par(mfrow=c(1,2))
hist(x=churn.data1.original$Aggregate_Total_Rev[which(churn.data1.original$Class=="Churned")])
hist(x=churn.data1.original$Aggregate_Total_Rev[which(churn.data1.original$Class=="Active")])

par(mfrow=c(1,2))
hist(x=churn.data1.original$Aggregate_SMS_Rev[which(churn.data1.original$Class=="Churned")])
hist(x=churn.data1.original$Aggregate_SMS_Rev[which(churn.data1.original$Class=="Active")])

par(mfrow=c(1,2))
hist(x=churn.data1.original$Aggregate_Data_Rev[which(churn.data1.original$Class=="Churned")])
hist(x=churn.data1.original$Aggregate_Data_Rev[which(churn.data1.original$Class=="Active")])

par(mfrow=c(1,2))
hist(x=churn.data1.original$Aggregate_Data_Vol[which(churn.data1.original$Class=="Churned")])
hist(x=churn.data1.original$Aggregate_Data_Vol[which(churn.data1.original$Class=="Active")])

par(mfrow=c(1,2))
hist(x=churn.data1.original$Aggregate_ONNET_REV[which(churn.data1.original$Class=="Churned")])
hist(x=churn.data1.original$Aggregate_ONNET_REV[which(churn.data1.original$Class=="Active")])

par(mfrow=c(1,2))
hist(x=churn.data1.original$Aggregate_OFFNET_REV[which(churn.data1.original$Class=="Churned")])
hist(x=churn.data1.original$Aggregate_OFFNET_REV[which(churn.data1.original$Class=="Active")])

par(mfrow=c(1,2))
hist(x=churn.data1.original$Aggregate_complaint_count[which(churn.data1.original$Class=="Churned")])
hist(x=churn.data1.original$Aggregate_complaint_count[which(churn.data1.original$Class=="Active")])

#Proportion churn vs non churn pour les variables numériques de Data 2
par(mfrow=c(1,2))
hist(x=churn.data2.original$account.length[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$account.length[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$area.code[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$area.code[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$number.vmail.messages[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$number.vmail.messages[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.day.minutes[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.day.minutes[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.day.calls[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.day.calls[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.day.charge[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.day.charge[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.eve.minutes[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.eve.minutes[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.eve.calls[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.eve.calls[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.eve.charge[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.eve.charge[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.night.minutes[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.night.minutes[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.night.calls[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.night.calls[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.night.charge[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.night.charge[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.intl.minutes[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.intl.minutes[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.intl.calls[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.intl.calls[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$total.intl.charge[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$total.intl.charge[which(churn.data2.original$churn=="False")])

par(mfrow=c(1,2))
hist(x=churn.data2.original$customer.service.calls[which(churn.data2.original$churn=="True")])
hist(x=churn.data2.original$customer.service.calls[which(churn.data2.original$churn=="False")])



churn.data1.original$aug_user_type <- as.numeric(as.factor(churn.data1.original$aug_user_type))
churn.data1.original$sep_user_type <- as.numeric(as.factor(churn.data1.original$sep_user_type))
churn.data1.original$aug_fav_a <- as.numeric(as.factor(churn.data1.original$aug_fav_a))
churn.data1.original$sep_fav_a <- as.numeric(as.factor(churn.data1.original$sep_fav_a))
churn.data1.original$Class <- as.numeric(as.factor(churn.data1.original$Class))

churn.data2.original$state <- as.numeric(as.factor(churn.data2.original$state))
churn.data2.original$phone.number <- as.numeric(as.factor(churn.data2.original$phone.number))
churn.data2.original$international.plan <- as.numeric(as.factor(churn.data2.original$international.plan))
churn.data2.original$voice.mail.plan <- as.numeric(as.factor(churn.data2.original$voice.mail.plan))
churn.data2.original$churn <- as.numeric(as.factor(churn.data2.original$churn))

churn.data1 <- churn.data1.original[,-14]
churn.data2 <- churn.data2.original[,-21]




# Matrices de correlation
#Matrice de DATA 1
churn.data1.cor <- cor(churn.data1)

#On peut conclure que les attributs suivants sont liés : 
#Aggregate_OFFNET_REV et Aggregate_Total_Rev
#Aggregate_Data_Rev et Aggregate_Total_Rev
#aug_user_type et sep_user_type


#Matrice de DATA 2
churn.data2.cor <- cor(churn.data2)

#On peut conclure que les attributs suivants sont liés :
#number.vmail.messages et voice.mail.plan
#total.day.charge et total.day.minutes
#total.eve.charge et total.eve.minutes
#total.night.charge et total.night.minutes
#total.intl.charge et total.intl.minutes


# 2. Arbres de décision
library(rpart)
library(lattice)
library(caret)
par(mfrow=c(1,1))

# Set a seed
set.seed(42)

# On ajoute une colonne "train" à notre dataset et on insère 1 si la ligne appartient à l'ensemble de test, 0 sinon
churn.data1.original[, "train"] <- ifelse(runif(nrow(churn.data1.original))<0.8,1,0)
churn.data2.original[, "train"] <- ifelse(runif(nrow(churn.data2.original))<0.8,1,0)

# On sépare le train et le test set
churn.data1.trainset <- churn.data1.original[churn.data1.original$train == 1,]
churn.data1.testset <- churn.data1.original[churn.data1.original$train == 0,]
churn.data2.trainset <- churn.data2.original[churn.data2.original$train == 1,]
churn.data2.testset <- churn.data2.original[churn.data2.original$train == 0,]

# On extrait le numéro de colonne correspondante à l'attribut train rajouté
churn.data1.trainColNum <- grep("train", names(churn.data1.trainset))
churn.data2.trainColNum <- grep("train", names(churn.data2.trainset))

# Puis on supprime cet attribut maintenant inutile des train/test sets
churn.data1.trainset <- churn.data1.trainset[,-churn.data1.trainColNum]
churn.data1.testset <- churn.data1.testset[,-churn.data1.trainColNum]
churn.data2.trainset <- churn.data2.trainset[,-churn.data2.trainColNum]
churn.data2.testset <- churn.data2.testset[,-churn.data2.trainColNum]

# Nous faisons le partitionnement des deux datasets
churn.data1.model <- rpart(Class~., data = churn.data1.trainset, method = "class")
churn.data2.model <- rpart(churn~., data = churn.data2.trainset, method = "class")

# Puis nous les affichons
plot(churn.data1.model)
text(churn.data1.model)
plot(churn.data2.model)
text(churn.data2.model)

# Nous remarquons que si le dataset 1 est un modèle assez simple, le dataset 2 est très complexe

# Nous réccupérons le numéro de colonne du target
churn.data1.typeColNum <- grep("Class", names(churn.data1.original))
churn.data2.typeColNum <- grep("churn", names(churn.data2.original))

# Nous faisons la prédiction en utilisant le testset et à partir des 2 modèles appris
churn.data1.predict <- predict(churn.data1.model, churn.data1.testset[,-churn.data1.typeColNum], type = "class")
churn.data2.predict <- predict(churn.data2.model, churn.data2.testset[,-churn.data2.typeColNum], type = "class")

mean(churn.data1.predict == churn.data1.testset$Class)
# Pour le modèle 1, nous avons 73% de bonnes prédictions (Vrais positifs ou vrais négatifs)

# Nous construisons la table de contingence
churn.data1.confusion <- table(pred = churn.data1.predict, true = churn.data1.testset$Class)/length(churn.data1.predict)
churn.data1.confusion.vp <- churn.data1.confusion[1,1]
churn.data1.confusion.vn <- churn.data1.confusion[2,2]
churn.data1.confusion.fp <- churn.data1.confusion[2,1]
churn.data1.confusion.fn <- churn.data1.confusion[1,2]

churn.data1.confusion.vp/(churn.data1.confusion.vp+churn.data1.confusion.fp)
# Nous avons une précision de 0.73

churn.data1.confusion.vp/(churn.data1.confusion.vp+churn.data1.confusion.vn)
# Nous avons un recall de 0.43

printcp(churn.data1.model)
# Nous constatons qu'il y'a seulement 3 attribus utilisés pour la construction du modèle qui sont :
# Aggregate_SMS_Rev, Aggregate_Total_Rev et sep_fav_a
# Le root node error nous indique quel est le pourcentage des data correctement prédis après le premier split (root)
# Nous pouvons voir la valeur 0.49, ce qui veut dire que la moitié (approximativement) des datas ont été correctement
# prédis avec seulement 1 seul attribut, ce qui veut dire que l'attribut Aggregate_Total_Rev est primordial pour la 
# prédiction
# On nous fournit aussi une table contenant les erreurs en fonction de CP. le CP (Complexity Parameter) indique
# Quel est le progrès minimum qu'on souhaite avoir afin de continuer à développer l'arbre. Plus le CP est petit
# Plus on est "exigents" en terme de précision mais plus notre arbre est complexe. Et plus le CP est grand plus on
# simplifie l'arbre au dépend d'une légère perte de précision

plotcp(churn.data1.model)
# Nous pouvons voir grâce à ce plot l'erreur en fonction de CP, comme nous le voyons plus le CP est petit plus l'erreur
# est petite. la ligne pointillée indique la valeur conseillée de CP pour faire l'équilibre entre simplicité de l'arbre
# et performance
# Ici la valeur conseillée est à peu près 0.023, nous prenons ce CP pour plutard


mean(churn.data2.predict == churn.data2.testset$churn)
# Pour le modèle 1, nous avons 93% de bonnes prédictions (Vrais positifs ou vrais négatifs)

printcp(churn.data2.model)
# Nous constatons qu'il y'a seulement 8 attribus utilisés pour la construction du modèle qui sont :
# customer.service.calls, international.plan, total.day.minutes, total.eve.minutes, total.intl.calls, total.intl.minutes
# total.night.minutes et voice.mail.plan
# Le root node error est égal à 0.13, ce qui veut dire que seulement 13% des data sont correctement prédis après 1 seul
# split, cela est parfaitement logique vu la complexité de ce deuxième dataset par rapport au premier.

plotcp(churn.data2.model)
# Ici le cp conseillé est environ 0.017, nous le prenons pour plutard

# Nous construisons la table de contingence pour le deuxième modèle
churn.data2.confusion <- table(pred = churn.data2.predict, true = churn.data2.testset$churn)/length(churn.data2.predict)
churn.data2.confusion.vp <- churn.data2.confusion[1,1]
churn.data2.confusion.vn <- churn.data2.confusion[2,2]
churn.data2.confusion.fp <- churn.data2.confusion[2,1]
churn.data2.confusion.fn <- churn.data2.confusion[1,2]

churn.data2.confusion.vp/(churn.data2.confusion.vp+churn.data2.confusion.fp)
# Nous avons une précision de 0.98

churn.data2.confusion.vp/(churn.data2.confusion.vp+churn.data2.confusion.vn)
# Nous avons un recall de 0.87


# Nous allons maintenant faire un élagage en prenant les cp "conseillés" de tout à l'heure
churn.data1.pruned_model <- prune(churn.data1.model, 0.023)
plot(churn.data1.pruned_model) 
text(churn.data1.pruned_model)

churn.data2.pruned_model <- prune(churn.data2.model, 0.017)
plot(churn.data2.pruned_model) 
text(churn.data2.pruned_model)

# Puis nous calculons la performance des deux modèles
churn.data1.pruned_model_predict <- predict(churn.data1.pruned_model, churn.data1.testset[, -churn.data1.typeColNum], type = "class")
mean(churn.data1.pruned_model_predict == churn.data1.testset$Class)

churn.data2.pruned_model_predict <- predict(churn.data2.pruned_model, churn.data2.testset[, -churn.data2.typeColNum], type = "class")
mean(churn.data2.pruned_model_predict == churn.data2.testset$churn)

# Nous remarquons que comme prévu, nous avons une perte de performance, cela est tout à fait normal puisque nous avons
# décidé de simplifier nos deux modèles. Mais cela est préférable car nous réduisons le risque d'overfitting, notre
# modèle est plus robuste face au bruit et il "généralise" mieux le concept churn sur des data qui n'ont pas déjà été vus.

# 5. Approche SVM

