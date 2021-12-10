library(tidyverse)
library(magrittr)
library(neuralnet)
library(caret)
set.seed(5)

# Load des donn�es modifi�es scal�es, fichier � modif
df_act_scaled <- read_csv("./scaled_dude_erk2_mk01_Descriptors.csv")

# 1 : 50/50
# 2 : 75/25
# 3 : toutes les donn�es
list_cases <- vector(mode = "list", length = 3)

# 3
list_cases[[3]] <- df_act_scaled

# 1
df_act_equi <- df_act_scaled %>%
  group_by(is_active) %>%
  slice_head(n = 79)

list_cases[[1]] <- df_act_equi

# 2
df_act_25 <- df_act_scaled %>%
  filter(is_active == 1)
df_act_75 <- df_act_scaled %>%
  filter(is_active == 0) %>%
  slice_sample(n = 3 * nrow(df_act_25))
df_act_25_75 <- rbind(df_act_25, df_act_75)

list_cases[[2]] <- df_act_25_75

# Cr�ation des �chantillons d'apprentissage et test

#vecteur pour les noms 

listenom <- c("cas1","cas2","cas3")

# boucle qui va creer pop_(app,val et test) de chaque population et enlever id et X1

for (i in 1:3) {
  nomapp <- paste(listenom[i],"app",sep="_")
  nomval <- paste(listenom[i],"val",sep="_")
  nomtest <- paste(listenom[i],"test",sep="_")
  ref <- seq(1:nrow(list_cases[[i]]))
  aleat1 <- sample(ref, nrow(list_cases[[i]])*1/3) 
  ref <- ref[-aleat1]
  aleat2 <- sample(ref,nrow(list_cases[[i]])*1/3)
  aleat3 <- setdiff(ref,aleat2)
  assign(nomapp,list_cases[[i]][3:9][aleat1,])
  assign(nomval,list_cases[[i]][3:9][aleat2,])
  assign(nomtest,list_cases[[i]][3:9][aleat3,])
}

listapp <- list(cas1_app,cas2_app,cas3_app)
listval <- list(cas1_val,cas2_val,cas3_val)
listtest <- list(cas1_test,cas2_test,cas3_test)

# boucle pour les r�seau de neurone sur app (�a prend du temps)

# Liste contenant les vecteur de topologie
listopos <- list(c(2),c(5,3),c(6),c(2,2),c(6,4,2))

for (i in 1:3) {
  for (j in 1:5){
    nomtopo <- paste("topos",j,sep="")
    nomres <- paste("res.app",listenom[i],nomtopo,sep="_")
    assign(nomres,neuralnet(is_active~SlogP+NumLipinskiHBA +NumLipinskiHBD +NumRotatableBonds + NumRings,data=listapp[[i]],hidden= listopos[[j]],linear.output=T,threshold = 0.01,stepmax=7e4))
  }
}




#list contenant tous les r�seau selon la population �tudi�
rescas1 <- list(res.app_cas1_topos1,res.app_cas1_topos2,res.app_cas1_topos3,res.app_cas1_topos4)
rescas2 <- list(res.app_cas2_topos1,res.app_cas2_topos2,res.app_cas2_topos3,res.app_cas2_topos4)
rescas3 <- list(res.app_cas3_topos1,res.app_cas3_topos2,res.app_cas3_topos3,res.app_cas3_topos4)

reseau <- list(rescas1,rescas2,rescas3)

# boucle pour la validation  une liste contenant les 3 cas qui contiennent eux m�me les 4 topos

for (i in 1:3) {
  for (j in 1:4) {
    nomvalres <- paste("resultcas",i,"topos",j,sep="")
    nomtopo <- paste("topos",j,sep="")
    nomresult <- paste("result.val",listenom[i],nomtopo,sep="_")
    assign(nomresult, predict(reseau[[i]][[j]], listval[[i]]))
  }
}

# liste de validation pour chaque cas
result_cas1 <- list(result.val_cas1_topos1,result.val_cas1_topos2,result.val_cas1_topos3,result.val_cas1_topos4)
result_cas2 <- list(result.val_cas2_topos1,result.val_cas2_topos2,result.val_cas2_topos3,result.val_cas2_topos4)
result_cas3 <- list(result.val_cas3_topos1,result.val_cas3_topos2,result.val_cas3_topos3,result.val_cas3_topos4)

#  une liste contenant les 3 cas qui contiennent eux m�me les 5 topos
listresult <- list(result_cas1,result_cas2,result_cas3)




#-------------Table de confusion & MCC-------------#
MCC <- function(cm) {
  MCC <- (cm[1,1]*cm[2,2]-cm[1,2]*cm[2,1])/
    (sqrt((cm[1,1]+cm[1,2])*(cm[1,1]+cm[2,1])*(cm[2,2]+cm[1,2])*(cm[2,2]+cm[2,1])))
  return (MCC)
}


listBestTopologie <- c(0, 0, 0)
listmaxAccurracy <- c(0, 0, 0)
listSensitivity <- c(0, 0, 0)
listSpecificity <- c(0, 0, 0)
listMCC <- c(0, 0, 0)

for (i in 1:length(listresult)) {
  for(j in 1:length(listresult[[i]])) {
    valid <- listval[[i]]$is_active
    predi <- as.vector(reseau[[i]][[j]]$response)
    # Condition v�rififiant s'il existe une diff�rence du nombre de facteurs
    if (length(levels(as.factor(valid))) == length(levels(as.factor(predi)))) {
      cm <- confusionMatrix(
        as.factor(valid),
        as.factor(predi))
      acc <- cm$overall[[1]]
      sens <- cm$byClass[1]
      spe <- cm$byClass[2]
      # Condition verifiant le meilleur reseau
      # specificite et sensitive sont interd�pendants donc une evaluation suffit
      if (listmaxAccurracy[i] <= acc && sens >= listSensitivity[i]) {
        print(cm$table)
        listmaxAccurracy[i] <- acc
        listBestTopologie[i] <- j
        listSensitivity[i] <- sens
        listSpecificity[i] <- spe
        listMCC[i] <- MCC(cm$table)
        
        # Visualisation avec ggplot
        dfcm <- as.data.frame(cm$table)
        namesCM <- paste("CMplot", i, sep="_")
        assign(namesCM, ggplot(data = dfcm,
               mapping = aes(x = Reference,
                             y = Prediction)) +
          geom_tile(aes(fill = Freq)) +
          geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
          scale_fill_gradient(low = "green",
                              high = "red",
                              trans = "log"))
      }
    }
  }
}
print(listmaxAccurracy)
print(listBestTopologie)
print(listSensitivity)
print(listSpecificity)
print(listMCC)# Semblerai que le 2 eme (cas2) reseau soit meilleur meme si on s'eloigne de 1
CMplot_1
CMplot_2
CMplot_3


#-------------ROC-------------#
if(!require(plotROC)) install.packages("plotROC")
ROCplots <- c(NULL, NULL, NULL)

for (i in 1:3) {
  j = listBestTopologie[i]
  val = listval[[i]]$is_active
  pred = reseau[[i]][[j]]$net.result[[1]]
  
  df1 <- data.frame(valide = val , predic = pred, 
                    stringsAsFactors = FALSE)
  namesROC <- paste("ROCplot", i, sep="_")
  assign(namesROC,  ggplot(df1, aes(d = valide, m = predic)) + geom_roc())
}
ROCplot_1
ROCplot_2
ROCplot_3


