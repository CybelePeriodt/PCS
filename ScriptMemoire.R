# Placement dans le desktop, là où se trouve les données 
setwd("~/Desktop")

# Chargement des packages de base :
library(labelled)
library(psych)
library(dplyr)

# Chargement de l'environnement de travail contenant les modules prêts à l'emploi 
main_file <- read.csv("~/Desktop/clean-20251201.csv")

sport_pcs_rs <- main_file %>% select(FFMQ_mean, FMI_mean, MAAS_mean, CAMS_mean, MAQ_mean, SMQ_mean, RS_mean, S1)
print(sport_pcs_rs)

# Création du data.frame avec les variables d'intérêt pour les régressions
data_variables <- data.frame(
  s1 = sport_pcs_rs$S1,
  ffmq_mean = sport_pcs_rs$FFMQ_mean,
  fmi_mean = sport_pcs_rs$FMI_mean,
  maas_mean = sport_pcs_rs$MAAS_mean,
  cams_mean = sport_pcs_rs$CAMS_mean,
  maq_mean = sport_pcs_rs$MAQ_mean,
  smq_mean = sport_pcs_rs$SMQ_mean,
  rs_mean = sport_pcs_rs$RS_mean
) 

## 3) Analyses statistiques 

# a) Analyse de médiation selon le modèle de Baron & Kenny 

#1) Effet c (sport sur rs)
total <- lm(rs_mean ~ s1, data = data_variables)
summary(total)

#2) Effet a (sport sur pcs)
a_ffmq <- lm(ffmq_mean ~ s1, data = data_variables)
summary(a_ffmq)

a_fmi <- lm(fmi_mean ~ s1, data = data_variables)
summary(a_fmi)

a_maas <- lm(maas_mean ~ s1, data = data_variables)
summary(a_maas)

a_cams <- lm(cams_mean ~ s1, data = data_variables)
summary(a_cams)

a_maq <- lm(maq_mean ~ s1, data = data_variables)
summary(a_maq)

a_smq <- lm(smq_mean ~ s1, data = data_variables)
summary(a_smq)


#3.1) Effet b (pcs sur rs) et c' (sport + pcs sur rs)
cprime_ffmq <- lm(rs_mean ~ s1 + ffmq_mean, data = data_variables)
summary(total_ffmq)

cprime_fmi <- lm(rs_mean ~ s1 + fmi_mean, data = data_variables)
summary(total_fmi)

cprime_maas <- lm(rs_mean ~ s1 + maas_mean, data = data_variables)
summary(total_maas)

cprime_cams <- lm(rs_mean ~ s1 + cams_mean, data = data_variables)
summary(total_cams)

cprime_maq <- lm(rs_mean ~ s1 + maq_mean, data = data_variables)
summary(total_maq)

cprime_smq <- lm(rs_mean ~ s1 + smq_mean, data = data_variables)
summary(total_smq)

#3.2) Création des coefficients des effets c et c' et calcul de la réduction de l'effet c 

#Création des coefficients

#Dans un premier temps, je crée un coefficient pour l'effet c
coef_total_effect <- coef(total)["s1"]

#Idem pour l'effet c'
c_prime_ffmq <- coef(cprime_ffmq)["s1"]
c_prime_fmi <- coef(cprime_fmi)["s1"]
c_prime_maas <- coef(cprime_maas)["s1"]
c_prime_cams <- coef(cprime_cams)["s1"]
c_prime_maq <- coef(cprime_maq)["s1"]
c_prime_smq <- coef(cprime_smq)["s1"]

#Enfin, je peux calculer la réduction (c - c')
reduction_ffmq <- coef_total_effect - c_prime_ffmq
reduction_fmi <- coef_total_effect - c_prime_fmi
reduction_maas <- coef_total_effect - c_prime_maas
reduction_cams <- coef_total_effect - c_prime_cams
reduction_maq <- coef_total_effect - c_prime_maq
reduction_smq <- coef_total_effect - c_prime_smq

#J'affiche mes valeurs
print(reduction_ffmq)
print(reduction_fmi)
print(reduction_maas)
print(reduction_cams)
print(reduction_maq)
print(reduction_smq)

# c) Pour plus de lisibilité : estimations standardisées et création d'un tableau

# Chargement du package parameters pour effectuer les régressions (+fonction if pour l'installer si besoin)
if (!require(parameters)) install.packages("parameters")
library(parameters)

# Retrait des variables de type haven_labelled sinon message d'erreur car non supportée par la fonction tab_model
data_variables[] <- lapply(data_variables, function(x) if (inherits(x, "haven_labelled")) zap_labels(x) else x)

# Obtenir les estimations standardisées : 

#pour H1
parameters(total, standardize = "basic")

#pour H2
parameters(a_ffmq, standardize = "basic")
parameters(a_fmi, standardize = "basic")
parameters(a_maas, standardize = "basic")
parameters(a_cams, standardize = "basic")
parameters(a_maq, standardize = "basic")
parameters(a_smq, standardize = "basic")

#pour H3 et H4
parameters(cprime_ffmq, standardize = "basic")
parameters(cprime_fmi, standardize = "basic")
parameters(cprime_maas, standardize = "basic")
parameters(cprime_cams, standardize = "basic")
parameters(cprime_maq, standardize = "basic")
parameters(cprime_smq, standardize = "basic")

# Tableau : Chargement du package sjPlot (+fonction if pour l'installer si besoin) pour l'affichage du tableau
if (!require(sjPlot)) install.packages("sjPlot")
library(sjPlot)

#pour H1 :
tab_model(total, file = "total.html")

#pour H2 : 
tab_model(a_ffmq)
tab_model(a_fmi)
tab_model(a_maas)
tab_model(a_cams)
tab_model(a_maq)
tab_model(a_smq)

#pour H3 et H4 : 
tab_model(direct_ffmq)
tab_model(direct_fmi)
tab_model(direct_maas)
tab_model(direct_cams)
tab_model(direct_maq)
tab_model(direct_smq)

# Sauvegarde dans un fichier png en passant par du html (solution trouvée suite à fichier pdf corrompu lors de son ouverture)

#pour H1
tab_model(total, file = "total.html")

#pour H2
tab_model(a_ffmq, file = "H2ffmq.html")
tab_model(a_fmi, file = "H2fmi.html")
tab_model(a_maas, file = "H2maas.html")
tab_model(a_cams, file = "H2cams.html")
tab_model(a_maq, file = "H2maq.html")
tab_model(a_smq, file = "H2smq.html")

#pour H3 et H4
tab_model(direct_ffmq, file="model_ffmq.html")
tab_model(direct_fmi, file="model_fmi.html")
tab_model(direct_maas, file="model_maas.html")
tab_model(direct_cams, file="model_cams.html")
tab_model(direct_maq, file="model_maq.html")
tab_model(direct_smq, file="model_smq.html")

# Chargement de la librairie webshot (+fonction if pour l'installer si besoin)
if (!require(webshot)) install.packages("webshot")
library(webshot)

# Exécution du PhantomJS avant de lancer la fonction webshot, sinon message d'erreur
install_phantomjs() 

# Conversion du fichier html en .png 

#pour H1
webshot("total.html", "total.png")

#pour H2 : 
webshot("H2ffmq.html", "H2ffmq.png")
webshot("H2fmi.html", "H2fmi.png")
webshot("H2maas.html", "H2maas.png")
webshot("H2cams.html", "H2cams.png")
webshot("H2maq.html", "H2maq.png")
webshot("H2smq.html", "H2smq.png")

#pour H3 : 
webshot("model_ffmq.html","model_ffmq.png")
webshot("model_fmi.html","model_fmi.png")
webshot("model_maas.html","model_maas.png")
webshot("model_cams.html","model_cams.png")
webshot("model_maq.html","model_maq.png")
webshot("model_smq.html","model_smq.png")

######### FIN DU SCRIPT ######### 




