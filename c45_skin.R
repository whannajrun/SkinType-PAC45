# library(dplyr) #normalization
library(readxl) # read file xlsx
library(RWeka) # lib for ID3, c45, c50 wth. gain support
library(partykit) # work with rweka
library(lattice)
library(ggplot2)
library(caret)

#read data
df_skin <- read_excel("C:/skin_type_dataset2_b.xlsx")

#making dataset
df_skin2 <- data.frame(
  Nama = as.character(df_skin$Nama),
  Jenis_Kelamin = as.factor(df_skin$Jenis_Kelamin),
  Umur = as.factor(df_skin$Umur),
  Keadaan_Disentuh = as.factor(df_skin$Keadaan_Disentuh),
  Keadaan_Pagi = as.factor(df_skin$Keadaan_Pagi),
  Keadaan_DgnPelembab = as.factor(df_skin$Keadaan_DgnPelembab),
  Keadaan_Siang = as.factor(df_skin$Keadaan_Siang),
  Produkbaru_Efeksamping= as.factor(df_skin$Produkbaru_Efeksamping),
  Produkbaru_Breakout = as.factor(df_skin$Produkbaru_Breakout),
  Komedo_Jerawat = as.factor(df_skin$Komedo_Jerawat),
  Pori2_Besar = as.factor(df_skin$Pori2_Besar),
  Tipe_Kulit = as.factor(df_skin$Tipe_Kulit)
)

#length(df_skin2)
summary(df_skin2)
#head(df_skin2)

split_data = sort(sample(nrow(df_skin2), nrow(df_skin2)*.8))
split_datalatih <-df_skin2[split_data,]
split_datauji <-df_skin2[-split_data,]

#making formula
formula <- Tipe_Kulit ~ Jenis_Kelamin + Umur + Keadaan_Disentuh + Keadaan_Pagi +
  Keadaan_DgnPelembab +  Keadaan_Siang + Produkbaru_Efeksamping +
  Produkbaru_Breakout + Komedo_Jerawat + Pori2_Besar

#run the function
C45_skin <- J48(formula, data = split_datalatih)

#seeing the fuzzy rule
print(C45_skin)
summary(C45_skin)

#making plot DT
plot(C45_skin,type="simple")

predictions <- predict(C45_skin, split_datauji)
cm <- table(predictions, split_datauji$Tipe_Kulit)
cm

cm2 <- confusionMatrix(predictions, reference = split_datauji$Tipe_Kulit)
cm2