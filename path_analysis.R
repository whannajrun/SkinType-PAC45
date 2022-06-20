# import lib
library(readxl)
library(lavaan) #path analysis lib

# import data
data = read_excel(path = "D:/dataset/skin_type_dataset2.xlsx",
                  col_names = TRUE)
# head(data)

#model specs
# mod.id = '
# status =~ Jenis_Kelamin + Umur
# keadaan =~ Keadaan_Disentuh + Keadaan_Pagi + Keadaan_DgnPelembab
# Tipe_Kulit ~ status + keadaan '

#cara 2: minus RMSE & p value
mod.id = '
Tipe_Kulit ~ Jenis_Kelamin + Umur + Keadaan_Disentuh + Keadaan_Pagi + Keadaan_DgnPelembab + Keadaan_Siang + Produkbaru_Efeksamping + Produkbaru_Breakout + Komedo_Jerawat + Pori2_Besar
'

#model estimation and identification
mod.est = sem(
  model = mod.id,
  data = data
)

summary(mod.est)
summary(mod.est,
        fit.measures = TRUE)

#path diagram
library(semPlot)
semPaths(
  object = mod.est,
  what = "path",
  whatLabels = "par",
  style = "ram",
  layout = "tree",
  sizeMan = 6,
  sizeLat = 5,
  color = "lightgray",
  edge.label.cex = 1,
  label.cex = 1.3
)