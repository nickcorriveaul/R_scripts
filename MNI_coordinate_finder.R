install_github("yunshiuan/label4MRI",subdir = "label4MRI")

library(devtools)
library(mni2aal)

read.table("PLS_Janvier2019_LV3.csv")
data = read.table("PLS_Janvier2019_LV3.csv", sep = ",", header = TRUE)
data = read.table("PLS_Janvier2019_LV3.csv", sep = ",", h = T)

t(mapply(FUN=mni_to_region_name,x=data$mni_x,y=data$mni_y,z=data$mni_z))
View(Result)
