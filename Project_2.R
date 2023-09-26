library(ggplot2)
library(GGally)

wine = read.table(file = "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",  
                  sep = ",", head=F)
colnames(wine) = c("class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium", 
                   "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins", 
                   "Color_intensity", "Hue", "OD280/OD315_of_diluted wines", "Proline")
wine = wine[which(wine$class!=1),c(1,5,11)]
wine$class=as.factor(wine$class-2)
colnames(wine)=c("y","x1","x2")
ggpairs(wine, ggplot2::aes(color=y)) + theme_bw(18)