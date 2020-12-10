datuak = c( 1, 1, 1, 2, 3, 3, 1, 2, 2, 1, 3, 1, 1 )

# paketea kargatu:
library(moments)
# fisher-en alborapen kof kalkulaltu
skewness(datuak)

#Pearson-en alborapen koefizientea:
pearson<-(mean(datuak)-1)/des.tip

#Kurtosia kalkulatzeko
kurtosis(datuak)-3

#KUTXA DIAGRAMA:
datuak = c(115, 232, 181, 161, 155, 137, 165, 171, 139, 130, 406)
boxplot(datuak)
boxplot(datuak, horizontal=T, col="gold")

boxplot.stats(datuak)  #kutxa irudikatzean ematen dizkigun estatistikoak lortzeko

