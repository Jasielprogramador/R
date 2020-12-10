#1.praktika
#-------------------------------------------------------------------------

2+2
2/6^3
6*8#Biderkaketa
a<-6
sqrt(a)
f<-function(x)x^2-9
plot(f,-5,5,col="dark green")
pisua<-c(45,68,47,35,59)
altuera<-scan()
5
8
7
8
4
seq(1,50)
rep(78,62)
((1+2)/(3+4))^2
sqrt(exp(2)+log2(3))
x<-seq(1,21)
prod(x)#21 elementuen biderketa
x=c(20.5,12.6,-23,-6.98,24,32.8,7,-8.6)
F=c(3,4,2,6,5,7,4,9)
f<-F/cumsum(F)
f
sum(x*F)
sum(x*f)
sum(x^2*f)

#3.Ariketa

kilometroak<-scan()
31422
31801
32131
32691
33077
33514
33992

km=c(31422,31801,32131,32691,33077,33514,33992)
diff(km)
#diff komandoak bektorearen barneko kenketa egiten du (31801-31422=379)
mean(km)#batazbesteko aritmetikoa
mean(diff(km)) #diferentzia horren batazbestekoa

P<-r$Pisua#Eskuragarri izateko rko pisua (data frame)

attach (r) #bektore guztiak eskuragarri (data frame)

#4. Ariketa
m<-seq(0,1.2,length=20)
m
length (m)
max(m)
min(m)
range(m)
m[10]=1.2
m
y<-exp(m)
y
plot(m,y, main="Funtzio esponentziala")
hist (m)
#header=T zutabea lehenengoa ekiditzeko




#Repaso 
#1.ariketa 
((1+2)/(3+4))^2

#2.ariketa
x=c(20.5,12.6,-23,-6.98,24,32.8,7,-8.6)
F=c(3,4,2,6,5,7,4,9)
f=F/cumsum(F)
sum(x*F)
sum(x*f)
sum(x^2*f)


#3.ariketa
km<-c(31422, 31801, 32131, 32691, 33077, 33514,33992)
diff(km)
mean(km)
mean(diff(km))

#----------------------------------------------------------------


#2.praktika

#----------------------------------------------------------------


#1. Ariketa
arrailak<-c(1,2,3,3,2,1,2,5,2,4,4,4,5,3,2,5,3,4,1,4,2,3,1,1,2,5,3,4,1,3)
table(arrailak)
taula<-as.data.frame(table(arrailak))
taula
Maiztasun.abs<-taula$Freq
taula
sum (Maiztasun.abs)
Maiztasun.er<-Maiztasun.abs/30
Met.Maiztasun.abs<-cumsum (Maiztasun.abs)
Met.Maiztasun.er<-cumsum(Maiztasun.er)
data.frame (arrailak,Maiztasun.abs,Met.Maiztasun.abs,Maiztasun.er,Met.Maiztasun.er)
barplot(table(arrailak))
pie(table(arrailak))
pie(table(arrailak),labels=c("oso mehea(1)", "mehea(2)","ertaina(3)","lodia(4)","oso lodia(5)"))

#2. Ariketa

barra<-c(33,21,32,44,35,22,40,36,22,37,20,37,42,31,23,44,32,30,44,44,42,35,40,36,32,31,37,43,24,40,25,30,26,35,33,41,25,44,36,27)
range(barra)
limiteak<-c(20,24,28,32,36,40,44)
barra.tarte<-cut(barra,limiteak,right=F,include.lowest =T )
barra.tarte
table(barra.tarte)
taula<-as.data.frame(table(barra.tarte))
taula
Tarteak<-taula$barra.tarte
Maiztasun.abs<-taula$Freq
sum(Maiztasun.abs)
Maiztasun.erl<-Maiztasun.abs/40
Met.Maiztasun.abs<-cumsum(Maiztasun.abs)
Met.Maiztasun.erl<-cumsum(Maiztasun.erl)
data.frame(Tarteak,Maiztasun.abs,Met.Maiztasun.abs,Maiztasun.erl,Met.Maiztasun.erl)
Mode <- function(barra) {
  ubarra <- unique(barra)
  ubarra[which.max(tabulate(match(barra, ubarra)))]
}
Mode(barra)
hist(barra,breaks=limiteak,right=F,include.lowest = T,col="orange")
x<-c(22,26,30,34,38,42)
plot(x,Maiztasun.abs,type="b",main="Maiztasun absolutuen poligonoa")
summary(barra)
#c)

Batezbestekoa<-mean(barra)
Batezbestekoa
y<-table(barra)
Moda<-names(y)[which(y==max(y))]#Posizio hortako izena izango da Moda
Moda
Mediana<-median (barra)
Mediana
Bariantza<-var(barra)*39/40
Bariantza
des.tip<-sqrt(Bariantza)
des.tip



#Errepasoa
#1.ariketa
arrailak<-c(0, 2, 3, 2, 4, 1, 2, 3, 3, 0, 2, 6, 2, 1, 2, 3, 1, 2, 3, 1, 2, 7, 2, 1, 4, 2, 3, 3, 1, 0)
taula<-as.data.frame(table(arrailak))
Maiztasun.abs<-taula$Freq
taula
sum (Maiztasun.abs)
Maiztasun.er<-Maiztasun.abs/30
Met.Maiztasun.abs<-cumsum (Maiztasun.abs)
Met.Maiztasun.er<-cumsum(Maiztasun.er)
data.frame (arrailak,Maiztasun.abs,Met.Maiztasun.abs,Maiztasun.er,Met.Maiztasun.er)
barplot(table(arrailak))
pie(table(arrailak))
pie(table(arrailak),labels=c("oso mehea(1)", "mehea(2)","ertaina(3)","lodia(4)","oso lodia(5)"))


mean(arrailak)


#-------------------------------------------------------

#3.praktika


#------------------------------------------------------------------
library(moments)
pearson1<- 3*(mean(datuak)-median(datuak))/des.tip
pearson1
semeak<-c(0,2,3,2,4,1,2,3,3,0,2,6,2,1,2,3,1,2,3,1,2,7,2,1,4,2,3,3,1,0)
table(semeak)
a<-as.data.frame(table(semeak))
a
Modalitatea<-a$semeak
Maiztasun.abs<-a$Freq
sum(Maiztasun.abs)
Maiztasun.erl<-Maiztasun.abs/30
Met.maiztasun.abs<-cumsum(Maiztasun.abs)
Met.maiztasun.erl<-cumsum(Maiztasun.erl)
data.frame(Modalitatea,Maiztasun.abs,Met.maiztasun.abs,Maiztasun.erl,Met.maiztasun.erl)
barplot(table(semeak))
plot(Modalitatea,Met.maiztasun.abs)
summary(semeak)
mean(semeak)
median(semeak)
table(semeak)#Moda 2 da, maiztasun absolutu handiena duelako.
heina<-max(semeak)-min(semeak)
heina
Q1<-quantile(semeak,0.25,type=2)
Q3<-quantile(semeak,0.75,type=2)
kuartilartekoheina<-Q3-Q1
kuartilartekoheina
IQR(semeak)#RIQ
var(semeak)
bariantza<-var(semeak)*(30-1)/30
bariantza
des.tip<-sqrt(bariantza)
des.tip
aldak.koef<-des.tip/mean(semeak)
aldak.koef
quantile(semeak,0.5,type=2)
quantile(semeak,0.1,type=2)
quantile(semeak,0.4,type=2)
quantile(semeak,0.9,type=2)
quantile(semeak,0.3,type=2)
quantile(semeak,0.85,type=2)

#Alborapena
skewness(semeak)#Eskuinerantz alboratua
kurtosis(semeak)-3#Banaketa leptokurtikoa

#Kutxa diagrama
boxplot(semeak,horizontal = T, col="light green")
boxplot.stats(semeak)#7 balio arraroa da.


#2. Ariketa

txakurrak<-c(2,2,3,3,3,3,4,4,4,5,5,5,5,8)
elefanteak<-c(3500,3500,4000,4000,4000,4000,4500,4500,4500,5000,5000,5000,5000,5500,5500)
mean(txakurrak)
mean(elefanteak)
length(txakurrak)
length(elefanteak)
bariantza<-var(txakurrak)*(14-1)/14
bariantza
des.tip<-sqrt(bariantza)
des.tip
bariantza1<-var(elefanteak)*(15-1)/15
bariantza1
des.tip1<-sqrt(bariantza)
des.tip1


#Erlatibokoki kalkulua
Zx<-(4000-mean(elefanteak))/(des.tip1)
Zx
Zy<-(3-mean(txakurrak))/des.tip
Zy
#Zy>Zx,orduan, 3kg-ko txakurrak pisatzen du erlatiboki gehiago.
CV1<-des.tip/mean(txakurrak)
CV1
CV2<-des.tip/mean(elefanteak)
CV2
#Batezbesteko adierazgarriagoa.
#Aldakuntza koefizienteak kalkulatuko ditugu.
#Beraz, elefanteen batezbestekoa da adierazgarriagoa eta sakabanaketa txikiagoa, aldakuntza koefizientea txikiagoa delako.


#2. Ariketa

#Beste era eraginkor batean.
txakur.pisuak<-c(2,3,4,5,8)
maiztasun.abs<-c(2,4,3,4,1)
txakur.pisu.guztiak<-rep(txakur.pisuak,maiztasun.abs)


#7. Ariketa

#a)

Zuntzak<-read.table("Zuntzak.txt",header=T)
Zuntzak

#b)
Luzera<-Zuntzak$Luzera_.mm
Luzera
N<-length(Luzera)
N
Diametroa<-Zuntzak$Diametroa_.mm
Diametroa
N1<-length(Diametroa)
N1
range(Luzera)
heina<-max(Luzera)-min(Luzera)
heina
Tartearen_zabalera<-heina/4
Tartearen_zabalera#15-ko zabalera
limiteak<-c(20.48,35.48,50.48,65.48,80.48)
taula.tarte<-cut(Luzera,limiteak,right=F,include.lowest = T)
taula.tarte
table(taula.tarte)
taula<-as.data.frame(table(taula.tarte))
taula
Tarteak<-taula$taula.tarte
Maiztasun.abs<-taula$Freq
Tarteak
Maiztasun.abs
taula
sum (Maiztasun.abs)
Maiztasun.er<-Maiztasun.abs/N
Met.Maiztasun.abs<-cumsum (Maiztasun.abs)
Met.Maiztasun.er<-cumsum(Maiztasun.er)
data.frame (Tarteak,Maiztasun.abs,Met.Maiztasun.abs,Maiztasun.er,Met.Maiztasun.er)
hist(Luzera,main="Luzerak Histograma", xlab="luzerak (cm)",ylab="Dentsitatea", breaks=c(20.48,35.48,50.48,65.48,80.48), col="orange")

#c)

mean(Diametroa)
bariantza<-var(Diametroa)*(N-1)/N
bariantza

#d)

quantile(Diametroa,0.88,type=2)

#e)

#Pi.r^2.l

Erradioa<-Diametroa/2
Erradioa
Masa<-(3.1416*Erradioa^2*Luzera*0.74)
Masa
#...


#Errepasoa

library(moments)
arrailak<-c(0, 2, 3, 2, 4, 1, 2, 3, 3, 0, 2, 6, 2, 1, 2, 3, 1, 2, 3, 1, 2, 7, 2, 1, 4, 2, 3, 3, 1, 0)
a<-as.data.frame(table(arrailak))
Modalitatea<-a$arrailak
Maiztasun.abs<-a$Freq
sum(Maiztasun.abs)
Maiztasun.erl<-Maiztasun.abs/30
Met.maiztasun.abs<-cumsum(Maiztasun.abs)
Met.maiztasun.erl<-cumsum(Maiztasun.erl)
data.frame(Modalitatea,Maiztasun.abs,Met.maiztasun.abs,Maiztasun.erl,Met.maiztasun.erl)
barplot(table(arrailak))
plot(Modalitatea,Met.maiztasun.abs)
mean(arrailak)
median(arrailak)
table(arrailak) #Moda 2 da maiztasun handiena baitu
heina<-max(arrailak)-min(arrailak)
heina
Q1<-quantile(arrailak,0.25,type=2)
Q3<-quantile(arrailak,0.75,type=2)
RIC<-Q3-Q1

  
#--------------------------------------------------------------------
