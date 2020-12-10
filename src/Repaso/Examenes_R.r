#Azterketako ariketak R

#2.praktikakoak

#1.ariketa
torloju.lodiera <- c(1,2,3,3,2,1,2,5,2,4,4,4,5,3,2,5,3,4,1,4,2,3,1,1,2,5,3,4,1,3)

a<-as.data.frame(table(torloju.lodiera))
a

Tarteak<-a$torloju.lodiera
Tarteak
Maiztasun.abs<-a$Freq
Maiztasun.erl<-Maiztasun.abs/30
Met.Maiztasun_erl<-cumsum(Maiztasun.erl)
Met.Maiztasun_abs<-cumsum(Maiztasun.abs)

data.frame(Tarteak,Maiztasun.abs,Met.Maiztasun_abs,Maiztasun_erl,Met.Maiztasun_erl)

barplot(table(torloju.lodiera))

pie(table(torloju.lodiera), names.arg = c("Oso mehea", "Mehea", "Ertaina", "Lodia", "Oso lodia"))

#2.ariketa
df = read.table("Hauste_tentsioa.txt", header = T)

#Fitxategiko komak puntuekin renplazatu
Hauste_tentsioa.Tn.cm2.Ald = gsub(",",".",df$Hauste_tentsioa.Tn.cm2.)
df = data.frame(Hauste_tentsioa.Tn.cm2.Ald)
df
stem(df,scale=2)

lim <- seq(from = min(df$Hauste_tentsioa.Tn.cm2.Ald),by=0.2 ,to = max(df$Hauste_tentsioa.Tn.cm2.Ald))
lim = c(lim,lim[length(lim)]+0.2)


hist(lim)

a<-as.data.frame(table(df))
a

Tarteak<-a$df
Tarteak
Maiztasun.abs<-a$Freq
Maiztasun.erl<-Maiztasun.abs/50
Met.Maiztasun_erl<-cumsum(Maiztasun.erl)
Met.Maiztasun_abs<-cumsum(Maiztasun.abs)

data.frame(Tarteak,Maiztasun.abs,Met.Maiztasun_abs,Maiztasun_erl,Met.Maiztasun_erl)
           
#Joera zentraleko neurriak
datuak = df$Hauste_tentsioa.Tn.cm2.Ald
datuak
mean(table(datuak))
median(table(datuak))


tarteak<-c(3.0,3.5,4.0,4.5,5.0,5.5,6.0)
hi<-c(0.08,0.152,0.224,0.304,0.144,0.096)
taula<-cut()

lim <- seq(from = 3.0, to=6.0, by=0.5 )


maiztasun<-c(0.08,0.15,0.22,0.3,0.144,0.096)
tarteak<-cut(maiztasun,lim,right=F)
tarteak
a<-data.frame(table(tarteak))
a
a$Freq=hi


#4.praktikakoak

#10.ariketa

#X="Bateko, errege txanka eta desberdina den edozein karta ateratzen diren kopurua)
X-B(200,4/40)
#P(X=5)
dbinom(5,200,4/40)

#P(X>5)
1-pbinom(4,200,4/40)


#11.ariketa 
120+145+200
#X="Hartzen diren aluminiozkoa hodi kopurua" X-B(465,120/465)
#Y="Hartzen diren kobrezko hodi kopurua" X-B(465,145/465)
#Z="Hartzen diren PVC-ko hodi kopurua" X-B(465,200/465)

#P(X=9)
dbinom(9,465,120/465)
#P(Y>=4)
1-pbinom(3,465,145/465)
#P(3<=Z<=7) P(Z<=7)-P(Z<=3)

pbinom(7,465,200/465)-pbinom(3,465,200/465)


#1.ariketa
#X="Urtean aholkularitza enpresak aholkua ematen dien pertsona kopurua"
X-P(1200)

#P(X>1085) 1-P(X<=1085)
ppois(1085,1200,lower.tail = FALSE)
1-ppois(1085,1200)

#P(1200<=X<=1300) P(X<=1300)-P(X<=1200)

ppois(1300,1200)-ppois(1200,1200)

#2.ariketa

#X="Lortu diren puntu kopurua" X ~ N(60,10)
#1.modua
#P(X>=70)
1-pnorm(69,60,10)

#P(39<=X<=80) P(X<=80)-P(X<=39)
pnorm(80,60,10)-pnorm(39,60,10)

#P(X-60<=20) P(60-20<=X<=60+20) P(40<=X<=80) P(X<=80)-P(X<=40)
pnorm(80,60,10)-pnorm(40,60,10)

#P(X-60>=20) P(60-20<=X>=60+20) P(40<=X>=80) P(X>=80)+P(X<=40)
pnorm(80,60,10,lower.tail = F)+pnorm(40,60,10)

#P(X>=70)
pnorm(70,60,10,lower.tail = F)*200
32

#2.modua
#P(X>=70)
1-pnorm((69-60)/10,0,1)

#P(39<=X<=80) P(X<=80)-P(X<=39)
pnorm((80-60)/10,0,1)-pnorm((39-60)/10,0,1)

#P(X-60<=20) P(60-20<=X<=60+20) P(40<=X<=80) P(X<=80)-P(X<=40)
pnorm((80-60)/10,0,1)-pnorm((40-60)/10,0,1)

#P(X-60>=20) P(60-20<=X>=60+20) P(40<=X>=80) P(X>=80)+P(X<=40)
pnorm((80-60)/10,0,1,lower.tail = F)+pnorm((40-60)/10,0,1)

#P(X>=70)
pnorm((70-60)/10,0,1,lower.tail = F)*200
32


#3.ariketa
#landa = 1/beta
#X="1. tresna elektronikoaren bizi-iraupena" X~E(1/40)
#Y="2. tresna elektronikoaren bizi-iraupena" Y~E(1/45)


#P(X>=45) 1-P(X<=44)
1-pexp(44,1/40)

#P(Y>=45) 1-P(Y<=44)
1-pexp(44,1/45)

#2.tresna elektronikoa aukeratuko nuke

curve(pexp(x,1/40),from = 0 ,to = 500)


#4.ariketa
#X="Sisteman funtzionatzen duten osagai kopurua" X~B(9,0.95)

#P(X>=6) 1-P(X<=5)
1-pbinom(5,9,0.95)

osagai<-0:9
plot(osagai,pbinom(osagai,9,0.95),type = "h", ylab = "p(x)")

#5.praktikakoak
#1.ariketa
#X="Minuturo bidegurutzera heltzen diren auto kopurua" X~P(1)

#P(X>=3) P(X<=2)
ppois(2,1)

#P(X<=3)
1-ppois(3,1)
#Ez ezin da ziurtatu


#2.ariketa
potentzia<-c(5,6,7,8,9,10,11,12)
ehunekoa<-c(7.5, 5.0, 20.0, 18.75, 15.0, 17.50, 8.75, 7.5)
Maiztasun.erl=ehunekoa/100
Maiztasun.abs=Maiztasun.erl*480
Met.maiztasun.abs=cumsum(Maiztasun.abs)
Met.maiztasun.erl = cumsum(Maiztasun.erl)
a<-data.frame(Maiztasun.abs,Met.maiztasun.abs,Maiztasun.erl,Met.maiztasun.erl)
a
tabla = rep(potentzia,ehunekoa*480)
tabla

mean(tabla)

a = var(tabla)*(479/480)
sqrt(a)

datuak.90gabe <- tabla[-seq(480,by = -1,length.out = 90)]
datu.berriak <- c(datuak.90gabe, rep(5,90))
datu.berriak
mean(datu.berriak)
(mean(tabla)-mean(datu.berriak))/mean(tabla)


#3.ariketa
#X="Bateko bat errege bat txanka bat eta desberdina den beste edozein karta ateratzen diren kopurua"
#errege->4/40
#txanka->4/40
#bateko->4/40
#desberdina->28/40
prob=4/40*4/40*4/40*28/40
#X~B(200,prob)
#P(X=5)
dbinom(5,200,prob)

#P(X>5)
pbinom(5,200,prob,lower.tail = F)

#4.ariketa

hm = c(40.6,44.1,41.0,39.8,39.4,42.6,41.8,52.3,45.5,43.8,42.4,41.5,42.3,33.7,34.8,35.1,34.8,38.9,37.4,36.5,36.4,37.6,35.8,34.5,31.1,31.2,32.7,33.5,34.6,35.8,46.1)

mean(hm)
median(hm)

boxplot(hm,horizontal = T)
boxplot.stats(hm)

bariantza = var(hm)*((length(hm)-1)/length(hm))
sqrt(bariantza)


hm.berria = hm*0.77
hm.berria

mean(hm.berria)

heina = max(hm)-min(hm)
k=sqrt(length(hm))
k
zabalera = heina/5
zabalera
br = seq(min(hm),max(hm),by=4.24)
br
hist(hm,breaks=br)


#6.ariketa
datuak = read.table("Tuboak.txt",header = T)
datuak
ordenatuta = sort(datuak$Tubo.luzera)
ordenatuta  

bariantza = var(ordenatuta)-((length(ordenatuta)-1)/length(ordenatuta))
sqrt(bariantza)

library(moments)  

#asimetria eta kurtosi koefizienteak

#fisher
skewness(ordenatuta)

#ezkerrera alboratua

#kurtosis
kurtosis(ordenatuta)-3
#platikurtikoa

#Maiztasun taula

heina = max(ordenatuta)-min(ordenatuta)
d = sqrt(length(ordenatuta))
d
#klase kopurua = 12
zabalera = heina /12
zabalera

limit = seq(min(datuak),by = zabalera, length.out = 12)
limit
tarte = cut(ordenatuta,limit,right=F)
tarte
a<-as.data.frame(table(tarte))
a

a$tarte[1]
a$tarte[2]

#NOLA ATERATZEN DA KLASE MARKA 
klase.marka=(a$tarte[1]a$tarte[2])/2
  
Maiztasun.abs=a$Freq
sum(Maiztasun.abs)
Maiztasun.erl = Maiztasun.abs/25
Met.maiztasun.abs = cumsum (Maiztasun.abs)
Met.maiztasun.erl = cumsum(Maiztasun.erl)

data.frame()



#7.ariketa
#X="Katamotzen pisua" X~N(8.6,1.4)

#P(X>9.5)
pnorm(9.5,8.6,1.4,lower.tail = F)

#P(X>10.5)
pnorm(10.5,8.6,1.4,lower.tail = F)

#80 perzentila 
quantile(lagina,0.8,type=z)

#P(X<=85)
pnorm(85,8.6,1.4)




