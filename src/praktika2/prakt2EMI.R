# Taula egiteko pausuak DATU DISKRETU JARRAITUEKIN

arrailak<-c(50,68,84,86,64,67,78,87,110,85,52,65,52,93,72,70,105,85,30,42,74,30,70,65,49)#Datuaksartu
limiteak<-c(30,40,50,60,70,75,85,90,110,Inf)#Tartebakoitzarenlimiteak
arrailak.tarte<-cut(arrailak,limiteak,right=F)#Tarteakeskuinetikirekiaknahiditugunezright=F.
arrailak.tarte

table(arrailak.tarte)

a<-as.data.frame(table(arrailak.tarte))#Tauladatu-markobateanbihurtudugu.Soilkdata.frame(...)jarridaitekeer
a

Tarteak<-a$arrailak.tarte

Maiztasun.abs<-a$Freq

sum(Maiztasun.abs)

Maiztasun.erl<-Maiztasun.abs/25

Met.maiztasun.abs<-cumsum(Maiztasun.abs)

Met.maiztasun.erl<-cumsum(Maiztasun.erl)

data.frame(Tarteak,Maiztasun.abs,Met.maiztasun.abs,Maiztasun.erl,Met.maiztasun.erl)
