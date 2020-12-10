#ERREPASOA
#5.praktikako ariketak

#3.ARIKETA
#a) Lor ezazu bateko bat, errege bat, txanka bat eta desberdina den beste edozein karta bost aldiz   lortzekoprobabilitatea.   Lau   interpretazio   posibleak   (ordena   eta/edo   itzulera) kontsideratu eta emaitzak era zehatzean lortu.
#ezer ez duenez esaten, itzulerarekin eta ordena garratzitsua.
#Permutazioa errepikapenarekin PR 
as<- 4/40
x<- 4/40
sota<- 4/40
beste<- 28/40
prob<- as*rey*sota*beste
  prob
#X="as, errege, txanka eta beste edozein karta orden horretan ateratako aldi kopurua."
  #X~ Bin(200,prob)
  #P(X=5)
dbinom(5,200,prob) 
#b) Kalkula  ezazu  aurreko  konbinazioa  zehaztutako  ordenan  bost  aldiz  baino  gehiagotan lortzeko probabilitatea.
# P(X>5)
1- pbinom(5,200,prob)
# Bin(200,prob)--> N(0.14,0.3740348) --> tipifikatu?
bb<-200*prob  
bb
bar<-sqrt(200*prob*(1-prob))
#tipikatu P(X>5)==(tipifikatu)== P(z> (5.5-0.14)/0.3740348)
bal<- (5.5-0.14)/0.3740348
bal
 #1- pnorm(bal, 0,1)?
   
   
#4. ARIKETA
  labhez=c(40.6,44.1,41.0,39.4,39.8,42.6,41.8,52.3,45.5,43.8,42.4,41.5,42.3,33.7,34.8,35.1,34.8,38.9,37.4,36.4,36.5,37.6,35.8,34.5,31.1,31.2,32.7,33.5,34.6,35.8,46.1)
length(labhez)
 #a) Kalkulatu  hezetasunaren  batezbestekoa  eta  mediana  ez-ohiko  datuak,  egotekotan, kontuan harturik eta kontuan hartu gabe.
#bb
mean(labhez)
#mediana
median(labhez)
#ez-ohiko datuak:
boxplot.stats(labhez) #honekin balio arraroak lortzen ditugu ($out), kasu honetan ez daude.
#$stats--> 1(hasierako bibotea),2(Q1),3(Q3),4(bukaerako bibotea)
#b) desbiderazio tipikoa
bar<- var(labhez)* (31-1)/31
bar
desb.tip=sqrt(bar)
desb.tip
#c) Hezetasuna jaisteko makina bat erosi da eta hezetasuna % 23 batean jaitsi egin da. Zein izango da hezetasunaren batezbesteko berria? 
labhezberria= labhez*0.77
labhezberria
mean(labhezberria)
#d) histograma eraiki 5 tarteekin
heina= max(labhez)-min(labhez)
max(labhez)
min(labhez)
    heina
tart= heina/5
tart
br= seq(31.1,52.3,by=4.24)
br
hist(labhez,main="HEZETASUNA", breaks = br)

#6.ARIKETA
tuboak=read.table("Tuboak.txt",header=TRUE)
attach(tuboak)
tuboak
ordenatuak= order(tuboak,na.last = TRUE, decreasing = FALSE)
ordenatuak
id= seq(1,141,by=1)
id
taula=data.frame(id=id, tuboak=tuboak)
taula

sort(Tubo.luzera)
#beste modu batean:
luzera= tuboak$Tubo.luzera
luzera
sort(luzera)
#AMAITU GABE!!


#7.ARIKETA
#X="Katamotz iberikoaren pisua kg-tan" Banaketa Normala--> N(8.6,1.4)
#9.5kg baino gehiagokoak askatu
#a) P(X>9.5) tipifikatu behar dugu?? --> ez da beharrezkoa
1-pnorm(9.5,8.6,1.4)
#tipifikatutako balioarekin emaitza berdina lortzen da.
1-pnorm(0.6428571,0,1)

#b) P(X>10.5|X>9.5) --> P(X>10.5) 
1-pnorm(10.5,8.6,1.4)

#c) 80. pertzentila
#hau egiteko lagin bat behar dugu(edo n bat, lagina zoriz lortzeko)
q80<- quantile(lagina,0.80,scale=2)
q80
#0.8 bere ezkerrean uzten duen datua.
qnorm(0.8,8.6,1.4)

#d) n=200 Y="9 kg baino gehiago pisatzen duten katamotz kopurua" Y~ BIN(200,p) p--> kalkulatuko dugu:
#P(X>9)
1-pnorm(9,8.6,1.4)
#p=0.2742531
#P(X<=85)
pbinom(85,200,0.2742531)

#2.ARIKETA
#n=480 
#a)
datuak= c(5,6,7,8,9,10,11,12)
frek=c(0.075,0.05,0.2,0.1875,0.15,0.1750,0.0875,0.075)

freklag= frek*480
freklag
b=rep(datuak,freklag) #datuak elkartzeko
b
a= table(b)
barplot(a)

#b)mean
mean(b)
#c) bariantza eta desb tip
bar= var(b)* (479/480)
bar
sqrt(bar)
#d)
datuakb= c(5,6,7,8,9,10)
frekberria=c(0.2625,0.05,0.2,0.1875,0.15,0.15)
frekblag= frekberria*480
d=rep(datuakb,frekblag)
d
mean(d)





