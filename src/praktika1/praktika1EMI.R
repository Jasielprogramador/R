#-----------------
#| R-ko sarrera: |
#-----------------

# 1. Saioa kudeatu:
#   > Kodea exekutatzeko:
#       Non:
#         Console lehioan scriptean idatzitakoa exekutatu
#         Console lehioan terminalean zuzenean idatzitakoa exekutatu daiteke
#       Nola:
#         Script-ekoa:  (Ctrl + Intro) → lerroz lerro
#                       (Ctrl + Alt + r) → skript osoa
#                       Code → Run region ... → nahi duzun era aukeratu
#         Terminalekoa:
#                       (Intro) → Terminalean zuzenean idatzitakoa exekutatu

#   > Direktorioa:
#       Lanean gaudenean, artxiboak ireki eta gordetzeko tokia
#         Lanean ari garen direktorioa: getwd()
#         Direktorioa aldatu: Session → Set Working Directory → Choose Directory
#                             (Ctrl+Shift+H)
#   > Bestelakoak:
#       Saioko aginduak erakusteko: History panelean ikusi
#                                   history(inf)
#       Erabilitako objektuak gordetzeko: Environment panelean Save sakatu.
#       Laguntza lortzeko:  Help → R Help
#                           Help panelean

# 2. Eragiketa sinpleak:
#   > Eragile aritmetikoen notazioa:
#       + → Gehiketa
#       - → Kenketa
#       * → Biderketa
#       ^ → Berreketa
#       / → Zatiketa

#   > Eragileen arauak:
#       Ezkerretik eskumara irakurri | Eragileen lehentasuna | "(" eta ")" erabili

#   > Eragiketa bat baino gehiago lerro batean:
#       ";" Karakterea eragiketen artean jarriz

#   > Funtzio matematiko eta estatistiko ugari:
#       log(n): ln(n) | exp(n): e^n | sin,cos,tan | sqrt()

# 3. Funtzioak eta aldagaiak:
#   > Funtzioak definitu eta irudikatu daitezke:
#       function() komandoa erabiliz
          f <- function (x,y)x+y+2
          f(1,2)
#       irudikatu plot()
          plot (-3,3) 
#   > Aldagaiak definitu:
#       <- : aldagaiaren izena <- balioa
#       -> : balioa -> aldagaiaren izena 
#       =  : aldagaiaren izena = balioa
          
# 4. Datuen bilketa:
#   > Bektoreak 
#       Datuak zenbakizkoak balioak edo karaktereak
#       Hainbat funtzio aplikatu zatzaizkie:
#         sum()   : Bektorearen gehiketa
#         mean()  : Bektorearen media
#         sort()  : Bektorea ordeneatu
#         max()   : Bektorearen maximoa
#         min()   : Bektorearen minimoa
#         range() : Bektorearen mugak, maximoa eta minimoa
#         cumsum(): Bektorearen gehiketa metatua
#         var()   : Bektorearen QuasiBariantza S --> s^2 = var()(n-1)/m
#         length(): Bektorearen luzeera
#       Definitzeko modu ezberdinak daude: 
#         c() → normalean, c(elem1, elem2, c(elem3))  = [elem1, elem2, elem3]
            x <- c(2.7,3.2,4.5,1.1,7.6,8,1.6,9.5,10.5,14.3,6.4,15.3,12.1,8.6)
#         scan()
            f <- scan() # Teklatutik irakurri
#         seq() → (from, to, by, length.out .. )
#         rep() → (x, times); times = [a,b], x = [2,3] -> [a, a, b, b, b]
#       Bektore bateko elementu zehatzekin lan egin daiteke:
            x[1]          #x bektoreko lehenengo elementua
            x[length(x)]  #x bektoreko azken elementua
            x[1:3]        #x bektoreko lehenengo 3 elementuak
            x[c(1,3,5)]   #x bektoreko 1, 3 eta 5 pozizioetan dauden balioak
            x[-1]         #x bektoreko 1 pozizioan zegoen elementua kenduk
            x<=1          #x bektoreko elementuetatik 1 edo txikiagoak direnak boolean multzoa
            x[x<=1]       #x bektoreko elementuetatik 1 edo txikiagoak direnak elementu multzoa
#       Eragile aritmetikoak bektoreko elementuz elementu lan egiten dute
            y <- x^2
#       Eragile logikoak erabil daitezke :
#         <, <=, >, >=, ==(berdin), &(eta), |(edo)
            x<=1
#       KONDIZIO JAKIN bat betetzen DUTEN bektore bateko ELEMENTUEN POSIZIOA 
#          jakiteko which() funtzioa erabiltzen da
#       Mota berekoak izan behar
#       Bi salbuespen daude:
#         NA (not available)
#         NaN (not a number
          
#   > Data frame
#       Zutabe ezberdinez eraturik dago, zutabe bakoitza aldagai bat da eta R-n  
#           bektore bat bezala definiturik daude
#       Bektore hauen datuak ez dira zergatik zenbakizkoak izan behar
#       Eraikitzeko:
#         data.frame(bek1,bek2,...)
            Tenperatura <- c(7,5.6,8.6,5.3,4.2,6.1,8.4,5.4,5.9)
            Hezetasuna <- c(12.3,14.6,13.4,17.3,11.4,10.4,12.5,13.4,12.8)
            Herria <- c("Gernika","Bilbo","Getxo","Getxo","Bilbo","Gernika","Gernika","Bilbo","Getxo")
            d.f <- data.frame(Tenperatura,Hezetasuna,Herria)
#       Data frame bateko egitura jakiteko:
#         str()   : objektuaren infoa
            str(d.f)
#         dimnames() : data framearen  lerro eta zutaben izenak
            dimnames(d.f)
#       Aldagai bat (bektore) ESKURAGARRI jartzeko:
#         $ : komandoa → dataframe_izena$zutabearen_izena
            d.f$Tenperatura
            d.f$Hezetasuna[3]
#         attach  (dataframe_izena) : zutabe guztiak bektore aldagaietan bihurtu
#         deatach (dataframe_izena) : attach en kontrakoa
#       Data frame berri bat eratu, aurreko data frame baten datuak erabiliz
            d.f[,-3] # Hirugarren zutabea ezabatu
            d.f[d.f$Tenperatura <7,]#7 ko tenperatura baina baxuagoa duten datuak
            which(d.f$Tenperatura >6)#6 ko tenperatura baina altuagoa duten datuen posizioa
            d.f$Tenperatura[Hezetasuna == 17.3] #17.3 ko hezetasuna duen tenperaturaren balioa
#       Aldagai batean hainbat instrukzio exekutatu daitezke:
#         with()
#       Aldagai guztiei funtzio(komando) bat aplikatu zatzaizkie:
#         sapply()
            sapply(d.f[,1:2],FUN=mean) #Tenperatura eta hezetasunaren batezbestekoak
#       D.f-eko datuak aukeratu daitezke:
#         subset()
#       Aldagaiak edo eta datuak aldatu daitezke
#         names(), rownames(), dinnames()
#       Objektu baten izaera aldatu daiteke:
#         as.character(), as.numeric(), as.integer() .. erabiliz
#       Zutabeak (Aldagaiak) edo errenkadak (datu berriak) gehitu:
#           cbind() eta rbind() : zutabeak , errenkadak
              errenkada.berriak = data.frame[Tenperatura =c(6.3,8.6),Hezetasuna =c(14.7,13.4),Herria = C("Getxo","Gernika")]# Errenkada berriak eratu
              d.f = rbind(d.f,errenkada.berriak) # Errenkada berriak gehitu
#           fix(dataframe)
              
# 5. Fitxategi bateko datuen irakurpena: 
#     Enviroment panelean import dataset
#     read.table() komandoa: .txt fitxategia irakurri
        read.table("8AriketaDATA.txt",header=T)#datuak1
        names(df) # df datu markoaren aldagaien zerrenda ematen du
        attach(df) # df
#     read.delim() : irakurtzeko eta data frame batean gehitzeko
        read.delim("8AriketaDATA.txt",header = T,sep = " ")
        
# 6. Datuak testu fitxategi batean gordetzea:
#     write table(): fitxategi bat eratu wd-an
      write.table(df ,"datuak.txt", header =T) # datuak izeneko artxibo baten erapena lan egiten ari den direktorioan
      typeof(read.table("datuak.txt",header=T))

# 7. Paketeak instalatu:  
#     Daudenak ikusteko      
        library()
#     Laguntza lortzeko/info:
#       library(help = paketearen izena)
        
               
# KLASEAN:
2+4
2-1
3*2
3^3
4/3;3*2 # Funtzioak ";"-ekin banatu daitezke
sqrt(4) # Erro karratua

#FUNTZIOAK: function(x)x+1 ; erabili
a=6.5                     #a aldagaiadefinitu
f<-function(x)x^3-x^2+15  #funtzioa definitu
f(4)                      #funtzioarenbalioax=4 denean 
plot(f,-3,3)              #funtzioairudikatux=-3,x=3 tartean

g=function(x,y)(sin(y)*cos(x))/(x^3+x*y+5)#bi aldagaikofuntzioadefinitu> 
g(8,3)

# BEKTOREAK:
x <- c(4,3,2,1)
y = scan()
y = seq(1, 10, 2) # 1, 3, 5, 7, 9
y = seq(1, 10, 5) # 1, 6
y = rep(2,7) # 2, 2, 2, 2, 2, 2, 2

# PLot
plot(y)
hist(y) # histograma
stem(y, scale=2)