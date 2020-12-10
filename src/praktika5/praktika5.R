# 5. Praktika:
#1.Ariketa
#X = "minuturo bidegurutze batera heltzen den auto kopuruak

#a) P(X >= 3)
  ppois(3,1,lower.tail = F)#TODO 
#b) P(X>3)
  ppois(3,1,lower.tail = F)
  #edo
  1 - ppois(3,1)
  #Ezin da zihurtatu

#2.Ariketa:
  #a) 
  bom.kop = 480
  bom.pot = seq(5,12,1)
  bom.ehun = c(7.5, 5.0, 20.0, 18.75, 15.0, 17.50, 8.75, 7.5)
  Maiztasun.erl = (bom.ehun/100)
  Maiztasun.abs = (Maiztasun.erl*bom.kop)
  meta=cumsum(Maiztasun.erl)
  meto=cumsum(Maiztasun.abs)
  a = data.frame(Maiztasun.erl,Maiztasun.abs,meta,meto)
  a
  datuak <- rep(bom.pot, Maiztasun.abs)
  a <- table(datuak)
  barplot(a, main = "Bonbilen potentzia barra grafikoa")
  
  #b)
  mean(datuak)
  #c)
  n <- length(datuak)
  bariantza <- var(datuak)*(n-1)/n
  bariantza
  des.tip <- sqrt(bariantza)
  des.tip
  #d)
  datuak.90gabe <- datuak[-seq(bom.kop,by = -1,length.out = 90)]
  datu.berriak <- c(datuak.90gabe, rep(5,90))
  datu.berriak
  (mean(datuak)-mean(datu.berriak))/mean(datuak)

#3.Ariketa:
  kop = 40
  atera.kop = 200
  # X = "zenbat alditan bateko bat, errege bat, txanka bat eta desberdina den beste edozein karta hartzeko porbabilitatea" 
  # Konbinazio hori lortzeko aldi kopurua 
  # X ~ Bin(n, p) // n = 200; p = ??
  # p = 
  #   Ordena eta itzulera: ((4/40) * (4/40) * (4/40) * (28/40))
      a <- (4/40) * (4/40) * (4/40) * (28/40)
  #   Ordena eta itzulera gabe: ((4/40) * (4/39) * (4/38) * (28/37))
      b <- (4/40) * (4/39) * (4/38) * (28/37)
  #   Edozein orden eta itzulera: ((4/40) * (4/40) * (4/40) * (28/40) * 4!)
      c <- (4/40) * (4/40) * (4/40) * (28/40) * factorial(4)
  #   Edozein orden eta itzulera gabe: ((4/40) * (4/39) * (4/38) * (28/37) * 4!)
      d <- (4/40) * (4/39) * (4/38) * (28/37) * factorial(4)
  
    #1
      binom(5,200, a)
    #2
      binom(5,200, b)
    #3
      binom(5,200, c)
    #4
      binom(5,200, d)      
#TODO      

#6.Ariketa
    #a)
      tabla <- read.table("Tuboak.txt",header = T)
      datuak <- tabla$Tubo.luzera
    #b)
      #Ordeneatu
      sort(Tubo.luzera)
      
      #Bariantza
      n <- length(datuak)
      bariantza <- var(datuak)*(n-1)*n
      
      #Asimetria eta kurtosi koefinzienteak: moments paketea kargatu
        #Fisher-en simetria koefizientea
        skewness(datuak)
        #Pearson
        
        #Kurtosia
        kurtosis(datuak)-3
        #TODO
    #c) Maiztasun taula 
      klase.kop <- round(sqrt(length(datuak)))
      klase.kop
      klase.zabalera <- (max(datuak)-min(datuak))/klase.kop
      klase.zabalera
      klase.zabalera <- 0.7
      limit <- seq(min(datuak),by = 0.7, length.out = klase.kop)
      limit
      sort(datuak)
      split(datuak,limit)
