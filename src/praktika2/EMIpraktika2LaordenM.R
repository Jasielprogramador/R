# 2.Praktika Mikel Laorden

# 1.Ariketa:
  torloju.lodiera <- c(1,2,3,3,2,1,2,5,2,4,4,4,5,3,2,5,3,4,1,4,2,3,1,1,2,5,3,4,1,3)
  #a
    a = as.data.frame(table(torloju.lodiera))
    Tarteak = a$torloju.lodiera
    Maiztasun.abs = a$Freq
    elem.kop = sum(Maiztasun.abs)
    Maiztasun.erl = Maiztasun.abs/elem.kop
    Met.maiztasun.abs = cumsum(Maiztasun.abs)
    Met.maiztasun.erl = cumsum(Maiztasun.erl)
    
    df = data.frame(Tarteak,Maiztasun.abs,Met.maiztasun.abs,Maiztasun.erl,Met.maiztasun.erl)
    df
  #b
    barplot(table(torloju.lodiera))
  #c
    pie(table(torloju.lodiera), main = "Torlojuen Lodiera", names.arg = c("Oso mehea", "Mehea", "Ertaina", "Lodia", "Oso lodia"))
  
# 2.Ariketa:
  #a) b)
    #Fitxategian komak putuak dihurtu eta fitxategi ".txt" berria sortu
    df = read.table("Hauste_tentsioa.txt", header = T)
    Hauste_tentsioa.Tn.cm2.Ald = gsub(",",".",df$Hauste_tentsioa.Tn.cm2.)
    df = data.frame(Hauste_tentsioa.Tn.cm2.Ald)
    write.table(df,"Hauste_Tentsioa2.txt")
    
  #b)  
    df = read.table("Hauste_tentsioa2.txt")
    df
    
  #c)
    stem(df$Hauste_tentsioa.Tn.cm2.Ald, scale=2)
    min(df$Hauste_tentsioa.Tn.cm2.Ald)
    max(df$Hauste_tentsioa.Tn.cm2.Ald)
  #d)
    sort(df$Hauste_tentsioa.Tn.cm2.Ald)
    # Limiteak sortu: 0,2 ko klase tartea
      lim <- seq(min(from = df$Hauste_tentsioa.Tn.cm2.Ald),by=0.2 ,to = max(df$Hauste_tentsioa.Tn.cm2.Ald))
      lim = c(lim,lim[length(lim)]+0.2)
    # Limiteak aplikatu
      tarte = cut(df$Hauste_tentsioa.Tn.cm2.Ald,lim,right = F)
    table(tarte)
    # Maiztasun taula sortu:
    a <- data.frame(table(tarte))
    Tarteak <- a$tarte
    a
    KlaseMarka <- head((lim + 0.1), length(lim)-1)
    Maiztasun.abs <- a$Freq
    zenbat = sum(Maiztasun.abs)
    Maiztasun.erl <- a$Freq/zenbat
    Met.maiztasun.abs <- cumsum(Maiztasun.abs)
    Met.maiztasun.erl <- cumsum(Maiztasun.erl)
    data.frame(Tarteak,KlaseMarka,Maiztasun.abs,Maiztasun.erl,Met.maiztasun.abs,Met.maiztasun.erl)
    
    # Histograma
    hist(df$Hauste_tentsioa.Tn.cm2.Ald,breaks = lim, right = F, main = "Hauste Tentsioa Histograma", xlab = "Hauste tentsioa(Tn/cm2)", ylab = "Maiztasun Abs.")
  
    #e) f) g)
    datuak = df$Hauste_tentsioa.Tn.cm2.Ald
    summary(datuak)
    #Batazbestekoa
      mean(datuak)
    #Mediana
      median(datuak)
    #Moda
      moda <- Tarteak[which(Maiztasun.abs==max(Maiztasun.abs))]
      moda
    #Bariantza
      bariantza = var(datuak)*((length(datuak)-1)/length(datuak))   
      bariantza
    #Desbiderazio tipikoa
      sqrt(bariantza)
    #Kuartilak
      quantile(datuak,type=2)
    #Dezillak
      quantile(datuak,probs=seq(0,1,0.1),type=2)
      
    #4 dezila edo 40.pertzentila (%40)
      quantile(datuak,0.4,type=2)
      
#3. Ariketa
    bihurd <- c(33, 21, 32, 44, 35, 22, 40, 36, 22, 37, 20, 37, 42, 31, 23, 44, 32, 30, 44, 44, 42, 35, 40, 36, 32, 31, 37, 43, 24, 40, 25, 30, 26, 35, 33, 41, 25, 44, 36, 27)
    length(bihurd)
  #a)
    heina = max(bihurd)-min(bihurd)
    k = sqrt(length(bihurd))
    klaseZabalera = ceiling(heina/k)
    klaseZabalera
    lim <- seq(min(bihurd), by= klaseZabalera, to = max(bihurd))
    # Frogatu ea limiteak gure datua guztiak batzen dituen
      lim[length(lim)] > max(bihurd)
      # False denez limitearen azken balioa datuen artean badagoen frogatuko dugu
        lim[length(lim)] = max(bihurd)
        # TRUE da: limitearen azkenego zenbakia tartearen barnean egon beharko da
        #           horretarako 'include.lowest = "T"' jari behar
    tarte <- cut(bihurd,lim,right = F,include.lowest = T)
    a = data.frame(table(tarte))
    Tarteak <- a$tarte
    KlaseMarka <- head(lim+klaseZabalera/2,length(lim)-1)
    Maiztasun.abs <- a$Freq
    Maiztasun.erl <- Maiztasun.abs/sum(Maiztasun.abs)
    Met.maiztasun.abs <- cumsum(Maiztasun.abs)
    Met.maiztasun.erl <- cumsum(Maiztasun.erl)
    data.frame(Tarteak,KlaseMarka, Maiztasun.abs,Met.maiztasun.abs, Maiztasun.erl, Met.maiztasun.erl)
    
  #b)
    #Histograma
    histo = hist(bihurd, breaks = lim,right = F,include.lowest = T)
    #Maiztasun absolutuen poligonoa
    plot(KlaseMarka,Maiztasun.abs,type = "o")
    
  #c)
    #Batazbestekoa:
      mean(bihurd)
    #Moda:
      Tarteak[which(Maiztasun.abs==max(Maiztasun.abs))]
    #Mediana:
      median(bihurd)
    #Bariantza
      quasiBariantza <- var(bihurd)
      bariantza = quasiBariantza*((length(bihurd)-1)/length(bihurd))
      bariantza
    #Desbiderazio tipikoa
      sqrt(bariantza)

#4.Ariketa:
  datuak <- c(51, 55, 42, 53, 46, 60, 29, 56, 20, 52, 51, 33, 61, 57, 55, 59, 38, 56, 41, 47, 68, 24, 67, 52, 64, 69, 43, 47, 42, 65, 96, 21, 48, 47, 25, 82, 37, 60, 12, 77, 56, 97, 28, 45, 63, 28, 45, 63, 28, 52, 60, 51, 61, 62, 52, 97, 73, 45, 69, 67, 29, 75, 63, 30, 17, 69, 68, 74, 16, 83, 47, 16)
  #a)
    lim <- seq(from = 10.5, to = max(datuak), by=(21.5-10.5) )
    lim[length(lim)] >= max(datuak)
    lim = c(lim, lim[length(lim)]+(21.5-10.5)) # Right OPEN
    tarte = cut(datuak,lim,right = F)
    table(tarte)
    a = data.frame(table(tarte))
    Tarteak = a$tarte
    KlaseMarka = head(lim,length(lim)-1)+(21.5-10.5)/2
    Maiztasun.abs = a$Freq
    Maiztasun.erl = Maiztasun.abs/sum(Maiztasun.abs)
    Met.maiztasun.abs = cumsum(Maiztasun.abs)
    Met.maiztasun.erl = cumsum(Maiztasun.erl)
    data.frame(Tarteak,KlaseMarka,Maiztasun.abs,Met.maiztasun.abs,Maiztasun.erl,Met.maiztasun.erl)
  #b)
    #Histograma
    hist(datuak,breaks = lim,right = F,include.lowest = F)
    
    #Zurtoin eta hosto grafikoa
    stem(datuak,scale = 1)
  #c)
    summary(datuak)
    #Batez bestekoa
    mean(datuak)
    #Mediana
    median(datuak)
    #Barintza
    quasiBariantza = var(datuak)
    bariantza = quasiBariantza*((length(datuak)-1)/length(datuak))
    bariantza
    #Desbiderazio tipikoa
    sqrt(bariantza)

#5.Ariketa:
  datuak = c(1,2,2,3,3,2,3,3,4,3,1,3,1,5,4,3,2,3)
  #a)
    barplot(table(datuak),names.arg = c("Oso ona", "Ona", "Egokia", "Txarra", "Oso txarra"))
    pie(table(datuak),labels =c("Oso ona", "Ona", "Egokia", "Txarra", "Oso txarra"))
  #b)
    summary(datuak)
    #Batez besteko indizea
      mean(datuak) 
    #Mediana
      median(datuak)

#6.Ariketa:
  x = c(37,37.20,37.50,38,38.10,38.50)
  Maiztasun.abs = c(1,5,13,6,10,5)
  n = sum(Maiztasun.abs)
  #a)
    #Batazbestekoa
    batazbestekoa = sum(Maiztasun.abs*x)/sum(Maiztasun.abs)
    batazbestekoa
    #Desbideratze Tipikoa
    bariantza = sum(Maiztasun.abs * (x^2))/sum(Maiztasun.abs)-batazbestekoa^2
    sqrt(bariantza)
    #Mediana
    Met.maiztasun.abs = cumsum(Maiztasun.abs)
    head(x[which(Met.maiztasun.abs>=n/2)],1)
    #Moda FALTA??
    x[which(Maiztasun.abs==max(Maiztasun.abs))]
  
  #b)
    pertzentila <- function(k){
      h = head(x[which(Met.maiztasun.abs>=(k*n/100))],2)
      ans = head(h,1)
      if (head(h,1)==k*n/100) {
        ans = sum(h)/length(h)
      }
      ans
    }
    #65. pertzentila:
    pertzentila(65)
    #95. pertzentila:
    pertzentila(95)
    #Zer adierazten du??
    #   Zein baliok uzten duen haren ezkerrean k pertzentil kopuru hori

#7.Ariketa:
  dentsitatea = c(2.2, 2.6, 0.6, 0.2)
  tarteak = c(10, 20, 30, 40, 60)
  diferentziak = diff(tarteak)
  Maiztasun.abs = dentsitatea*diferentziak
  Met.maiztasun.abs = cumsum(Maiztasun.abs)
  n = sum(Maiztasun.abs)
  diferentziak
  KlaseMarka = tail(tarteak,-1) + diferentziak/2
  #a)
    #Batazbestekoa:
      batazbestekoa = sum(KlaseMarka*Maiztasun.abs)/n
      batazbestekoa
    #Mediana:
      i_1 = length(tarteak[which(n/2>Met.maiztasun.abs)])
      i = i_1 + 1
      tarteak[i]+(n/2 - Met.maiztasun.abs[i_1])*diferentziak[i]/Maiztasun.abs[i]
    #Desbiderazio tipikoa:
      sqrt(sum(Maiztasun.abs*KlaseMarka^2)/n - batazbestekoa^2)
  
  #b)
    Maiztasun.erl = Maiztasun.abs/n
    Met.maiztasun.erl = cumsum(Maiztasun.erl)
    a = data.frame(table(cut(tarteak,tarteak,right = F,include.lowest = T)))
    Tarteak = a$Var1
    data.frame(Tarteak,KlaseMarka,Maiztasun.abs,Met.maiztasun.abs,Maiztasun.erl,Met.maiztasun.erl)
      # MODA KALKULATU : Moda <- names(y)[which(y==max(y))]
  
  #c)
    #Pozizio Neurriak:
    
    #Kuartilarteko heina:
  
#8.Ariketa
  #a)
    datuak = read.table("Kotxeak.txt", header = T)
    
    datuak
  #b)
    a = c("Batazbestekoa","Mediana","Desbiderazio Tipikoa")
    gasol = datuak$Gasolina.kotxeak
    gas = datuak$Gas.oil.kotxeak
    n = length(gas)
    gasolC = c(mean(gasol),median(gasol),sqrt(var(gasol)*(n-1)/n))
    gasC = c(mean(gas),median(gas),sqrt(var(gas)*(n-1)/n))
    data.frame(a, gasolC,gasC)
  #c)
    
    
    