# 1. Ariketa

  datuak = c(0, 2, 3, 2, 4, 1, 2, 3, 3, 0, 2, 6, 2, 1, 2, 3, 1, 2, 3, 1, 2, 7, 2, 1, 4, 2, 3, 3, 1, 0)

  #a)
    a = data.frame(table(datuak))
    tarteak = a$datuak
    Maiztasun.abs = a$Freq
    Maiztasun.erl = Maiztasun.abs/length(datuak)
    Maiz.abs.met = cumsum(Maiztasun.abs)
    Maiz.erl.met = cumsum(Maiztasun.erl)
    data.frame(tarteak,Maiztasun.abs,Maiz.abs.met, Maiztasun.erl, Maiz.erl.met)
  
  #b)
    barplot(table(datuak))
    barplot(Maiz.abs.met, space = 0,names.arg = tarteak)
    plot(tarteak,Maiz.abs.met,type="s")
    
  
  #c)
    mean(datuak)
    median(datuak)
    tarteak[which(Maiztasun.abs == max(Maiztasun.abs))]

  #d)
    #Heina:
      max(datuak)-min(datuak)
      range(datuak)
    #Kuartilarteko heina:
      quantile(datuak,0.75,type=2) - quantile(datuak,0.25,type=2)
    #Bariantza
      n = length(datuak)
      vari = var(datuak)*(n-1)/n
      vari 
    #Desbiderazio tipikoa
      desb = sqrt(vari)
      desb
    #Aldak-koef
      cv = desb/vari
      cv
  #e)
    #Bigarren kuartilla
      quantile(datuak, 0.5, type = 2)
    #Lehenengo dezilla
      quantile(datuak, 0.1, type = 2)
    #Laugarren dezilla
      quantile(datuak, 0.4, type = 2)
    #9. dezilla
      quantile(datuak, 0.9, type = 2)
    # P30
      quantile(datuak, 0.3, type = 2)
    # P85
      quantile(datuak, 0.85, type = 2)
  #f)
    library(moments)
    skewness(datuak)
    pearson = 3*(mean(datuak)-median(datuak))/desb
    #Kurtosia
    kurtosis(datuak)-3
  #g)
    boxplot(datuak,horizontal = T)
    boxplot.stats(datuak)    

#2.Ariketa:
  txak.pisu = c(2,3,4,5,8)
  txak.maizt.abs = c(2,4,3,4,1)
  txak = rep(txak.pisu,txak.maizt.abs)
  elf.pisu = c(3500,4000,4500,5000,5500)
  elf.maizt.abs = c(2,4,3,4,2)
  elf = rep(elf.pisu,elf.maizt.abs)
  
  txak.bataz = mean(txak)
  elf.bataz = mean(elf)
  txak.desb = sqrt(var(txak)*(length(txak)-1))/length(txak)
  elf.desb = sqrt(var(elf)*(length(elf)-1))/length(elf)
  
  txak.tip = (txak - txak.bataz)/txak.desb
  txak.tip = (txak - txak.bataz)/txak.desb
  
  #Egin
  
  #Aldakuntza koef
    cv.txak = txak.desb/txak.bataz
    cv.elf = elf.desb/elf.bataz
    aldak <- c(cv.txak,cv.elf)
    aldak
    which(aldak == min(aldak))

#3.Ariketa 
    tarte = seq(4.3, by=0.1,to = 5.6)
    Maiz.abs = c(1,1,3,4,5,7,5,2,2,1,0,0,0,1)
    dat = rep(tarte, Maiz.abs)
  #a)
    bataz = mean(dat)
    desb = sqrt(var(dat)*(length(dat)-1)/length(dat))
    tarte[which(Maiz.abs == max(Maiz.abs))]  
  #b)
    boxplot(dat, horizontal = T,col="gold")
  #c)
    stats = boxplot.stats(dat)
    stats
    ezhoik.dat = which(dat == stats$out) 
    Ezhoiko.dat.gabe = dat[-ezhoik.dat]
    Ezhoiko.dat.gabe
    