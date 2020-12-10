# R -ko Sarrera
  #1. Ariketa
    # a)
      ((1+2)/(3+4))^2
      
    # b)
      sqrt(exp(2)+log(2,3))
    
    # c) (Bektoreak erabili)
      x <- seq(1,21)
      prod(x)
      
  #2. Ariketa
    x = c(20.5, 12.6, -23, -6.98, -24, 32.8, 7, -8.6)
    F_ = c(3, 4, 2, 6, 5, 7, 4, 9)
    #a)
      f = F_/cumsum(F_)
      f
    # b)
      cumsum(x*F_)
    # c)
      cumsum(x*f)
    # d)
      cumsum(x^2*f)
      
  #3.Ariketa
    km = c(31422, 31801, 32131, 32691, 33077, 33514,33992)
    #a)
      km
    #b)
      diff(km) 
      # Funtzio honek km_i eta km_i+1 arteko diferentzia bueltazen du
      # Hau da: km_1(31422) eta km_2(31801) artean 379
      #         km_2(31801) eta km_3(31801) artean 330
      #             ***             ***     artean 560
      #                                         ***
    # c)
      mean(km)
      mean(diff(km))
      mean(c(0,10)) # Emaitza 5 da, hau da 1 eta 10-en batez bestekoa
      # Balio guztien batezbestekoa egiten du: Media
  
  #4. Ariketa
    # a)
      x <- seq(0,1.2,1.2/20)    # Ez
      x <- seq(0,1.2,1.2/19)    # BAI
      x <- seq(0,1.2,length=20) # BAI
      x
    # b)
      length(x)
    # c)
      min(x)
      max(x)
      range(x)
    # d)
      x(10)=20
    # e)
      y = exp(x)
    # f, g)
      plot(x,y)
      plot(x,y,xlab = "Abziza ardatza",ylab = "Ordenatu ardatza", type="p")
    # h)
      hist(x)
      
  #5. Ariketa
    x = seq(1,by = 2,length = 100)
    sort(x,decreasing = F) # - > +  (Txikienetik handienera)
    sort(x)
    sort(x,decreasing = T) # + > -  (Handienetik txikienera)
    rev(sort(x))
  #6. Ariketa
    x = seq(100,199,2)
    plot(x,log(x))
    plot(x,cos(x))
  
  #7. Ariketa
    y = c(0,1,2,3,4,5,6,7,8,9,10)
    BatezBestekoa1 = function(x)(cumsum(x)[length(x)]/length(x))
    BatezBestekoa2 = function(x)(sum(x)/length(x))
    BatezBestekoa1(y)
    BatezBestekoa2(y)
    # cumsum() != sum() ?-> cumsum: Batuketa metatua y: [0,1,3,6,10,...,55]
    #                       sum: Batuketa y-ren kasuan [55]
  
  #8. Ariketa
    f <- function(x)(sin(x^2+x^3))
    pi # PI
    # a)
      f(-3*pi);f(-2*pi);f(pi);f(0);f(pi);f(2*pi);f(3*pi)
    
    # b)
      plot(f,-pi,pi)
  
  #9. Ariketa
    x = seq(0,by=2,length=20)
    Karratua = x^2
    Kuboa = x^3
    d.f = data.frame(x,Karratua,Kuboa)
    
    write.table(d.f,"8AriketaDATA.txt")
  
  #10. Ariketa
    # Instalatu datasets: Packages panelean (BEHE ESKUMA)
    library()               #Dauden paketeak jakiteko
    library(help="datasets")  #Pakete baten infoa
    
    # dataframe-ko mtcars dataframe-a erabili:
    mtcars
    #a)
      names(mtcars)
      dimnames(mtcars)
    #b)
      #am = 1
      mtcars$am == 1   #Datu garrantzitzua jakitea honen emaitza
      
      mtcars[mtcars$am == 1,]         #Honek T/F balioa sortzen du errenkada bakoitzeko
                                      # T ematen duten balioak aukeratzen dira
      
      which(mtcars$am == 1)   #Datu garrantzitzua jakitea honen emaitza
      
      mtcars[which(mtcars$am == 1),]  # Beste honek balio hori duten errenkaden
                                      # zenbakiak lortu eta ondoren zenbaki hauek
                                      # erabiliz d.f-a filtratu egiten da
    
    #c)
      mtcars_21Gehiago = mtcars[mtcars$mpg>21,]
      mtcars_21Gehiago
    
    #d)
      # EZ: mtcars_21GehEtaEsk = mtcars[mtcars$mpg>21,][MTCARS$am == 1,] 
      
      mtcars_21GehEtaEsk = mtcars[mtcars$mpg>21,]
      mtcars_21GehEtaEsk = mtcars_21GehEtaEsk[mtcars_21GehEtaEsk$am == 1,]
      
      mtcars_21GehEtaEsk = mtcars[mtcars$mpg>21 & mtcars$am == 1,]
      mtcars_21GehEtaEsk
      
    #e)
      mtcars[mtcars$wt > 3.000,]$hp
    
    #f)
      names(mtcars)
      
      sapply(mtcars[which(names(mtcars)=="wt")], FUN=mean)
      mean(mtcars$wt)
    #g)
      # 1: Erregai kuntsumoa(mpg)
      # 3: Pistoien bolumena(disp)
      # 4: Potentzia(hp)
      sapply(mtcars[c(4,1,3)], FUN=mean)
    
    