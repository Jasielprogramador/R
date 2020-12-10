#4. Ordenagailu Praktika EMI Mikel Laorden:

# 1.Ariketa:
#   X ~ P(landa)
#   X : "Urtero aholkularitza enplesak aholkuak jasotzen dituzten pertsona kopurua"
#   a) P(x>1085) = 1 - P(x<=1085)
        1-ppois(1085,1200)
        ppois(1085,1200,lower.tail = F)
#   b) P(1200 <= x <= 1300)
        ppois(1300,1200)-ppois(1199,1200)
  
# 4.Ariketa:
#   X : "Era egokian funtzionatzen duten osagai kopurua"
#   Banakata binomiala: X ~ B(n,p)
      
#   a) Osagai batekera egokian funtzionatzeko probabilitatea 0.95
#       P(X>=6)      
        1-pbinom(5,9,0.95)
        pbinom(5,9,0.95, lower.tail = F)
      
#   b) Probabilitate funtzioa irudikatu.
        osagai<-0:9
        plot(osagai,pbinom(osagai,9,0.95),type = "h", ylab = "p(x)")

# 2.Ariketa:
#   a) Gutxienez 70 puntu lortzeko probabilitatea
#     1)
        1-pnorm((70-60)/10,0,1)
        pnorm((70-60)/10,0,1,lower.tail = F)
#     2)
        1-pnorm(70,60,10)
        pnorm(70,60,10,lower.tail = F)
#   b) Gutxienez 39 eta gehienez 80 puntu lortzeko probabilitatea
#     1)
        pnorm((70-60)/10,0,1) -  pnorm((39-60)/10,0,1)
#     2)
        pnorm(70,60,10) -  pnorm(39,60,10)
    
#   c) Ikaslearen puntuazioa batezbestekotik gehienez 20 puntu urruntzeko probabilitatea. 
#     P(-20<=x-60<=20)
#     1) 
        pnorm(20/10,0,1) - pnorm(-20/10,0,1)
#     2)
        pnorm(80,60,10) - pnorm(40,60,10) 
  
#   d) Ikaslearen puntuazioa batezbestekotik gutxienez 20 puntu urruntzeko probabilitatea. 
#     1)
        pnorm(20/10,0,1,lower.tail = F) + pnorm(-20/10,0,1)
#     2)
        pnorm(80,60,10,lower.tail = F) + pnorm(40,60,10)
    
#   e) Kalkulatu 70 puntu edo gehiago lortu duten ikasleen kopurua
#     1)
        200*pnorm((70-60)/10,0,1,lower.tail = F)
#     2)
        200*pnorm(70,60,10,lower.tail = F)
        

# 3.Ariketa:
    #a) 45 orduz erabili nahi izanez gero?
      # 40 ordukoa
        1-pexp(45,1/40)
        
      # 45 ordukoa
        1-pexp(45,1/45)
      # 45 ordukoa de tresna elektronikoa   
        
    #b) Lehenengo aldagaiaren banaketa funtzioa irudikatu. 
        curve(pexp(x,1/40),from = 0 ,to = 500)
        
        