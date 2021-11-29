#f00:tabe chegalie toam tahte parametrhaye H0 bezaye dadehaye shabih saazi shode az tozie fH0
#f01:tabe chegalie toam tahte parametrhaye H1 bezaye dadehaye shabih saazi shode az tozie fH0
#f10:tabe chegalie toam tahte parametrhaye H0 bezaye dadehaye shabih saazi shode az tozie fH1
#f11:tabe chegalie toam tahte parametrhaye H1 bezaye dadehaye shabih saazi shode az tozie fH0 

en <-  function(f00,f01,f10,f11,alpha,beta){
a  <-   log((1-beta)/alpha)
b  <-   log(beta/(1-alpha))
zH0<- log(f01/f00)
zH1<- log(f11/f10)
if(mean(zH0)==0)EH0N<- -(a*b)/mean((zH0)^2)
else EH0N<- (a*alpha +b*(1-beta))/mean(zH0)
if(mean(zH1)==0)EH1N<- -(a*b)/mean((zH1)^2)
else EH1N<- (a*(1-alpha)+ b*beta)/mean(zH1)
cat("\n","tedad nemone morede entezar baraye ghabole H0 :",EH0N,"\n",
"tedad nemone morede entezar baraye ghabole H1 :"    ,EH1N,"\n","\n")
}

x0<- rnorm(10000,0,1)
x1<- rnorm(10000,1,1)
en(dnorm(x0,0,1),dnorm(x0,1,1),dnorm(x1,0,1),dnorm(x1,1,1),0.05,0.05)