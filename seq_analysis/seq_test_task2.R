## the seqential for composite hypothesis
#h0 =   h1 = exp(theat)
x=rexp(100,0.5)
f= function(theat){
  lambda = (1-exp(-(x[2]/theat)))*(1/theat)*exp((-1/theat)*x)
}
#setting lambda as result value 
lambda = 0
#sum(t-n) ln integrate(p(x_t:theat)*pi(theat),theat_plus,inf)/intgrate(p(x_t:theat)*pi(theat),-inf,theat_minus)
for (i in 1:length(x-1)){

  lambda = lambda+log(integrate(function(theat) (1-exp(-(x[i]/theat)))*(1/theat)*exp((-1/theat)*x),lower=2, upper=Inf)/integrate(function(theat) (1-exp(-(x[i]/theat)))*(1/theat)*exp((-1/theat)*x),lower=1, upper=2))
  }

