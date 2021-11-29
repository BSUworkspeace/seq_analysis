rm(list=ls())

#                     sprt : Sequentual Probabiliti Retio Test

# agar xm nemoneye estekhrage shode dar marhaleye m ba shad:
# fH0  : f( x1,x2,...,xm | H0 )
# fH1  : f( x1,x2,...,xm | H1 )
# alpha: Khataye noe avval
# beta : Khataye noe dovvom
# R[m] = f( x1,x2,...,xm | H0 )/f( x1,x2,...,xm | H1 )

sprt<-function(fH0,fH1,alpha,beta){ 
A  <-(1-beta)/alpha    #agar B< R[m] <A bashad nemonegi ra edameh midahim  
B  <- beta/(1-alpha)   #agar R[m]>A bashad H0 ba N=m ta nemone rad mishvad
f1 <-fH1               #agar R[m]<B bashad H0 ba N=m ta nemone ghabol mishavad
f0 <-fH0
cp1<- cumprod(f1)
cp0<- cumprod(f0)
m<- length(cp1)
R<- rep(0,m)
R<-cp1/cp0
y<- R
x<- rep(1:m)

if(R[m]>B & R[m]<A){

cat("\n","        B <  R","[",m,"]",

"  < A    ","\n","",B,"<",R[m],"<",A,"   -->",

" nemonehgiri ra edameh dahid","\n","\n")}

else if(R[m]>A)
    
cat("\n","R","[",m,"]","  > A    ","\n",R[m],">",A,"      -->",

" farze H0 rad mishavad. Final procese","\n","\n")
 
else cat("\n","B >","R","[",m,"]","\n",B,">",R[m],"       -->",

" farze H0 pazirofteh mishavad. Final proces","\n","\n")
if(R[m]<B | R[m]>A) {
plot(1,R[1],type="o",xlim=c(0,m),ylim=c(0,A+3));
u<-c(0,0,m,m);v<-c(0,B,B,0);polygon(u,v,col='green');
z<-c(A,A+3,A+3,A);polygon(u,z,col='red');
lines(x,y,xlab="N",type="o",ylab=expression(R[N]),main="Nemodare masire ( N ,expression(R[N]))",lwd=2);
abline(h=A,col='red');abline(h=B,col='red');
text(m/2,A+1.5,"nahieye radde H0",col='blue',lwd=1);
text(m/2,B/2,"nahieye ghabole Ho",col='blue',lwd=1);

}}
#________________________________________________________________________________
z <- c(1,0,0,1,1,1,0,1,1) 

x<-z[c(1:6)]
sprt(dbinom(x,1,0.3),dbinom(x,1,0.6),0.2,0.2)


