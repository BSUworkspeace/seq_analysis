A<- 4
B<- 1/4
r1<- c(2,5)
r2<- c(6,1)
r3<- c(6,1/5)
r4<- c(1/5,1)
r5<- c(1/5,6)

for(i in 1:2)
if(r3[i]> B & r3[i]< A){next} else if(r3[i]> A){cat("Rej H0") break }else {cat("Accept H0")break}} 