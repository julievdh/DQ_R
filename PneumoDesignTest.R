library(ggplot2)
library(pracma)

rho <- 1.176
eta <- 1.839E-5
Vmax <- 12.0
Vmin <- 0.1
VT <- 2.0
Rrs <- 0.24
aR <-0.2
aV <- 0.1
delPmin <- 0.1
Ltot <-4

fun1 <- function(x) (Vmax*rho)/(1000*pi*eta)
fun2 <- function(x) nthroot((8*eta*x*Vmin*1000)/(pi*delPmin),4)
fun3 <- function(x) nthroot((8*eta*Ltot*x)/(aR*pi*Rrs),4)
fun4 <- function(x) sqrt((aV*VT*1000)/(pi*Ltot*x))
x1 = seq(5,25)
mydf = data.frame(x1, y1=fun1(x1), y2=fun2(x1),y3= fun3(x1),y4 = fun4(x1))
mydf <-  transform(mydf, z = pmax(y1,pmin(y2,y3)))
ggplot(mydf, aes(x = x1)) + 
  geom_line(aes(y = y1), colour = 'blue') +
  geom_line(aes(y = y2), colour = 'green') +
  geom_line(aes(y = y3), colour = 'red') +
  geom_line(aes(y = y3), colour = 'black') +
  geom_ribbon(aes(ymin=y1,ymax = z), fill = 'gray60')