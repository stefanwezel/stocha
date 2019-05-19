

library("ggplot2")

eq = function(x){x^(-x)}
area = integrate(eq, 0, 4)
# print(area[1])
c = 1/as.numeric(area3[1])
print(c)

density = function(x){c * (x^(-x))}
curve(density, from=-1, to=5, xlab="x", ylab="y", 
      xlim = c(-1, 6),
      ylim = c(0, 3),
      col = "coral",
      col.lab="darkslategray",
      col.main = "darkslategray",
      col.axis = "darkslategray",
      bty = "n")





#f = function(t){integrate(density, lower = -Inf, upper = Inf, t)}
F <- function(t){
   if(t < 0){
      return(0)
   }
   else if(t >=0 && t < 4){
      return(as.numeric(integrate(density, lower = 0, upper = t)[1]))
   }
   else {
      return(1)
   }
}


ggplot(data.frame(x=c(-1, 5)), aes(x=x)) + 
   stat_function(fun=Vectorize(F), geom="line", color = "coral") + 
   xlab("x") + 
   ylab("y")





# Finv <- function(s){
#    if(s<0){
#       return(1)
#    }
#    else if(s >=0 && s < 4){
#       return(uniroot(F, c(0,1)))
#    }
#    else{
#       return(0)
#    }
# }

Finv <- function(s){
   return(uniroot(F, lower = -200, upper = 200))
}


print(Finv(1)$root)
# ggplot(data.frame(x=c(0, 1)), aes(x=x)) +
#    stat_function(fun=Vectorize(Finv), geom="line", color = "cornflowerblue") +
#    xlab("x") +
#    ylab("y")






