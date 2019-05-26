

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

# Finv <- function(s){function(x){uniroot(F(x) - s, lower = 0, upper = 1)}}
# Finv <- function (s) uniroot((function (x) F(x) - s), lower = 0, upper = 1)[1]
# inverse <- function (f, lower = 0, upper = 1) {
#    function (s) uniroot((function (x) f(x) - s), lower = lower, upper = upper,extendInt = "yes")[1]
# }
   Finv <- Vectorize(function (s)uniroot((function (x) F(x) - s), lower = 0, upper = 4)[1])
   curve(Finv, 0,1)
   

# Finv <- inverse(F, 0,1)
# Finv <- function(s){
#    if(s<0){
#       return(0)
#    }
#    else if(s >=0 && s < 4){
#       # return(uniroot(F, c(0,1)))
#       # return(uniroot(F,c(0,100),extendInt = "yes")$f.root)
#    }
#    else{
#       return(1)
#    }}


# print(Finv(0.9))
# ggplot(data.frame(x=c(0, 1)), aes(x=x)) +
#    stat_function(fun=Finv, geom="line", color = "cornflowerblue") +
#    xlab("x") +
#    ylab("y")

   
ex = function(x){(x * ((5/8)*(1-x^4)))}
expected_value = integrate(ex, -1, 1)

print(expected_value)

var = function(x){(x-0)^2*((5/8)*(1-x^4))}
variance = integrate(var, -1,1)
print(variance)







