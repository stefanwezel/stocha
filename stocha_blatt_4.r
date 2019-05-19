eq = function(x){x^(-x)}

area = integrate(eq, 0, 4)
print(area[1])
c = 1/as.numeric(area3[1])
print(c)


curve(eq, from=-1, to=5, xlab="x", ylab="y", 
      xlim = c(-1, 6),
      ylim = c(0, 3),
      col = "coral",
      col.lab="darkslategray",
      col.main = "darkslategray",
      col.axis = "darkslategray",
      bty = "n")

F = function(t){integrate(eq, limits = NULL, t)}
curve(F, from=-10, to=10, xlab="x", ylab="y", 
      xlim = c(-20, 20),
      col = "coral",
      col.lab="darkslategray",
      col.main = "darkslategray",
      col.axis = "darkslategray",
      bty = "n")

