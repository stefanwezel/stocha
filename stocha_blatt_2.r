  # Create the data frame.
  data <- data.frame(
    name = c("Oskar","Uta","Franz","Emil","Maria", "Ines", "Udo", "Iwan", "Lea", "Anna", "Julia", "Jonas", "Paul", "Ivo", "Pia", "Felix", "Theo", "Emma"),
    age = c(28, 25, 22, 22, 23, 28, 23, 25,24, 21, 23,25,21,22,26,24,25,21),
    height = c(175,172,170,175,168,168, 180, 180, 165, 160, 175, 185, 178, 182, 165, 182, 175, 162),
    weight = c(76,65,65,68,60,55,70,76,55,55,70,85,76,78,60,80,70,60),
    hair_color = c("blond", "blond", "schwarz", "braun", "schwarz", "rot", "braun", "schwarz", "blond", "blond", "braun", "schwarz", 
                   "schwarz", "braun", "blond", "braun", "rot", "braun")
  
    
  
  )
  # Print the data frame.			
  #print(length(data$hair_color))
  brown_count = 0
  for (i in data$hair_color) {
    if(i == "braun"){
      brown_count <- brown_count + 1
    }
  }
  print(brown_count)
  hist(data[,"age"],
       breaks = c(20.5,21.5, 22.5, 23.5,24.5,25.5,26.5,27.5, 28.5), 
       xlim = c(21, 28), 
       col.lab="darkslategray",
       col.main = "darkslategray",
       col.axis = "darkslategray",
       col = "coral",
       main = "Aufgabe 8 (a)",
       xlab = "Age",
       bty="n")
  
  pearson <- cor(data$height, y = data$weight, method = "pearson")
  print(pearson)
  
  
  
  
  
  
  lm_height_weight = lm(formula = data$weight ~ data$height)
  
  print(lm_height_weight)
  #abline(122.7513, 0.7414)
  plot(data$weight, data$height,
       ylim = c(155, 190),
       main = "Aufgabe 8 (b)",
       xlab = "weight(kg)",
       ylab = "height(cm)",
       col.lab="darkslategray",
       col.main = "darkslategray",
       col.axis = "darkslategray",
       col = "coral",
       pch = 19,
       bty="n")
  
  abline(lm(data$height ~ data$weight), col = 'cornflowerblue')
  
  
  #X = rnorm(data$age) # X is a sample of 100 normally distributed random variables
  #P = ecdf(X)    # P is a function giving the empirical CDF of X
  #P(21)         # This returns the empirical CDF at zero (should be close to 0.5)
  #plot(P)
  age.ordered = sort(data$age)
  age.edcf = ecdf(data$age)
  plot(age.edcf)
  n = sum(!is.na(data$age))
  plot(age.ordered, (1:n)/n, type = 's', ylim = c(0, 1), 
       xlab = 'Quantile des Alters', ylab = '', 
       main = 'Verteilungsfunktion des Alters',
       bty = 'n',
       col.lab="darkslategray",
       col.main = "darkslategray",
       col.axis = "darkslategray",
       col = 'coral')
print(data)



###########################
#eq = function(x){x*x}
c = 5/8
eq = function(x){(1 - x^4)}
eq2 = function(x){0.625 * (1 - x^4)}
area = (integrate(eq, -1, 1))
area_2 = integrate(eq2, -0.5, 0.5)
print(area_2)
#print(area)
normalized_area = (1 / 1.6)
#print(normalized_area)

curve(eq, from=-1, to=1, xlab="x", ylab="y", 
      xlim = c(-2, 2),
      col = "coral",
      col.lab="darkslategray",
      col.main = "darkslategray",
      col.axis = "darkslategray",
      bty = "n")



