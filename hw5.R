# This is a simulation for Solow-Swan (or exogenous) growth model. When you 
# provide the arguments of the function it will run a simulation for 70 years.
library(data.table)
library(ggplot2)
# Q is output
# K is Capital Stock
# L is Labor 
# I is Investment
# S is Savings
# A is Productivity Parameter
# a is Output Elasticity of Capital
# d is Depreciation Rate
# n is Population Growth Rate
# s is Saving Rate
# g is TFP Growth Rate

simulation <- function(Q,K,L,I,S,A,a,d,n,s,g){
  DT <- data.table(year = c(1:70),
                   Q  = c(Q,rep(NA,69)),
                   K = c(K,rep(NA,69)),
                   L = c(L,rep(NA,69)),
                   S = c(S, rep(NA,69)),
                   I = c(I,rep(NA,69)),
                   A = c(A,rep(NA,69)))
  for (i in c(2:70)) {
    DT$S[i] <- s*(DT$Q[i-1])
    DT$I[i] <- DT$S[i]
    DT$L[i] <- (1+n)*(DT$L[i-1])
    DT$A[i] <- (1+g)*(DT$A[i-1])
    DT$K[i] <- DT$I[i] + (1-d)*(DT$K[i-1])
    DT$Q[i] <- (DT$A[i]) * ((DT$K[i])^a) * ((DT$L[i])^(1-a))
  }
  graph <- ggplot(DT, aes(x = year)) +
    geom_line(aes(y=S, color = "Savings")) +
    geom_line(aes(y=I, color = "Investment")) +
    geom_line(aes(y=Q, color = "GDP")) +
    geom_line(aes(y=K, color = "Capital Stock")) +
    labs(title = 'Solow Growth Model',
         y = "Total Amount (in million of USD)", x = "Years") +
    theme_bw() +
    scale_color_manual(name = NULL,
                       values= c("GDP" = "orange",
                                 "Investment" = "blue",
                                 "Capital Stock" = "yellow",
                                 "Savings" = "green"))
  return(graph)
}
