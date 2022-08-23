#carregando dados externos: 
#install.packages("readxl")

library(readxl)

#verificando a estrutua das colunas
df_excel=read_excel(file.choose())
df_excel
dados=df_excel[,2:22]

dados

xbarra <- round(apply(dados,2,mean),4)
xbarrabarra <- round(mean(xbarra),6)


s <- round(apply(dados,2,sd),4)

sbarra <- round(mean(s),4)

##-------##
rm=numeric(0)
for(k in 1:(length(xbarra)-1))
{
  rm[k]=abs(xbarra[k]-xbarra[k+1]) # Amplitude das medias
}
rm
rmbarra=mean(rm) # Media das amplitudes medias
##CV##

cv= sbarra/xbarra
A= as.matrix(cbind(xbarra, rm, s, cv))
require(xtable)
#install.packages("xtable")
xtable(A,digits= 4)

####-----------------------##

#Boxplot de cada amostra
boxplot(dados,xlab="Amostra",ylab="Tamanho da rolha")
hist(dados,xlab="Amostra",ylab="Tamanho da rolha")
####-----------------------##

# Construção dos gráficos 3-D#
n<-27
d2<-2.66
D4<- 3.267
D3<- 1.420025
c4 <- (4*(n-1))/(4*(n-3))
B3<- 1 - 3/(c4*sqrt(2*(n-1)))
B4<- 1 + 3/(c4*sqrt(2*(n-1)))
A3= 3/(c4*sqrt(n))


# Gráfico para a média X-bar S #
LSC1 <- xbarrabarra + (A3*sbarra)
LC1 <- xbarrabarra
LIC1 <- xbarrabarra - (A3*sbarra)

plot(xbarra,xlab="Subgrupos",ylab="Média Xbar-S",type="o",
     ylim=c(5.96,6.05), pch = 16)
abline(h=LSC1,col="blue")
abline(h=LIC1,col="red")
abline(h=LC1)



# Gráfico para a média X-bar Rm #


LSC <- xbarrabarra + (d2*rmbarra)
LC <- xbarrabarra
LIC <- xbarrabarra - (d2*rmbarra)

plot(xbarra,xlab="Subgrupos",ylab="Média Xbar-Rm",type="o",
     ylim=c(5.95,6.05), pch = 16)
abline(h=LSC,col="blue")
abline(h=LIC,col="red")
abline(h=LC)



# Gráfico para a amplitude móvel #
LSC2 <- D4*rmbarra
LC2 <- rmbarra
LIC2 <- 0
plot(rm,xlab="Subgrupos",ylab="Amplitude Móvel",type="o",
     ylim=c(0,0.05), pch = 16)
abline(h=LSC2,col="blue")
abline(h=LIC2,col="red")
abline(h=LC2)



# Gráfico para o desvio padrão #
LSC3 <- B4*sbarra
LC3 <- sbarra
LIC3 <- B3*sbarra
plot(s,xlab="Subgrupos",ylab="Desvio Padrão",type="o",
     ylim=c(0,0.025), pch = 16)
abline(h=LSC3,col="blue")
abline(h=LIC3,col="red")
abline(h=LC3)

###---------------------------------------------------------------#####
#hist(xbarra, col="darkblue", border= "black")
#shapiro.test(xbarra)

#-----valores para estudo com alterações----
n <- 27
c4 <- round((4*(n-1))/(4*n - 3),4)
sbarra <- 0.0166
#LSC <- 6.0424  #limite superior de controle para X-bar Rm
#LIC <- 5.9749  #limite inferior para X-bar RM

LSC<-6.01823    #limite superior de controle para X-bar Rm
LIC<-5.998912   #limite inferior para X-bar RM

xbarrabarra <- 6.0086
rmbarra=0.0127
sigmax <- round(sbarra/c4,4)
sigmaxbarra <- round(0.0166/(c4 * sqrt(n)),4)

##--------------------------------------------------------------------###
# alteracao media

 delta <- seq (0.25 ,3 ,0.25)   ##valores de delta

 F_LSC <- (LSC - (xbarrabarra + delta * sigmax))/sigmaxbarra
 F_LIC <- (LIC - (xbarrabarra + delta * sigmax))/sigmaxbarra

 probdetec <- 1 - ( pnorm ( F_LSC ) - pnorm ( F_LIC ) )
 NMA <- 1/ probdetec

 plot(delta,probdetec, type = "o", col = "#E31A1C", xlab = "Delta",
      ylab = "Probabilidade", pch = 16)
 
 plot(delta,NMA, type = "o", col = "#E31A1C", xlab = "Delta",
      ylab = "NMA", pch = 16)
 
 
 ##tabela para latex
 A= as.matrix(cbind(delta, probdetec, NMA))
 require(xtable)
 #install.packages("xtable")
 xtable(A,digits= 4) 

####-------- Eficiência Relativa  -----------##
 
 NMAGCS= c(22.287961  ,2.854286 , 1.214582 , 1.012684 , 1.000190 , 1.000001 , 1.000000 , 1.000000 ,
           1.000000  ,1.000000, 1.000000 , 1.000000)
 NMAGCRm=c(     Inf ,1.000800e+15 ,5.759558e+10 ,1.849601e+07, 3.157439e+04, 2.778198e+02,
                1.182512e+01 ,2.104898e+00 ,1.118130e+00 ,1.005223e+00, 1.000053e+00 ,1.000000e+00)
 ER=numeric()
 for (i in 1:12){
   ER[i]<-  NMAGCS[i]/NMAGCRm[i]
 }
 
 plot(delta,ER, type = "o", lty=2,col = "#E31A1C", xlab = "Delta",
      ylab = "ER", ylim=c(0,1), pch = 16)

 #--------exportação para latex---########
 A= as.matrix(cbind(delta,NMAGCS, NMAGCRm,  ER))
 require(xtable)
 #install.packages("xtable")
 xtable(A,digits= 4) 
 