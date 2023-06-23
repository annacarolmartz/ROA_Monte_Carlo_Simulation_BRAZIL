
#-----------Packages---------------#
install.packages("deSolve")
install.packages("ggplot2")
install.packages("tidyr")

library("deSolve")
library("ggplot2")
library("tidyr")
#----------------------------------#


set.seed(10) #aleatory seems

df <- data.frame()
repeticoes = 100

for (i in 1:repeticoes) {
  #################### price ###################################################
  pm <-0.69
  pb <- runif(1,pm,0.88)
  pp <- runif(1,0.49,pm)
  
  #################utilization##################################################
  Um <-0.53
  Ub <- runif(1,Um,0.61)
  Up <- runif(1,0.45,Um)
  
  ###################salary#####################################################
  wm <-0.89
  wb <- runif(1,0.74,wm)
  wp <- runif(1,wm,1.03)
  
  ##############Productivity####################################################
  Am <-wm/0.13
  Ab <- wb/0.13
  Ap <-wp/0.13
  
  # Best/Worst scenary
  pb_list <- c()
  pp_list <- c()
  
  for (t in 1:40) {
    PB <-((pb*Ub*2.399400)- ((wb/Ab)*2.399400))/((1.08)^t)
    PP <-((pp*Ub*2.399400)- ((wp/Ap)*2.399400))/((1.08)^t)
    pb_list <- append(pb_list, PB)
    pp_list <- append(pp_list, PP)
  }
  
  PB <- sum(pb_list)
  PP <- sum(pp_list)
  
  #############variance#########################################################
  v <- log(PB-PP)/sqrt(40^(1/4))
  
  #########up_and_down_factors##################################################
  up <- exp(v*0.33)
  dw <- 1/up
  delta_t = runif(1,2,10)
  
  
  S0 <- 187589100.00 #initial investment
  s1up <- S0 * up
  s1down <- S0 * dw
  
  q <-(exp(0.045*delta_t)-up)/(up-dw)
  Vn <- exp(-0.045*delta_t)*(q*s1up+(1-q)*s1down)
  
  # escrever variaveis pra uma estrutura
  row <- c(pm, pb, pp, Um, Ub, Up, wm, wb, wp, Am, Ab, Ap, PB, PP, v, up, dw, delta_t, S0, s1up, s1down, q, Vn)
  df <- rbind(df, row)
}
# raname headers
colnames(df) <- c("pm", "pb", "pp", "Um", "Ub", "Up", "wm", "wb", "wp", "Am", "Ab", "Ap", "PB", "PP", "v", "up", "dw", "delta_t", "S0", "s1up", "s1down", "q", "Vn")
write.csv(df, "simulation.csv")

################################################################################
#################Sensitivity Analysis###########################################
################################################################################

simulation <- read.csv("simulation.csv")
df <- data.frame(simulation)


### transform_variables#####


df$lnvn <-log(df$Vn) #Vn in log
df$dummyq <- factor(ifelse (df$q>=0.5,1,0))


###############################################################################


ggplot(df,aes(lnvn))+geom_histogram(binwidth=0.1, fill="white", colour="black")#histogram

###############Density plot###################################################
ggplot(df, aes(x=lnvn, fill=dummyq)) + geom_density(alpha=.3)+
  labs(title = "Calculated Option Value",x="Log(Option Value)")+
  labs(fill="(0.5<q)/(q>=0.5)")

########### subset with v>=0 ################################################
dfv <- subset(df,v>=0)
dfv <- subset(dfv,q>=0)
dfv <- subset(dfv,q<=1)
summary(dfv)
dfv100 <- subset(df,v>=0)

########################Dummy probability increse and decresce################
dfv$dummyq <- factor(ifelse (dfv$q>=0.5,1,0))
dfv100$dummyq <- factor(ifelse (dfv100$q>=0.5,1,0)) # so funciona se tiver o arquivo com 100 rep
###############################################################################

ggplot(dfv, aes(x=lnvn, y=v, size=delta_t, colour=dummyq)) +
  geom_point(alpha=.2) +
  scale_size_area() + # Make area proportional to numeric value
  scale_colour_brewer(palette="Set1")+
  labs(title = "Expected Project Volatility and the Corresponding Option Value ",subtitle = "(100.000 repetitons)")+
  labs(x="Log(Option Value)",y="volatitily")+
  labs(colour="(0.5<q)/(q>=0.5)")+
  labs(size="Δ𝑡")


ggplot(dfv100, aes(x=lnvn, y=v, size=delta_t, colour=dummyq)) +
  geom_point(alpha=.2) +
  scale_size_area() + # Make area proportional to numeric value
  scale_colour_brewer(palette="Set1")+
  labs(title = "Expected Project Volatility and the Corresponding Option Value ",subtitle = "(100repetitons)")+
  labs(x="Log(Option Value)",y="volatitily")+
  labs(colour="(0.5<q)/(q>=0.5)")+
  labs(size="Δ𝑡")

################################################################################
############ Relation between p, u, w,A e Vn####################################
################################################################################

ggplot()+
geom_boxplot(dfv, mapping=aes(x=factor(dummyq), y=wp),width=0.5)+ 
geom_boxplot(dfv, mapping=aes(x=factor(dummyq), y=wb),width=0.5)+
  labs(x="q probability ",y="Salary in Best and Worst Scenario")+
  labs(title="q probability vs salary in the best- and worst-case scenario")

ggplot(dfv,aes(x=factor(dummyq),y=pp))+geom_boxplot()+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(title="q probability vs energy tariff in the worst scenario")+
  labs(x="probability (q<0.5)/(q>=0.5)",y="tariff in the worst scenario")

ggplot(dfv,aes(x=factor(dummyq),y=pb))+geom_boxplot()+
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(title="q probability vs energy tariff in the best scenario")+
  labs(x="probability (q<0.5)/(q>=0.5)",y="tariff in the best scenario")



