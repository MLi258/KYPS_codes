
#load packages
library(tidyverse)
library(here)
library(fs)
library(haven)
library(broom)
library(tidyr)
library(lavaan)
library(nlme)
library(mice)
library(NHANES)
library(dplyr)
library(naniar)
library(plotrix)


setwd("set your work direction here")
d<- read.csv ("input your raw data here")
pre <- read.csv ("input your prediction here")
dim(d)# check the dataset

# change the char to numeric
d <- as.data.frame(apply(d, 2, as.numeric)) 

# transfer to matrix
pre1 <- data.matrix (pre)
rownames(pre1) <- colnames(pre1)
#str(pre1)

# run the imputation
d_multimp <- mice (d, m=10, meth="pmm", seed=5, predictorMatrix= pre1)

#pool the ML
d1 <- complete (d_multimp, action=1L)
d2 <- complete (d_multimp, action=2L)
d3 <- complete (d_multimp, action=3L)
d4 <- complete (d_multimp, action=4L)
d5 <- complete (d_multimp, action=5L)
d6 <- complete (d_multimp, action=6L)
d7 <- complete (d_multimp, action=7L)
d8 <- complete (d_multimp, action=8L)
d9 <- complete (d_multimp, action=9L)
d10<- complete (d_multimp, action=10L)


#average motivation

d1$m1 <- rowMeans (d1[, 3:6],  na.rm=T)
d1$m2 <- rowMeans (d1[, 7:10], na.rm=T)
d1$m3 <- rowMeans (d1[, 11:14],na.rm=T)
d1$m4 <- rowMeans (d1[, 15:18],na.rm=T)
d1$m5 <- rowMeans (d1[, 19:22],na.rm=T)

d2$m1 <- rowMeans (d2[, 3:6],na.rm=T)
d2$m2 <- rowMeans (d2[, 7:10],na.rm=T)
d2$m3 <- rowMeans (d2[, 11:14],na.rm=T)
d2$m4 <- rowMeans (d2[, 15:18],na.rm=T)
d2$m5 <- rowMeans (d2[, 19:22],na.rm=T)

d3$m1 <- rowMeans (d3[, 3:6],na.rm=T)
d3$m2 <- rowMeans (d3[, 7:10],na.rm=T)
d3$m3 <- rowMeans (d3[, 11:14],na.rm=T)
d3$m4 <- rowMeans (d3[, 15:18],na.rm=T)
d3$m5 <- rowMeans (d3[, 19:22],na.rm=T)

d4$m1 <- rowMeans (d4[, 3:6],na.rm=T)
d4$m2 <- rowMeans (d4[, 7:10],na.rm=T)
d4$m3 <- rowMeans (d4[, 11:14],na.rm=T)
d4$m4 <- rowMeans (d4[, 15:18],na.rm=T)
d4$m5 <- rowMeans (d4[, 19:22],na.rm=T)

d5$m1 <- rowMeans (d5[, 3:6],na.rm=T)
d5$m2 <- rowMeans (d5[, 7:10],na.rm=T)
d5$m3 <- rowMeans (d5[, 11:14],na.rm=T)
d5$m4 <- rowMeans (d5[, 15:18],na.rm=T)
d5$m5 <- rowMeans (d5[, 19:22],na.rm=T)

d6$m1 <- rowMeans (d6[, 3:6],na.rm=T)
d6$m2 <- rowMeans (d6[, 7:10],na.rm=T)
d6$m3 <- rowMeans (d6[, 11:14],na.rm=T)
d6$m4 <- rowMeans (d6[, 15:18],na.rm=T)
d6$m5 <- rowMeans (d6[, 19:22],na.rm=T)

d7$m1 <- rowMeans (d7[, 3:6],na.rm=T)
d7$m2 <- rowMeans (d7[, 7:10],na.rm=T)
d7$m3 <- rowMeans (d7[, 11:14],na.rm=T)
d7$m4 <- rowMeans (d7[, 15:18],na.rm=T)
d7$m5 <- rowMeans (d7[, 19:22],na.rm=T)

d8$m1 <- rowMeans (d8[, 3:6],na.rm=T)
d8$m2 <- rowMeans (d8[, 7:10],na.rm=T)
d8$m3 <- rowMeans (d8[, 11:14],na.rm=T)
d8$m4 <- rowMeans (d8[, 15:18],na.rm=T)
d8$m5 <- rowMeans (d8[, 19:22],na.rm=T)

d9$m1 <- rowMeans (d9[, 3:6],na.rm=T)
d9$m2 <- rowMeans (d9[, 7:10],na.rm=T)
d9$m3 <- rowMeans (d9[, 11:14],na.rm=T)
d9$m4 <- rowMeans (d9[, 15:18],na.rm=T)
d9$m5 <- rowMeans (d9[, 19:22],na.rm=T)

d10$m1 <- rowMeans (d10[, 3:6],na.rm=T)
d10$m2 <- rowMeans (d10[, 7:10],na.rm=T)
d10$m3 <- rowMeans (d10[, 11:14],na.rm=T)
d10$m4 <- rowMeans (d10[, 15:18],na.rm=T)
d10$m5 <- rowMeans (d10[, 19:22],na.rm=T)



#reverse the rank
fun =function(x){(100-x)/100}
d1[,94:98] <- lapply (d1[,94:98],fun)
d2[,94:98] <- lapply (d2[,94:98],fun)
d3[,94:98] <- lapply (d3[,94:98],fun)
d4[,94:98] <- lapply (d4[,94:98],fun)
d5[,94:98] <- lapply (d5[,94:98],fun)
d6[,94:98] <- lapply (d6[,94:98],fun)
d7[,94:98] <- lapply (d7[,94:98],fun)
d8[,94:98] <- lapply (d8[,94:98],fun)
d9[,94:98] <- lapply (d9[,94:98],fun)
d10[,94:98] <- lapply (d10[,94:98],fun)


#add the time
d1 %>% mutate (t1= select(., q44a2w1:q45c3w1) %>% rowSums(na.rm=T))
d1 %>% mutate (t2= select(., q44a2w2:q45c3w2) %>% rowSums(na.rm=T))
d1 %>% mutate (t3= select(., q44a2w3:q45c3w3) %>% rowSums(na.rm=T))
d1 %>% mutate (t4= select(., q44a2w4:q45c3w4) %>% rowSums(na.rm=T))
d1 %>% mutate (t5= select(., q44a2w5:q45c3w5) %>% rowSums(na.rm=T))

d2 %>% mutate (t1= select(., q44a2w1:q45c3w1) %>% rowSums(na.rm=T))
d2 %>% mutate (t2= select(., q44a2w2:q45c3w2) %>% rowSums(na.rm=T))
d2 %>% mutate (t3= select(., q44a2w3:q45c3w3) %>% rowSums(na.rm=T))
d2 %>% mutate (t4= select(., q44a2w4:q45c3w4) %>% rowSums(na.rm=T))
d2 %>% mutate (t5= select(., q44a2w5:q45c3w5) %>% rowSums(na.rm=T))

d3 %>% mutate (t1= select(., q44a2w1:q45c3w1) %>% rowSums(na.rm=T))
d3 %>% mutate (t2= select(., q44a2w2:q45c3w2) %>% rowSums(na.rm=T))
d3 %>% mutate (t3= select(., q44a2w3:q45c3w3) %>% rowSums(na.rm=T))
d3 %>% mutate (t4= select(., q44a2w4:q45c3w4) %>% rowSums(na.rm=T))
d3 %>% mutate (t5= select(., q44a2w5:q45c3w5) %>% rowSums(na.rm=T))

d4 %>% mutate (t1= select(., q44a2w1:q45c3w1) %>% rowSums(na.rm=T))
d4 %>% mutate (t2= select(., q44a2w2:q45c3w2) %>% rowSums(na.rm=T))
d4 %>% mutate (t3= select(., q44a2w3:q45c3w3) %>% rowSums(na.rm=T))
d4 %>% mutate (t4= select(., q44a2w4:q45c3w4) %>% rowSums(na.rm=T))
d4 %>% mutate (t5= select(., q44a2w5:q45c3w5) %>% rowSums(na.rm=T))

d5 %>% mutate (t1= select(., q44a2w1:q45c3w1) %>% rowSums(na.rm=T))
d5 %>% mutate (t2= select(., q44a2w2:q45c3w2) %>% rowSums(na.rm=T))
d5 %>% mutate (t3= select(., q44a2w3:q45c3w3) %>% rowSums(na.rm=T))
d5 %>% mutate (t4= select(., q44a2w4:q45c3w4) %>% rowSums(na.rm=T))
d5 %>% mutate (t5= select(., q44a2w5:q45c3w5) %>% rowSums(na.rm=T))

d6 %>% mutate (t1= select(., q44a2w1:q45c3w1) %>% rowSums(na.rm=T))
d6 %>% mutate (t2= select(., q44a2w2:q45c3w2) %>% rowSums(na.rm=T))
d6 %>% mutate (t3= select(., q44a2w3:q45c3w3) %>% rowSums(na.rm=T))
d6 %>% mutate (t4= select(., q44a2w4:q45c3w4) %>% rowSums(na.rm=T))
d6 %>% mutate (t5= select(., q44a2w5:q45c3w5) %>% rowSums(na.rm=T))

d7 %>% mutate (t1= select(., q44a2w1:q45c3w1) %>% rowSums(na.rm=T))
d7 %>% mutate (t2= select(., q44a2w2:q45c3w2) %>% rowSums(na.rm=T))
d7 %>% mutate (t3= select(., q44a2w3:q45c3w3) %>% rowSums(na.rm=T))
d7 %>% mutate (t4= select(., q44a2w4:q45c3w4) %>% rowSums(na.rm=T))
d7 %>% mutate (t5= select(., q44a2w5:q45c3w5) %>% rowSums(na.rm=T))

d8 %>% mutate (t1= select(., q44a2w1:q45c3w1) %>% rowSums(na.rm=T))
d8 %>% mutate (t2= select(., q44a2w2:q45c3w2) %>% rowSums(na.rm=T))
d8 %>% mutate (t3= select(., q44a2w3:q45c3w3) %>% rowSums(na.rm=T))
d8 %>% mutate (t4= select(., q44a2w4:q45c3w4) %>% rowSums(na.rm=T))
d8 %>% mutate (t5= select(., q44a2w5:q45c3w5) %>% rowSums(na.rm=T))

d9 %>% mutate (t1= select(., q44a2w1:q45c3w1) %>% rowSums(na.rm=T))
d9 %>% mutate (t2= select(., q44a2w2:q45c3w2) %>% rowSums(na.rm=T))
d9 %>% mutate (t3= select(., q44a2w3:q45c3w3) %>% rowSums(na.rm=T))
d9 %>% mutate (t4= select(., q44a2w4:q45c3w4) %>% rowSums(na.rm=T))
d9 %>% mutate (t5= select(., q44a2w5:q45c3w5) %>% rowSums(na.rm=T))

d10 %>% mutate (t1= select(., q44a2w1:q45c3w1) %>% rowSums(na.rm=T))
d10 %>% mutate (t2= select(., q44a2w2:q45c3w2) %>% rowSums(na.rm=T))
d10 %>% mutate (t3= select(., q44a2w3:q45c3w3) %>% rowSums(na.rm=T))
d10 %>% mutate (t4= select(., q44a2w4:q45c3w4) %>% rowSums(na.rm=T))
d10 %>% mutate (t5= select(., q44a2w5:q45c3w5) %>% rowSums(na.rm=T))


#creat a list
x_list <- c(d1=data.frame(d1$m1,d1$m2,d1$m3,d1$m4,d1$m5,d1$t1,d1$t2,d1$t3,d1$t4,d1$t5,
                          d1$aw1,d1$aw2,d1$aw3,d1$aw4,d1$aw5,d1$r1,d1$r2,d1$r3,d1$r4,d1$r5),
            d2=data.frame(d2$m1,d2$m2,d2$m3,d2$m4,d2$m5,d2$t1,d2$t2,d2$t3,d2$t4,d2$t5,
                          d2$aw1,d2$aw2,d2$aw3,d2$aw4,d2$aw5,d2$r1,d2$r2,d2$r3,d2$r4,d2$r5),
            d3=data.frame(d3$m1,d3$m2,d3$m3,d3$m4,d3$m5,d3$t1,d3$t2,d3$t3,d3$t4,d3$t5,
                          d3$aw1,d3$aw2,d3$aw3,d3$aw4,d3$aw5,d3$r1,d3$r2,d3$r3,d3$r4,d3$r5),
            d4=data.frame(d4$m1,d4$m2,d4$m3,d4$m4,d4$m5,d4$t1,d4$t2,d4$t3,d4$t4,d4$t5,
                          d4$aw1,d4$aw2,d4$aw3,d4$aw4,d4$aw5,d4$r1,d4$r2,d4$r3,d4$r4,d4$r5),
            d5=data.frame(d5$m1,d5$m2,d5$m3,d5$m4,d5$m5,d5$t1,d5$t2,d5$t3,d5$t4,d5$t5,
                          d5$aw1,d5$aw2,d5$aw3,d5$aw4,d5$aw5,d5$r1,d5$r2,d5$r3,d5$r4,d5$r5),
            d6=data.frame(d6$m1,d6$m2,d6$m3,d6$m4,d6$m5,d6$t1,d6$t2,d6$t3,d6$t4,d6$t5,
                          d6$aw1,d6$aw2,d6$aw3,d6$aw4,d6$aw5,d6$r1,d6$r2,d6$r3,d6$r4,d6$r5),
            d7=data.frame(d7$m1,d7$m2,d7$m3,d7$m4,d7$m5,d7$t1,d7$t2,d7$t3,d7$t4,d7$t5,
                          d7$aw1,d7$aw2,d7$aw3,d7$aw4,d7$aw5,d7$r1,d7$r2,d7$r3,d7$r4,d7$r5),
            d8=data.frame(d8$m1,d8$m2,d8$m3,d8$m4,d8$m5,d8$t1,d8$t2,d8$t3,d8$t4,d8$t5,
                          d8$aw1,d8$aw2,d8$aw3,d8$aw4,d8$aw5,d8$r1,d8$r2,d8$r3,d8$r4,d8$r5),
            d9=data.frame(d9$m1,d9$m2,d9$m3,d9$m4,d9$m5,d9$t1,d9$t2,d9$t3,d9$t4,d9$t5,
                          d9$aw1,d9$aw2,d9$aw3,d9$aw4,d9$aw5,d9$r1,d9$r2,d9$r3,d9$r4,d9$r5),
            d10=data.frame(d10$m1,d10$m2,d10$m3,d10$m4,d10$m5,d10$t1,d10$t2,d10$t3,d10$t4,d10$t5,
                           d10$aw1,d10$aw2,d10$aw3,d10$aw4,d10$aw5,d10$r1,d10$r2,d10$r3,d10$r4,d10$r5))


#rename vairable
for (i in 0:9) {
              names(x_list)[11+20*i] <- "a1"
              names(x_list)[12+20*i] <- "a2"
              names(x_list)[13+20*i] <- "a3"
              names(x_list)[14+20*i] <- "a4"
              names(x_list)[15+20*i] <- "a5"
                  }

for (i in 0:9) {
              names(x_list)[1+20*i] <- "m1"
              names(x_list)[2+20*i] <- "m2"
              names(x_list)[3+20*i] <- "m3"
              names(x_list)[4+20*i] <- "m4"
              names(x_list)[5+20*i] <- "m5"
                  }

for (i in 0:9) {
              names(x_list)[6+20*i] <- "t1"
              names(x_list)[7+20*i] <- "t2"
              names(x_list)[8+20*i] <- "t3"
              names(x_list)[9+20*i] <- "t4"
              names(x_list)[10+20*i] <- "t5"
                  }

##here is the rank
for (i in 0:9) {
              names(x_list)[16+20*i] <- "a1"
              names(x_list)[17+20*i] <- "a2"
              names(x_list)[18+20*i] <- "a3"
              names(x_list)[19+20*i] <- "a4"
              names(x_list)[20+20*i] <- "a5"
                  }


#self-rating made as data.frame
ds1<- data.frame (x_list[1:15])
ds2<- data.frame (x_list[21:35])
ds3<- data.frame (x_list[41:55])
ds4<- data.frame (x_list[61:75])
ds5<- data.frame (x_list[81:95])
ds6<- data.frame (x_list[101:115])
ds7<- data.frame (x_list[121:135])
ds8<- data.frame (x_list[141:155])
ds9<- data.frame (x_list[161:175])
ds10<- data.frame (x_list[181:195])


#ranking
dr1<- data.frame (x_list[1:10],x_list[16:20])
dr2<- data.frame (x_list[21:30],x_list[36:40])
dr3<- data.frame (x_list[41:50],x_list[56:60])
dr4<- data.frame (x_list[61:70],x_list[76:80])
dr5<- data.frame (x_list[81:90],x_list[96:100])
dr6<- data.frame (x_list[101:110],x_list[116:120])
dr7<- data.frame (x_list[121:130],x_list[136:140])
dr8<- data.frame (x_list[141:150],x_list[156:160])
dr9<- data.frame (x_list[161:170],x_list[176:180])
dr10<- data.frame (x_list[181:190],x_list[196:200])


#Usami's code
#Cross-lagged panel model

CLPM <- '
a1~mua1*1 
m1~mum1*1 
a2~mua2*1 
m2~mum2*1 
a3~mua3*1 
m3~mum3*1 
a4~mua4*1 
m4~mum4*1  
a5~mua5*1 
m5~mum5*1 


a1~~0*a1
m1~~0*m1 
a2~~0*a2 
m2~~0*m2 
a3~~0*a3 
m3~~0*m3 
a4~~0*a4 
m4~~0*m4 
a5~~0*a5 
m5~~0*m5 

FFa1~0*1 #the intercept of FFa1 is 0 
FFm1~0*1 #the intercept of FFm1 is 0 
FFa1~~phia*FFa1 #variance of FFa1 is phia
FFm1~~phim*FFm1 #variance of FFm1 is phim
FFa1~~phiam*FFm1 #covariance of FFa1 and FFm1 is phiam (yellow curved arrow)

FFm2~ betam*FFm1+gammam*FFa2 #FFm2 regresses onto FFm1 and FFa1 with betam (self-feedback) and gammam (coupling) parameters
FFa2~ betaa*FFa1+gammaa*FFm1 #the green & red arrows

FFm3~ betam*FFm2+gammam*FFa3 #current achievement have an effect on motivation 
FFa3~ betaa*FFa2+gammaa*FFm2

FFm4~ betam*FFm3+gammam*FFa4 
FFa4~ betaa*FFa3+gammaa*FFm3

FFm5~ betam*FFm4+gammam*FFa5 
FFa5~ betaa*FFa4+gammaa*FFm4

FFa2~~Omegaa*FFa2 #variance of FFa2 is Omegaa
FFa3~~Omegaa*FFa3 
FFa4~~Omegaa*FFa4 
FFa5~~Omegaa*FFa5 

FFm2~~Omegam*FFm2 #variance of FFm2 is Omegam
FFm3~~Omegam*FFm3 
FFm4~~Omegam*FFm4 
FFm5~~Omegam*FFm5 

FFm1 =~ 1*m1 #define the latent variable f*m1 (from fm) #here Abe suggests to also freely estimate insteading of fixing the parameters to 1
FFm2 =~ 1*m2 
FFm3 =~ 1*m3 
FFm4 =~ 1*m4 
FFm5 =~ 1*m5 

FFa1 =~ 1*a1 #define the latent variable a*1 (from fa)
FFa2 =~ 1*a2 
FFa3 =~ 1*a3 
FFa4 =~ 1*a4 
FFa5 =~ 1*a5 
'


#Random intercept cross-lagged model

RICLPM5 <- '
a1~mua1*1 
m1~mum1*1 
a2~mua2*1 
m2~mum2*1 
a3~mua3*1 
m3~mum3*1 
a4~mua4*1 
m4~mum4*1  
a5~mua5*1 
m5~mum5*1 


a1~~0*a1 
m1~~0*m1 
a2~~0*a2 
m2~~0*m2 
a3~~0*a3 
m3~~0*m3 
a4~~0*a4 
m4~~0*m4 
a5~~0*a5 
m5~~0*m5 


Ia~~0*FFa1
Ia~~0*FFm1 
Im~~0*FFa1 
Im~~0*FFm1

Ia=~1*a1+1*a2+1*a3+1*a4+1*a5 #define Ia 
Im=~1*m1+1*m2+1*m3+1*m4+1*m5 #define Im 

Ia~0*1 #the intercept of Ia is 0?? 
Im~0*1 #the intercept of Im is 0?? 
Ia~~taua*Ia #variance of Ia is taux 
Im~~taum*Im #variance of Im is tauy
Ia~~tauam*Im #covariance of Ia and Im is tauam

FFa1~0*1 #the intercept of FFa1 is 0 
FFm1~0*1 #the intercept of FFm1 is 0 
FFa1~~phia*FFa1 #variance of FFa1 is phia
FFm1~~phim*FFm1 #variance of FFm1 is phim
FFa1~~phiam*FFm1 #covariance of FFa1 and FFm1 is phiam (yellow curved arrow)


FFm2~ betam*FFm1+gammam*FFa2 #FFm2 regresses onto FFm1 and FFa1 with betam (self-feedback) and gammam (coupling) parameters
FFa2~ betaa*FFa1+gammaa*FFm1 #the green & red arrows

FFm3~ betam*FFm2+gammam*FFa3 #current achievement have an effect on motivation 
FFa3~ betaa*FFa2+gammaa*FFm2

FFm4~ betam*FFm3+gammam*FFa4 
FFa4~ betaa*FFa3+gammaa*FFm3

FFm5~ betam*FFm4+gammam*FFa5 
FFa5~ betaa*FFa4+gammaa*FFm4

FFa2~~Omegaa*FFa2 #variance of FFa2 is Omegaa
FFa3~~Omegaa*FFa3 
FFa4~~Omegaa*FFa4 
FFa5~~Omegaa*FFa5 

FFm2~~Omegam*FFm2 #variance of FFm2 is Omegam
FFm3~~Omegam*FFm3 
FFm4~~Omegam*FFm4 
FFm5~~Omegam*FFm5 


FFm1 =~ 1*m1 #define the latent variable f*m1 (from fm) #here Abe suggests to also freely estimate insteading of fixing the parameters to 1
FFm2 =~ 1*m2 
FFm3 =~ 1*m3 
FFm4 =~ 1*m4 
FFm5 =~ 1*m5 


FFa1 =~ 1*a1 #define the latent variable a*1 (from fa)
FFa2 =~ 1*a2 
FFa3 =~ 1*a3 
FFa4 =~ 1*a4 
FFa5 =~ 1*a5 

'

#Random curve-cross lagged model

RCCLPM <- '
# growth factors(random intercepts and slopes)
im =~ 1*m1+1*m2+1*m3+1*m4+1*m5
sm =~ 0*m1+1*m2+2*m3+3*m4+4*m5
im ~~ varim*im
sm ~~ varsm*sm
im ~ meanim*1
sm ~ meansm*1
m1+m2+m3+m4+m5 ~ 0*1

ia =~ 1*a1+1*a2+1*a3+1*a4+1*a5
sa =~ 0*a1+1*a2+2*a3+3*a4+4*a5
ia ~~ 0*sa
ia ~~ varia*ia
sa ~~ varsa*sa
ia ~ meania*1
sa ~ meansa*1
a1+a2+a3+a4+a5 ~ 0*1

#covariance between growth factors
im ~~ 0*sm
ia ~~ 0*sa
im ~~ covimia*ia
im ~~ covimsa*sa
sm ~~ 0*ia
sm ~~ covsmsa*sa

#controls for covariates

#Residuals or "deviations" from growth trajectories(impulses)
epsilon_m1 =~ 1*m1
epsilon_m2 =~ 1*m2
epsilon_m3 =~ 1*m3
epsilon_m4 =~ 1*m4
epsilon_m5 =~ 1*m5

epsilon_a1 =~ 1*a1
epsilon_a2 =~ 1*a2
epsilon_a3 =~ 1*a3
epsilon_a4 =~ 1*a4
epsilon_a5 =~ 1*a5

epsilon_m1+epsilon_m2+epsilon_m3+epsilon_m4+epsilon_m5 ~ 0*1
epsilon_a1+epsilon_a2+epsilon_a3+epsilon_a4+epsilon_a5 ~ 0*1

m1 ~~ 0*m1
m2 ~~ 0*m2
m3 ~~ 0*m3
m4 ~~ 0*m4
m5 ~~ 0*m5
a1 ~~ 0*a1
a2 ~~ 0*a2
a3 ~~ 0*a3
a4 ~~ 0*a4
a5 ~~ 0*a5

epsilon_m1 ~~ varm1*epsilon_m1
epsilon_m2 ~~ varm2*epsilon_m2
epsilon_m3 ~~ varm3*epsilon_m3
epsilon_m4 ~~ varm4*epsilon_m4
epsilon_m5 ~~ varm5*epsilon_m5
epsilon_a1 ~~ vara1*epsilon_a1
epsilon_a2 ~~ vara2*epsilon_a2
epsilon_a3 ~~ vara3*epsilon_a3
epsilon_a4 ~~ vara4*epsilon_a4
epsilon_a5 ~~ vara5*epsilon_a5

 
#Autoregressive effects of residuals (equality constraints high school for motivation)
epsilon_m2 ~ beta_mm*epsilon_m1
epsilon_m3 ~ beta_mm*epsilon_m2
epsilon_m4 ~ beta_mm*epsilon_m3
epsilon_m5 ~ beta_mm*epsilon_m4
epsilon_a2 ~ beta_aa*epsilon_a1
epsilon_a3 ~ beta_aa*epsilon_a2
epsilon_a4 ~ beta_aa*epsilon_a3
epsilon_a5 ~ beta_aa*epsilon_a4

#Cross-lagged effects of residuals (equality constraints high school)
epsilon_a2 ~ beta_am*epsilon_m1
epsilon_a3 ~ beta_am*epsilon_m2
epsilon_a4 ~ beta_am*epsilon_m3
epsilon_a5 ~ beta_am*epsilon_m4
epsilon_m2 ~ beta_ma*epsilon_a2
epsilon_m3 ~ beta_ma*epsilon_a3
epsilon_m4 ~ beta_ma*epsilon_a4
epsilon_m5 ~ beta_ma*epsilon_a5

'

####trivariate RI-CLPM###

RICLPMT5M <- '
a1~mua1*1 #fix the intercept of a1 to mean of a1, which is a freely estimated parameter 
m1~mum1*1 #this step already defines the latent variableS
a2~mua2*1 
m2~mum2*1 
a3~mua3*1 
m3~mum3*1 
a4~mua4*1 
m4~mum4*1  
a5~mua5*1 
m5~mum5*1 


a1~~0*a1 #variance of a1 is 0
m1~~0*m1 
a2~~0*a2 
m2~~0*m2 
a3~~0*a3 
m3~~0*m3 
a4~~0*a4 
m4~~0*m4 
a5~~0*a5 
m5~~0*m5 

Ia~~0*FFa1 # covariance of Ia and FFa1 is fixed to 0 -> Ia, Im, FFa1, and FFm1 are orthogonal factors
Ia~~0*FFm1 
Ia~~0*FFt1
Im~~0*FFa1 
Im~~0*FFm1
Im~~0*FFt1
It~~0*FFm1
It~~0*FFa1
It~~0*FFt1

Ia=~1*a1+1*a2+1*a3+1*a4+1*a5 #define Ia 
Im=~1*m1+1*m2+1*m3+1*m4+1*m5  #define Im 
It=~1*t1+1*t2+1*t3+1*t4+1*t5 #define It 

Ia~0*1 #the intercept of Ia is 0?? 
Im~0*1 #the intercept of Im is 0?? 
It~0*1
Ia~~taua*Ia #variance of Ia is taux 
Im~~taum*Im #variance of Im is tauy
It~~taut*It #variance of It is taut

Ia~~tauat*It #covariance of Ia and It is tauat
Im~~taumt*It #covariance of Im and It is taumt
Ia~~tauam*Im #covariance of Im and Ia is tauam

FFa1~0*1 #the intercept of FFa1 is 0 
FFm1~0*1 #the intercept of FFm1 is 0 
FFt1~0*1 #the intercept of FFt1 is 0

FFa1~~phia*FFa1 #variance of FFa1 is phia
FFm1~~phim*FFm1 #variance of FFm1 is phim
FFt1~~phit*FFt1 #variance of FFt1 is phit

FFa1~~phiat*FFt1 #covariance of FFa1 and FFt1 is phiat (yellow curved arrow)
FFa1~~phiam*FFm1 #covariance of FFa1 and FFm1 is phiam (yellow curved arrow)
FFt1~~phitm*FFm1 #covariance of FFt1 and FFm1 is phitm (yellow curved arrow)

FFm2~ betam*FFm1+gammam*FFa2 #FFm2 regresses onto FFm1 and FFa1 with betam (self-feedback) and gammam (coupling) parameters
FFt2~ betat*FFt1+epsilont*FFm2 #the gray (self-feedback) and blue (coupling) arrows
FFa2~ betaa*FFa1+zetaa*FFt1 #FFa2 regresses onto FFa1 (self-feedback) and zeta (the green & purple arrow)

FFm3~ betam*FFm2+gammam*FFa3
FFt3~ betat*FFt2+epsilont*FFm3
FFa3~ betaa*FFa2+zetaa*FFt2

FFm4~ betam*FFm3+gammam*FFa4 
FFt4~ betat*FFt3+epsilont*FFm4
FFa4~ betaa*FFa3+zetaa*FFt3

FFm5~ betam*FFm4+gammam*FFa5 
FFt5~ betat*FFt4+epsilont*FFm5
FFa5~ betaa*FFa4+zetaa*FFt4


FFa2~~Omegaa*FFa2 #variance of FFa2 is Omegaa
FFa3~~Omegaa*FFa3 
FFa4~~Omegaa*FFa4 
FFa5~~Omegaa*FFa5 

FFm2~~Omegam*FFm2 #variance of FFm2 is Omegam
FFm3~~Omegam*FFm3 
FFm4~~Omegam*FFm4 
FFm5~~Omegam*FFm5 

FFt2~~Omegat*FFt2 #variance of FFt2 is Omegat
FFt3~~Omegat*FFt3 
FFt4~~Omegat*FFt4 
FFt5~~Omegat*FFt5 

FFm1 =~ 1*m1 #define the latent variable f*m1 (from fm) #here Abe suggests to also freely estimate insteading of fixing the parameters to 1
FFm2 =~ 1*m2 
FFm3 =~ 1*m3 
FFm4 =~ 1*m4 
FFm5 =~ 1*m5 

FFa1 =~ 1*a1 #define the latent variable a*1 (from fa)
FFa2 =~ 1*a2 
FFa3 =~ 1*a3 
FFa4 =~ 1*a4 
FFa5 =~ 1*a5 

FFt1 =~ 1*t1 #define the latent variable t*1 (from ft) currently not in the graph
FFt2 =~ 1*t2 
FFt3 =~ 1*t3 
FFt4 =~ 1*t4 
FFt5 =~ 1*t5 
'

