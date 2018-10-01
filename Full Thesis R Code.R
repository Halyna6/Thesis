library(JudgeIt)
data(house6311)
head(house6311)
attach(house6311)
names(house6311)
names(house6311$`1898`)
house6311[[1]]
head(house6311[[4]])

egfun<- function(x){
  DEMCOUNT<-round(VOTE*TURNOUT)
  REPCOUNT<-TURNOUT-DEMCOUNT
  DEMWIN<- ifelse(VOTE>0.5,1,0)
  REPWIN<-ifelse(VOTE<0.5,1,0)
  DEMWASTE<-ifelse(DEMWIN==0, DEMCOUNT,
                   DEMCOUNT-TURNOUT*0.5)
  REPWASTE<-ifelse(REPWIN==0, REPCOUNT,
                   REPCOUNT-TURNOUT*0.5)
  TDEMWASTE<-xtabs(formula=DEMWASTE~STATE,
                   data=x)
  TREPWASTE<-xtabs(formula=REPWASTE~STATE,
                   data=x)
  TTURNOUT<-xtabs(formula=TURNOUT~STATE, data=x)
  EG<-cbind(TDEMWASTE, TREPWASTE)
  EG<-cbind(EG, TTURNOUT)
  DEMEG<-(EG[,2]-EG[,1])/EG[,3]
  
}
attach(house6311$`1896`)
house1<-egfun(house6311[[1]])
attach(house6311$`1898`)
house2<-egfun(house6311[[2]])
attach(house6311$`1900`)
house3<-egfun(house6311[[3]])
house1<-data.frame(house1)
house2<-data.frame(house2)
house3<-data.frame(house3)
house1$statecode<-rownames(house1)
house2$statecode<-rownames(house2)
house3$statecode<-rownames(house3)
eg1896<-merge(house1,house2,all=TRUE)
eg1896<-merge(eg1896,house3,all=TRUE)
setwd("C:\\Users\\Halyna\\Google Drive\\
      KU Leuven\\Thesis\\Thesis R")
save(eg1896, file="eg1896.Rda")
attach(house6311$`1902`)
house4<-egfun(house6311[[4]])
attach(house6311$`1904`)
house5<-egfun(house6311[[5]])
attach(house6311$`1906`)
house6<-egfun(house6311[[6]])
attach(house6311$`1908`)
house7<-egfun(house6311[[7]])
attach(house6311$`1910`)
house8<-egfun(house6311[[8]])
house4<-data.frame(house4)
house5<-data.frame(house5)
house6<-data.frame(house6)
house7<-data.frame(house7)
house8<-data.frame(house8)
house4$statecode<-rownames(house4)
house5$statecode<-rownames(house5)
house6$statecode<-rownames(house6)
house7$statecode<-rownames(house7)
house8$statecode<-rownames(house8)
eg1902<-merge(house4,house5,all=TRUE)
eg1902<-merge(eg1902,house6,all=TRUE)
eg1902<-merge(eg1902,house7,all=TRUE)
eg1902<-merge(eg1902,house8,all=TRUE)
save(eg1902, file="eg1902.Rda")
attach(house6311$`1912`)
house9<-egfun(house6311[[9]])
attach(house6311$`1914`)
house10<-egfun(house6311[[10]])
attach(house6311$`1916`)
house11<-egfun(house6311[[11]])
attach(house6311$`1918`)
house12<-egfun(house6311[[12]])
attach(house6311$`1920`)
house13<-egfun(house6311[[13]])
house9<-data.frame(house9)
house10<-data.frame(house10)
house11<-data.frame(house11)
house12<-data.frame(house12)
house13<-data.frame(house13)
house9$statecode<-rownames(house9)
house10$statecode<-rownames(house10)
house11$statecode<-rownames(house11)
house12$statecode<-rownames(house12)
house13$statecode<-rownames(house13)
eg1912<-merge(house9,house10,all=TRUE)
eg1912<-merge(eg1912,house11,all=TRUE)
eg1912<-merge(eg1912,house12,all=TRUE)
eg1912<-merge(eg1912,house13,all=TRUE)
save(eg1912, file="eg1912.Rda")
attach(house6311$`1922`)
house14<-egfun(house6311[[14]])
attach(house6311$`1924`)
house15<-egfun(house6311[[15]])
attach(house6311$`1926`)
house16<-egfun(house6311[[16]])
attach(house6311$`1928`)
house17<-egfun(house6311[[17]])
attach(house6311$`1930`)
house18<-egfun(house6311[[18]])
house14<-data.frame(house14)
house15<-data.frame(house15)
house16<-data.frame(house16)
house17<-data.frame(house17)
house18<-data.frame(house18)
house14$statecode<-rownames(house14)
house15$statecode<-rownames(house15)
house16$statecode<-rownames(house16)
house17$statecode<-rownames(house17)
house18$statecode<-rownames(house18)
eg1922<-merge(house14,house15,all=TRUE)
eg1922<-merge(eg1922,house16,all=TRUE)
eg1922<-merge(eg1922,house17,all=TRUE)
eg1922<-merge(eg1922,house18,all=TRUE)
save(eg1922, file="eg1922.Rda")
attach(house6311$`1932`)
house19<-egfun(house6311[[19]])
attach(house6311$`1934`)
house20<-egfun(house6311[[20]])
attach(house6311$`1936`)
house21<-egfun(house6311[[21]])
attach(house6311$`1938`)
house22<-egfun(house6311[[22]])
attach(house6311$`1940`)
house23<-egfun(house6311[[23]])
house19<-data.frame(house19)
house20<-data.frame(house20)
house21<-data.frame(house21)
house22<-data.frame(house22)
house23<-data.frame(house23)
house19$statecode<-rownames(house19)
house20$statecode<-rownames(house20)
house21$statecode<-rownames(house21)
house22$statecode<-rownames(house22)
house23$statecode<-rownames(house23)
eg1932<-merge(house19,house20,all=TRUE)
eg1932<-merge(eg1932,house21,all=TRUE)
eg1932<-merge(eg1932,house22,all=TRUE)
eg1932<-merge(eg1932,house23,all=TRUE)
save(eg1932, file="eg1932.Rda")
attach(house6311$`1942`)
house24<-egfun(house6311[[24]])
attach(house6311$`1944`)
house25<-egfun(house6311[[25]])
attach(house6311$`1946`)
house26<-egfun(house6311[[26]])
attach(house6311$`1948`)
house27<-egfun(house6311[[27]])
attach(house6311$`1950`)
house28<-egfun(house6311[[28]])
house24<-data.frame(house24)
house25<-data.frame(house25)
house26<-data.frame(house26)
house27<-data.frame(house27)
house28<-data.frame(house28)
house24$statecode<-rownames(house24)
house25$statecode<-rownames(house25)
house26$statecode<-rownames(house26)
house27$statecode<-rownames(house27)
house28$statecode<-rownames(house28)
eg1942<-merge(house24,house25,all=TRUE)
eg1942<-merge(eg1942,house26,all=TRUE)
eg1942<-merge(eg1942,house27,all=TRUE)
eg1942<-merge(eg1942,house28,all=TRUE)
save(eg1942, file="eg1942.Rda")
attach(house6311$`1952`)
house29<-egfun(house6311[[29]])
attach(house6311$`1954`)
house30<-egfun(house6311[[30]])
attach(house6311$`1956`)
house31<-egfun(house6311[[31]])
attach(house6311$`1958`)
house32<-egfun(house6311[[32]])
attach(house6311$`1960`)
house33<-egfun(house6311[[33]])
house29<-data.frame(house29)
house30<-data.frame(house30)
house31<-data.frame(house31)
house32<-data.frame(house32)
house33<-data.frame(house33)
house29$statecode<-rownames(house29)
house30$statecode<-rownames(house30)
house31$statecode<-rownames(house31)
house32$statecode<-rownames(house32)
house33$statecode<-rownames(house33)
eg1952<-merge(house29,house30,all=TRUE)
eg1952<-merge(eg1952,house31,all=TRUE)
eg1952<-merge(eg1952,house32,all=TRUE)
eg1952<-merge(eg1952,house33,all=TRUE)
save(eg1952, file="eg1952.Rda")
attach(house6311$`1962`)
house34<-egfun(house6311[[34]])
attach(house6311$`1964`)
house35<-egfun(house6311[[35]])
attach(house6311$`1966`)
house36<-egfun(house6311[[36]])
attach(house6311$`1968`)
house37<-egfun(house6311[[37]])
attach(house6311$`1970`)
house38<-egfun(house6311[[38]])
house34<-data.frame(house34)
house35<-data.frame(house35)
house36<-data.frame(house36)
house37<-data.frame(house37)
house38<-data.frame(house38)
house34$statecode<-rownames(house34)
house35$statecode<-rownames(house35)
house36$statecode<-rownames(house36)
house37$statecode<-rownames(house37)
house38$statecode<-rownames(house38)
eg1962<-merge(house34,house35,all=TRUE)
eg1962<-merge(eg1962,house36,all=TRUE)
eg1962<-merge(eg1962,house37,all=TRUE)
eg1962<-merge(eg1962,house38,all=TRUE)
save(eg1962, file="eg1962.Rda")
attach(house6311$`1972`)
house39<-egfun(house6311[[39]])
attach(house6311$`1974`)
house40<-egfun(house6311[[40]])
attach(house6311$`1976`)
house41<-egfun(house6311[[41]])
attach(house6311$`1978`)
house42<-egfun(house6311[[42]])
attach(house6311$`1980`)
house43<-egfun(house6311[[43]])
house39<-data.frame(house39)
house40<-data.frame(house40)
house41<-data.frame(house41)
house42<-data.frame(house42)
house43<-data.frame(house43)
house39$statecode<-rownames(house39)
house40$statecode<-rownames(house40)
house41$statecode<-rownames(house41)
house42$statecode<-rownames(house42)
house43$statecode<-rownames(house43)
eg1972<-merge(house39,house40,all=TRUE)
eg1972<-merge(eg1972,house41,all=TRUE)
eg1972<-merge(eg1972,house42,all=TRUE)
eg1972<-merge(eg1972,house43,all=TRUE)
save(eg1972, file="eg1972.Rda")
attach(house6311$`1982`)
house44<-egfun(house6311[[44]])
attach(house6311$`1984`)
house45<-egfun(house6311[[45]])
attach(house6311$`1986`)
house46<-egfun(house6311[[46]])
attach(house6311$`1988`)
house47<-egfun(house6311[[47]])
attach(house6311$`1990`)
house48<-egfun(house6311[[48]])
attach(house6311$`1992`)
house49<-egfun(house6311[[49]])
house44<-data.frame(house44)
house45<-data.frame(house45)
house46<-data.frame(house46)
house47<-data.frame(house47)
house48<-data.frame(house48)
house49<-data.frame(house49)
house44$statecode<-rownames(house44)
house45$statecode<-rownames(house45)
house46$statecode<-rownames(house46)
house47$statecode<-rownames(house47)
house48$statecode<-rownames(house48)
house49$statecode<-rownames(house49)
eg1982<-merge(house44,house45,all=TRUE)
eg1982<-merge(eg1982,house46,all=TRUE)
eg1982<-merge(eg1982,house47,all=TRUE)
eg1982<-merge(eg1982,house48,all=TRUE)
eg1982<-merge(eg1982,house49,all=TRUE)
save(eg1982, file="eg1982.Rda")

#load saved eg data
egfull<- merge(eg1896, eg1902, all=TRUE)
egfull<- merge(egfull, eg1912, all=TRUE)
egfull<- merge(egfull, eg1922, all=TRUE)
egfull<- merge(egfull, eg1932, all=TRUE)
egfull<- merge(egfull, eg1942, all=TRUE)
egfull<- merge(egfull, eg1952, all=TRUE)
egfull<- merge(egfull, eg1962, all=TRUE)
egfull<- merge(egfull, eg1972, all=TRUE)
egfull<- merge(egfull, eg1982, all=TRUE)
egfull<-data.matrix(egfull)
#add row and column means
statemeandata <- egfull[,2:50]
statemean <- rowMeans(statemeandata, na.rm=TRUE)
statecode <- egfull[,1] 
statemean
yearmean <- colMeans(egfull, na.rm=TRUE)
yearmean <- yearmean[2:50]
yearvar <- colVars(egfull, na.rm=TRUE)
yearvar <- yearvar[2:50]
yearstd <- colStdevs(egfull, na.rm=TRUE)
yearstd <- yearstd[2:50]
year<-seq(1896,1992,2)
plot(year, yearmean, type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Average Efficiency Gap")
segments(year, yearmean-yearstd,year, yearmean+yearstd)

#state plots for appendix
egfull[,1]
#new england
par(mfrow=c(2,2))
plot(year, egfull[1,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Connecticut")
abline(h=-0.064584155, lty=2)
plot(year, egfull[6,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Maine")
abline(h=-0.154781171, lty=2)
plot(year, egfull[12,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Massachusetts")
abline(h=-0.104870963, lty=2)
plot(year, egfull[20,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="New Hampshire")
abline(h=-0.223222712, lty=2)
plot(year, egfull[31,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Rhode Island")
abline(h=0.066870198, lty=2)
plot(year, egfull[37,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Vermont")
abline(h=-0.097004349, lty=2)

#Middle Atlantic
plot(year, egfull[2,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Delaware")
abline(h=-0.088456786, lty=2)
plot(year, egfull[3,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="New Jersey")
abline(h=-0.080425644, lty=2)
plot(year, egfull[4,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="New York")
abline(h=-0.053420260, lty=2)
plot(year, egfull[5,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Pennsylvania")
abline(h=-0.040847569, lty=2)
#East North Central
plot(year, egfull[7,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Illinois")
abline(h=-0.043348158, lty=2)
plot(year, egfull[8,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Indiana")
abline(h=-0.033884140, lty=2)
plot(year, egfull[9,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Michigan")
abline(h=-0.074438168, lty=2)
plot(year, egfull[10,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Ohio")
abline(h=-0.077010670, lty=2)
plot(year, egfull[11,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Wisconsin")
abline(h=-0.025417738, lty=2)

#West North Central
plot(year, egfull[13,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Iowa")
abline(h=-0.137221226, lty=2)
plot(year, egfull[14,2:50], type="l", ylim=c(-0.5,0.5),
     xlab = "Election Year", ylab = "Efficiency Gap",
     main="Kansas")
abline(h=-0.140501863, lty=2)
plot(year, egfull[15,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Minnesota")
abline(h=0.013831048, lty=2)
plot(year, egfull[16,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Missouri")
abline(h=0.098990417, lty=2)
plot(year, egfull[17,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Nebraska")
abline(h=-0.00858882, lty=2)
plot(year, egfull[18,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="North Dakota")
abline(h=0.038354617, lty=2)
plot(year, egfull[19,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="South Dakota")
abline(h=-0.061828799, lty=2)

#Solid South
plot(year, egfull[21,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Virginia")
abline(h=-0.071229899, lty=2)
plot(year, egfull[22,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Alabama")
abline(h=-0.243942853, lty=2)
plot(year, egfull[23,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Arkansas")
abline(h=-0.171997697, lty=2)
plot(year, egfull[24,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Florida")
abline(h=-0.152335086, lty=2)
plot(year, egfull[25,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Georgia")
abline(h=-0.316281848, lty=2)
plot(year, egfull[26,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Louisiana")
abline(h=-0.353544697, lty=2)
plot(year, egfull[27,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Mississippi")
abline(h=-0.377728444, lty=2)
plot(year, egfull[28,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="North Carolina")
abline(h=0.091541655, lty=2)
plot(year, egfull[29,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="South Carolina")
abline(h=-0.345974473, lty=2)
plot(year, egfull[30,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Texas")
abline(h=-0.231481856, lty=2)

#Border States
plot(year, egfull[32,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Kentucky")
abline(h=0.082317362, lty=2)
plot(year, egfull[33,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Maryland")
abline(h=0.041326146, lty=2)
plot(year, egfull[34,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year",
     ylab = "Efficiency Gap",main="Oklahoma")
abline(h=0.056206719, lty=2)
plot(year, egfull[35,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Tennessee")
abline(h=-0.093890643, lty=2)
plot(year, egfull[36,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="West Virginia")
abline(h=0.016033386, lty=2)

#Mountain states
plot(year, egfull[38,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Arizona")
abline(h=-0.004518149, lty=2)
plot(year, egfull[39,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Colorado")
abline(h=0.012070312, lty=2)
plot(year, egfull[40,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Idaho")
abline(h=-0.097571621, lty=2)
plot(year, egfull[41,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Montana")
abline(h=0.018463902, lty=2)
plot(year, egfull[42,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Nevada")
abline(h=0.023429845, lty=2)
plot(year, egfull[43,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="New Mexico")
abline(h=0.058674234, lty=2)
plot(year, egfull[44,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Utah")
abline(h=-0.038365731, lty=2)
plot(year, egfull[45,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Wyoming")
abline(h=-0.177975456, lty=2)

#pacific states
plot(year, egfull[46,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="California")
abline(h=0.002938470, lty=2)
plot(year, egfull[47,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Oregon")
abline(h=-0.046046009, lty=2)
plot(year, egfull[48,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Washington")
abline(h=0.030782904, lty=2)

#external states
plot(year, egfull[49,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Alaska")
abline(h=-0.076260500, lty=2)
plot(year, egfull[50,2:50], type="l", 
     ylim=c(-0.5,0.5),xlab = "Election Year", 
     ylab = "Efficiency Gap",main="Hawaii")
abline(h=-0.073505088, lty=2)

#uniform swing curves
library(pscl)
house6311$`1992`
#indiana
state1year1992<-data.frame(house6311$`1992`[110:119,4])
curve<-seatsVotes(state1year1992, method="uniformSwing")
plot(curve, type = c("seatsVotes", "density"))
plot(curve)
#NJ
state1year1992<-data.frame(house6311$`1992`[25:37,4])
curve<-seatsVotes(state1year1992, method="uniformSwing")
plot(curve, type = c("seatsVotes", "density"))
plot(curve)
curve #bias5 0.0385
#D got v=0.4977 s=0.5385
#if R got v=0.4977, s=0.4615
curve$s[252]
curve$s[249]
#Texas
house6311$'1992'$STATE==49
state1year1992<-data.frame(house6311$`1992`[281:310,4])
curve<-seatsVotes(state1year1992, method="uniformSwing")
plot(curve, type = c("seatsVotes", "density"))
plot(curve)
curve #bias5 0.16667; average vote 0.5402
curve$s[271]
curve$s[230] #actual results, D get v=0.5402 s=0.7, 
curve$v[271] # if R got v=0.5402, s=0.4667
curve$v[230]

#king & gelman model
unc <- function(inp) -1*(inp<0.05)+1*(inp>0.95)
elecyears <- as.numeric(names(house6311))
house6311$'1896'
#all US
j.ob<-judgeit(model.form=VOTE~unc(VOTE)+INC,
              vote.form=TURNOUT~1,data=house6311,
              same.districts=(elecyears%%10!=2), use.last.votes=T)
#south
j.ob<-judgeit(model.form=VOTE~unc(VOTE)+INC,
              vote.form=TURNOUT~1,data=house6311,
              same.districts=(elecyears%%10!=2), use.last.votes=T, 
              subset=DELSOUTH==1)
#nonsouth
j.ob<-judgeit(model.form=VOTE~unc(VOTE)+INC,
              vote.form=TURNOUT~1,data=house6311,
              same.districts=(elecyears%%10!=2), use.last.votes=T,
              subset=DELSOUTH==0)
j.ob$beta
summary(j.ob)
a<-bias.resp(j.ob, year=1896)
summary1896<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1898)
summary1898<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1900)
summary1900<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1902)
summary1902<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1904)
summary1904<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1906)
summary1906<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1908)
summary1908<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1910)
summary1910<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1912)
summary1912<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1914)
summary1914<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1916)
summary1916<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1918)
summary1918<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1920)
summary1920<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1922)
summary1922<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1924)
summary1924<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1926)
summary1926<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1928)
summary1928<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1930)
summary1930<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1932)
summary1932<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1934)
summary1934<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1936)
summary1936<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1938)
summary1938<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1940)
summary1940<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1942)
summary1942<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1944)
summary1944<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1946)
summary1946<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1948)
summary1948<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1950)
summary1950<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1952)
summary1952<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1954)
summary1954<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1956)
summary1956<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1958)
summary1958<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1960)
summary1960<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1962)
summary1962<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1964)
summary1964<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1966)
summary1966<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1968)
summary1968<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1970)
summary1970<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1972)
summary1972<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1974)
summary1974<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1976)
summary1976<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1978)
summary1978<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1980)
summary1980<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1982)
summary1982<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1984)
summary1984<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1986)
summary1986<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1988)
summary1988<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1990)
summary1990<-data.frame(a$svsums)
a<-bias.resp(j.ob, year=1992)
summary1992<-data.frame(a$svsums)
summary<-rbind(summary1896, summary1898, summary1900,
               summary1902, summary1904, summary1906, summary1908, 
               summary1910, summary1912, summary1914, summary1916, 
               summary1918, summary1920, summary1922, summary1924, 
               summary1926, summary1928, summary1930, summary1932, 
               summary1934, summary1936, summary1938, summary1940,
               summary1942, summary1944, summary1946, summary1948, 
               summary1950, summary1952, summary1954, summary1956, 
               summary1958, summary1960, summary1962, summary1964, 
               summary1966, summary1968, summary1970, summary1972,
               summary1974, summary1976, summary1978,summary1980, 
               summary1982, summary1984, summary1986, summary1988,
               summary1990, summary1992)
f1<-seq(1,196,4)
f2<-seq(2,196,4)
f3<-seq(3,196,4)
f4<-seq(4,196,4)
#for all of US, bias at 0.5, 0.45-0.55 
#resp at 0.45-55, resp at obs
bias5<-data.frame(summary[f1,])
bias4555<-data.frame(summary[f2,])
resp4555<-data.frame(summary[f3,])
respobs<-data.frame(summary[f4,])

#plots
year<-seq(1896, 1992, 2)
plot(year,bias5[,1], 'l', ylab = "Partisan Bias at 0.5", 
     xlab="Election Year", ylim=c(-0.2,0.2))
lines(year, bias5[,3], 'l', lty=2)
lines(year, bias5[,5], 'l', lty=2)
plot(year,bias4555[,1], 'l', 
     ylab = "Partisan Bias at 0.45-0.55", xlab="Election Year", 
     ylim=c(-0.2,0.2))
lines(year, bias4555[,3], 'l', lty=2)
lines(year, bias4555[,5], 'l', lty=2)
plot(year,resp4555[,1], 'l', 
     ylab = "Responsiveness at 0.45-0.55", xlab="Election Year", 
     ylim=c(0,3.5))
lines(year, resp4555[,3], 'l', lty=2)
lines(year, resp4555[,5], 'l', lty=2)
plot(year,respobs[,1], 'l', 
     ylab = "Responsiveness at Observed Results", 
     xlab="Election Year", ylim=c(0,3.5))
lines(year, respobs[,3], 'l', lty=2)
lines(year, respobs[,5], 'l', lty=2)

#which states are southern
south<-lapply(house6311, subset, DELSOUTH==1)
south$`1992`$STATE