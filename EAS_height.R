#new 
#/BiO/hae/phase6/00_QT/Height/pheno
#PGS002803 (BBJ, PGS003890 X)

library(data.table)
library(dplyr)
library(ggplot2)

hexam20 <- read.table('hexa_height.m.20',header=T)
hexam80 <- read.table('hexa_height.m.80',header=T)
hexaf20 <- read.table('hexa_height.f.20',header=T)
hexaf80 <- read.table('hexa_height.f.80',header=T)
PGS <- read.table("../PGScatalog/PGS002803.txt.v2.HEXA.profile",header=T)
PGS <- PGS[,c(1,6)]
 hexaf80 <- left_join(hexaf80,PGS,by="FID")
 hexaf20 <- left_join(hexaf20,PGS,by="FID")
 hexam80 <- left_join(hexam80,PGS,by="FID")
 hexam20 <- left_join(hexam20,PGS,by="FID")
 model_hexaf80 <- lm(height ~ scale(SCORESUM) + age ,data=hexaf80)
model_hexam80 <- lm(height ~ scale(SCORESUM) + age ,data=hexam80)

 hexaf20 <- data.frame(hexaf20)
 hexam20 <- data.frame(hexam20)
 hexaf20["pred"] <- model_hexaf80$coefficients[1] + scale(hexaf20$SCORESUM)*model_hexaf80$coefficients[2] + hexaf20$age*model_hexaf80$coefficients[3]
 hexam20["pred"] <- model_hexam80$coefficients[1] + scale(hexam20$SCORESUM)*model_hexam80$coefficients[2] + hexam20$age*model_hexam80$coefficients[3]

 hexam20['error'] <- abs(hexam20$pred-hexam20$height)
 hexaf20['error'] <- abs(hexaf20$pred-hexaf20$height)

cor.test(hexam20$height,hexam20$pred)
cor.test(hexaf20$height,hexaf20$pred)

test <- hexam20

plot_y <- NULL
plot_x <- NULL
tmp_x <- NULL
tmp_y <- NULL
plot_y <- data.frame()
for (i in seq(0,10, by = 0.1)) {
 b <- paste("Range",i,sep="")
 test[b] <- ifelse(abs(test$error) < i ,1,0) 
  tmp_y <- data.frame(i)
 plot_y <- rbind(plot_y,tmp_y)
 }

plot_x <- NULL
plot_x <- data.frame()
for (i in seq(13, 113, by = 1)) {
 a <- (sum(test[i])/dim(test)[1])
 tmp_x <- data.frame(a)
 plot_x  <- rbind(plot_x,tmp_x)
 }

plot_xy <- cbind(plot_x,plot_y)
plot_xy

ggplot(plot_xy,aes(x=i,y=a)) + geom_point() + geom_hline(yintercept=0.8,linetype='dashed',color='red') + labs(x="Error range",y="Accuracy",title="Standing height in Male (PGS002803)")


test <- hexaf20

plot_y <- NULL
plot_x <- NULL
tmp_x <- NULL
tmp_y <- NULL
plot_y <- data.frame()
for (i in seq(0,10, by = 0.1)) {
 b <- paste("Range",i,sep="")
 test[b] <- ifelse(abs(test$error) < i ,1,0) 
  tmp_y <- data.frame(i)
 plot_y <- rbind(plot_y,tmp_y)
 }

plot_x <- NULL
plot_x <- data.frame()
for (i in seq(13, 113, by = 1)) {
 a <- (sum(test[i])/dim(test)[1])
 tmp_x <- data.frame(a)
 plot_x  <- rbind(plot_x,tmp_x)
 }

plot_xy <- cbind(plot_x,plot_y)
plot_xy

ggplot(plot_xy,aes(x=i,y=a)) + geom_point() + geom_hline(yintercept=0.8,linetype='dashed',color='red') + labs(x="Error range",y="Accuracy",title="Standing height in Female (PGS002803)")

