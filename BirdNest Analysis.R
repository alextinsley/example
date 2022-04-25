

# Bird Nest analysis 
# name: ??

# Nov 2019 

# use this code to load the data 


wants <- c("ggfortify", "here")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#load library 

library(here)
library(ggplot2)

# load data and check the top 6 rows. 

nest<-read.csv(here( "BirdNest.csv"))
head(nest)



###### copy and past your code below here.......

summary(nest)

table(nest$Nesttype)
table(nest$Location)

plot(nest$Length, nest$Nestling)
plot(nest$Length, nest$No.eggs)

boxplot(Nestling~Nesttype, data=nest)

ggplot(nest, aes(Length, Nestling, colour=as.factor(Closed.)))+geom_point(shape=1)

hist(nest$No.eggs)


#Model building

M1.nest<-glm(Nestling~Length+Nesttype+Location+No.eggs, data=nest)
summary(M1.nest)

plot(M1.nest)

library(ggfortify)
autoplot(M1.nest)

M2.nest<-glm(Nestling~Length+Closed.+Location+No.eggs, data=nest)
summary(M2.nest)

M3.nest<-glm(Nestling~Length+Closed.+Location, data=nest)
summary(M3.nest)

