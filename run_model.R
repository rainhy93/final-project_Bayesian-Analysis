library(dplyr)
library(na.tools)

data <- read.csv(file = "STAT578 Final Project.csv", header = T)
data <- data[-c(1)] 

data.new <- na.rm(data)

cor(data.new$balance, data.new$grip, use = "complete.obs")

data.new$HHID <- as.factor(data.new$HHID)

count <- data.new %>% count(HHID)



names(data.new)[names(data.new) == "Wellbeing12"] <- "Wellbeing"
names(data.new)[names(data.new) == "education"] <- "edu"
names(data.new)[names(data.new) == "gaitspeed12"] <- "gait"
names(data.new)[names(data.new) == "grip12"] <- "grip"
names(data.new)[names(data.new) == "balancetime12"] <- "balance"


hist(data.new$Wellbeing)
hist(data.new$age)
hist(data.new$edu, freq = FALSE)
hist(log(data.new$BMI))
var(data.new$BMI)
hist(log(data.new$gait))
var(data.new$gait)
hist(data.new$grip)
var(data.new$grip)
hist(data.new$balance)


##filter out individuals with a partner
data.new = inner_join(data.new, count, by = 'HHID')
colnames(data.new)[10] = 'n_person'

data_groups = data.new[which(data.new$n_person > 1),]

hist(data_groups$Wellbeing)
hist(data_groups$age)
hist(data_groups$edu, freq = FALSE)
hist(log(data_groups$BMI))
var(data_groups$BMI)
hist(log(data_groups$gait))
var(data_groups$gait)
hist(data_groups$grip)

boxplot(data.new$Wellbeing ~ data.new$n_person)







##model running and convergence diagnostics
data_groups$HHID = droplevels(data_groups$HHID)


d1 <- list(Wellbeing = factor(data_groups$Wellbeing),
           age = as.vector(scale(data_groups$age)),
           edu = as.vector(scale(data_groups$edu)),
           gait = as.vector(scale(data_groups$gait)),
           grip = as.vector(scale(data_groups$grip)),
           #balance = as.vector(scale(data.new$balance,
           #                                scale=2*sd(data.new$balance))),
           BMI = as.vector(scale(data_groups$BMI)),
           HHID = data_groups$HHID,
           ngroups = length(unique(data_groups$HHID)),
           beta0 = rep(0,5),
           sigmabetainv = 100^(-2)*diag(5)
           )


inits1 <- list(list(c0 = c(-2,-1,0,1)),
               list(c0 = c(-1,-0.5,0,0.5))
               )
               #list(c0 = c(-2.5,-1.5,0,1.5)),
               #list(c0 = c(-1.5,-1,0,1)))

library(rjags)
load.module("glm")
list.modules()


m1 <- jags.model('HRS.5.bug', d1, inits1, n.chains = 2)

update(m1, 1000)

x1 = coda.samples(m1, c('beta','sigmaalpha','c'), n.iter = 2000)
gelman.diag(x1, autoburnin = FALSE) #run more
x1 = coda.samples(m1, c('beta','sigmaalpha','c'), n.iter = 4000)
gelman.diag(x1, autoburnin = FALSE) #run more
x1 = coda.samples(m1, c('beta','sigmaalpha','c'), n.iter = 8000)
gelman.diag(x1, autoburnin = FALSE) #run more
x1 = coda.samples(m1, c('beta','sigmaalpha','c'), n.iter = 16000)
gelman.diag(x1, autoburnin = FALSE)
effectiveSize(x1[,])


plot(x1[,'sigmaalpha'], smooth = FALSE) #mixing is good, approx. density is smooth
plot(x1[,'c[1]'], smooth = FALSE) #good mixing, smooth approx. density
plot(x1[,'c[2]'], smooth = FALSE) #good mixing, smooth approx. density
plot(x1[,'c[3]'], smooth = FALSE) #good mixing, smooth approx. density
plot(x1[,'c[4]'], smooth = FALSE)  #good mixing, smooth approx. density
plot(x1[,'beta[1]'], smooth = FALSE)  #good mixing, smooth approx. density
plot(x1[,'beta[2]'], smooth = FALSE)  #good mixing, smooth approx. density
plot(x1[,'beta[3]'], smooth = FALSE)  #good mixing, smooth approx. density
plot(x1[,'beta[4]'], smooth = FALSE)  #good mixing, smooth approx. density
plot(x1[,'beta[5]'], smooth = FALSE)  #good mixing, smooth approx. density

summary(x1[,c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]', 'beta[5]')])
summary(x1[,'sigmaalpha'])

x1 = coda.samples(m1, c('beta','sigmaalpha','c','alpha'), n.iter = 20000)
post.samp = as.matrix(x1)
dim(post.samp)

hist(apply(post.samp[,paste0("alpha[",1:571,"]")], 2, mean))
