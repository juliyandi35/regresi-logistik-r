library(readxl)
dt <- read_excel("Data tanpa nomor new.xlsx")
head(dt)
str(dt)
summary(dt)

# MLE
LM_mll <- function(formula, data = environment(formula))
{
  y <- model.response(model.frame(formula, data))
  X <- model.matrix(formula, data)
  b0 <- numeric(NCOL(X))
  names(b0) <- colnames(X)
  function(b=b0, sigma=1)
    -sum(dnorm(y, X %*% b, sigma, log=TRUE))
}

mll <- LM_mll(`Tindakan Pasien (Y)`~., data = dt)

summary(lm(`Tindakan Pasien (Y)`~., data = dt)) # for comparison -- notice variance bias in MLE
summary(mle(mll, lower=list(sigma = 0.01))) # alternative specification

confint(mle(mll, lower=list(sigma = 0.01)))
plot(profile(mle(mll, lower=list(sigma = 0.01))))

# Plotting data dengan Pie chart
library(plotrix)
Y = table(dt$`Tindakan Pasien (Y)`)
Y =c(690,67)
label=c("Tidak ada tindakan = ","Adanya tindakan = ")
persen=round(Y/sum(Y)*100) ##buat persentase
label=paste(label,persen)
label=paste(label,'%',sep ='')
pie3D(Y,labels=label,col=c('yellow','blue'),
      main="Persentase Tindakan Pasien")

# Plot data dengan Histogram
library(ggplot2)
ggplot(dt, aes(`Tindakan Pasien (Y)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(dt, aes(`Status Pasien (X1)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(dt, aes(`Penjamin (X2)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(dt, aes(`Ruangan (X3)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(dt, aes(`Kasus Pasien (X4)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(dt, aes(`Kelengkapan Alat Kesehatan (X5)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(dt, aes(`Sumber Daya Manusia (X6)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")

# Uji korelasi
library("PerformanceAnalytics")
chart.Correlation(dt, histogram=TRUE, pch=19)

# Pembangunan model dugaan
logit1 <- glm(`Tindakan Pasien (Y)`~., data = dt, family = binomial(link="logit"))
summary(logit1)

library(pscl)
pR2(logit1)
qchisq(0.95, 756)

logit2 <- glm(`Tindakan Pasien (Y)`~`Status Pasien (X1)`+`Ruangan (X3)`+`Kasus Pasien (X4)`+`Kelengkapan Alat Kesehatan (X5)`+`Sumber Daya Manusia (X6)`, data = dt, family = binomial(link="logit"))
summary(logit2)

logit3 <- glm(`Tindakan Pasien (Y)`~`Status Pasien (X1)`+`Ruangan (X3)`+`Kasus Pasien (X4)`+`Kelengkapan Alat Kesehatan (X5)`, data = dt, family = binomial(link="logit"))
summary(logit3)

logit4 <- glm(`Tindakan Pasien (Y)`~`Status Pasien (X1)`+`Kasus Pasien (X4)`+`Kelengkapan Alat Kesehatan (X5)`, data = dt, family = binomial(link="logit"))
summary(logit4)

model <- c("Model 1","Model 2","Model 3","Model 4")
AIC <- c(logit1$aic,logit2$aic,logit3$aic,logit4$aic)
kriteria <- data.frame(model,AIC)
kriteria


library(ResourceSelection)
hoslem.test(logit4$y, fitted(logit4))
qchisq(0.95, 1)

pR2(logit4)
logitt <- glm(`Tindakan Pasien (Y)`~1, data=dt, family= binomial(link="logit"))
1-as.vector(logLik(logit4)/logLik(logitt))

# Odds Ratio
Table.Prob <- table(true=dt$`Tindakan Pasien (Y)`,pred=round(fitted(logit4)))
Table.Prob
prop.out <- prop.table(Table.Prob)
prop.out

#peluang proficiency yes
prof1=prop.out[1,2]+prop.out[2,2]
#odds of proficiency yes
oddsprof1=prof1/(1-prof1)
oddsprof1

#Peluang proficiency yes bersyarat Nes:
prop.outnes1 <- prop.table(Table.Prob,margin=1)
prop.outnes1

#Odds of proficiency yes bersyarat Nes yes
oddsprof1nes<-prop.outnes1[2,2]/prop.outnes1[2,1]
oddsprof1nes

#Odds of proficiency yes bersyarat Nes no
oddsprof1nes0<-prop.outnes1[1,2]/prop.outnes1[1,1]
oddsprof1nes0

# Odds ratio adalah ratio antara odds bersyarat NES yes dengan Odds bersyarat Nes no
oddsratio<-oddsprof1nes/oddsprof1nes0
oddsratio
