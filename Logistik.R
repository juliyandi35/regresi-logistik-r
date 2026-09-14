library(ggplot2)
library(readxl)
data <- read_excel("Data tanpa nomor new.xlsx")
head(data)
summary(data)
ggplot(data, aes(data$`Tindakan Pasien (Y)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(data, aes(data$`Status Pasien (X1)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(data, aes(data$`Penjamin (X2)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(data, aes(data$`Ruangan (X3)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(data, aes(data$`Kasus Pasien (X4)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(data, aes(data$`Kelengkapan Alat Kesehatan (X5)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")
ggplot(data, aes(data$`Sumber Daya Manusia (X6)`,fill=..count..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="purple")

YES <- sum(data$`Tindakan Pasien (Y)` == 1)
NO <- sum(data$`Tindakan Pasien (Y)` == 0)
slices <- c(YES,NO)
lbls<- c("Adanya Tindakan Pasien","Tidak Adanya Tindakan Pasien")
lbls <- paste(lbls, slices)
lbls <- paste(lbls)
par(mfrow=c(1,1))
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart Tindakan Pasien")

cor(data, method="spearman")

plot(data, col=data$`Tindakan Pasien (Y)`)

logit1 <- glm(`Tindakan Pasien (Y)`~., data = data, family = binomial(link="logit"))
summary(logit1)

logit2 <- glm(`Tindakan Pasien (Y)`~`Status Pasien (X1)`+`Ruangan (X3)`+`Kasus Pasien (X4)`+`Kelengkapan Alat Kesehatan (X5)`+`Sumber Daya Manusia (X6)`, data = data, family = binomial(link="logit"))
summary(logit2)

logit3 <- glm(`Tindakan Pasien (Y)`~`Status Pasien (X1)`+`Ruangan (X3)`+`Kasus Pasien (X4)`+`Kelengkapan Alat Kesehatan (X5)`, data = data, family = binomial(link="logit"))
summary(logit3)

logit4 <- glm(`Tindakan Pasien (Y)`~`Status Pasien (X1)`+`Kasus Pasien (X4)`+`Kelengkapan Alat Kesehatan (X5)`, data = data, family = binomial(link="logit"))
summary(logit4)

model <- c("Model 1","Model 2","Model 3","Model 4")
AIC <- c(logit1$aic,logit2$aic,logit3$aic,logit4$aic)
kriteria <- data.frame(model,AIC)
kriteria

logitt <- glm(`Tindakan Pasien (Y)`~1, data=data, family =binomial(link="logit"))
1-as.vector(logLik(logit4)/logLik(logitt))

table(true=data$`Tindakan Pasien (Y)`,pred=round(fitted(logit4)))

