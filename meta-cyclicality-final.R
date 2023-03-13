rm(list = ls()) #clear list

#automatic installation of required packages
packages <- c("xlsx","calibrate","stargazer","sandwich","lmtest","getopt","CausalGAM","ggplot2","reshape2","xts",
              "lattice","gridExtra","gtable","plm","lfe","lmtest","car","tis","foreign","MASS","quantreg","ggrepel",
              "dplyr","stringr","datasets","rio","psych","systemfit","MatchIt","CRTgeeDR","eurostat","zoo","ggthemes",
              "robumeta","metafor","dplyr","clubSandwich","Hmisc","metafor","pracma","pkgs","broom","sjPlot", "here", "data.table", "pscore")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

#load packages
library(xlsx) #Excel-Paket laden
library(calibrate) #Laden des Pakets, das f??r Datenbeschriftung n??tig ist
library (stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
library(sandwich)
library(lmtest)
library(getopt)
library(CausalGAM)
library(ggplot2)
library(reshape2)
library(xts)
library(lattice)
library(gridExtra)
library(gtable)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(tis)
library(foreign)
library(MASS)
library(quantreg)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggplot2)
library(datasets)
library(rio)
library(psych)
library(systemfit)
library(foreign)
library(MatchIt)
library(CRTgeeDR)
library(eurostat)
library(zoo)
library(ggthemes)
library("robumeta")
library("metafor")
library("dplyr")
library(clubSandwich)
library(Hmisc)
library(metafor)
library(pracma)
library(broom)
library(sjPlot)
library(here)
library(data.table)
library(pscore)

#Load data
dat <- fread(here("Coding-fiscal-cyclicality-final.csv"))


#transform
dat$CorrelationCoefficient<-dat$CorrelationCoefficient*dat$Transform
dat$Tstatistic<-dat$Tstatistic*dat$Transform
dat$CorrelationCoefficientCorrected<-dat$CorrelationCoefficientCorrected*dat$Transform
dat$TstatisticCorrected<-dat$TstatisticCorrected*dat$Transform

#only fiscal balance
#dat <- subset(dat, GovSpend %in% c('0'))
#dat <- subset(dat, TaxRev %in% c('0'))

#additional variables and data transformations
#calculate the partial correlation coefficient
dat$PartialCorrelationCoefficient<- dat$TstatisticCorrected / (sqrt((dat$TstatisticCorrected^2)+dat$DegreesofFreedom))

#calculate standard error of the partial correlation coefficient
dat$StandardErrorPartialCorrelation <- sqrt((1-(dat$PartialCorrelationCoefficient)^2)/dat$DegreesofFreedom)

#Precision of partial correlation
dat$PrecSE <- 1 / dat$StandardErrorPartialCorrelation
#Inverse standard error of partial correlation
dat$InverseSE <- 1 / dat$StandardErrorCorrected
#Inverse standard error of corrected correlation coefficient
dat$InverseSECorrected <- 1 / dat$StandardErrorCorrected

#Variance partial correlation
dat$Variance <- dat$StandardErrorPartialCorrelation^2
#Variance of corrected correlation coefficient
dat$VarianceSECorrected <- dat$StandardErrorCorrected^2
#PrecVariance partial correlation
dat$PrecVariance <- 1 / dat$Variance
#PrecVariance corrected
dat$PrecVarianceSECorrected <- 1 / dat$VarianceSECorrected

#Proxy for standard error
dat$proxySE <- 1 / (sqrt(dat$Observations))

#taking logs
#dat$YearofPublication <- log(dat$YearofPublication)
dat$NumberofCountries <- log(dat$NumberofCountries)
dat$Citations <- log(dat$Citations)

dat$InstrumentSE <- 1 / (sqrt(dat$DegreesofFreedom))
dat$InstrumentVariance <- dat$InstrumentSE^2
dat$PrecInstrumentVariance <- 1 / dat$InstrumentVariance
dat$PublicationYear_clean <- dat$YearofPublication
dat$PublicationYear <- dat$YearofPublication - mean(dat$YearofPublication)
dat$MeanYearData_c<- (dat$StartYear+dat$EndYear)/2
dat$MeanYearData<-dat$MeanYearData_c - mean(dat$MeanYearData_c)
dat$InstrumentSE <- 1 / (sqrt(dat$DegreesofFreedom))
dat$InstrumentVariance <- dat$InstrumentSE^2
dat$PrecInstrumentVariance <- 1 / dat$InstrumentVariance
dat$PrecNumberRegressions <- 1 / dat$NumberRegressions
#normalize impact factor
dat$JournalImpactFactor <- as.numeric(dat$JournalImpactFactor)
dat$MaxImpactFactor <- max(dat$JournalImpactFactor)
dat$MaxImpactFactor <- as.numeric(dat$MaxImpactFactor)
dat$NormalizedImpactFactor <- dat$JournalImpactFactor / max(dat$JournalImpactFactor)

#alternative weight based on degrees of freedom
dat$weightDF <- 1/dat$DegreesofFreedom
dat$alternativeweights <- 1/dat$NumberRegressions
dat$PrecStandardErrorCorrected <- 1/dat$StandardErrorCorrected

#transform data
dat_long <- melt(dat, id=1:91)

#sub-sets of the data
#corrected coefficients
dat_long_corrected_c <- subset(dat_long, YNPartial %in% c('1'))
dat_long_corrected_c$PrecStandardErrorCorrected <- 1/dat_long_corrected_c$StandardErrorCorrected

#preferred estimates only
#dat_long_preferred <- subset(dat_long_corrected, Preferred %in% c('1'))

#without inferior estimates
#dat_long_without_inferior <- subset(dat_long_corrected, Preferred %in% c('1', '0'))

#Descriptive statistics
#all-set

#semi-balance gap only
dat_SemiBalanceGap <- subset(dat, SemiBalanceGap %in% c('1'))
dat_SemiBalanceGrowth <- subset(dat, SemiBalanceGrowth %in% c('1'))
dat_SpendingElasticity <- subset(dat, SpendingElasticity %in% c('1'))
dat_TaxRevenueGap <- subset(dat, TaxRevenueGap %in% c('1'))

#winsorising at the 2nd and 98th percentiles
dat_2p98p <- dat

dat_2p98p$StandardErrorCorrected <- winsorizor(dat_2p98p$StandardErrorCorrected, c(0.02), na.rm=TRUE)
dat_2p98p$CorrelationCoefficientCorrected <- winsorizor(dat_2p98p$CorrelationCoefficientCorrected, c(0.02), na.rm=TRUE)
dat_2p98p$InstrumentSE <- winsorizor(dat$InstrumentSE, c(0.02), na.rm=TRUE)
dat_2p98p$InstrumentVariance <- dat_2p98p$InstrumentSE^2
dat_2p98p$PrecInstrumentVariance <- 1 / dat_2p98p$InstrumentVariance

#calculate standard errors corrected after precision was winsorised
dat_2p98p$InverseSECorrected <- 1 / dat_2p98p$StandardErrorCorrected

#Variance of corrected correlation coefficient
dat_2p98p$VarianceSECorrected <- dat_2p98p$StandardErrorCorrected^2

#TStatisticCorrected
dat_2p98p$TstatisticCorrected <- dat_2p98p$CorrelationCoefficientCorrected/dat_2p98p$StandardErrorCorrected

#PrecVariance corrected
dat_2p98p$PrecVarianceSECorrected <- 1 / dat_2p98p$VarianceSECorrected

#calculate the partial correlation coefficient
dat_2p98p$PartialCorrelationCoefficient<- dat_2p98p$TstatisticCorrected / (sqrt((dat_2p98p$TstatisticCorrected^2)+dat$DegreesofFreedom))

#calculate standard error of the partial correlation coefficient
dat_2p98p$StandardErrorPartialCorrelation <- sqrt((1-(dat_2p98p$PartialCorrelationCoefficient)^2)/dat_2p98p$DegreesofFreedom)

#data set for partial correlations
dat_long_corrected <- dat_2p98p
dat_preferred <- subset(dat_long_corrected, Preferred %in% c('1'))

#SemiBalanceGap
dat_2p98p_SemiBalanceGap <- dat_SemiBalanceGap

dat_2p98p_SemiBalanceGap$StandardErrorCorrected <- winsorizor(dat_2p98p_SemiBalanceGap$StandardErrorCorrected, c(0.02), na.rm=TRUE)
dat_2p98p_SemiBalanceGap$CorrelationCoefficientCorrected <- winsorizor(dat_2p98p_SemiBalanceGap$CorrelationCoefficientCorrected, c(0.02), na.rm=TRUE)
dat_2p98p_SemiBalanceGap$InstrumentSE <- winsorizor(dat_2p98p_SemiBalanceGap$InstrumentSE, c(0.02), na.rm=TRUE)
dat_2p98p_SemiBalanceGap$InstrumentVariance <- dat_2p98p_SemiBalanceGap$InstrumentSE^2
dat_2p98p_SemiBalanceGap$PrecInstrumentVariance <- 1 / dat_2p98p_SemiBalanceGap$InstrumentVariance

#calculate standard errors corrected after precision was winsorised
dat_2p98p_SemiBalanceGap$InverseSECorrected <- 1 / dat_2p98p_SemiBalanceGap$StandardErrorCorrected

#Variance of corrected correlation coefficient
dat_2p98p_SemiBalanceGap$VarianceSECorrected <- dat_2p98p_SemiBalanceGap$StandardErrorCorrected^2
#PrecVariance corrected
dat_2p98p_SemiBalanceGap$PrecVarianceSECorrected <- 1 / dat_2p98p_SemiBalanceGap$VarianceSECorrected

#prec standard error
dat_2p98p_SemiBalanceGap$PrecStandardErrorCorrected <- 1/dat_2p98p_SemiBalanceGap$StandardErrorCorrected

#SemiBalanceGrowth
dat_2p98p_SemiBalanceGrowth <- dat_SemiBalanceGrowth

dat_2p98p_SemiBalanceGrowth$StandardErrorCorrected <- winsorizor(dat_2p98p_SemiBalanceGrowth$StandardErrorCorrected, c(0.02), na.rm=TRUE)
dat_2p98p_SemiBalanceGrowth$CorrelationCoefficientCorrected <- winsorizor(dat_2p98p_SemiBalanceGrowth$CorrelationCoefficientCorrected, c(0.02), na.rm=TRUE)
dat_2p98p_SemiBalanceGrowth$InstrumentSE <- winsorizor(dat_SemiBalanceGrowth$InstrumentSE, c(0.02), na.rm=TRUE)
dat_2p98p_SemiBalanceGrowth$InstrumentVariance <- dat_2p98p_SemiBalanceGrowth$InstrumentSE^2
dat_2p98p_SemiBalanceGrowth$PrecInstrumentVariance <- 1 / dat_2p98p_SemiBalanceGrowth$InstrumentVariance

#calculate standard errors corrected after precision was winsorised
dat_2p98p_SemiBalanceGrowth$InverseSECorrected <- 1 / dat_2p98p_SemiBalanceGrowth$StandardErrorCorrected

#Variance of corrected correlation coefficient
dat_2p98p_SemiBalanceGrowth$VarianceSECorrected <- dat_2p98p_SemiBalanceGrowth$StandardErrorCorrected^2
#PrecVariance corrected
dat_2p98p_SemiBalanceGrowth$PrecVarianceSECorrected <- 1 / dat_2p98p_SemiBalanceGrowth$VarianceSECorrected

#prec standard error
dat_2p98p_SemiBalanceGrowth$PrecStandardErrorCorrected <- 1/dat_2p98p_SemiBalanceGrowth$StandardErrorCorrected

#SpendingElasticity
dat_2p98p_SpendingElasticity <- dat_SpendingElasticity

dat_2p98p_SpendingElasticity$StandardErrorCorrected <- winsorizor(dat_2p98p_SpendingElasticity$StandardErrorCorrected, c(0.02), na.rm=TRUE)
dat_2p98p_SpendingElasticity$CorrelationCoefficientCorrected <- winsorizor(dat_2p98p_SpendingElasticity$CorrelationCoefficientCorrected, c(0.02), na.rm=TRUE)
dat_2p98p_SpendingElasticity$InstrumentSE <- winsorizor(dat_2p98p_SpendingElasticity$InstrumentSE, c(0.02), na.rm=TRUE)
dat_2p98p_SpendingElasticity$InstrumentVariance <- dat_2p98p_SpendingElasticity$InstrumentSE^2
dat_2p98p_SpendingElasticity$PrecInstrumentVariance <- 1 / dat_2p98p_SpendingElasticity$InstrumentVariance

#calculate standard errors corrected after precision was winsorised
dat_2p98p_SpendingElasticity$InverseSECorrected <- 1 / dat_2p98p_SpendingElasticity$StandardErrorCorrected

#Variance of corrected correlation coefficient
dat_2p98p_SpendingElasticity$VarianceSECorrected <- dat_2p98p_SpendingElasticity$StandardErrorCorrected^2
#PrecVariance corrected
dat_2p98p_SpendingElasticity$PrecVarianceSECorrected <- 1 / dat_2p98p_SpendingElasticity$VarianceSECorrected

#Variance of corrected correlation coefficient
dat_2p98p_SpendingElasticity$VarianceSECorrected <- dat_2p98p_SpendingElasticity$StandardErrorCorrected^2
#PrecVariance corrected
dat_2p98p_SpendingElasticity$PrecVarianceSECorrected <- 1 / dat_2p98p_SpendingElasticity$VarianceSECorrected

#prec standard error
dat_2p98p_SpendingElasticity$PrecStandardErrorCorrected <- 1/dat_2p98p_SpendingElasticity$StandardErrorCorrected

#SpendingElasticity
dat_2p98p_SpendingElasticity <- dat_SpendingElasticity

dat_2p98p_SpendingElasticity$StandardErrorCorrected <- winsorizor(dat_2p98p_SpendingElasticity$StandardErrorCorrected, c(0.02), na.rm=TRUE)
dat_2p98p_SpendingElasticity$CorrelationCoefficientCorrected <- winsorizor(dat_2p98p_SpendingElasticity$CorrelationCoefficientCorrected, c(0.02), na.rm=TRUE)
dat_2p98p_SpendingElasticity$InstrumentSE <- winsorizor(dat_2p98p_SpendingElasticity$InstrumentSE, c(0.02), na.rm=TRUE)
dat_2p98p_SpendingElasticity$InstrumentVariance <- dat_2p98p_SpendingElasticity$InstrumentSE^2
dat_2p98p_SpendingElasticity$PrecInstrumentVariance <- 1 / dat_2p98p_SpendingElasticity$InstrumentVariance

#calculate standard errors corrected after precision was winsorised
dat_2p98p_SpendingElasticity$InverseSECorrected <- 1 / dat_2p98p_SpendingElasticity$StandardErrorCorrected

#Variance of corrected correlation coefficient
dat_2p98p_SpendingElasticity$VarianceSECorrected <- dat_2p98p_SpendingElasticity$StandardErrorCorrected^2
#PrecVariance corrected
dat_2p98p_SpendingElasticity$PrecVarianceSECorrected <- 1 / dat_2p98p_SpendingElasticity$VarianceSECorrected

#Variance of corrected correlation coefficient
dat_2p98p_SpendingElasticity$VarianceSECorrected <- dat_2p98p_SpendingElasticity$StandardErrorCorrected^2
#PrecVariance corrected
dat_2p98p_SpendingElasticity$PrecVarianceSECorrected <- 1 / dat_2p98p_SpendingElasticity$VarianceSECorrected

#prec standard error
dat_2p98p_SpendingElasticity$PrecStandardErrorCorrected <- 1/dat_2p98p_SpendingElasticity$StandardErrorCorrected


#TaxRevenueGap
dat_2p98p_TaxRevenueGap <- dat_TaxRevenueGap

dat_2p98p_TaxRevenueGap$StandardErrorCorrected <- winsorizor(dat_2p98p_TaxRevenueGap$StandardErrorCorrected, c(0.02), na.rm=TRUE)
dat_2p98p_TaxRevenueGap$CorrelationCoefficientCorrected <- winsorizor(dat_2p98p_TaxRevenueGap$CorrelationCoefficientCorrected, c(0.02), na.rm=TRUE)
dat_2p98p_TaxRevenueGap$InstrumentSE <- winsorizor(dat_2p98p_TaxRevenueGap$InstrumentSE, c(0.02), na.rm=TRUE)
dat_2p98p_TaxRevenueGap$InstrumentVariance <- dat_2p98p_TaxRevenueGap$InstrumentSE^2
dat_2p98p_TaxRevenueGap$PrecInstrumentVariance <- 1 / dat_2p98p_TaxRevenueGap$InstrumentVariance

#calculate standard errors corrected after precision was winsorised
dat_2p98p_TaxRevenueGap$InverseSECorrected <- 1 / dat_2p98p_TaxRevenueGap$StandardErrorCorrected

#Variance of corrected correlation coefficient
dat_2p98p_TaxRevenueGap$VarianceSECorrected <- dat_2p98p_TaxRevenueGap$StandardErrorCorrected^2
#PrecVariance corrected
dat_2p98p_TaxRevenueGap$PrecVarianceSECorrected <- 1 / dat_2p98p_TaxRevenueGap$VarianceSECorrected

#Variance of corrected correlation coefficient
dat_2p98p_TaxRevenueGap$VarianceSECorrected <- dat_2p98p_TaxRevenueGap$StandardErrorCorrected^2
#PrecVariance corrected
dat_2p98p_TaxRevenueGap$PrecVarianceSECorrected <- 1 / dat_2p98p_TaxRevenueGap$VarianceSECorrected

#prec standard error
dat_2p98p_TaxRevenueGap$PrecStandardErrorCorrected <- 1/dat_2p98p_TaxRevenueGap$StandardErrorCorrected

write.csv(dat_long_corrected, "dat_long_corrected.csv")
#write.csv(dat_2p98p_SemiBalanceGap, "dat_2p98p_SemiBalanceGap.csv")



#Funnel plot
#partial correlations
plot_funnel_partialcorr <- ggplot(data=dat_long_corrected,
                                                            aes(x=PartialCorrelationCoefficient, y=PrecSE)) +
  geom_point(size=1.5, color="blue") +
  xlab("Partial correlation") +
  ylab("Inverse of standard error (precision)") +
  ggtitle("Partial correlation coefficient of fiscal policy with the cycle variable\n (N=3536)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position="bottom")+
  theme(legend.title=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(legend.text = element_text(colour="black", size = 4))+
  theme(axis.text.x=element_text(size=13))+
  theme(axis.title.x=element_text(size=13)) +
  theme(axis.text.y=element_text(size=13))+
  theme(axis.title.y=element_text(size=13))
plot_funnel_partialcorr

#SemiBalanceGap
plot_funnel_SemiBalanceGap <- ggplot(data=dat_2p98p_SemiBalanceGap,
                                  aes(x=CorrelationCoefficientCorrected, y=PrecStandardErrorCorrected)) +
  geom_point(size=1.5, color="blue") +
  xlab("Semi-elasticity") +
  ylab("Inverse of standard error (precision)") +
  ggtitle("Semi-elasticity of the (primary) fiscal balance with respect to the output gap\n (N=1757)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position="bottom")+
  theme(legend.title=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(legend.text = element_text(colour="black", size = 4))+
  theme(axis.text.x=element_text(size=13))+
  theme(axis.title.x=element_text(size=13)) +
  theme(axis.text.y=element_text(size=13))+
  theme(axis.title.y=element_text(size=13))
plot_funnel_SemiBalanceGap

#SemiBalanceGrowth
plot_funnel_SemiBalanceGrowth <- ggplot(data=dat_2p98p_SemiBalanceGrowth,
                                     aes(x=CorrelationCoefficientCorrected, y=PrecStandardErrorCorrected)) +
  geom_point(size=1.5, color="blue") +
  xlab("Semi-elasticity") +
  ylab("Inverse of standard error (precision)") +
  ggtitle("Semi-elasticity of the (primary) fiscal balance with respect to real GDP growth\n (N=249)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position="bottom")+
  theme(legend.title=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(legend.text = element_text(colour="black", size = 4))+
  theme(axis.text.x=element_text(size=13))+
  theme(axis.title.x=element_text(size=13)) +
  theme(axis.text.y=element_text(size=13))+
  theme(axis.title.y=element_text(size=13))
plot_funnel_SemiBalanceGrowth

#SpendingElasticity
plot_funnel_SpendingElasticity <- ggplot(data=dat_2p98p_SpendingElasticity,
                                        aes(x=CorrelationCoefficientCorrected, y=PrecStandardErrorCorrected)) +
  geom_point(size=1.5, color="blue") +
  xlab("Elasticity") +
  ylab("Inverse of standard error (precision)") +
  ggtitle("Elasticity of government spending with respect to changes in real GDP\n (N=806)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position="bottom")+
  theme(legend.title=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(legend.text = element_text(colour="black", size = 4))+
  theme(axis.text.x=element_text(size=13))+
  theme(axis.title.x=element_text(size=13)) +
  theme(axis.text.y=element_text(size=13))+
  theme(axis.title.y=element_text(size=13))
plot_funnel_SpendingElasticity

#TaxRevenueGap
plot_funnel_TaxRevenueGap <- ggplot(data=dat_2p98p_TaxRevenueGap,
                                         aes(x=CorrelationCoefficientCorrected, y=PrecStandardErrorCorrected)) +
  geom_point(size=1.5, color="blue") +
  xlab("Semi-elasticity") +
  ylab("Inverse of standard error (precision)") +
  ggtitle("Semi-elasticity of tax revenue with respect to the output gap\n (N=126)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position="bottom")+
  theme(legend.title=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(legend.text = element_text(colour="black", size = 4))+
  theme(axis.text.x=element_text(size=13))+
  theme(axis.title.x=element_text(size=13)) +
  theme(axis.text.y=element_text(size=13))+
  theme(axis.title.y=element_text(size=13))
plot_funnel_TaxRevenueGap

#publication bias
#partial correlation
pub_bias_partial_1 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation, weights=PrecVarianceSECorrected, data=dat_long_corrected)
summary(pub_bias_partial_1)

coef_test(pub_bias_partial_1, vcov = "CR0", 
          cluster = dat_long_corrected$id, test = "naive-t")

#SemiBalanceGap
pub_bias_SemiBalanceGap_1 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected, weights=PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGap)
summary(pub_bias_SemiBalanceGap_1)

coef_test(pub_bias_SemiBalanceGap_1, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")

#SemiBalanceGrowth
pub_bias_SemiBalanceGrowth_1 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected, weights=PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGrowth)
summary(pub_bias_SemiBalanceGrowth_1)

coef_test(pub_bias_SemiBalanceGrowth_1, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#SpendingElasticity
pub_bias_SpendingElasticity_1 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected, weights=PrecVarianceSECorrected, data=dat_2p98p_SpendingElasticity)
summary(pub_bias_SpendingElasticity_1)

coef_test(pub_bias_SemiBalanceGrowth_1, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#TaxRevenueGap
pub_bias_TaxRevenueGap_1 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected, weights=PrecVarianceSECorrected, data=dat_2p98p_TaxRevenueGap)
summary(pub_bias_TaxRevenueGap_1)

coef_test(pub_bias_SemiBalanceGrowth_1, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#partial correlations
#preferred only

pub_bias_partial_2 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation, weights=PrecVarianceSECorrected, data=dat_preferred)
summary(pub_bias_partial_2)

coef_test(pub_bias_partial_2, vcov = "CR0", 
          cluster = dat_preferred$id, test = "naive-t")

#IV2SLS
library(AER)
pub_bias_partial_3 <- AER::ivreg(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation | InstrumentSE, weights=1/InstrumentSE, data=dat_long_corrected)
summary(pub_bias_partial_3)

coef_test(pub_bias_partial_3, vcov = "CR0", 
          cluster = dat_long_corrected$id, test = "naive-t")

#preparing latex tables
ses_pub_bias_partial_1 <- list(coef_test(pub_bias_partial_1, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_pub_bias_partial_1 <- list(coef_test(pub_bias_partial_1, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_pub_bias_partial_1 <- list(coef_test(pub_bias_partial_1, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_pub_bias_partial_2 <- list(coef_test(pub_bias_partial_2, vcov = "CR0", cluster = dat_preferred$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_pub_bias_partial_2 <- list(coef_test(pub_bias_partial_2, vcov = "CR0", cluster = dat_preferred$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_pub_bias_partial_2 <- list(coef_test(pub_bias_partial_2, vcov = "CR0", cluster = dat_preferred$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_pub_bias_SemiBalanceGap_1 <- list(coef_test(pub_bias_SemiBalanceGap_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_pub_bias_SemiBalanceGap_1 <- list(coef_test(pub_bias_SemiBalanceGap_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_pub_bias_SemiBalanceGap_1 <- list(coef_test(pub_bias_SemiBalanceGap_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_pub_bias_SemiBalanceGrowth_1 <- list(coef_test(pub_bias_SemiBalanceGrowth_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_pub_bias_SemiBalanceGrowth_1 <- list(coef_test(pub_bias_SemiBalanceGrowth_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_pub_bias_SemiBalanceGrowth_1 <- list(coef_test(pub_bias_SemiBalanceGrowth_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_pub_bias_SpendingElasticity_1 <- list(coef_test(pub_bias_SpendingElasticity_1, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_pub_bias_SpendingElasticity_1 <- list(coef_test(pub_bias_SpendingElasticity_1, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_pub_bias_SpendingElasticity_1 <- list(coef_test(pub_bias_SpendingElasticity_1, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_pub_bias_TaxRevenueGap_1 <- list(coef_test(pub_bias_TaxRevenueGap_1, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_pub_bias_TaxRevenueGap_1 <- list(coef_test(pub_bias_TaxRevenueGap_1, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_pub_bias_TaxRevenueGap_1 <- list(coef_test(pub_bias_TaxRevenueGap_1, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

#table
stargazer(pub_bias_partial_1, pub_bias_partial_2, pub_bias_SemiBalanceGap_1, pub_bias_SemiBalanceGrowth_1, pub_bias_TaxRevenueGap_1, pub_bias_SpendingElasticity_1, t=list(unlist(tvals_pub_bias_partial_1), unlist(tvals_pub_bias_partial_2), unlist(tvals_pub_bias_SemiBalanceGap_1), unlist(tvals_pub_bias_SemiBalanceGrowth_1), unlist(tvals_pub_bias_TaxRevenueGap_1), unlist(tvals_pub_bias_SpendingElasticity_1)), se=list(unlist(ses_pub_bias_partial_1), unlist(ses_pub_bias_partial_2), unlist(ses_pub_bias_SemiBalanceGap_1), unlist(ses_pub_bias_SemiBalanceGrowth_1), unlist(ses_pub_bias_TaxRevenueGap_1), unlist(ses_pub_bias_SpendingElasticity_1)), p=list(unlist(pvals_pub_bias_partial_1), unlist(pvals_pub_bias_partial_2), unlist(pvals_pub_bias_SemiBalanceGap_1), unlist(pvals_pub_bias_SemiBalanceGrowth_1), unlist(pvals_pub_bias_TaxRevenueGap_1), unlist(pvals_pub_bias_SpendingElasticity_1)))

#meta-regression
#partial correlation
MRA_partial_1 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=PrecVariance, data=dat_long_corrected)
summary(MRA_partial_1)

coef_test(MRA_partial_1, vcov = "CR0", 
          cluster = dat_long_corrected$id, test = "naive-t")

#SemiBalanceGap
MRA_SemiBalanceGap_1 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGap)
summary(MRA_SemiBalanceGap_1)

coef_test(MRA_SemiBalanceGap_1, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")

#SemiBalanceGrowth
MRA_SemiBalanceGrowth_1 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect, weights=PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGrowth)
summary(MRA_SemiBalanceGrowth_1)

coef_test(MRA_SemiBalanceGrowth_1, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#SpendingElasticity
MRA_SpendingElasticity_1 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect, weights=PrecVarianceSECorrected, data=dat_2p98p_SpendingElasticity)
summary(MRA_SpendingElasticity_1)

coef_test(MRA_SemiBalanceGrowth_1, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#TaxRevenueGap
MRA_TaxRevenueGap_1 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + DataMixofCountries  + TacklingEndogeneity, weights=PrecVarianceSECorrected, data=dat_2p98p_TaxRevenueGap)
summary(MRA_TaxRevenueGap_1)

coef_test(MRA_SemiBalanceGrowth_1, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#save figure
ggsave(plot = plot_funnel_partialcorr, 
       filename = paste0(here("figures/funnel_w2_98"), 
                         ".pdf"),
       width = 6.5, height = 4)

#preparing latex tables
ses_MRA_partial_1 <- list(coef_test(MRA_partial_1, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_partial_1 <- list(coef_test(MRA_partial_1, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_partial_1 <- list(coef_test(MRA_partial_1, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SemiBalanceGap_1 <- list(coef_test(MRA_SemiBalanceGap_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SemiBalanceGap_1 <- list(coef_test(MRA_SemiBalanceGap_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SemiBalanceGap_1 <- list(coef_test(MRA_SemiBalanceGap_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SemiBalanceGrowth_1 <- list(coef_test(MRA_SemiBalanceGrowth_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SemiBalanceGrowth_1 <- list(coef_test(MRA_SemiBalanceGrowth_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SemiBalanceGrowth_1 <- list(coef_test(MRA_SemiBalanceGrowth_1, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SpendingElasticity_1 <- list(coef_test(MRA_SpendingElasticity_1, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SpendingElasticity_1 <- list(coef_test(MRA_SpendingElasticity_1, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SpendingElasticity_1 <- list(coef_test(MRA_SpendingElasticity_1, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_TaxRevenueGap_1 <- list(coef_test(MRA_TaxRevenueGap_1, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_TaxRevenueGap_1 <- list(coef_test(MRA_TaxRevenueGap_1, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_TaxRevenueGap_1 <- list(coef_test(MRA_TaxRevenueGap_1, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

#Table 3
#table
stargazer(MRA_partial_1, MRA_SemiBalanceGap_1, MRA_SemiBalanceGrowth_1, MRA_TaxRevenueGap_1, MRA_SpendingElasticity_1, t=list(unlist(tvals_MRA_partial_1), unlist(tvals_MRA_SemiBalanceGap_1), unlist(tvals_MRA_SemiBalanceGrowth_1), unlist(tvals_MRA_TaxRevenueGap_1), unlist(tvals_MRA_SpendingElasticity_1)), se=list(unlist(ses_MRA_partial_1), unlist(ses_MRA_SemiBalanceGap_1), unlist(ses_MRA_SemiBalanceGrowth_1), unlist(ses_MRA_TaxRevenueGap_1), unlist(ses_MRA_SpendingElasticity_1)), p=list(unlist(pvals_MRA_partial_1), unlist(pvals_MRA_SemiBalanceGap_1), unlist(pvals_MRA_SemiBalanceGrowth_1), unlist(pvals_MRA_TaxRevenueGap_1), unlist(pvals_MRA_SpendingElasticity_1)))

#different continents (space dimension)
#partial correlation
MRA_partial_2 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + EUonly + USAonly + Africaonly + LatinAmericaonly + Asiaonly  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=PrecVariance, data=dat_long_corrected)
summary(MRA_partial_2)

coef_test(MRA_partial_2, vcov = "CR0", 
          cluster = dat_long_corrected$id, test = "naive-t")

#SemiBalanceGap
MRA_SemiBalanceGap_2 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + EUonly + USAonly + Africaonly + LatinAmericaonly + Asiaonly  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGap)
summary(MRA_SemiBalanceGap_2)

coef_test(MRA_SemiBalanceGap_2, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")

#SemiBalanceGrowth
MRA_SemiBalanceGrowth_2 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + EUonly + USAonly + Africaonly + LatinAmericaonly + Asiaonly   + TacklingEndogeneity + LongRunEffect, weights=PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGrowth)
summary(MRA_SemiBalanceGrowth_2)

coef_test(MRA_SemiBalanceGrowth_2, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#SpendingElasticity
MRA_SpendingElasticity_2 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + EUonly + USAonly + Africaonly + LatinAmericaonly + Asiaonly   + TacklingEndogeneity + LongRunEffect, weights=PrecVarianceSECorrected, data=dat_2p98p_SpendingElasticity)
summary(MRA_SpendingElasticity_2)

coef_test(MRA_SpendingElasticity_2, vcov = "CR0", 
          cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")

#TaxRevenueGap
MRA_TaxRevenueGap_2 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + EUonly + USAonly + Asiaonly  + TacklingEndogeneity, weights=PrecVarianceSECorrected, data=dat_2p98p_TaxRevenueGap)
summary(MRA_TaxRevenueGap_2)

coef_test(MRA_TaxRevenueGap_2, vcov = "CR0", 
          cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")

#save figure
#ggsave(plot = plot_funnel_CorrelationCoefficientCorrected_2p98p, 
 #      filename = paste0(here("figures/funnel_w2_98"), 
   #                      ".pdf"),
  #     width = 6.5, height = 4)

#preparing latex tables
ses_MRA_partial_2 <- list(coef_test(MRA_partial_2, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_partial_2 <- list(coef_test(MRA_partial_2, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_partial_2 <- list(coef_test(MRA_partial_2, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SemiBalanceGap_2 <- list(coef_test(MRA_SemiBalanceGap_2, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SemiBalanceGap_2 <- list(coef_test(MRA_SemiBalanceGap_2, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SemiBalanceGap_2 <- list(coef_test(MRA_SemiBalanceGap_2, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SemiBalanceGrowth_2 <- list(coef_test(MRA_SemiBalanceGrowth_2, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SemiBalanceGrowth_2 <- list(coef_test(MRA_SemiBalanceGrowth_2, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SemiBalanceGrowth_2 <- list(coef_test(MRA_SemiBalanceGrowth_2, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SpendingElasticity_2 <- list(coef_test(MRA_SpendingElasticity_2, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SpendingElasticity_2 <- list(coef_test(MRA_SpendingElasticity_2, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SpendingElasticity_2 <- list(coef_test(MRA_SpendingElasticity_2, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_TaxRevenueGap_2 <- list(coef_test(MRA_TaxRevenueGap_2, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_TaxRevenueGap_2 <- list(coef_test(MRA_TaxRevenueGap_2, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_TaxRevenueGap_2 <- list(coef_test(MRA_TaxRevenueGap_2, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

#table
stargazer(MRA_partial_2, MRA_SemiBalanceGap_2, MRA_SemiBalanceGrowth_2, MRA_TaxRevenueGap_2, MRA_SpendingElasticity_2, t=list(unlist(tvals_MRA_partial_2), unlist(tvals_MRA_SemiBalanceGap_2), unlist(tvals_MRA_SemiBalanceGrowth_2), unlist(tvals_MRA_TaxRevenueGap_2), unlist(tvals_MRA_SpendingElasticity_2)), se=list(unlist(ses_MRA_partial_2), unlist(ses_MRA_SemiBalanceGap_2), unlist(ses_MRA_SemiBalanceGrowth_2), unlist(ses_MRA_TaxRevenueGap_2), unlist(ses_MRA_SpendingElasticity_2)), p=list(unlist(pvals_MRA_partial_2), unlist(pvals_MRA_SemiBalanceGap_2), unlist(pvals_MRA_SemiBalanceGrowth_2), unlist(pvals_MRA_TaxRevenueGap_2), unlist(pvals_MRA_SpendingElasticity_2)))

#data and publication characteristics
#partial correlation
MRA_partial_3 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData + NormalizedImpactFactor + ReviewedJournal + Citations + Primary + LaggedDependentVariable + LaggedCycleVariable + MeanYearData, weights=PrecVariance, data=dat_long_corrected)
summary(MRA_partial_3)

coef_test(MRA_partial_3, vcov = "CR0", 
          cluster = dat_long_corrected$id, test = "naive-t")

#SemiBalanceGap
MRA_SemiBalanceGap_3 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData + NormalizedImpactFactor + ReviewedJournal + Citations + Primary, weights=PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGap)
summary(MRA_SemiBalanceGap_3)

coef_test(MRA_SemiBalanceGap_3, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")

#SemiBalanceGrowth
MRA_SemiBalanceGrowth_3 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + NormalizedImpactFactor + ReviewedJournal + Citations + Primary, weights=PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGrowth)
summary(MRA_SemiBalanceGrowth_3)

coef_test(MRA_SemiBalanceGrowth_3, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#SpendingElasticity
MRA_SpendingElasticity_3 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + NormalizedImpactFactor + ReviewedJournal + Citations + Primary, weights=PrecVarianceSECorrected, data=dat_2p98p_SpendingElasticity)
summary(MRA_SpendingElasticity_3)

coef_test(MRA_SpendingElasticity_3, vcov = "CR0", 
          cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")

#TaxRevenueGap
MRA_TaxRevenueGap_3 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + DataMixofCountries  + TacklingEndogeneity + NormalizedImpactFactor + ReviewedJournal + Citations + Primary, weights=PrecVarianceSECorrected, data=dat_2p98p_TaxRevenueGap)
summary(MRA_TaxRevenueGap_3)

coef_test(MRA_SemiBalanceGrowth_3, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#save figure
ggsave(plot = plot_funnel_partialcorr, 
       filename = paste0(here("figures/funnel_w2_98"), 
                         ".pdf"),
       width = 6.5, height = 4)

#preparing latex tables
ses_MRA_partial_3 <- list(coef_test(MRA_partial_3, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_partial_3 <- list(coef_test(MRA_partial_3, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_partial_3 <- list(coef_test(MRA_partial_3, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SemiBalanceGap_3 <- list(coef_test(MRA_SemiBalanceGap_3, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SemiBalanceGap_3 <- list(coef_test(MRA_SemiBalanceGap_3, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SemiBalanceGap_3 <- list(coef_test(MRA_SemiBalanceGap_3, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SemiBalanceGrowth_3 <- list(coef_test(MRA_SemiBalanceGrowth_3, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SemiBalanceGrowth_3 <- list(coef_test(MRA_SemiBalanceGrowth_3, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SemiBalanceGrowth_3 <- list(coef_test(MRA_SemiBalanceGrowth_3, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SpendingElasticity_3 <- list(coef_test(MRA_SpendingElasticity_3, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SpendingElasticity_3 <- list(coef_test(MRA_SpendingElasticity_3, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SpendingElasticity_3 <- list(coef_test(MRA_SpendingElasticity_3, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_TaxRevenueGap_3 <- list(coef_test(MRA_TaxRevenueGap_3, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_TaxRevenueGap_3 <- list(coef_test(MRA_TaxRevenueGap_3, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_TaxRevenueGap_3 <- list(coef_test(MRA_TaxRevenueGap_3, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

#table
stargazer(MRA_partial_3, MRA_SemiBalanceGap_3, MRA_SemiBalanceGrowth_3, MRA_TaxRevenueGap_3, MRA_SpendingElasticity_3, t=list(unlist(tvals_MRA_partial_3), unlist(tvals_MRA_SemiBalanceGap_3), unlist(tvals_MRA_SemiBalanceGrowth_3), unlist(tvals_MRA_TaxRevenueGap_3), unlist(tvals_MRA_SpendingElasticity_3)), se=list(unlist(ses_MRA_partial_3), unlist(ses_MRA_SemiBalanceGap_3), unlist(ses_MRA_SemiBalanceGrowth_3), unlist(ses_MRA_TaxRevenueGap_3), unlist(ses_MRA_SpendingElasticity_3)), p=list(unlist(pvals_MRA_partial_3), unlist(pvals_MRA_SemiBalanceGap_3), unlist(pvals_MRA_SemiBalanceGrowth_3), unlist(pvals_MRA_TaxRevenueGap_3), unlist(pvals_MRA_SpendingElasticity_3)))

#partial correlation
MRA_partial_4 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData + Election + FiscalRule + FiscalFatigue, weights=PrecVariance, data=dat_long_corrected)
summary(MRA_partial_4)

coef_test(MRA_partial_4, vcov = "CR0", 
          cluster = dat_long_corrected$id, test = "naive-t")

#SemiBalanceGap
MRA_SemiBalanceGap_4 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData + Election + FiscalRule + NoPublicDebt, weights=PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGap)
summary(MRA_SemiBalanceGap_4)

coef_test(MRA_SemiBalanceGap_4, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")

#SemiBalanceGrowth
MRA_SemiBalanceGrowth_4 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + Election + FiscalRule + NoPublicDebt, weights=PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGrowth)
summary(MRA_SemiBalanceGrowth_4)

coef_test(MRA_SemiBalanceGrowth_4, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#SpendingElasticity
MRA_SpendingElasticity_4 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + Election + FiscalRule + NoPublicDebt, weights=PrecVarianceSECorrected, data=dat_2p98p_SpendingElasticity)
summary(MRA_SpendingElasticity_4)

coef_test(MRA_SemiBalanceGrowth_4, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#TaxRevenueGap
MRA_TaxRevenueGap_4 <- lm(CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + DataMixofCountries  + TacklingEndogeneity + Election + FiscalRule + NoPublicDebt, weights=PrecVarianceSECorrected, data=dat_2p98p_TaxRevenueGap)
summary(MRA_TaxRevenueGap_4)

coef_test(MRA_SemiBalanceGrowth_4, vcov = "CR0", 
          cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")

#save figure
ggsave(plot = plot_funnel_partialcorr, 
       filename = paste0(here("figures/funnel_w2_98"), 
                         ".pdf"),
       width = 6.5, height = 4)

#preparing latex tables
ses_MRA_partial_4 <- list(coef_test(MRA_partial_4, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_partial_4 <- list(coef_test(MRA_partial_4, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_partial_4 <- list(coef_test(MRA_partial_4, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SemiBalanceGap_4 <- list(coef_test(MRA_SemiBalanceGap_4, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SemiBalanceGap_4 <- list(coef_test(MRA_SemiBalanceGap_4, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SemiBalanceGap_4 <- list(coef_test(MRA_SemiBalanceGap_4, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGap$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SemiBalanceGrowth_4 <- list(coef_test(MRA_SemiBalanceGrowth_4, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SemiBalanceGrowth_4 <- list(coef_test(MRA_SemiBalanceGrowth_4, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SemiBalanceGrowth_4 <- list(coef_test(MRA_SemiBalanceGrowth_4, vcov = "CR0", cluster = dat_2p98p_SemiBalanceGrowth$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_SpendingElasticity_4 <- list(coef_test(MRA_SpendingElasticity_4, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_SpendingElasticity_4 <- list(coef_test(MRA_SpendingElasticity_4, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_SpendingElasticity_4 <- list(coef_test(MRA_SpendingElasticity_4, vcov = "CR0", cluster = dat_2p98p_SpendingElasticity$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_TaxRevenueGap_4 <- list(coef_test(MRA_TaxRevenueGap_4, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_TaxRevenueGap_4 <- list(coef_test(MRA_TaxRevenueGap_4, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_TaxRevenueGap_4 <- list(coef_test(MRA_TaxRevenueGap_4, vcov = "CR0", cluster = dat_2p98p_TaxRevenueGap$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

#table
stargazer(MRA_partial_4, MRA_SemiBalanceGap_4, MRA_SemiBalanceGrowth_4, MRA_TaxRevenueGap_4, MRA_SpendingElasticity_4, t=list(unlist(tvals_MRA_partial_4), unlist(tvals_MRA_SemiBalanceGap_4), unlist(tvals_MRA_SemiBalanceGrowth_4), unlist(tvals_MRA_TaxRevenueGap_4), unlist(tvals_MRA_SpendingElasticity_4)), se=list(unlist(ses_MRA_partial_4), unlist(ses_MRA_SemiBalanceGap_4), unlist(ses_MRA_SemiBalanceGrowth_4), unlist(ses_MRA_TaxRevenueGap_4), unlist(ses_MRA_SpendingElasticity_4)), p=list(unlist(pvals_MRA_partial_4), unlist(pvals_MRA_SemiBalanceGap_4), unlist(pvals_MRA_SemiBalanceGrowth_4), unlist(pvals_MRA_TaxRevenueGap_4), unlist(pvals_MRA_SpendingElasticity_4)))

#IV
library(AER)
#partial correlation
MRA_partial_5 <- AER::ivreg(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData | InstrumentSE + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=1/InstrumentSE, data=dat_long_corrected)
summary(MRA_partial_5)

coef_test(MRA_partial_5, vcov = "CR0", 
          cluster = dat_long_corrected$id, test = "naive-t")

#Random Effects
MRA_partial_6 <- plm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, index = c("id","pobs"), model="random", data=dat_long_corrected)
summary(MRA_partial_6)

coef_test(MRA_partial_6, vcov = "CR0", 
          cluster = dat_long_corrected$id, test = "naive-t")

#Preferred with dummy
MRA_partial_7 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData + Preferred, weights=PrecVariance, data=dat_long_corrected)
summary(MRA_partial_7)

coef_test(MRA_partial_7, vcov = "CR0", 
          cluster = dat_long_corrected$id, test = "naive-t")

#Preferred sub-sample
MRA_partial_8 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=PrecVariance, data=dat_preferred)
summary(MRA_partial_8)

coef_test(MRA_partial_8, vcov = "CR0", 
          cluster = dat_preferred$id, test = "naive-t")

#Developing countries as continuous variable
MRA_partial_9 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + ShareofDevelopingCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=PrecVariance, data=dat_long_corrected)
summary(MRA_partial_9)

coef_test(MRA_partial_9, vcov = "CR0", 
          cluster = dat_long_corrected$id, test = "naive-t")

#preparing latex tables
ses_MRA_partial_5 <- list(coef_test(MRA_partial_5, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_partial_5 <- list(coef_test(MRA_partial_5, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_partial_5 <- list(coef_test(MRA_partial_5, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_partial_6 <- list(coef_test(MRA_partial_6, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_partial_6 <- list(coef_test(MRA_partial_6, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_partial_6 <- list(coef_test(MRA_partial_6, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_partial_7 <- list(coef_test(MRA_partial_7, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_partial_7 <- list(coef_test(MRA_partial_7, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_partial_7 <- list(coef_test(MRA_partial_7, vcov = "CR0", cluster = dat_long_corrected$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

ses_MRA_partial_8 <- list(coef_test(MRA_partial_8, vcov = "CR0", cluster = dat_preferred$id, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
tvals_MRA_partial_8 <- list(coef_test(MRA_partial_8, vcov = "CR0", cluster = dat_preferred$id, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals_MRA_partial_8 <- list(coef_test(MRA_partial_8, vcov = "CR0", cluster = dat_preferred$id, test = "naive-t")[,6]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

#Table 4
#stargazer table
stargazer(MRA_partial_1, MRA_partial_2, MRA_partial_3, MRA_partial_4, MRA_partial_6, MRA_partial_7, MRA_partial_8, t=list(unlist(tvals_MRA_partial_1), unlist(tvals_MRA_partial_2), unlist(tvals_MRA_partial_3), unlist(tvals_MRA_partial_4), unlist(tvals_MRA_partial_6), unlist(tvals_MRA_partial_7), unlist(tvals_MRA_partial_8)), se=list(unlist(ses_MRA_partial_1), unlist(ses_MRA_partial_2), unlist(ses_MRA_partial_3), unlist(ses_MRA_partial_4), unlist(ses_MRA_partial_6), unlist(ses_MRA_partial_7), unlist(ses_MRA_partial_8)), p=list(unlist(pvals_MRA_partial_1), unlist(pvals_MRA_partial_2), unlist(pvals_MRA_partial_3), unlist(pvals_MRA_partial_4), unlist(pvals_MRA_partial_6), unlist(pvals_MRA_partial_7), unlist(pvals_MRA_partial_8)))

#time trends
#relation between standardised effect size and publication year
library(mgcv)
m1 <- ggplot(dat_long_corrected, aes(x=MeanYearData_c,y=PartialCorrelationCoefficient))
m1 <- m1 + geom_point(color='blue', size=1) + ylab("Partial correlation") + xlab("Mean year data") + ggtitle("Time trends by mean year of data") + theme_bw()

## m1 <- m1 + geom_smooth(method='loess',span=1.0,color='black') + geom_smooth(method='loess',span=0.2,color='black')
m1 <- m1 + geom_smooth(method=gam, color='black',formula= y ~ s(x, bs = "cs")) + xlim(1975, 2015)
## m1 <- m1 + geom_smooth(method='auto', color='black')
print(m1)

#relation between standardised effect size and publication year

m1_pubyear <- ggplot(dat_long_corrected, aes(x=PublicationYear_clean, y=PartialCorrelationCoefficient))
m1_pubyear <- m1_pubyear + geom_point(color='blue', size=1) + ylab("Partial correlation") + xlab("Publication year") + theme_bw() +   ggtitle("Time trends by publication year") + xlim(1995, 2021)

m1_pubyear <- m1_pubyear + geom_smooth(method=gam, color='black',formula= y ~ s(x, bs = "cs"))
## m1 <- m1 + geom_smooth(method='auto', color='black')
print(m1_pubyear)

#grid plots
library(ggpubr)
fig_time_trends <- ggpubr::ggarrange(
  m1, m1_pubyear, ncol = 1, nrow=2, labels = c("A)", "B)"), 
  common.legend = TRUE, legend = "bottom")
fig_time_trends

filename <- "figures/fig_time_trends.pdf"
ggsave(filename, plot = fig_time_trends, width = 10, height = 8)

mean(dat_2p98p$PartialCorrelationCoefficient)
sd(dat_2p98p$PartialCorrelationCoefficient)

mean(dat_2p98p$StandardErrorPartialCorrelation)
sd(dat_2p98p$StandardErrorPartialCorrelation)

max(dat_SemiBalanceGap$CorrelationCoefficientCorrected)
min(dat_SemiBalanceGap$CorrelationCoefficientCorrected)

mean(dat_SemiBalanceGap$StandardErrorCorrected)
sd(dat_SemiBalanceGap$StandardErrorCorrected)

mean(dat_SemiBalanceGrowth$StandardErrorCorrected)
sd(dat_SemiBalanceGrowth$StandardErrorCorrected)

mean(dat_TaxRevenueGap$CorrelationCoefficientCorrected)
sd(dat_TaxRevenueGap$CorrelationCoefficientCorrected)

mean(dat_TaxRevenueGap$StandardErrorCorrected)
sd(dat_TaxRevenueGap$StandardErrorCorrected)

mean(dat_SpendingElasticity$CorrelationCoefficientCorrected)
sd(dat_SpendingElasticity$CorrelationCoefficientCorrected)

mean(dat_SpendingElasticity$StandardErrorCorrected)
sd(dat_SpendingElasticity$StandardErrorCorrected)

mean(dat_2p98p$CyclicalAdjustment)
sd(dat_2p98p$CyclicalAdjustment)

mean(dat_2p98p$FiscalBalance)
sd(dat_2p98p$FiscalBalance)

mean(dat_2p98p$GovSpend)
sd(dat_2p98p$GovSpend)

mean(dat_2p98p$Taxes)
sd(dat_2p98p$Taxes)

mean(dat_2p98p$CycleGDP)
sd(dat_2p98p$CycleGDP)

mean(dat_2p98p$LaggedCycleVariable)
sd(dat_2p98p$LaggedCycleVariable)

mean(dat_2p98p$DataAdvancedCountries)
sd(dat_2p98p$DataAdvancedCountries)

mean(dat_2p98p$DataDevelopingCountries)
sd(dat_2p98p$DataDevelopingCountries)

mean(dat_2p98p$DataMixofCountries)
sd(dat_2p98p$DataMixofCountries)

mean(dat_2p98p$ShareofDevelopingCountries)
sd(dat_2p98p$ShareofDevelopingCountries)

mean(dat_2p98p$EUonly)
sd(dat_2p98p$EUonly)

mean(dat_2p98p$USAonly)
sd(dat_2p98p$USAonly)

mean(dat_2p98p$LatinAmericaonly)
sd(dat_2p98p$LatinAmericaonly)

mean(dat_2p98p$Africaonly)
sd(dat_2p98p$Africaonly)

mean(dat_2p98p$Asiaonly)
sd(dat_2p98p$Asiaonly)

mean(dat_2p98p$IntraNational)
sd(dat_2p98p$IntraNational)

mean(dat_2p98p$NonOLS)
sd(dat_2p98p$NonOLS)

mean(dat_2p98p$LongRunEffect)
sd(dat_2p98p$LongRunEffect)

mean(dat_2p98p$TacklingEndogeneity)
sd(dat_2p98p$TacklingEndogeneity)

mean(dat_2p98p$RealTimeData)
sd(dat_2p98p$RealTimeData)

mean(dat_2p98p$MeanYearData_c)
sd(dat_2p98p$MeanYearData_c)

mean(dat_2p98p$LaggedDependentVariable)
sd(dat_2p98p$LaggedDependentVariable)

mean(dat_2p98p$NormalizedImpactFactor)
sd(dat_2p98p$NormalizedImpactFactor)

mean(dat_2p98p$ReviewedJournal)
sd(dat_2p98p$ReviewedJournal)

mean(dat_2p98p$Citations)
sd(dat_2p98p$Citations)

mean(dat_2p98p$Primary)
sd(dat_2p98p$Primary)

mean(dat_2p98p$Election)
sd(dat_2p98p$Election)

mean(dat_2p98p$FiscalRule)
sd(dat_2p98p$FiscalRule)

mean(dat_2p98p$FiscalFatigue)
sd(dat_2p98p$FiscalFatigue)

mean(dat_2p98p$NoPublicDebt)
sd(dat_2p98p$NoPublicDebt)

mean(dat_2p98p$PublicationYear_clean)
sd(dat_2p98p$PublicationYear_clean)

mean(dat_2p98p$Preferred)
sd(dat_2p98p$Preferred)

#Descriptive statistics mean, median, unrestricted WLS

#all estimates, partial correlation
median(dat_long_corrected$PartialCorrelationCoefficient)
sd(dat_long_corrected$PartialCorrelationCoefficient)

reguwa_partial <- lm(PartialCorrelationCoefficient~1, data=dat_long_corrected)
summary(reguwa_partial)
confint(reguwa_partial, level=0.95)

#(precision-weighted) average
regwa_partial <- lm(PartialCorrelationCoefficient~1, data=dat_long_corrected, weights=PrecVarianceSECorrected)
summary(regwa_partial)
confint(regwa_partial, level=0.95)

#seDeficitGap
median(dat_2p98p_SemiBalanceGap$CorrelationCoefficientCorrected)
sd(dat_2p98p_SemiBalanceGap$CorrelationCoefficientCorrected)

reguwa_seDeficitGap <- lm(CorrelationCoefficientCorrected~1, data=dat_2p98p_SemiBalanceGap)
summary(reguwa_seDeficitGap)
confint(reguwa_seDeficitGap, level=0.95)

#(precision-weighted) average
regwa_seDeficitGap <- lm(CorrelationCoefficientCorrected~1, data=dat_2p98p_SemiBalanceGap, weights=PrecVarianceSECorrected)
summary(regwa_seDeficitGap)
confint(regwa_seDeficitGap, level=0.95)

#seDeficitGrowth
median(dat_2p98p_SemiBalanceGrowth$CorrelationCoefficientCorrected)
sd(dat_2p98p_SemiBalanceGrowth$CorrelationCoefficientCorrected)

reguwa_seDeficitGrowth <- lm(CorrelationCoefficientCorrected~1, data=dat_2p98p_SemiBalanceGrowth)
summary(reguwa_seDeficitGrowth)
confint(reguwa_seDeficitGrowth, level=0.95)

#(precision-weighted) average
regwa_seDeficitGrowth <- lm(CorrelationCoefficientCorrected~1, data=dat_2p98p_SemiBalanceGrowth, weights=PrecVarianceSECorrected)
summary(regwa_seDeficitGrowth)
confint(regwa_seDeficitGrowth, level=0.95)

#seTaxGap
median(dat_2p98p_TaxRevenueGap$CorrelationCoefficientCorrected)
sd(dat_2p98p_TaxRevenueGap$CorrelationCoefficientCorrected)

reguwa_seTaxGap <- lm(CorrelationCoefficientCorrected~1, data=dat_2p98p_TaxRevenueGap)
summary(reguwa_seTaxGap)
confint(reguwa_seTaxGap, level=0.95)

#(precision-weighted) average
regwa_seTaxGap <- lm(CorrelationCoefficientCorrected~1, data=dat_2p98p_TaxRevenueGap, weights=PrecVarianceSECorrected)
summary(regwa_seTaxGap)
confint(regwa_seTaxGap, level=0.95)

#elSpendingGrowth
median(dat_2p98p_SpendingElasticity$CorrelationCoefficientCorrected)
sd(dat_2p98p_SpendingElasticity$CorrelationCoefficientCorrected)

reguwa_elSpendingGrowth <- lm(CorrelationCoefficientCorrected~1, data=dat_2p98p_SpendingElasticity)
summary(reguwa_elSpendingGrowth)
confint(reguwa_elSpendingGrowth, level=0.95)

#(precision-weighted) average
regwa_elSpendingGrowth <- lm(CorrelationCoefficientCorrected~1, data=dat_2p98p_SpendingElasticity, weights=PrecVarianceSECorrected)
summary(regwa_elSpendingGrowth)
confint(regwa_elSpendingGrowth, level=0.95)

#best-practice: linear predictions
library(rigr)

#partial correlations
#prediction: AdvancedCountries
testReg_1 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_1 <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
lincom(testReg_1, testC_1, robustSE=TRUE)

#prediction: AdvancedCountries + GovSpend
testReg_2 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_2 <- c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
lincom(testReg_2, testC_2, robustSE=TRUE)

#prediction: DevelopingCountries
testReg_3 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_3 <- c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
lincom(testReg_3, testC_3, robustSE=TRUE)

#prediction: DevelopingCountries + GovSpend
testReg_4 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_4 <- c(1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0)
lincom(testReg_4, testC_4, robustSE=TRUE)

#prediction: AdvancedCountries + CyclicalAdjustment
testReg_5 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_5 <- c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
lincom(testReg_5, testC_5, robustSE=TRUE)

#prediction: AdvancedCountries + CyclicalAdjustment + GovSpend
testReg_6 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_6 <- c(1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0)
lincom(testReg_6, testC_6, robustSE=TRUE)

#prediction: DevelopingCountries + CyclicalAdjustment
testReg_7 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_7 <- c(1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0)
lincom(testReg_7, testC_7, robustSE=TRUE)

#prediction: DevelopingCountries + CyclicalAdjustment + GovSpend
testReg_7 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_7 <- c(1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0)
lincom(testReg_7, testC_7, robustSE=TRUE)

#SemiBalanceGap
#prediction: AdvancedCountries + CyclicalAdjustment
testReg_8 <- regress ("mean", CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_2p98p_SemiBalanceGap$PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGap)
testC_8 <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
lincom(testReg_8, testC_8, robustSE=TRUE)

#prediction: DevelopingCountries + CyclicalAdjustment
testReg_9 <- regress ("mean", CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_2p98p_SemiBalanceGap$PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGap)
testC_9 <- c(1, 0, 0, 0, 1, 0, 0, 0, 0)
lincom(testReg_9, testC_9, robustSE=TRUE)

#prediction: DevelopingCountries + CyclicalAdjustment + GovSpend
testReg_10 <- regress ("mean", CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_2p98p_SemiBalanceGap$PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGap)
testC_10 <- c(1, 0, 1, 0, 1, 0, 0, 0, 0)
lincom(testReg_10, testC_10, robustSE=TRUE)

#SemiBalanceGrowth
#prediction: AdvancedCountries
testReg_11 <- regress ("mean", CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect, weights=dat_2p98p_SemiBalanceGrowth$PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGrowth)
testC_11 <- c(1, 0, 0, 0, 0, 0, 0)
lincom(testReg_11, testC_11, robustSE=TRUE)

#prediction: DevelopingCountries
testReg_12 <- regress ("mean", CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect, weights=dat_2p98p_SemiBalanceGrowth$PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGrowth)
testC_12 <- c(1, 0, 0, 0, 1, 0, 0)
lincom(testReg_12, testC_12, robustSE=TRUE)

#GovSpendGrowth
testReg_13 <- regress ("mean", CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect, weights=dat_2p98p_SpendingElasticity$PrecVarianceSECorrected, data=dat_2p98p_SpendingElasticity)
testC_13 <- c(1, 0, 0, 0, 0, 0, 0)
lincom(testReg_13, testC_13, robustSE=TRUE)

#Conclusions
#prediction: AdvancedCountries + TacklingEndogeneity
testReg_1 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_1 <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
lincom(testReg_1, testC_1, robustSE=TRUE)

#prediction: DevelopingCountries + TacklingEndogeneity
testReg_1 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_1 <- c(1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0)
lincom(testReg_1, testC_1, robustSE=TRUE)

#prediction: AdvancedCountries + TacklingEndogeneity + GovSpend
testReg_1 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_1 <- c(1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
lincom(testReg_1, testC_1, robustSE=TRUE)

#prediction: DevelopingCountries + TacklingEndogeneity + GovSpend
testReg_1 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_1 <- c(1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0)
lincom(testReg_1, testC_1, robustSE=TRUE)

#prediction: AdvancedCountries + TacklingEndogeneity + CyclicalAdjustment
testReg_1 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_1 <- c(1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0)
lincom(testReg_1, testC_1, robustSE=TRUE)

#prediction: DevelopingCountries + TacklingEndogeneity + CyclicalAdjustment
testReg_1 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_1 <- c(1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0)
lincom(testReg_1, testC_1, robustSE=TRUE)

#prediction: DevelopingCountries + TacklingEndogeneity + CyclicalAdjustment + GovSpend
testReg_1 <- regress ("mean", PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + CyclicalAdjustment + CycleGDP + GovSpend + Taxes + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_long_corrected$PrecVariance, data=dat_long_corrected)
testC_1 <- c(1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0)
lincom(testReg_1, testC_1, robustSE=TRUE)

#SemiBalanceGap
#prediction: AdvancedCountries + TacklingEndogeneity
testReg_8 <- regress ("mean", CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_2p98p_SemiBalanceGap$PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGap)
testC_8 <- c(1, 0, 0, 0, 0, 0, 1, 0, 0)
lincom(testReg_8, testC_8, robustSE=TRUE)

#prediction: DevelopingCountries + TacklingEndogeneity
testReg_8 <- regress ("mean", CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect + RealTimeData, weights=dat_2p98p_SemiBalanceGap$PrecVarianceSECorrected, data=dat_2p98p_SemiBalanceGap)
testC_8 <- c(1, 0, 0, 0, 1, 0, 1, 0, 0)
lincom(testReg_8, testC_8, robustSE=TRUE)

#GovSpendGrowth
#prediction: AdvancedCountries
testReg_13 <- regress ("mean", CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect, weights=dat_2p98p_SpendingElasticity$PrecVarianceSECorrected, data=dat_2p98p_SpendingElasticity)
testC_13 <- c(1, 0, 0, 0, 0, 0, 1)
lincom(testReg_13, testC_13, robustSE=TRUE)

#prediction: DevelopingCountries
testReg_13 <- regress ("mean", CorrelationCoefficientCorrected ~ StandardErrorCorrected + CyclicalAdjustment + IntraNational + DataDevelopingCountries + DataMixofCountries  + TacklingEndogeneity + LongRunEffect, weights=dat_2p98p_SpendingElasticity$PrecVarianceSECorrected, data=dat_2p98p_SpendingElasticity)
testC_13 <- c(1, 0, 0, 0, 1, 0, 1)
lincom(testReg_13, testC_13, robustSE=TRUE)

