#install.packages("xtable")
library(xtable)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("stargazer")
library(stargazer)
#install.packages("margins")
library(margins)
#install.packages("MASS")
library(MASS)
library(tidyverse)
library(haven)
#install.packages("ordinal")
library(ordinal)
#install.packages("oglmx")
library("oglmx")

setwd("D:/Documents/MA Economics/Courses - Winter 2022/ECON6645 - Applied Econometrics/Assignment 4")
rm(list=ls())

CCHS <- read_dta('CCHS_2016.dta')
# attributes(CCHS$marital)
#CCHS$income <- as.integer(CCHS$income)

tab1 <- CCHS %>%
  group_by(marital, illicit) %>%
  summarise(n = n(),
            median_income = median(income, na.rm=T))

tab2 <- CCHS %>%
  group_by(ehg2dvr3, illicit) %>%
  summarise(n = n(),
            median_income = median(income, na.rm=T))

tab3 <- CCHS %>%
  group_by(visible_minority, immigrant, illicit) %>%
  summarise(n = n(),
            median_income = median(income, na.rm=T)
            ) %>%
  print(n=50)

tab4 <- CCHS %>%
  group_by(female,  illicit) %>%
  summarise(n = n(),
            median_income = median(income, na.rm=T),
            age_q25 = quantile(age, .25),
            age_q50 = quantile(age, .50),
            age_q75 = quantile(age, .75),
            ) %>%
  print(n=50)

print(xtable(tab1, type = "latex"), file = "tab1.tex")
print(xtable(tab2, type = "latex"), file = "tab2.tex")
print(xtable(tab3, type = "latex"), file = "tab3.tex")
print(xtable(tab4, type = "latex"), file = "tab4.tex")






CCHS_plot1 <- CCHS %>%
  mutate(income_decile =ntile(income, 20)) %>%
  filter(illicit==1) %>%
  group_by(income_decile) %>%
  summarise(sum_illicit=sum(illicit))
#CCHS$income_decile <- quantile(CCHS$income, probs = c(0.1,0.2,0.3,0.4,0.5,
#                                 0.6,0.7,0.8,0.9), na.rm=T)

plot1 <- ggplot(CCHS_plot1, aes(x=income_decile, y=sum_illicit)) +
         geom_bar(stat='identity') +
         labs(title="Substance Use by Income Vigintile",
              x = "Income Quantile %",
              y = "Respondents with Recent Substance Use")

CCHS_plot2 <- CCHS %>%
  mutate(age_decile =ntile(age, 20)) %>%
  filter(illicit==1) %>%
  group_by(age_decile) %>%
  summarise(sum_illicit=sum(illicit))
#CCHS$income_decile <- quantile(CCHS$income, probs = c(0.1,0.2,0.3,0.4,0.5,
#                                 0.6,0.7,0.8,0.9), na.rm=T)

plot2 <- ggplot(CCHS_plot2, aes(x=age_decile, y=sum_illicit)) +
  geom_bar(stat='identity') +
  labs(title="Substance Use by Age Vigintile",
       x = "Age Quantile %",
       y = "Respondents with Recent Substance Use")

plot_final <- grid.arrange(plot1, plot2, ncol=2)


CCHS$ln_income <- log(CCHS$income)
CCHS$income2 <- (CCHS$income^2)
CCHS$ehg2dvr3 <- factor(CCHS$ehg2dvr3, levels=c(3,1,2,6,7,8,9))
CCHS$marital <- factor(CCHS$marital, levels=c(4,1,2,3,6,7,8,9))

lpm <- lm(illicit ~
          ln_income + income2 + factor(ehg2dvr3) + factor(marital) + immigrant
            + visible_minority + female + age,
          CCHS)
probit <- glm(illicit ~
          ln_income + income2 + factor(ehg2dvr3) + factor(marital) + immigrant
            + visible_minority + female + age,
          family = binomial(link="probit"),
          CCHS)
#print(xtable(probit), file='probit1.tex', compress=F)
stargazer(lpm, probit, title="Q1 Probabilistic Regression", align=T)


#probit1 <- glm(illicit ~
#               ln_income,
#               family = binomial(link="probit"),
#               CCHS)
#plot(x=CCHS$illicit,
#     y=CCHS$ln_income,
#     pch=20,
#     ylim=c(-0.4,1.4),
#     cex.main=0.85)
#abline(h = 1, lty = 2, col = "darkred")
#abline(h = 0, lty = 2, col = "darkred")
#text(2.5, 0.9, cex = 0.8, "Mortgage denied")
#text(2.5, -0.1, cex= 0.8, "Mortgage approved")
#x <- seq(0, 3, 0.01)
#y <- predict(probit1, list(ln_income = x), type = "response")
#lines(x, y, lwd = 1.5, col = "steelblue")







lpm2 <- lm(gendvmhi ~
               scale(ln_income) + scale(income2) + factor(ehg2dvr3) + factor(marital) + immigrant
                + visible_minority + female + scale(age),
               CCHS)

ord.probit <- polr(factor(gendvmhi) ~
                    scale(ln_income) + scale(income2) + factor(ehg2dvr3) + factor(marital) + immigrant
                    + visible_minority + female + scale(age),
                    CCHS,
                    method="probit",
                    na.action = na.omit, Hess = TRUE)
ord.probit.sum <- summary(ord.probit)
ord.probit.AME <- marginal_effects(margins(ord.probit))

stargazer(lpm2, ord.probit, ord.probit.AME, title="Q1 Ordered Probabilistic Regression", align=T)

#cplot(ord.probit, what="stacked")

#ord.probit2 <- clm(as.factor(gendvmhi) ~
#                     ln_income + income2 + ehg2dvr3 + marital + immigrant
#                     + visible_minority + female + age,
#                     na.omit(CCHS),
#                   link="probit")
#summary(ord.probit2)





