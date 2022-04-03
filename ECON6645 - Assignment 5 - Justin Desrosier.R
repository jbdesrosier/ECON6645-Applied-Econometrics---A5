library(stargazer)
library(MASS)
library(tidyverse)
library(haven)
library(dplyr)
library(plm)
#install.packages("Hmisc")
library(Hmisc)
#install.packages("hexbin")
library(hexbin)

setwd("D:/Documents/MA Economics/Courses - Winter 2022/ECON6645 - Applied Econometrics/Assignment 5")
rm(list=ls())

SLID2 <- read_dta('SLID2.dta')

  SLID2$income <- as.integer(SLID2$income)
  SLID2$id <- as.character(SLID2$id)

  SLID2$ln_income <- log(SLID2$income)
  SLID2$experience2 <- (SLID2$experience)^2
  SLID2$age2 <- (SLID2$age)^2

  SLID2$region <- factor(SLID2$region, levels=c(3,1,2,4,5))
  SLID2$educ_level <- factor(SLID2$region, levels=c(6,1,2,3,4,5,7,8,9,10,11,12))
      #count(SLID2, educ_level)

##  Question 1 pooled OLS
pooled <- plm(ln_income ~
                yrs_schooling + experience + experience2 + age + age2 + vm + female + imm
                + factor(marital) + factor(region),
                    index = c("id","wave"), model="pooling",
                    data = SLID2)
  summary(pooled)

##  Question 2 FE
SLID2 <- SLID2 %>%
            group_by(id) %>%
            filter(n_distinct(wave)==2)

fe <- plm(ln_income ~
            yrs_schooling + experience + experience2 + age + age2 + vm + female + imm
            + factor(marital) + factor(region),
                index = c("id","wave"), model="within",
                data = SLID2)
  summary(fe)


stargazer(pooled, fe, header=FALSE, type="latex")

star <- stargazer(pooled, fe, header=FALSE, type="latex",
          style = "qje", notes.append = FALSE, notes.align = "l",
          notes = "to be replaced")
note.latex <- "\\multicolumn{5}{l} {\\parbox[t]{11cm}{ \\textit{Notes:} (1) Pooled OLS Regression (2) Fixed Effects (within) Regression, Index: Respondent ID, Wave (1993, 1994) of the Survey of Labour and Income Dynamics. [$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01]}} \\\\"
star[grepl("Note",star)] <- note.latex
cat (star, sep = "\n")

#tests
SLID2 <- SLID2 %>%
            group_by(id) %>%
            mutate(diff_educ= yrs_schooling-Lag(yrs_schooling))
  mean(SLID2$diff_educ, na.rm=T)
SLID2 <- SLID2 %>%
            group_by(id) %>%
            mutate(diff_income= income-Lag(income))
  mean(SLID2$diff_income, na.rm=T)

SLID3 <- SLID2 %>% drop_na(diff_educ, diff_income)
    sapply(split(SLID3, SLID3$diff_educ > 0), function(x) mean(x$diff_income))


#plots
ggplot((SLID2 %>%
              filter(income<200000, yrs_schooling>=10)),
       aes(x=yrs_schooling, y=income)) +
  geom_hex(bins=25,
           aes(fill = stat(cut(count,
                               breaks = c(1,25,75,200,Inf),
                               labels=F,
                               right=T,
                               include.lowest=T)))) +
  scale_fill_continuous(name = 'Respondents',
                        labels = c('1','25','75','200+')) +
  geom_smooth(method='loess', se=F, colour='red') +
  xlab("Years of Schooling") +
  ylab("Income") +
  ggtitle("Returns on Schooling") +
  theme(legend.position = c(0.11, 0.77),
        legend.background = element_rect(fill="white"))


ggplot((SLID2 %>%
          filter(income<200000)), aes(x=age,
                                      y=income)) +
  geom_hex(bins=30,
           aes(fill = stat(cut(count, breaks = c(1,25,75,200,Inf), labels=F,right=T,include.lowest=T)))) +
  scale_fill_continuous(name = 'Respondents', labels = c('1','25','75','200', '250+')) +
  geom_smooth(method='loess', se=F, colour='red') +
  xlab("Years of Schooling") +
  ylab("Income") +
  ggtitle("Returns on Schooling") +
  theme(legend.position = c(0.11, 0.77),
        legend.background = element_rect(fill="white"))






