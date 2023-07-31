library (tidyverse)
library(readxl)


db<-read_excel(path = "database/base orientacion.xlsx")
db$orientation<-as.factor(db$orientation)
db$site<-as.factor(db$site)
db$prop_rep<- (db$rep_count/db$count)
a<- glm(rep_count ~ orientation+ observer
        + site, db, family = quasipoisson, na.action = na.omit)
anova(a, test = "Chisq")
summary (a)

b<- lm(prop_rep ~ orientation+ observer
        + site, db, na.action = na.omit, subset = count != 0)
anova (b)
