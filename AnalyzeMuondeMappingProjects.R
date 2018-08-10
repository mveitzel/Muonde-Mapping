# This script creates a data frame out of the data given in Table 1
# of the main paper and tests the hypothesis that the origin of a
# mapping project idea can predict the scope of the project.  See
# the text for definitions of these concepts.

d<-data.frame(Idea=c(rep("Community",13),rep("Visitors",8)),Scope=as.factor(c(1,2,2,3,3,3,4,4,4,5,5,5,5, 1,2,2,2,3,4,4,4)),VisitorPresent=c(F,T,F,F,F,F,F,F,F,F,F,F,F, T,F,F,T,F,T,T,T),Name=c(
"Distance for Electrification",
"Mudhomori Village grazing land",
"Mudhomori Locations and Roads",
"Madzoke Valley Stone Wall Projects",
"Utaya Irrigation Project",
"Mhototi Ward Borehole Wells",
"Proposed Muonde Trust Center",
"War bases",
"Sacred Forest (Rambotemwa) Mapping",
"Zvishavane Town (old and new)",
"ZESA Electrical Lines",
"Mlezu Agricultural College",
"Kufunda Village",
"Individual homes (bindu)",
"Chinguo Village Sample Households",
"Distance to Services",
"Zoza Garden",
"Higgs quadrat resurvey",
"Great Zimbabwe",
"Matobo National Park",
"Lake Kyle (Mutirikwi) Recreational Park"))
Scopes<-data.frame(Scope=c(1,2,3,4,5),ScopeName=c("Individual","Village","Ward","Communal Area","Regional/National"))
d<-merge(d,Scopes,by.x="Scope",by.y="Scope")

library(MASS)
fit.full<-polr(Scope~Idea,data=d,Hess=TRUE)
fit.red<-polr(Scope~1,data=d,Hess=TRUE)
#LR test for unbalanced design
anova(fit.full,fit.red) #0.16 (N=21)

fit.full2<-polr(Scope~Idea,data=d,Hess=TRUE,subset=!(d$VisitorPresent))
fit.red2<-polr(Scope~1,data=d,Hess=TRUE,subset=!(d$VisitorPresent))
#LR test for unbalanced design
anova(fit.full2,fit.red2) #0.08 (N=15)


