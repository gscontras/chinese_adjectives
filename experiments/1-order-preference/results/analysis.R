library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)

setwd("~/git/chinese_adjectives/experiments/1-order-preference/Submiterator-master")

num_round_dirs = 5
df1 = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    '../Submiterator-master/round', i, '/chinese-order2.csv', sep='')) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df1, select=c("workerid","nounEnglish","gender","nounclass","slide_number", "predicate1English", "predicate2English", "class1","class2","response","sense","condition","language","age","assess","education","lived","proficiency","homeLanguage","yearsLived","outsideLanguage"))

# only lived both before and after 8 in Chinese-speaking country
d = d[d$lived=="both8",]
# only 5plus years in Chinese-speaking country
d = d[d$yearsLived=="5plus",]
# only native Chinese speakers
d = d[d$language=="chinese"|d$language=="Chinese"|d$language=="Mandarin "|d$language=="中文"|d$language=="普通話",]


length(unique(d$workerid)) # n=7

summary(d)

d$predicate1 = d$predicate1English
d$predicate2 = d$predicate2English
d$noun = d$nounEnglish

#write.csv(d,"~/Documents/git/cocolab/adjective_ordering/experiments/analysis/order-preference-trimmed.csv")

#####
## sense analysis
#####

table(d$sense,d$condition)


#####
## duplicate observations by first predicate
#####

library(tidyr)
o <- d
o$rightpredicate1 = o$predicate2
o$rightpredicate2 = o$predicate1
o$rightresponse = 1-o$response
agr = o %>% 
        select(predicate1,rightpredicate1,response,rightresponse,workerid,noun,nounclass,class1,class2) %>%
        gather(predicateposition,predicate,predicate1:rightpredicate1,-workerid,-noun,-nounclass,-class1,-class2)
agr$correctresponse = agr$response
agr[agr$predicateposition == "rightpredicate1",]$correctresponse = agr[agr$predicateposition == "rightpredicate1",]$rightresponse
agr$correctclass = agr$class1
agr[agr$predicateposition == "rightpredicate1",]$correctclass = agr[agr$predicateposition == "rightpredicate1",]$class2
head(agr[agr$predicateposition == "rightpredicate1",])
agr$response = NULL
agr$rightresponse = NULL
agr$class1 = NULL
agr$class2 = NULL
nrow(agr) #2340
#write.csv(agr,"~/Documents/git/cocolab/adjective_ordering/experiments/analysis/naturalness-duplicated.csv")

adj_agr = aggregate(correctresponse~predicate*correctclass,FUN=mean,data=agr)
adj_agr

class_agr = aggregate(correctresponse~correctclass,FUN=mean,data=agr)
class_agr
