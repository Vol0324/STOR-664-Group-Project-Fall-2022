 rm(list = ls())
 setwd("desktop")


##### initial data clearning #####
total_stats = read.csv("./NBA_season_stats_from_basketball_references-2017-18_total.csv")
salary = read.csv("./NBA_season1718_salary_from_Kaggle.csv")

#### Remove accent marks from players in total_stats ----
require(stringi)
players_noaccent = sapply(X = total_stats$Player, FUN = stri_trans_general, id = "Latin-ASCII")
total_stats$Player.noaccent = players_noaccent

length(setdiff(salary$Player, total_stats$Player.noaccent)) ## 75
length(setdiff(total_stats$Player.noaccent, salary$Player)) ## 80
length(which(duplicated(salary$Player))) ## 38
length(which(duplicated(total_stats$Player))) ## 124

#### Remove players that appear more than once from salary ----
salary = salary[-c(which(duplicated(salary$Player))), ]

#### Remove duplicate rows in total_stats ----
require(tidyverse)
total_stats.noDuplicates =  total_stats %>% distinct(Player.noaccent, .keep_all = TRUE) 

#### Merge salary and data and drop uselesss columns
final_dataset = merge(x = total_stats.noDuplicates, y = salary, by.x = "Player.noaccent", by.y = "Player") %>%
  select(-c("Player.noaccent", "Rk", "Player.additional", "X", "Tm.x", "Tm.y")) %>%
  select("Player", "season17_18", everything()) %>%
  rename(salery = "season17_18")
  

final_dataset2 = merge(x = total_stats.noDuplicates, y = salary, by.x = "Player.noaccent", by.y = "Player") %>%
  select(-c("Player.noaccent", "Rk", "Player.additional", "X", "Tm.x", "Tm.y")) %>%
  mutate(MPG = MP/G, PPG = PTS/G, APG = AST/G, 
         RPG = TRB/G, TOPG = TOV/G, BPG = BLK/G, 
         SPG = STL/G) %>%
  rename(salary = "season17_18") 
  


write.csv(final_dataset, "./final_dataset.csv")
write.csv(final_dataset2, "./final_dataset2.csv")




# initial variable selection 
data = read.csv("./final_dataset2.csv")
data = data[data[,"G"] >= 40,]      #screen out less statistically significant data points
data_reduced = data[,c('Player', 'Pos', 'salary','Age','FG.','FT.','MPG','PPG','APG','RPG','TOPG','BPG','SPG')]

corrplot()


datapr = prcomp(data_reduced[,-c(1,2,3)], scale = TRUE)
summary(datapr)         # proportion explained by the principal components
round(datapr$rot[,1],4)       # load of the first principal component



#### variable transformation (linearity)
attach(mtcars)
par(mfrow=c(2,4))
plot(data_reduced$salary, data_reduced$MPG)
plot(data_reduced$salary, data_reduced$PPG)
plot(data_reduced$salary, data_reduced$APG)
plot(data_reduced$salary, data_reduced$RPG)
plot(data_reduced$salary, data_reduced$TOPG)
plot(data_reduced$salary, data_reduced$BPG)
plot(data_reduced$salary, data_reduced$SPG)



attach(mtcars)
par(mfrow=c(2,4))
hist(data_reduced$MPG)
hist(data_reduced$PPG)
hist(data_reduced$APG)
hist(data_reduced$RPG)
hist(data_reduced$TOPG)
hist(data_reduced$BPG)
hist(data_reduced$SPG)




par(mfrow=c(2,2))
model1 = lm(salary~MPG+PPG+APG+RPG+TOPG+BPG+SPG, data = data_reduced)
summary(model1) # rss 5761000
plot(model1)

model2 = lm(log(salary)~MPG+PPG+APG+RPG+TOPG+BPG+SPG, data = data_reduced)
summary(model2)     # rss 0.8022
plot(model2)

model3 = lm(sqrt(salary)~MPG+PPG+APG+RPG+TOPG+BPG+SPG, data = data_reduced)
summary(model3)     # rss 977.2
plot(model3)




# variable/model selection (diff by position)
data_PG =  data_reduced[data_reduced[,2] == "PG", ];
data_PF =  data_reduced[data_reduced[,2] == "PF", ];
data_SF =  data_reduced[data_reduced[,2] == "SF", ];
data_C = data_reduced[data_reduced[,2] == "C", ];
data_SG = data_reduced[data_reduced[,2] == "SG", ];

corrplot(cor(data_PG[,-c(1,2)]), method = 'number')
corrplot(cor(data_PF[,-c(1,2)]), method = 'number')
corrplot(cor(data_SF[,-c(1,2)]), method = 'number')
corrplot(cor(data_C[,-c(1,2)]), method = 'number')
corrplot(cor(data_SG[,-c(1,2)]), method = 'number')



require(leaps) 
# PG
dim(data_PG)
n = 64
m_AIC_reg = regsubsets(log(salary)~MPG+PPG+APG+RPG+TOPG+BPG+SPG, data=data_PG)
m_AIC_regs = summary(m_AIC_reg)
m_AIC_regs$which
AIC = n*log(m_AIC_regs$rss/n) + (2:8)*2
which.min(AIC)
plot(2:8,m_AIC_regs$cp,xlab="No. of Parameters", ylab="Cp Statistic")
abline(0,1)


sort(data_PG[,'BPG'], decreasing = TRUE, index.return = TRUE)
m_temp = lm(log(salary)~MPG+PPG+APG+RPG+TOPG+BPG+SPG, data=data_PG)
influence.measures(m_temp)      #2, 16, 19, 31, 37, 49, 55, use  which(data_PG == 'Stephen Curry', arr.ind=TRUE)


data_PG2 = data_PG[-c(2, 16, 19, 31, 37, 49, 55),]
n2 = 57
m_AIC_reg2 = regsubsets(log(salary)~MPG+PPG+APG+RPG+TOPG+BPG+SPG, data=data_PG2)
m_AIC_regs2 = summary(m_AIC_reg2)
m_AIC_regs2$which
AIC2 = n2*log(m_AIC_regs2$rss/n) + (2:8)*2
which.min(AIC2)
corrplot(cor(data_PG2[,-c(1,2)]), method = 'number')


m_AIC_reg3 = regsubsets(log(salary)~MPG+PPG+APG+RPG+TOPG+sqrt(BPG)+SPG, data=data_PG)
m_AIC_regs3 = summary(m_AIC_reg3)
m_AIC_regs3$which
AIC3 = n*log(m_AIC_regs3$rss/n) + (2:8)*2





attach(mtcars)
par(mfrow=c(2,4))
plot(log(data_PG$salary), data_PG$MPG)
plot(log(data_PG$salary), data_PG$PPG)
plot(log(data_PG$salary), data_PG$APG)
plot(log(data_PG$salary), data_PG$RPG)
plot(log(data_PG$salary), data_PG$TOPG)
plot(log(data_PG$salary), sqrt(data_PG$BPG))
plot(log(data_PG$salary), data_PG$SPG)


par(mfrow=c(2,4))
hist(data_PG$MPG)
hist(data_PG$PPG)
hist(data_PG$APG)
hist(data_PG$RPG)
hist(data_PG$TOPG)
hist(data_PG$BPG)
hist(data_PG$SPG)











# PF
dim(data_PF)
n = 62
m_AIC_reg = regsubsets(log(salary)~MPG+PPG+APG+RPG+TOPG+BPG+SPG, data=data_PF)
m_AIC_regs = summary(m_AIC_reg)
m_AIC_regs$which
AIC = n*log(m_AIC_regs$rss/n) + (2:8)*2
which.min(AIC)
which.max(m_AIC_regs$adjr2)



# SF
dim(data_SF)
n = 44
m_AIC_reg = regsubsets(log(salary)~MPG+PPG+APG+RPG+TOPG+BPG+SPG, data=data_SF)
m_AIC_regs = summary(m_AIC_reg)
m_AIC_regs$which
AIC = n*log(m_AIC_regs$rss/n) + (2:8)*2
which.min(AIC)
which.max(m_AIC_regs$adjr2)


# C
dim(data_C)
n = 73
m_AIC_reg = regsubsets(log(salary)~MPG+PPG+APG+RPG+TOPG+BPG+SPG, data=data_C)
m_AIC_regs = summary(m_AIC_reg)
m_AIC_regs$which
AIC = n*log(m_AIC_regs$rss/n) + (2:8)*2
which.min(AIC)
which.max(m_AIC_regs$adjr2)


# SG
dim(data_SG)
n = 77
m_AIC_reg = regsubsets(log(salary)~MPG+PPG+APG+RPG+TOPG+BPG+SPG, data=data_SG)
m_AIC_regs = summary(m_AIC_reg)
m_AIC_regs$which
AIC = n*log(m_AIC_regs$rss/n) + (2:8)*2
which.min(AIC)
which.max(m_AIC_regs$adjr2)







rmse <- function(x,y) sqrt(mean((x-y)^2))
test_data_PG = data_PG[seq_len(nrow(data_PG)) %% 10 == 0,];
train_data_PG = data_PG[seq_len(nrow(data_PG)) %% 10 != 0,];
dim(train_data_PG)
n = 290


m_AIC = regsubsets(log(salary)~MPG+PPG+APG+RPG+TOPG+BPG+SPG, data=train_data)
m_AIC_regs = summary(m_AIC)
m_AIC_regs$which
AIC = n*log(m_AIC_regs$rss/n) + (2:8)*2
par(mfrow=c(1,1))
plot(AIC~I(1:7), ylab="AIC", xlab="Number of Predictors")



















