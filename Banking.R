

train = read.csv(choose.files())
test = read.csv(choose.files())

head(train)

apply(train, 2, function(x)sum(is.na(x)))
apply(test, 2, function(x)sum(is.na(x)))


test$y = NA
train$data = 'train'
test$data = 'test'
all_data = rbind(train,test)


library(dplyr)
apply(all_data, 2, function(x)sum(is.na(x)))
glimpse(all_data)


t = table(all_data$job)
t
sort(t)

tt = round(prop.table(table(all_data$job,all_data$y),1)*100,1)
tt

ttt = addmargins(tt,2)
ttt
sort(ttt[,1])

all_data=all_data %>% 
  mutate(job_1=as.numeric(job %in% c("self-employed","unknown","technician")), 
         job_2=as.numeric(job %in% c("services","housemaid","entrepreneur")),
         job_3=as.numeric(job %in% c("management","admin")),
         job_4=as.numeric(job=="student"),
         job_5=as.numeric(job=="retired"),
         job_6=as.numeric(job=="unemployed")) %>% 
  select(-job)

glimpse(all_data)

t = table(all_data$marital)
t
sort(t)

all_data=all_data %>% 
  mutate(divorced=as.numeric(marital %in% c("divorced")),
         single=as.numeric(marital %in% c("single"))
  ) %>% 
  select(-marital)


glimpse(all_data)

t = table(all_data$education)
t
sort(t)
all_data=all_data %>% 
  mutate(edu_primary=as.numeric(education %in% c("primary")),
         edu_sec=as.numeric(education %in% c("secondary")),
         edu_tert=as.numeric(education %in% c("tertiary"))
  ) %>% 
  select(-education)

glimpse(all_data)

t = table(all_data$default)
t

all_data$default=as.numeric(all_data$default=="yes")
all_data$housing=as.numeric(all_data$housing=="yes")

glimpse(all_data)


table(all_data$loan)
all_data$loan = as.numeric(all_data$loan == 'yes')

glimpse(all_data)


t = table(all_data$contact)
t
sort(t)
all_data=all_data %>% 
  mutate(co_cellular=as.numeric(contact %in% c("cellular")),
         co_tel=as.numeric(contact %in% c("telephone"))
  ) %>% 
  select(-contact)

glimpse(all_data)

t = table(all_data$month)
t
sort(t)
fm=round(prop.table(table(all_data$month,all_data$y),1)*100,1)
fm
tt = addmargins(fm,2)
tt
sort(tt[,1])
all_data=all_data %>% 
  mutate(month_1=as.numeric(month %in% c("aug","jun","nov","jan","jul")), 
         month_2=as.numeric(month %in% c("dec","sep")),
         month_3=as.numeric(month=="mar"),
         month_4=as.numeric(month=="oct"),
         month_5=as.numeric(month=="apr"),
         month_6=as.numeric(month=="feb")) %>% 
  select(-month)

glimpse(all_data)

t = table(all_data$poutcome)
t
sort(t)
all_data=all_data %>% 
  mutate(poc_success=as.numeric(poutcome %in% c("success")),
         poc_failure=as.numeric(poutcome %in% c("failure")),
         poc_other=as.numeric(poutcome %in% c("other"))
  )%>% 
  select(-poutcome)

glimpse(all_data)

table(all_data$y)

all_data$y = as.numeric(all_data$y == "yes")
t = table(all_data$y)
t

glimpse(all_data)


train = all_data %>% 
  filter(data=='train') %>% 
  select(-data)

test = all_data %>% 
  filter(data=='test') %>% 
  select(-data,-y)

set.seed(33)
s = sample(1:nrow(train),0.75*nrow(train))
train1 = train[s,]
test1 = train[-s,]


library(car)
vif_ = lm(y~. ,data = train)
summary(vif_)

colnames(train)
fit_train=train %>% 
  select(-edu_sec)


fit_ = glm(y~. ,family = 'binomial',data = fit_train)
summary(fit)

fit = step(fit)
fit_final=glm(y~balance + housing + loan + duration + campaign + ID + 
                job_3 + job_5 + divorced + single + edu_primary + 
                co_cellular + co_tel + month_1 + month_2 + month_3 + month_4 + 
                month_5 + month_6 + poc_success + poc_failure + poc_other ,data=fit_train,family="binomial")
summary(fit_final)

train$score = predict(fit_final,newdata = train,type = "response")

head(train$score)

library(ggplot2)
ggplot(train,aes(y=y,x=score,color=factor(y)))+
  geom_point()+geom_jitter()



cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)
for (i in cutoffs){
  predicted=as.numeric(train$score>i)
  
  TP=sum(predicted==1 & train$y==1)
  FP=sum(predicted==1 & train$y==0)
  FN=sum(predicted==0 & train$y==1)
  TN=sum(predicted==0 & train$y==0)
  cutoff_data=rbind(cutoff_data,c(i,TP,FP,FN,TN))
}

cutoff_data=cutoff_data[-1,]
cutoff_data

cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP, #total positives and negatives
         Sn=TP/P, #sensitivity
         Sp=TN/N, #specificity
         KS=abs((TP/P)-(FP/N)),
         Accuracy=(TP+TN)/(P+N),
         Lift=(TP/P)/((TP+FP)/(P+N)),
         Precision=TP/(TP+FP),
         Recall=TP/P
  ) %>% 
  select(-P,-N)

KS_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
KS_cutoff


test$score=predict(fit_final,newdata =test,type = "response")

test$left=as.numeric(test$score>KS_cutoff)
table(test$left)

test$leftfinal=factor(test$left,levels = c(0,1),labels=c("no","yes"))
table(test$leftfinal)

test1$score=predict(fit_final,newdata =test1,type = "response")
table(test1$y,as.numeric(test1$score>KS_cutoff))
table(test1$y)


# TP = 5831 FN = 1164 
# FP = 113  TN = 804
5831 + 804 + 113 + 804
6635/7552


library(pROC)

roccurve=roc(test1$y,test1$score)  
plot(roccurve)

auc(roccurve)
# 0.9248

