
library('survival')
require('coxme')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv('table_episodic_jour.csv')

df$richness <- df$no_subreddits #log(df$no_subreddits)+1
df$evenness <- (df$pielou_eve)
df$addict_even <- df$addict_pielou_eve
df$reco_even <- df$reco_pielou_eve
df$other_even <- df$other_pielou_eve

myvars <- c("user_id",  "start",  "stop", "event", "richness", "evenness", 'recovery_addiction_id',
            'drug', 'addiction', 'recovery', 'others', 'addict_even', 'reco_even', 'other_even', 
            'total_posts','drug_posts', 'recovery_posts','addiction_posts', 'other_posts')


df <- df[myvars]

relativemeasure <- function(a, b){
   c=(a)/(b)
   return(c)
}
df$drug_rel <- apply(df[,c('drug','richness')], 1, function(x) relativemeasure(x[1],x[2]))
df$addic_rel <- apply(df[,c('addiction','richness')], 1, function(x) relativemeasure(x[1],x[2]))
df$rec_rel <- apply(df[,c('recovery','richness')], 1, function(x) relativemeasure(x[1],x[2]))
df$other_rel <- apply(df[,c('others','richness')], 1, function(x) relativemeasure(x[1],x[2]))


refined_df <- df
nrow(refined_df)

relapsed_users <- subset(refined_df, event==1)
length(unique(relapsed_users$user_id))
`%notin%` <- Negate(`%in%`)
recovered_users <- refined_df[refined_df$user_id %notin% relapsed_users$user_id,]
length(unique(recovered_users$user_id))


fit.tdc <- coxph(Surv(start,stop,event)~ recovery_addiction_id + cluster(user_id),refined_df)
fit.tdc
fit.tdc <- coxph(Surv(start,stop,event)~ addiction + cluster(user_id),refined_df)
fit.tdc
fit.tdc <- coxph(Surv(start,stop,event)~ addic_rel + cluster(user_id),refined_df)
fit.tdc
fit.tdc <- coxph(Surv(start,stop,event)~ addiction_posts + cluster(user_id),refined_df)
fit.tdc
fit.tdc <- coxph(Surv(start,stop,event)~ addiction+addiction_posts + cluster(user_id),refined_df)
fit.tdc


fit.tdc <- coxph(Surv(start,stop,event)~ recovery + cluster(user_id),refined_df)
fit.tdc
fit.tdc <- coxph(Surv(start,stop,event)~ rec_rel + cluster(user_id),refined_df)
fit.tdc
fit.tdc <- coxph(Surv(start,stop,event)~ recovery_posts + cluster(user_id),refined_df)
fit.tdc
fit.tdc <- coxph(Surv(start,stop,event)~ recovery+ recovery_posts + cluster(user_id),refined_df)
fit.tdc



fit.tdc <- coxph(Surv(start,stop,event)~ others + cluster(user_id),refined_df)
fit.tdc
fit.tdc <- coxph(Surv(start,stop,event)~ other_rel + cluster(user_id),refined_df)
fit.tdc
fit.tdc <- coxph(Surv(start,stop,event)~ other_posts + cluster(user_id),refined_df)
fit.tdc
fit.tdc <- coxph(Surv(start,stop,event)~ other_posts + others + cluster(user_id),refined_df)
fit.tdc

fit.tdc <- coxph(Surv(start,stop,event)~  addiction + recovery + others + cluster(user_id),refined_df)
fit.tdc


## droping nans
setwd('/home/en291/Elahe/PythonCode/myCode/addiction_depression_Identity/data/opiatesrecovery/tables/')

df <- read.csv('table_episodic_liwcmodified_jour.csv')
df$richness <- df$no_subreddits #log(df$no_subreddits)+1
df$evenness <- (df$pielou_eve)
df$addict_even <- df$addict_pielou_eve
df$reco_even <- df$reco_pielou_eve
df$other_even <- df$other_pielou_eve

myvars <- c('user_id', 'start', 'stop','event', 'other_even')
refined_df <- df[myvars]


relapsed_users <- subset(refined_df, event==1)
length(unique(relapsed_users$user_id))
recovered_users <- refined_df[refined_df$user_id %notin% relapsed_users$user_id,]
length(unique(recovered_users$user_id))

nan_df <- refined_df[which(is.na(refined_df$other_even)), ]
relapsed_users <- nan_df[which(nan_df$event==1),]
nrow(relapsed_users)
`%notin%` <- Negate(`%in%`)
refined_df <- refined_df[refined_df$user_id %notin% relapsed_users$user_id,]
refined_df <- na.omit(refined_df)

relapsed_users <- subset(refined_df, event==1)
length(unique(relapsed_users$user_id))

recovered_users <- refined_df[refined_df$user_id %notin% relapsed_users$user_id,]
length(unique(recovered_users$user_id))

nrow(refined_df)
fit.tdc <- coxph(Surv(start,stop,event)~ other_even + cluster(user_id),refined_df)
fit.tdc




