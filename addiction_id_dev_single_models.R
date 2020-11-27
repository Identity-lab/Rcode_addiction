library(lme4)
library(nlme)
library(ggplot2)
library('dplyr')
library(ggpubr)
library('sjPlot')
library('sjmisc')
library('caret')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv('table_episodic_jour.csv')

df$richness <- df$no_subreddits #log(df$no_subreddits)+1
df$evenness <- df$pielou_eve
df$addict_even <- df$addict_pielou_eve
df$reco_even <- df$reco_pielou_eve
df$other_even <- df$other_pielou_eve

drops <- c('simpson_eve', 'pielou_eve', 'no_posts', 'addicreco_rate', 'addiction_rate', 'drug_rate', 'recovery_rate')
df <- df[ , !(names(df) %in% drops)]
head(df)


relativemeasure <- function(a, b){
  c=(a)/(b)
  return(c)
}
df$drug_rel <- apply(df[,c('drug','richness')], 1, function(x) relativemeasure(x[1],x[2]))
df$other_rel <- apply(df[,c('others','richness')], 1, function(x) relativemeasure(x[1],x[2]))
df$addic_rel <- apply(df[,c('addiction','richness')], 1, function(x) relativemeasure(x[1],x[2]))
df$rec_rel <- apply(df[,c('recovery','richness')], 1, function(x) relativemeasure(x[1],x[2]))

head(df)


# dropping rows with nan values, for relapssed users, dropping the whole user dataset
# we can do it by dropping the relapsed users that their last month contains nan, or by just dropping the nans
# as here, I'm trying to show the trajectory over time, there is no need to drop all the data from relapsed 
# users whose event row contain nan

#nan_df <- df[which(is.na(df$evenness)), ]
#relapsed_users <- nan_df[which(nan_df$event==1),]

#`%notin%` <- Negate(`%in%`)
#refined_df <- df[df$user_id %notin% relapsed_users$user_id,]

#refined_df <- na.omit(refined_df)
#nrow(refined_df)
###

refined_df <- df
refined_df$stop <- as.integer(refined_df$stop)
refined_df$user_id <- as.factor(refined_df$user_id)
refined_df$event <- as.factor(refined_df$event)


# building a table for multi-level analysis
relapsed_users <- subset(refined_df, event==1)
relapsed_df <- refined_df[refined_df$user_id %in% relapsed_users$user_id,]
mean(relapsed_df$others)
length(unique(relapsed_df$user_id))
relapsed_df$event = 1
nrow(relapsed_df)


`%notin%` <- Negate(`%in%`)
recovered_users <- refined_df[refined_df$user_id %notin% relapsed_users$user_id,]
recovered_df <- refined_df[refined_df$user_id %in% recovered_users$user_id,]
mean(recovered_df$others)
length(unique(recovered_df$user_id))
recovered_df$event = 0
nrow(recovered_df)

final_df <- rbind(relapsed_df,recovered_df)

###

final_df$stop <- as.numeric(final_df$stop)
final_df$user_id <- as.factor(final_df$user_id)
final_df$Relapse <- as.factor(final_df$event)
final_df$Relapse <- as.logical(final_df$event)



# limiting months of recovery
final_df <-subset(final_df, stop <= 24)
nrow(final_df)


###### defining new features
x_centered <- function(x_mean, x){
  return (x - x_mean)
}

newvariable <- function(df){
  data <- data.frame()
  for (userid in unique(df$user_id)) {
    subdf <- subset(df, user_id == userid)
    
    
    subdf$addiction_mean <- mean(subdf$addiction)
    subdf$addiction_cen <- apply(subdf[,c('addiction_mean','addiction')], 1, function(x) x_centered(x[1],x[2]))
    stopifnot(max(subdf$addiction_cen) - min(subdf$addiction_cen) == max(subdf$addiction) - min(subdf$addiction))
    
    subdf$recovery_mean <- mean(subdf$recovery)
    subdf$recovery_cen <- apply(subdf[,c('recovery_mean','recovery')], 1, function(x) x_centered(x[1],x[2]))
    stopifnot(max(subdf$recovery_cen) - min(subdf$recovery_cen) == max(subdf$recovery) - min(subdf$recovery))
    
    subdf$others_mean <- mean(subdf$others)
    subdf$others_cen <- apply(subdf[,c('others_mean','others')], 1, function(x) x_centered(x[1],x[2]))
    stopifnot(max(subdf$others_cen) - min(subdf$others_cen) == max(subdf$others) - min(subdf$others))
    
    ####
    
    subdf$addiction_posts_mean <- mean(subdf$addiction_posts)
    subdf$addiction_posts_cen <- apply(subdf[,c('addiction_posts_mean','addiction_posts')], 1, function(x) x_centered(x[1],x[2]))
    
    subdf$recovery_posts_mean <- mean(subdf$recovery_posts)
    subdf$recovery_posts_cen <- apply(subdf[,c('recovery_posts_mean','recovery_posts')], 1, function(x) x_centered(x[1],x[2]))
    
    subdf$other_posts_mean <- mean(subdf$other_posts)
    subdf$other_posts_cen <- apply(subdf[,c('other_posts_mean','other_posts')], 1, function(x) x_centered(x[1],x[2]))
    
    ####
    
    subdf$addict_even_mean <- mean(subdf$addict_even)
    subdf$addict_even_cen <- apply(subdf[,c('addict_even_mean','addict_even')], 1, function(x) x_centered(x[1],x[2]))
    
    subdf$reco_even_mean <- mean(subdf$reco_even)
    subdf$reco_even_cen <- apply(subdf[,c('reco_even_mean','reco_even')], 1, function(x) x_centered(x[1],x[2]))
    
    subdf$other_even_mean <- mean(subdf$other_even)
    subdf$other_even_cen <- apply(subdf[,c('other_even_mean','other_even')], 1, function(x) x_centered(x[1],x[2]))
    
    ####
    
    subdf$other_rel_mean <- mean(subdf$other_rel)
    subdf$other_rel_cen <- apply(subdf[,c('other_rel_mean','other_rel')], 1, function(x) x_centered(x[1],x[2]))
    
    subdf$addic_rel_mean <- mean(subdf$addic_rel)
    subdf$addic_rel_cen <- apply(subdf[,c('addic_rel_mean','addic_rel')], 1, function(x) x_centered(x[1],x[2]))
    
    subdf$rec_rel_mean <- mean(subdf$rec_rel)
    subdf$rec_rel_cen <- apply(subdf[,c('rec_rel_mean','rec_rel')], 1, function(x) x_centered(x[1],x[2]))
    
    ####
    
    data <- rbind(data, subdf)
  }
  return (data)
}

final_df <- newvariable(final_df)


######## as the feature mean is a grouping factor, we keep only 2 digits
truncate <- function(a){
  return(signif(a, digits = 2))
}

final_df$other_rel_mean <- apply(final_df['other_rel_mean'], 2, function(x) truncate(x))
final_df$addic_rel_mean <- apply(final_df['addic_rel_mean'], 2, function(x) truncate(x))
final_df$rec_rel_mean <- apply(final_df['rec_rel_mean'], 2, function(x) truncate(x))
final_df$addiction_mean <- apply(final_df['addiction_mean'], 2, function(x) truncate(x))
final_df$recovery_mean <- apply(final_df['recovery_mean'], 2, function(x) truncate(x))
final_df$others_mean <- apply(final_df['others_mean'], 2, function(x) truncate(x))
final_df$addiction_posts_mean <- apply(final_df['addiction_posts_mean'], 2, function(x) truncate(x))
final_df$recovery_posts_mean <- apply(final_df['recovery_posts_mean'], 2, function(x) truncate(x))
final_df$other_posts_mean <- apply(final_df['other_posts_mean'], 2, function(x) truncate(x))
final_df$addict_even_mean <- apply(final_df['addict_even_mean'], 2, function(x) truncate(x))
final_df$reco_even_mean <- apply(final_df['reco_even_mean'], 2, function(x) truncate(x))
final_df$other_even_mean <- apply(final_df['other_even_mean'], 2, function(x) truncate(x))

######## buiding the model #########

myvars <- c('user_id', 'stop', 'Relapse', 'other_rel_cen', 'other_rel_mean', 'other_posts','recovery_addiction_id')
final_df <- final_df[myvars]

#final_df <- na.omit(final_df)

# rescaling features
final_df <- final_df %>% mutate_at(c('other_posts'), ~(scale(.) %>% as.vector))
head(final_df)


feature_cen <- final_df$other_rel_cen
feature_mean <- final_df$other_rel_mean

m.null <- lmer(recovery_addiction_id ~ (1| user_id), 
                data=final_df, control = lmerControl(optimizer ="bobyqa"))
m.base0 <- lmer(recovery_addiction_id ~ stop  + (1| user_id), 
            data=final_df, control = lmerControl(optimizer ="bobyqa"))
m.base1 <- lmer(recovery_addiction_id ~ stop + (1| user_id) + (0 + stop| user_id), 
                data=final_df, control = lmerControl(optimizer ="bobyqa")) 
m.1     <- lmer(recovery_addiction_id ~ stop + feature_cen + (1| user_id) + (0 + stop| user_id),
                data=final_df, control = lmerControl(optimizer ="bobyqa"))
m.1.1     <- lmer(recovery_addiction_id ~ stop + feature_cen + other_posts + (1| user_id) + (0 + stop| user_id),
                data=final_df, control = lmerControl(optimizer ="bobyqa"))
m.2     <- lmer(recovery_addiction_id ~ (stop + feature_cen) + (1| user_id) + (0 + stop| user_id)+ 
                  (1| feature_mean),  data=final_df, control = lmerControl(optimizer ="bobyqa"))
m.2.1     <- lmer(recovery_addiction_id ~ stop + feature_cen + other_posts + (1| user_id) + (0 + stop| user_id)+
                    (1| feature_mean),data=final_df, control = lmerControl(optimizer ="bobyqa"))

anova(m.null, m.base0, m.base1, m.1, m.2)
anova(m.1, m.1.1)
summary(m.1.1)
coefs <- data.frame(coef(summary(m.1.1))) 
coefs$p <- format.pval(2*(1-pnorm(abs(coefs$t.value))), digits=2, eps=0.0001) 
coefs
#acf(resid(n.1))


