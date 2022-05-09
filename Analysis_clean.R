rm(list = ls())
setwd("....to source file (Gel_data.csv)...")

### Load required libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(MuMIn)
library(lme4)


### Data cleaning
Gel <- read_csv("Gel_data.csv")
Gel$Participant <- as.character(Gel$Participant)
Gel <- Gel %>% separate(Plan_profile, c("Plan_cat", "Profile_cat"), remove = FALSE)
Gel$Artifact_ID <- as.factor(Gel$ID)
Gel <- Gel %>% add_column("sLength_mm" = scale(Gel$Length_mm), 
                          "sHand_force" = scale(Gel$Hand_force),
                          "sPlan" = scale(Gel$Plan),
                          "sProfile" = scale(Gel$Profile),
                          "sEdge_length_mm" = scale(Gel$Edge_length_mm))

Gel %>% summarize(mean_HS = mean(Hand_force), sd = sd(Hand_force), range = range(Hand_force))

### Plot for time vs. edge length for high and low plan curvature
Gel <- Gel %>% add_column("Plan Curvature" = ifelse(Gel$Plan_cat == "high", "High", "Low"))
ggplot(Gel, aes(x=sEdge_length_mm, y=Time_s, color=`Plan Curvature`, shape = `Plan Curvature`)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"))+
  geom_point(size = 2.2)+ geom_smooth(method = "lm", size = 0.5) + theme_bw() +
  theme(plot.title = element_text(size = 18, face="bold", hjust = 0.5), 
        axis.text=element_text(size=18), 
        axis.title=element_text(size=18,face="bold"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position = c(0.82, 0.88),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(x="Edge length (Z-score)", y = "Time (s)")

######### ANALYSIS ######## 

## Function that takes in a formula and data, then performs model selection and model averaging, then returns the results in a list

full_model_selection <- function(formula, data){
  model <- glmer(formula, family = poisson, data = data, na.action = "na.fail")
  chat <- deviance(model) / df.residual(model)
  model.selection <- dredge(model, rank = "QAICc", chat = chat)
  model.average <- model.avg(model.selection)
  a <- list("model.selection" = model.selection, "model.average" = summary(model.average))
  return(a)
}

######### FORMULAS ########

plan_profile_hand <- Time_s ~ sPlan + sProfile + sEdge_length_mm + sHand_force + (1|Participant) + (1|Session)
plan_profile <- Time_s ~ sPlan + sProfile + sEdge_length_mm + (1|Participant) + (1|Session) 

####### MODEL SELECTIONS #######

### Models including edge length, plan and profile segmentation, and hand strength.

## Full model selection results
mod_sel_plan_profile_hand <- full_model_selection(plan_profile_hand, Gel)$model.selection
mod_sel_plan_profile_hand
write.csv(round(mod_sel_plan_profile_hand, digits = 3), "model_selection_hand.csv")

## Model averaging results
avg_models_plan_profile_hand <- full_model_selection(plan_profile_hand, Gel)$model.average
avg_models_plan_profile_hand
confint(avg_models_plan_profile_hand)


### Models including edge length, plan and profile segmentation, and hand strength.

## Full model selection results
mod_sel_plan_profile <- full_model_selection(plan_profile, Gel)$model.selection
mod_sel_plan_profile
write.csv(round(mod_sel_plan_profile, digits = 3), "model_selection.csv")

## Model averaging results
avg_models_plan_profile <- full_model_selection(plan_profile, Gel)$model.average
avg_models_plan_profile
confint(avg_models_plan_profile)
