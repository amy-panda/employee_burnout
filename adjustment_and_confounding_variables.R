
# Load library ------------------------------------------------------------

library(tidyverse)
library(corrplot)
library(janitor)


# Import csv file ---------------------------------------------------------

e_burnout <- read_csv("employee_burn_out_rate.csv")

e_burnout <-clean_names(e_burnout)


# View the data structure -------------------------------------------------
names(e_burnout)

glimpse(e_burnout)

e_burnout$date_of_joining <- as.Date(e_burnout$date_of_joining,"%d/%m/%Y")



# Check missing values ----------------------------------------------------

cbind(
  lapply(
    lapply(e_burnout, is.na),sum)
)

e_burnout_clean <- drop_na(e_burnout)

cbind(
  lapply(
    lapply(e_burnout_clean, is.na),sum)
)

# colSums(is.na(employee_burnout)) can also check missing values for columns



# Transform the categorical variables and date variable -------------------

e_burnout_num <- 
  e_burnout_clean %>% 
  mutate(
        countdays=as.numeric(as.difftime(Sys.Date()-date_of_joining)),
        isfemale=ifelse(gender=="Female",1,0),
        isservice=ifelse(company_type=="Service",1,0),
        iswfh=ifelse(wfh_setup_available=="Yes",1,0)
         )
  
glimpse(e_burnout_num)


# Correlation -------------------------------------------------------------

e_burnout_sub <- 
  e_burnout_num %>% 
  select(isfemale,isservice,iswfh,countdays,designation,resource_allocation,mental_fatigue_score,burn_rate)

corr_burnout <- cor(e_burnout_sub)

corrplot(corr_burnout, type = "upper", tl.col = "darkblue", tl.srt = 45,addCoef.col = 'darkorange')


# Adjustment - wfh --------------------------------------------------------

#wfh_setup_available

lm_w <- lm(burn_rate ~ iswfh, data = e_burnout_num)
summary(lm_w)
coef(lm_w)

e_burnout_wfh <- e_burnout_num %>% filter(iswfh==1)
e_burnout_notwfh <- e_burnout_num %>% filter(iswfh==0)

ggplot(e_burnout_num,aes(x=as.factor(iswfh),y=burn_rate))+
  geom_point(aes(color=wfh_setup_available))+
  geom_hline(yintercept = mean(e_burnout_wfh$burn_rate), color = "light blue", lwd = 1, lty = 2) +
  geom_hline(yintercept = mean(e_burnout_notwfh$burn_rate), color = "pink", lwd = 1, lty = 2)+
  labs(x="Whether work from home facility is available", y="Employee burnout rate",color="WFH setup availability")

#mental_fatigue_score

lm_m <- lm(burn_rate ~ mental_fatigue_score, data = e_burnout_num)
summary(lm_m)
coef(lm_m)

lm_mw <- lm(burn_rate ~ mental_fatigue_score+iswfh, data = e_burnout_num)
summary(lm_mw)
coef(lm_mw)


ggplot(e_burnout_num,aes(mental_fatigue_score,burn_rate))+
  geom_point(aes(color=wfh_setup_available))+
  geom_hline(yintercept = mean(e_burnout_wfh$burn_rate), color = "light blue", lwd = 1, lty = 2) +
  geom_hline(yintercept = mean(e_burnout_notwfh$burn_rate), color = "pink", lwd = 1, lty = 2)+
  geom_abline(intercept = coef(lm_mw)[1]+coef(lm_mw)[3], slope = coef(lm_mw)[2], color = "light blue", lwd = 1.5) +
  geom_abline(intercept = coef(lm_mw)[1], slope = coef(lm_mw)[2], color = "pink", lwd = 1.5) +
  geom_abline(intercept = coef(lm_m)[1], slope = coef(lm_m)[2])+
  labs(x="Mental Fatigue Score",y="Employee burnout rate",color="WFH setup availability")


#resource_allocation

lm_r <- lm(burn_rate ~ resource_allocation, data = e_burnout_num)
summary(lm_r)
coef(lm_r)

lm_rw <- lm(burn_rate ~ resource_allocation+iswfh, data = e_burnout_num)
summary(lm_rw)
coef(lm_rw)

ggplot(e_burnout_num,aes(resource_allocation,burn_rate))+
  geom_point(aes(color=wfh_setup_available))+
  geom_hline(yintercept = mean(e_burnout_wfh$burn_rate), color = "light blue", lwd = 1, lty = 2) +
  geom_hline(yintercept = mean(e_burnout_notwfh$burn_rate), color = "pink", lwd = 1, lty = 2)+
  geom_abline(intercept = coef(lm_rw)[1]+coef(lm_rw)[3], slope = coef(lm_rw)[2], color = "light blue", lwd = 1.5) +
  geom_abline(intercept = coef(lm_rw)[1], slope = coef(lm_rw)[2], color = "pink", lwd = 1.5) +
  geom_abline(intercept = coef(lm_r)[1], slope = coef(lm_r)[2])+
  labs(x="Resource allocation",y="Employee burnout rate",color="WFH setup availability")


#designation

lm_d <- lm(burn_rate ~ designation, data = e_burnout_num)
summary(lm_d)
coef(lm_d)

lm_dw <- lm(burn_rate ~ designation+iswfh, data = e_burnout_num)
summary(lm_dw)
coef(lm_dw)

ggplot(e_burnout_num,aes(designation,burn_rate))+
  geom_point(aes(color=wfh_setup_available))+
  geom_hline(yintercept = mean(e_burnout_wfh$burn_rate), color = "light blue", lwd = 1, lty = 2) +
  geom_hline(yintercept = mean(e_burnout_notwfh$burn_rate), color = "pink", lwd = 1, lty = 2)+
  geom_abline(intercept = coef(lm_dw)[1]+coef(lm_dw)[3], slope = coef(lm_dw)[2], color = "light blue", lwd = 1.5) +
  geom_abline(intercept = coef(lm_dw)[1], slope = coef(lm_dw)[2], color = "pink", lwd = 1.5) +
  geom_abline(intercept = coef(lm_d)[1], slope = coef(lm_d)[2])+
  labs(x="Designation",y="Employee burnout rate",color="WFH setup availability")

