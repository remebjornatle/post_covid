
#NOTE:
#The "prep"-part of this file was different depending on subsample. This file shows
#for the main model.
#prerun the prep.R-file to ensure packages are ready

############################Load library and data############################### 

#preloaded: the data-file named "dat"
library(forestploter)
library(tidyverse)

#load file with ICPC2-chapter names
names = read.delim(".../data/icpc2 chapters.txt", sep = ";")

#################################Prep############################################

text = "main"

#create dataframe for analytical sample
temp_dat = df %>% 
  select(-alder) %>% #remove age variable (use age groups instead)
  filter(innlagt == 0) %>% #remove hospitalized 
  select(-innlagt) %>% 
  filter(dbbl_mlm == 0) %>% #remove double infections 
  select(-dbbl_mlm)

#Recode health care variables as binary
temp_dat = temp_dat %>% 
  mutate(
    across(matches("_dis|_sym"), ~ifelse(. > 1,1,.)) #for separated version
  )

#Recode health care variables as binary
temp_dat = temp_dat %>% 
  mutate(
    across(matches("_chap"), ~ifelse(. > 1,1,.)) #for merged version
  )

#create binary marker for Wuhan
temp_dat = temp_dat %>% 
  mutate(Wuhan_virus = ifelse(Alpha_virus+Omikron_virus+Delta_virus == 0,1,0))

###############################Models###########################################

#multivariate model - healthcare utilization variables as outcomes
MuVar_kuhr = temp_dat %>% 
  select(-pid) %>% #remove id variable
  pivot_longer(matches("_chap|virus")) %>% #pivot healthcare and virus long 
  group_by(name) %>% #group by each long variable (healthcare + vaccine)
  do(tidy(glm(y ~ value + kvinne + `age_group_[30,40]` +`age_group_(40,50]` +
                `age_group_(50,60]`+`age_group_(60,70]` +
                `income_q_(20,40]`+`income_q_(40,60]`+`income_q_(60,80]`+`income_q_(80,100]` +
                educ_Secondary + `educ_Low Uni`+`educ_High Uni`+  vaks + immigrant, family = "binomial", .))) %>% 
  filter(term == "value") %>% 
  mutate(mod = "Multivariate")

#multivariate model - demographics and socioeconomic vars
MuVar_soc = temp_dat %>%
  select(-pid) %>% #remove id variable
  select(-matches("_sym|_dis"), -matches("pid"), -matches("smitte_ym")) %>% #remove ununsed
  pivot_longer(-c(y,matches("_chap|virus"))) %>% #make long, except healthcare utilization
  group_by(name) %>% 
  do(tidy(glm(y ~ value + A_chap + B_chap + D_chap + K_chap +
                F_chap + H_chap + L_chap + N_chap + P_chap + R_chap +
                S_chap + T_chap + Alpha_virus + Wuhan_virus + Delta_virus, family = "binomial", .))) %>% 
  filter(term == "value") %>% 
  mutate(mod = "Multivariate") 

#bivariate model
BiVar = temp_dat %>% 
  select(-pid) %>% #remove unused
  select(-matches("_sym|_dis"), -matches("pid"), -matches("smitte_ym")) %>% #remove unused
  pivot_longer(-c(y)) %>% #keep y wide
  group_by(name) %>% #loop over vars
  do(tidy(glm(y ~ value, family = "binomial", .))) %>% 
  filter(!grepl("cept", term)) %>% 
  mutate(mod = "Bivariate")

#merge models
models = bind_rows(MuVar_kuhr, MuVar_soc, BiVar)

#find ORs and CIs
models = models %>% 
  mutate(est = exp(estimate),
         LL = exp(estimate - 1.96*std.error),
         UL = exp(estimate + 1.96*std.error)) %>% 
  select(name, est, LL, UL, mod) 

########################Fix labels##############################################

options(scipen = 999)

#Fix var names, round numbers and create OR column
Var_mod = models %>%
  rename(term = name) %>% 
  mutate(term = gsub("kvinne", "Sex: Female", term),
         term = gsub("Male", "Sex: Male", term),
         term = gsub("income_q_", "Income percentile ", term),
         term = gsub("educ_", "Education: ", term),
         term = gsub("vaks", "Vaccine", term),
         term = gsub("age_group_", "Age group ", term),
         term = gsub("immigrant", "Immigrant", term),
         term = gsub("Delta_virus", "Virus: Delta", term),
         term = gsub("Omikron_virus", "Virus: Omicron", term),
         term = gsub("Wuhan_virus", "Virus: Wuhan", term),
         term = gsub("Alpha_virus", "Virus: Alpha", term)
  ) %>% 
  mutate(est_r = round(est, 2),
         LL_r = round(LL, 2),
         UL_r = round(UL, 2),
         `OR [95% CI]` = paste0(est_r," [",LL_r,",",UL_r, "]"))


#create a type variable to sort socioeconomic, vaccine/virus and KUHR
Var_mod$type = ifelse(grepl("chap", Var_mod$term), 1,0) #1 = kuhr; rest = 0
Var_mod$type = ifelse(grepl("Virus:|Vaccine", Var_mod$term), 0.5,Var_mod$type) #0.5 is virus/vaccine

#create group names with numbers that sort them between types    
groups = data.frame(term = c("Demographic and socioeconomic",
                             "Virus and vaccine",
                             "Healthcare util. 2017-2019 (ICPC2-chap.)"), type = c(-1,0.2,0.7))

#add fancier KUHR names
Var_mod = Var_mod %>% 
  mutate(chap = substr(term,1,1)) %>% 
  left_join(names, by = c("chap" = "chapter")) %>%
  mutate(term = ifelse(grepl("chap", term), paste0(description, " (",chap, ")"), term)) 

#add groups as rows - to later sort
Var_mod = bind_rows(Var_mod, groups)

#REORDER
Var_mod = Var_mod %>% 
  arrange(type,term)

#################################FIX ORDER######################################

#generate row number
Var_mod = Var_mod %>% 
  ungroup(term) %>% 
  mutate(row_n = row_number())

Var_mod = Var_mod %>% 
  mutate(row_n = ifelse(term == "Age group [30,40]", 1.5, row_n),
         row_n = ifelse(term == "Education: Primary", 10, row_n),
         row_n = ifelse(term == "Education: Secondary", 11, row_n),
         row_n = ifelse(term == "Education: Low Uni", 12, row_n),
         row_n = ifelse(term == "Education: High Uni", 13, row_n),
         row_n = ifelse(term == "Virus: Wuhan", 34.5, row_n),
  )

#generate row number
Var_mod = Var_mod %>% 
  arrange(type,row_n)

###############################PREP FIGURE######################################

#create blank column to draw plot
Var_mod$` ` <- paste(rep(" ", 20), collapse = " ") #blank name and blank space


Var_mod$term = ifelse(is.na(Var_mod$est),
                      Var_mod$term,
                      paste0("    ",Var_mod$term)) #Add space for innrykk

#create help variable to help sort variables
Var_mod$`OR [95% CI]`= ifelse(Var_mod$type == -1 |Var_mod$type == 0.2 | Var_mod$type == 0.7, 
                              " ",
                              Var_mod$`OR [95% CI]`)
#simple fixes to labels
Var_mod = Var_mod %>% 
  rename(Variable = term) %>% 
  mutate(Variable = ifelse(grepl("(B)", Variable), "    Blood (B)", Variable))

#create temporary copy for subsetting during plotting
Var_mod_obj = Var_mod

############################Plot univariate####################################

Var_mod = Var_mod_obj[Var_mod_obj$mod == "Bivariate" | is.na(Var_mod_obj$mod),]

Var_mod = Var_mod %>% 
  filter(is.na(UL_r) | UL_r < 10)

p <- forest(Var_mod[,c(1,14,9)],
            est = Var_mod$est_r,
            lower = Var_mod$LL_r,
            upper = Var_mod$UL_r,
            ticks_at = c(0.5,1,1.5, 2, 4.5),
            xlim = c(0,4.5),
            ref_line = 1,
            arrow_lab = c("Lower risk", "Higher risk"),
            ci_column = 2)

p <- add_underline(p, part = "header")

plot(p)

ggsave(paste("G:/Helseregistre/BeredtC19/TeamNytteKost/LongCovid_pred/utvida_periode/tables and figures/",text,"_BIV_model.pdf"), 
       plot = p, width = 7, height = 13)

############################Plot multivariate######################################


Var_mod = Var_mod_obj[Var_mod_obj$mod == "Multivariate" | is.na(Var_mod_obj$mod),]

Var_mod = Var_mod %>% 
  filter(is.na(UL_r) | UL_r < 10)

p <- forest(Var_mod[,c(1,14,9)],
            est = Var_mod$est_r,
            lower = Var_mod$LL_r,
            upper = Var_mod$UL_r,
            ticks_at = c(0.5,1,1.5, 2, 4.5),
            xlim = c(0,4.5),
            ref_line = 1,
            arrow_lab = c("Lower risk", "Higher risk"),
            ci_column = 2)

p <- add_underline(p, part = "header")

plot(p)

ggsave(paste("G:/Helseregistre/BeredtC19/TeamNytteKost/LongCovid_pred/utvida_periode/tables and figures/",text,"_MUV_model.pdf"), 
       plot = p, width = 7, height = 13)
