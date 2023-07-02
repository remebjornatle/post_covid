
#prerun the prep.R-file to ensure packages are ready

########################Load files######################################

#Load names of ICPC2 chapters
names = read.delim("..../icpc2 chapters.txt", sep = ";")

#fix a name that is too long
names = names %>% 
  mutate(description = ifelse(chapter == "B", "Blood and Immune Mechanism", description))

#load lasso VIs
las_vi <- read_csv(".../las_vi.csv")
las_vi$type = "LASSO"

#load RF VIs
rf_vi <- read_csv(".../rf_vi.csv")
rf_vi$type = "Random forest"

#>INSERTING sign-file here
rf_signs = read.csv2(".../data/rf_sign.csv")

rf_vi = rf_vi %>% 
  left_join(rf_signs, by = c("Variable" = "var"))

#small adjustment 
rf_vi = rf_vi %>% 
  select(-diff, -Sign) %>% 
  rename(Sign = sign)

#merge files
temp = bind_rows(las_vi, rf_vi)

###############Variable imp Random forest#################################
temp %>% 
  mutate(term = Variable) %>% 
  mutate(term = gsub("kvinne", "Female", term),
         term = gsub("income_q_", "Income percentile ", term),
         term = gsub("educ_", "Education: ", term),
         term = gsub("vaks", "Vaccine", term),
         term = gsub("age_group_", "Age group ", term),
         term = gsub("immigrant", "Immigrant", term),
         term = gsub("Alpha_virus", "Alpha virus", term),
         term = gsub("Omikron_virus", "Omikron virus", term)
  ) %>% 
  mutate(chap = substr(term,1,1)) %>% 
  left_join(names, by = c("chap" = "chapter")) %>%
  mutate(term = ifelse(grepl("chap", term), paste0(description, " (",chap, ")"), term)) %>%
  filter(type == "Random forest")  %>% 
  ggplot(aes(x =reorder(term, Importance), y = Importance, fill=Sign)) +
  geom_col(width = 0.6, alpha = 0.8) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1", name = "Sign") +
  ylab("") +
  xlab("") +
  coord_flip() 

ggsave(".../tables and figures/RF_vi.pdf", width = 7, height = 4.5)

#################Variable imp Lasso####################################
temp %>% 
  mutate(term = Variable) %>% 
  mutate(term = gsub("kvinne", "Female", term),
         term = gsub("income_q_", "Income percentile ", term),
         term = gsub("educ_", "Education: ", term),
         term = gsub("vaks", "Vaccine", term),
         term = gsub("age_group_", "Age group ", term),
         term = gsub("immigrant", "Immigrant", term),
         term = gsub("Alpha_virus", "Alpha virus", term)
  ) %>% 
  mutate(chap = substr(term,1,1)) %>% 
  left_join(names, by = c("chap" = "chapter")) %>%
  mutate(term = ifelse(grepl("chap", term), paste0(description, " (",chap, ")"), term)) %>%
  filter(type == "LASSO")  %>% 
  filter(Importance > 0.001) %>% 
  ggplot(aes(x =reorder(term, Importance), y = Importance, fill = Sign)) +
  geom_col(width = 0.6, alpha = 0.8) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1", name = "Sign") +
  ylab("") +
  xlab("") +
  coord_flip() 

ggsave(".../tables and figures/LASSO_vi.pdf", width = 7, height = 4.5)

#######################ROC######################################################

#file with stored with ROC-data for each model. "Model" shows which model.
roc <- read_csv(".../roc.csv")

roc_plot <- roc %>%
  ggplot(aes(x = 1-specificity, y = sensitivity, group=Model, color=Model))+
  geom_path(lwd=0.5)+
  geom_abline(lty=3)+coord_equal()+
  geom_ribbon(aes(ymin=sensitivity_lower, ymax=sensitivity_higher),lwd=0.2, alpha=0.05)+
  scale_colour_brewer(palette='Set1', name = "Model")+
  xlab("False positive rate") +
  ylab("False negative rate") +
  theme_bw()

ggsave(filename='.../tables and figures/auc_plot.pdf', width=5, height = 7)



