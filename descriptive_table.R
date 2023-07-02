
#NOTE: 
#Datafile called "df" preloaded into memory
#prerun the prep.R-file to ensure packages are ready


##########################TABLE OF DESCRIPTIVES##################################

#avoid scientific notation
options(scipen = 999)

#Filter to prepare the analytical sample 
tab = df %>% 
  filter(innlagt == 0) %>% #remove hospitalized
  select(-innlagt) %>% 
  filter(dbbl_mlm == 0) %>% #remove double infections
  select(-dbbl_mlm)

#make healthcare-data binary
tab = tab %>% 
  select(-matches("_dis|_sym")) %>% #remove all variables splitting chapters on diagnoses and symptoms
  mutate(
    across(matches("_chap"), ~ifelse(. > 1,1,.)) #create binary version of each chapter; instead of count
  )

#remove ID variable, as this is not needed to create summary stats
tab$pid = NULL

#create variable for all - this is to allow for "full sample"-column
tab$All = 1

#fix naming on variables
tab$`Non-immigrant` = ifelse(tab$immigrant == 0,1,0)
tab$`Wuhan virus` = ifelse(tab$Alpha_virus + tab$Omikron_virus + tab$Delta_virus == 0,1,0)
tab$`Alpha virus` = ifelse(tab$Alpha_virus == 1,1,0)
tab$`Delta virus` = ifelse(tab$Delta_virus == 1,1,0)
tab$`Omicron virus` = ifelse(tab$Omikron_virus  == 1,1,0)
tab$Male = ifelse(tab$kvinne == 0,1,0)

tot = nrow(tab) #count total number of obs
c19 = nrow(tab[tab$y == "Post-Covid",]) #count cases
nc19 = nrow(tab[tab$y == "Not Post-Covid",]) #count non-cases

#Table with full sample summary stats
t_all_count = tab %>% 
  summarise(across(where(is.numeric), list(sum = ~sum(., na.rm = TRUE)))) %>% #sum of dummies in each column (binary vars)
  pivot_longer(everything()) %>% #make long
  separate(name, into = c("var", "stat"), sep = "_(?=[^_]+$)") %>% #remove "sum" from new variable name by splitting
  pivot_wider(names_from = "stat") %>%  #change name of value column to sum
  mutate(y = as.factor("All")) #create a variable that allows for merging table (All;post-COVID;not post-COVID)

#Create table with "post-COVID" split
t_split_count = tab %>%
  group_by(y) %>% #group by post-COVID group
  summarise(across(where(is.numeric), list(sum = ~sum(., na.rm = TRUE)))) %>% #sum of dummies in each column (binary vars)
  pivot_longer(-y) %>% #keep separate columns for "post-COVID" and "Not post-COVID"
  separate(name, into = c("var", "stat"), sep = "_(?=[^_]+$)") %>% #remove "sum" from new variable name by splitting
  pivot_wider(names_from = "stat") #change name of value column to sum

########################COUNT TABLE MERGE#######################################

#merge tables by rows
table_desc_count = bind_rows(t_all_count, t_split_count)

#Create the final table
table_desc_count = table_desc_count %>% 
  mutate(chap = substr(var,1,1)) %>% #find first letter to prepare for merging in ICPC2-chapter names 
  left_join(names, by = c("chap" = "chapter")) %>% #merge-in chapter names (from a names-file with chapter names)
  mutate(name = paste0(description," (",chap,")"), #create chapter name labels
         var = ifelse(substr(var,2,2) == "_", name, var)) %>% #only replace the healthcare variables
  mutate(group = y, #This block fixes variable names
         var = gsub("kvinne", "Female", var),
         var = gsub("income_q_", "Income percentile ", var),
         var = gsub("educ_", "Education: ", var),
         var = gsub("vaks", "Vaccine", var),
         var = gsub("age_group_", "Age group ", var),
         var = gsub("immigrant", "Immigrant", var)
  ) %>% 
  select(group,var, sum) %>% #keep essential columns
  pivot_wider(names_from = group,  #pivot wide by groups (All;post-COVID;not post-COVID)
              values_from = sum) %>% 
  rename(Variable = var) %>% 
  mutate(Incidence = `Post-Covid`/(`Post-Covid`+`Not Post-Covid`), #calculate incidence and CIs
         Factor = (Incidence*(1-Incidence))/(`Post-Covid`+`Not Post-Covid`),
         LL = Incidence-1.96*sqrt(Factor),
         UL = Incidence+1.96*sqrt(Factor),
         Incidence = round(Incidence,4)*100, #round
         UL = round(UL,4)*100,
         LL = round(LL,4)*100,
         Incidence = paste0(Incidence,"%"), #paste together incidence and CI
         CI = paste0("[",LL,"%", "-",UL,"%", "]"),
         est = paste0(Incidence," ",CI)) %>% 
  select(Variable, All, `Not Post-Covid`,  `Post-Covid`, est) #subset
