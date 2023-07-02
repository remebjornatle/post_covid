
#prerun the prep.R-file to ensure packages are ready

##########################READ DATA#############################################

df = read.csv(".../data.csv")

set.seed(1337)

#remove hospitalized and double infections
df <- df%>%filter(dbbl_mlm == 0)%>%select(-dbbl_mlm)
df <- df%>%filter(innlagt == 0)%>%select(-innlagt)


#remove variables not in use for the analysis
df <- df%>%
  select(-matches('_symp|_dis|pid|smitte_ym|alder'))%>%
  select(y, everything())

#change healthcare utilization to binary 
df <- df%>%mutate(
  across(matches('chap'), ~ifelse(.x==0, 0, 1)
  ))

#Split dataset
split <- initial_split(df, strata = y, prop=4/5)
train_r992 <- training(split)
test_r992  <- testing(split)


#matrix version for glmnet
y = as.matrix(train_r992$y)
y = ifelse(y == "Not Post-Covid",0,1)

X = train_r992 %>% 
  select(-y)
X = as.matrix(X)

#folds for cross-validation and val_set for simple training/validation
folds <- vfold_cv(train_r992, v=10)
val_set <- validation_split(train_r992, strata = y, prop=0.8)


