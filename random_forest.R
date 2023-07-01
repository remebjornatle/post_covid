
source(".../model_prep.r")

#######RECIPE AND WORKFLOW###############
rf_rec <-  recipe(y ~ ., data=train_r992)
rf_mod <- rand_forest(mtry=tune(), min_n=tune(), trees=1000 )%>%
          set_engine('ranger', num.threads=8)%>%
          set_mode('classification')

rf_wflow <- 
  workflow()%>%
  add_model(rf_mod)%>%
  add_recipe(rf_rec)

##############FITTING#####################
#with tuning grid for mtry= number of param per tree and min_n = minimum number of entries per split
optimering <- function(){
grid_rf <- grid_regular(
  parameters(
    mtry( range = c(1, 5)), #mtry er antall variabler som blir valgt for treet
    min_n(range = c(1, 40))), #min_n er minste antall punkter som m? v?re i hver split
    levels=c(5,40)
    
  )

rf_fit <- rf_wflow%>%
          tune_grid(folds,
                    grid=grid_rf,
                    control=control_grid(save_pred = T),
                    metrics=metric_set(roc_auc, accuracy))

rf_fit$.metrics%>%data.frame()%>%ggplot(aes(x=mtry, y=min_n))+geom_tile(aes(fill=.estimate))
  
rf_best <- rf_fit%>%select_best(metric='roc_auc')
return(rf_best)
}

#NOTE: best parameters when tuning: mtry=1 min_n =30

###############FINAL MODEL##########################

#Inserting optimal rf_best$mtry and rf_best$min_n 
rf_final_mod <- rand_forest(mtry=1, min_n = 30, trees = 1000)%>%
  set_engine('ranger', importance='impurity')%>%
  set_mode('classification')

#update model
rf_final_fit <- rf_wflow%>%update_model(rf_final_mod)%>%last_fit(split)
rf_fit <- rf_final_fit%>%extract_fit_engine()

########VARIABLE IMPORTANCE ####################
rf_fit_vi <- rf_final_fit%>%extract_fit_parsnip()%>%vi()

########################VIEW VIs###############
rf_fit_vi%>%vip(num_features=27)

########ROC og AUC #############################

#predict with model
rf_pred <- rf_final_fit%>%
  collect_predictions()%>%
  within(y <- relevel(y, ref=2))%>%
  select(y, p = '.pred_Post-Covid')

rf_pred%>%roc_auc(estimate=p, truth='y')
rf_pred%>%rmse(estimate='p', truth=as.numeric(y)-1)

#Calculate AUC and 95%CIs
rf_pred%>%
  roc('y', 'p')%>%
  ci.auc(boot.n=1000)

#Calculate ROC and 95% CIs
bootstr <- rf_pred%>%
  roc('y', 'p', percent=T, ci=T, of='se', sp=seq(0,100,2))

rf_roc <- bootstr$ci%>%data.frame(row.names = NULL)%>%
  mutate(sensitivity_lower  = X2.5./100,
         sensitivity        = X50./100,
         sensitivity_higher = X97.5./100,
         specificity = seq(0,1,0.02),
         .keep='unused')

rf_roc$Model = 'Random Forest'
rf_roc$AUC <- as.numeric(rf_pred%>%roc_auc(estimate='p', truth='y')%>%select(.estimate))