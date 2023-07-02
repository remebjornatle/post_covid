
#prerun the prep.R-file to ensure packages are ready

########TUNING TIDYMODELS###########
#train model
las_rec <-  recipe(y ~ ., data=train_r992)
las_mod <- logistic_reg(penalty = tune(), mixture=1)%>%
  set_engine('glmnet', num.threads=8)

#create workflow
las_wflow <- 
  workflow()%>%
  add_model(las_mod)%>%
  add_recipe(las_rec)

grid_las <- grid_regular(penalty(), levels=c(500))

las_fit <- las_wflow%>%
  tune_grid(folds,
            grid=grid_las,
            control=control_grid(save_pred = T),
            metrics=metric_set(roc_auc))

####OPTIMAL MODEL TIDYMODELS###############
#find best model
las_best <- las_fit%>%select_best()

#implement best model
las_final_mod <- logistic_reg(penalty = las_best$penalty, mixture=1)%>%
  set_engine('glmnet', num.threads=8)
  
las_final_wflow <- las_wflow%>%
  finalize_workflow(las_best)

#remove unneccessary
rm(list=c('las_fit', 'las_wflow', 'las_mod', 'las_rec', 'grid_las'))

#final model
las_final_fit <- las_final_wflow%>%last_fit(split)


las_fit_eng <- las_final_fit%>%extract_fit_engine() #return model object
las_fit_pars <- las_final_fit%>%extract_fit_parsnip() #return model specification
las_fit_vi <-  las_fit_pars%>%vi() #return variable importance

#see VIs
las_fit_vi%>%vip(num_features=27)

#write file
write.csv(las_fit_vi, ".../las_vi.csv", row.names = F)

#########AUC OG ROC###########

#predict with model                                        
las_pred <- las_final_fit%>%collect_predictions()%>%
  within(y <- relevel(y, ref=2))

las_pred <- las_pred%>%select(y, p = '.pred_Post-Covid')

las_pred%>%roc_auc(estimate='p', truth='y')

#Calculate AUC and 95% CI
las_pred%>%
  roc('y', 'p')%>%
  ci.auc(boot.n=1000)

#Calculate ROC and 95% CI
bootstr <- las_pred%>%
  roc('y', 'p', percent=T, ci=T, of='se', sp=seq(0,100,2))


las_roc <- bootstr$ci%>%data.frame(row.names = NULL)%>%
  mutate(sensitivity_lower  = X2.5./100,
         sensitivity        = X50./100,
         sensitivity_higher = X97.5./100,
         specificity = seq(0,1,0.02),
         .keep='unused')


las_roc$Model = 'Lasso'
las_roc$AUC <- as.numeric(las_pred%>%roc_auc(estimate='p', truth='y')%>%select(.estimate))