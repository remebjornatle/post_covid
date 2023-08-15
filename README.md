# post_covid
These codes show the main analysis presented in the paper
Note that since the data used in this paper was subject to strict access regulations, the code only shows 
what was done, and cannot be re-run on the data.

Quick explanation: 
1. "descriptive_table.R" shows code behind the descriptive stats table
2. "forest_plots.R" shows the code underlying all the forest plots. Note the subsample varied for different sensitivity analysis
3. "model_prep.R" are prepping the data for the machine learning estimations
4. "lasso_tidy.R" and "random_forest.R" show the machine learning estimations
5. "rf_sign_check.R" shows how we tested signs for the variable importance scores for the random forest
6. "ml_figures.R" shows how we graphed the ml-results
