
#NOTE:
#df is preloaded into memory and the rf_fit model

#check rf-sign: input - rf-model

#empty frame where results will be stored
frame = data.frame(var = as.character(),
                   diff = as.numeric(),
                   sign = as.character())

#subset frame for prediction
df_temp = df[,2:ncol(df)]

#create benchmark prediction
#p <- predict(rf_fit, data=df[,2:ncol(df)])


#LOOOP OVER ALL VARIABLES

for (i in 1:ncol(df_temp)) { 

  #set the relevant variable to zero for all
  df_temp[,i] = 0

  #predict outcome
  p_mod_0 <- predict(rf_fit, data=df_temp)
  
  #set the relevant variable to zero for all
  df_temp[,i] = 1
  
  #predict outcome
  p_mod_1 <- predict(rf_fit, data=df_temp)
  
  #average difference between benchmark and modified data
  diff = mean(p_mod_1$predictions[,2]) - mean(p_mod_0$predictions[,2])

  #set the sign
  sign = ifelse(diff > 0, "POS", "NEG")

  #concat results
  temp_frame = data.frame(var = names(df_temp)[i], 
                        diff = diff,
                        sign = sign)

  frame = bind_rows(frame, temp_frame)

  #reassign dataframe
  df_temp = df[,2:ncol(df)]

}

write.csv2(frame, ".../data/rf_sign.csv",
           row.names = F)