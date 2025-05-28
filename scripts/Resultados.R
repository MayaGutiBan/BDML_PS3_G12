#Results

# For Random Forest
rf_resample <- RF_Grid$results
rf_best <- RF_Grid$bestTune
rf_best_row <- rf_resample[
  rf_resample$mtry == rf_best$mtry & 
    rf_resample$min.node.size == rf_best$min.node.size &
    rf_resample$splitrule == rf_best$splitrule, ]

# For Neural Network
final_epoch <- length(history$metrics$val_loss)
nn_val_mae <- history$metrics$val_mae[final_epoch]
nn_val_rmse <- sqrt(history$metrics$val_loss[final_epoch])

model_comparison <- data.frame(
  Model = c("Random Forest", "Neural Network"),
  MAE = c(rf_best_row$MAE, nn_val_mae),
  RMSE = c(rf_best_row$RMSE, nn_val_rmse)
)

print(model_comparison)