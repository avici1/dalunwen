RSFLC_sofa_fold <- function(baseline_data,
                            longitude_data,
                            t0,
                            prediction_baseline,
                            prediction_longitude) {
  stopifnot(
    is.data.frame(baseline_data),
    is.data.frame(longitude_data),
    is.data.frame(prediction_baseline),
    is.data.frame(prediction_longitude)
  )

  train_inputs <- prep_sofa_inputs(baseline_data, longitude_data)
  pred_inputs  <- prep_sofa_inputs(prediction_baseline, prediction_longitude)

  timeVar_sofa <- "time"
  timeVarModel_sofa <- list(
    sofa_24hours = list(fixed = sofa_24hours ~ 1, random = ~ time)
  )

  res_dyn_sofa <- DynForest::dynforest(
    timeData     = train_inputs$timeData,
    fixedData    = train_inputs$fixedData,
    timeVar      = timeVar_sofa,
    idVar        = "hadm_id",
    timeVarModel = timeVarModel_sofa,
    Y            = train_inputs$Y,
    mtry         = 3,
    nodesize     = 5,
    ncores       = 1,
    ntree        = 50,
    seed         = 1234
  )

  attr(res_dyn_sofa, "model_name") <- "RSFLC_sofa_fold"

  metrics_sofa <- cal_3_test(
    model         = res_dyn_sofa,
    t0            = t0,
    timeData      = pred_inputs$timeData,
    fixedData     = pred_inputs$fixedData,
    baseline_data = prediction_baseline
  )

  list(
    metrics_sofa = metrics_sofa,
    res_dyn_sofa = res_dyn_sofa
  )
}
