RSFLC_gcs <- function(baseline_data, longitude_data, t0 = 5) {
  stopifnot(is.data.frame(baseline_data), is.data.frame(longitude_data))

  # 1) timeData：前 10 天 GCS，每人 >= 2 条
  timeData_train_gcs <- longitude_data %>%
    dplyr::filter(times <= 10) %>%
    dplyr::transmute(
      hadm_id = as.integer(hadm_id),
      time    = as.integer(times),
      gcs     = as.numeric(gcs)
    ) %>%
    dplyr::group_by(hadm_id) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::ungroup() %>%
    as.data.frame()

  valid_patients_gcs <- unique(timeData_train_gcs$hadm_id)

  # 2) fixedData：8 个静态基线变量
  fixedData_train_gcs <- baseline_data %>%
    dplyr::filter(hadm_id %in% valid_patients_gcs) %>%
    dplyr::transmute(
      hadm_id = as.integer(hadm_id),
      age = as.numeric(age),
      charlson_comorbidity_index = as.integer(charlson_comorbidity_index),
      apsiii = as.integer(apsiii),
      sapsii = as.integer(sapsii),
      oasis = as.integer(oasis),
      preiculos = as.numeric(preiculos),
      mechvent = factor(mechvent, levels = c(0, 1), labels = c("no", "yes")),
      electivesurgery = factor(electivesurgery, levels = c(0, 1), labels = c("no", "yes"))
    ) %>%
    dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
    as.data.frame()

  # 3) timeVarModel：GCS 轨迹 fixed ~ 1, random ~ time
  timeVar_gcs <- "time"
  timeVarModel_gcs <- list(
    gcs = list(fixed = gcs ~ 1, random = ~ time)
  )

  # 4) Y：28 天死亡
  Y_gcs <- list(
    type = "factor",
    Y = baseline_data %>%
      dplyr::filter(hadm_id %in% valid_patients_gcs) %>%
      dplyr::transmute(
        hadm_id = as.integer(hadm_id),
        event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
      ) %>%
      dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
      as.data.frame()
  )

  stopifnot(
    identical(sort(unique(timeData_train_gcs$hadm_id)), sort(fixedData_train_gcs$hadm_id)),
    identical(sort(fixedData_train_gcs$hadm_id), sort(Y_gcs$Y$hadm_id)),
    sum(is.na(timeData_train_gcs$gcs)) == 0
  )

  res_dyn_gcs <- DynForest::dynforest(
    timeData     = timeData_train_gcs,
    fixedData    = fixedData_train_gcs,
    timeVar      = timeVar_gcs,
    idVar        = "hadm_id",
    timeVarModel = timeVarModel_gcs,
    Y            = Y_gcs,
    mtry         = 3,
    nodesize     = 5,
    ncores       = 1,
    ntree        = 50,
    seed         = 1234
  )

  attr(res_dyn_gcs, "baseline") <- baseline_data
  attr(res_dyn_gcs, "model_name") <- "RSFLC_gcs"

  metrics_gcs <- cal_3(res_dyn_gcs, t0 = t0)

  list(
    metrics_gcs = metrics_gcs,
    res_dyn_gcs = res_dyn_gcs
  )
}
