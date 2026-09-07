RSFLC_sofa <- function(baseline_data, longitude_data, t0 = 5) {
  stopifnot(is.data.frame(baseline_data), is.data.frame(longitude_data))

  # 1) timeData：前 10 天 SOFA，每人 >= 2 条
  timeData_train_sofa <- longitude_data %>%
    dplyr::filter(times <= 10) %>%
    dplyr::transmute(
      hadm_id      = as.integer(hadm_id),
      time         = as.integer(times),
      sofa_24hours = as.numeric(sofa_24hours)
    ) %>%
    dplyr::group_by(hadm_id) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::ungroup() %>%
    as.data.frame()

  valid_patients_sofa <- unique(timeData_train_sofa$hadm_id)

  # 2) fixedData：8 个静态基线变量
  fixedData_train_sofa <- baseline_data %>%
    dplyr::filter(hadm_id %in% valid_patients_sofa) %>%
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

  # 3) timeVarModel：SOFA 轨迹 fixed ~ 1, random ~ time
  timeVar_sofa <- "time"
  timeVarModel_sofa <- list(
    sofa_24hours = list(fixed = sofa_24hours ~ 1, random = ~ time)
  )

  # 4) Y：28 天死亡
  Y_sofa <- list(
    type = "factor",
    Y = baseline_data %>%
      dplyr::filter(hadm_id %in% valid_patients_sofa) %>%
      dplyr::transmute(
        hadm_id = as.integer(hadm_id),
        event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
      ) %>%
      dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
      as.data.frame()
  )

  stopifnot(
    identical(sort(unique(timeData_train_sofa$hadm_id)), sort(fixedData_train_sofa$hadm_id)),
    identical(sort(fixedData_train_sofa$hadm_id), sort(Y_sofa$Y$hadm_id)),
    sum(is.na(timeData_train_sofa$sofa_24hours)) == 0
  )

  res_dyn_sofa <- DynForest::dynforest(
    timeData     = timeData_train_sofa,
    fixedData    = fixedData_train_sofa,
    timeVar      = timeVar_sofa,
    idVar        = "hadm_id",
    timeVarModel = timeVarModel_sofa,
    Y            = Y_sofa,
    mtry         = 3,
    nodesize     = 5,
    ncores       = 1,
    ntree        = 50,
    seed         = 1234
  )

  attr(res_dyn_sofa, "model_name") <- "RSFLC_sofa"

  metrics_sofa <- cal_3_test(
    model         = res_dyn_sofa,
    t0            = t0,
    timeData      = timeData_train_sofa,
    fixedData     = fixedData_train_sofa,
    baseline_data = baseline_data
  )

  list(
    metrics_sofa = metrics_sofa,
    res_dyn_sofa = res_dyn_sofa
  )
}
