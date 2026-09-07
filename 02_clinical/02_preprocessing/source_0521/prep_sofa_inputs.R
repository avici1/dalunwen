prep_sofa_inputs <- function(baseline_data, longitude_data) {
  stopifnot(is.data.frame(baseline_data), is.data.frame(longitude_data))

  timeData <- longitude_data %>%
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

  valid_patients <- unique(timeData$hadm_id)

  fixedData <- baseline_data %>%
    dplyr::filter(hadm_id %in% valid_patients) %>%
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

  Y <- list(
    type = "factor",
    Y = baseline_data %>%
      dplyr::filter(hadm_id %in% valid_patients) %>%
      dplyr::transmute(
        hadm_id = as.integer(hadm_id),
        event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
      ) %>%
      dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
      as.data.frame()
  )

  stopifnot(
    identical(sort(unique(timeData$hadm_id)), sort(fixedData$hadm_id)),
    identical(sort(fixedData$hadm_id), sort(Y$Y$hadm_id)),
    sum(is.na(timeData$sofa_24hours)) == 0
  )

  list(timeData = timeData, fixedData = fixedData, Y = Y)
}
