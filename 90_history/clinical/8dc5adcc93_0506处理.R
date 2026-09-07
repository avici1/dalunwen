data_0423_wide_inputed<-read.csv("F:/文章_大论文/0417/stroke_patients_inputed_0428_1.csv")


TARGET_MEAN <- 13.91
TARGET_SD  <- 12.44
RNG_SEED   <- 20260506  ## 可改；保证可重复
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
set.seed(RNG_SEED)

## 存活者整数住院天数：四舍五入后再微调，使总和 = target_alive_sum（且每天 >= 1）
.integer_balance_alive <- function(x, target_sum, min_val = 1L) {
  x <- as.integer(x)
  diff <- as.integer(target_sum - sum(x))
  if (diff == 0L) {
    return(x)
  }
  n <- length(x)
  if (diff > 0L) {
    x <- x + as.integer(tabulate(sample.int(n, diff, replace = TRUE), nbins = n))
    return(x)
  }
  need <- -diff
  spare <- sum(x - min_val)
  if (spare < need) {
    warning(
      "存活者住院天数无法下调至目标总和（每人至少 ", min_val, " 天）。",
      "实际下调 ", spare, " 天，剩余偏差 ", need - spare, "。"
    )
    need <- spare
  }
  while (need > 0L) {
    elig <- which(x > min_val)
    if (!length(elig)) {
      break
    }
    j <- sample(elig, 1L)
    x[j] <- x[j] - 1L
    need <- need - 1L
  }
  x
}

## 入院层面一行（同一 subject_id + hadm_id 视为一次住院）
adm <- data_0423_wide_inputed %>%
  distinct(subject_id, hadm_id, survival, survival_time_days)
n  <- nrow(adm)
id_dead  <- which(adm$survival == 1)
id_alive <- which(adm$survival == 0)
nd <- length(id_dead)
na <- length(id_alive)
if (n == 0) stop("adm 为空，请检查数据。")

## hosp_time 全程为整数（天）
hosp_time <- rep(NA_integer_, n)
S_target <- as.integer(round(TARGET_MEAN * n))

## ---------- 死亡：表中 survival_time_days 四舍五入为整数天 ----------
if (nd > 0) {
  hd <- adm$survival_time_days[id_dead]
  if (anyNA(hd)) {
    stop("死亡患者存在 survival_time_days 缺失，请先填补或剔除。")
  }
  hosp_time[id_dead] <- as.integer(round(hd))
}

## ---------- 存活：矩约束连续抽样 -> 取整 -> 总和与 S_target 对齐 ----------
if (na == 0) {
  warning("无存活患者，hosp_time 仅由死亡者决定。")
} else {
  Sa <- S_target - sum(hosp_time[id_dead], na.rm = TRUE)
  if (Sa < na) {
    stop(
      "目标总住院人天 round(n*TARGET_MEAN)=", S_target,
      " 过小：死亡者整数住院天之和已占满，无法满足存活者每人至少 1 天。"
    )
  }
  
  if (na == 1L) {
    hosp_time[id_alive] <- as.integer(Sa)
  } else {
    mu <- TARGET_MEAN
    m_a <- Sa / na
    SS_tot <- (n - 1) * TARGET_SD^2
    dead_ctr <- if (nd > 0) hosp_time[id_dead] else integer()
    SS_dead <- if (nd > 0) sum((dead_ctr - mu)^2) else 0
    Qa <- SS_tot - SS_dead
    inner <- Qa - na * (m_a - mu)^2
    if (!is.finite(inner) || inner < 0) {
      warning(
        "在给定死亡者住院时间下，无法同时达到目标标准差（方差约束不可行）。",
        "已将存活者波动设为 0（每人住院天数相同）。"
      )
      inner <- 0
    }
    c_sd <- sqrt(inner / (na - 1))
    z <- rnorm(na)
    z <- z - mean(z)
    rz <- sqrt(sum(z^2))
    if (rz == 0) {
      z <- rep_len(c(-1, 1), na)
    } else {
      z <- z / rz * sqrt(na - 1)
    }
    y <- m_a + c_sd * z
    alive_int <- as.integer(pmax(round(y), 1L))
    hosp_time[id_alive] <- .integer_balance_alive(alive_int, Sa, min_val = 1L)
  }
}





## 写回入院表并并入宽表 -> data_0423_wide1（不覆盖原始读入对象）
adm <- adm %>% mutate(hosp_time = as.integer(hosp_time))
data_0423_wide1 <- data_0423_wide_inputed %>%
  left_join(
    adm %>% select(subject_id, hadm_id, hosp_time),
    by = c("subject_id", "hadm_id")
  )
## 核对（入院层面）
chk <- adm$hosp_time
cat(
  "入院层面: n =", length(chk),
  "  mean =", round(mean(chk), 4),
  "  sd =", round(sd(chk), 4), "\n"
)





####################################

## ---------- data_0423_wide1：模拟 time（距入院天数；院内不规则随机采样）----------
## 规则：每组内按当前数据行顺序，time 严格递增；max(time) < hosp_time；间隔随机不等距。
.sim_measurement_times <- function(n, H) {
  if (length(n) != 1L || !is.numeric(n) || n < 1L) {
    stop(".sim_measurement_times: n 必须为正整数标量。")
  }
  n <- as.integer(n)
  if (!is.finite(H) || H <= 0) {
    stop(".sim_measurement_times: hosp_time 必须为正有限值。")
  }
  eps <- max(1e-6, H * 1e-9)
  if (H <= 2 * eps) {
    stop("hosp_time 过小，无法放入随机采样时间。")
  }
  lo <- eps
  hi <- H - eps
  if (n == 1L) {
    return(runif(1L, lo, hi))
  }
  ## Gamma → Dirichlet(1,...,1) 随机分割：累积分割点不规则、严格递增，且末点 < hi
  w <- as.numeric(stats::rgamma(n + 1L, shape = 1, rate = 1))
  w <- w / sum(w)
  frac <- cumsum(w)[seq_len(n)]
  lo + frac * (hi - lo)
}
library(dplyr)
## 若要先按 visit 排好再模拟，可取消注释：
# data_0423_wide1 <- data_0423_wide1 %>% arrange(subject_id, hadm_id, visit)
data_0423_wide2 <- data_0423_wide1 %>%
  dplyr::group_by(subject_id, hadm_id) %>%
  dplyr::group_modify(~ {
    df <- .x
    n <- nrow(df)
    Hv <- unique(df$hosp_time)
    if (length(Hv) != 1L || is.na(Hv)) {
      stop("每组 (subject_id, hadm_id) 须有唯一非 NA 的 hosp_time。")
    }
    df$time <- .sim_measurement_times(n, as.numeric(Hv))
    df
  }) %>%
  dplyr::ungroup()


write.csv(data_0423_wide2,"F:/文章_大论文/0417/data_0506.csv")































