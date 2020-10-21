#'
#' # Stress data
#'

data_dir <- '/data/jflournoy/SEA/sea_rsfc_behavioral_data/'

stress_cols <- c('ID', 'TIME',
                 'WCEN_CHRONICSEV', 'GCEN_CHRONICSEV', 
                 'WCEN_EPISODICTOT', 'GCEN_EPISODICTOT',
                 'WCEN_IP_CHRONICSEV', 'GCEN_IP_CHRONICSEV', 
                 'WCEN_IP_EPISODICTOT', 'GCEN_IP_EPISODICTOT')
stress_ <- haven::read_sav(file.path(data_dir, 'SEA_DATA_BETWEEN_WITHIN_CENTERED_LONG_UCLA_INTERVIEW.sav'))
stress <- stress_[, stress_cols]

#'
#' # GAD data
#'

GAD_ <- haven::read_sav(file.path(data_dir, 'SEA_DATA_SCORED_GAD7_ALL_MONTHS.sav'))
GAD_ <- dplyr::filter(GAD_, ID != '1017')
GAD_l <- tidyr::gather(GAD_, key = 'key', value = 'value', -ID)
GAD_l <- tidyr::extract(GAD_l, key, c('TIME', 'ITEM'), 'm(\\d+)_(?:gas7|GAD7)_c*(\\d+|TOT)')
GAD_w <- tidyr::spread(GAD_l, ITEM, value)

GAD_check <- tidyr::gather(GAD_w, key = 'key', value = 'value' , matches('[1-7]'))
GAD_check_sum <- dplyr::summarize(dplyr::group_by(GAD_check, ID, TIME),
                                  TOT = unique(TOT),
                                  mean = mean(value, na.rm = TRUE),
                                  check_TOT = mean*7,
                                  n_NA = sum(is.na(value)))
GAD_Error <- dplyr::filter(GAD_check_sum, TOT != check_TOT)
warning(sprintf('Erroneous GAD scores due to sum scores with missing values: %d', dim(GAD_Error)[[1]]))
print(GAD_Error)

GAD_w <- dplyr::left_join(GAD_w, GAD_check_sum[, c('ID', 'TIME', 'check_TOT')])
GAD_l$TIME <- as.numeric(GAD_l$TIME)
print(psych::multilevel.reliability(dplyr::filter(GAD_l, ITEM != 'TOT'), 
                                    lmer = TRUE,
                                    grp = 'ID', Time = 'TIME', long = TRUE, 
                                    items = 'ITEM', values = 'value',
                                    alpha=TRUE), 
      short = FALSE)

GAD <- GAD_w[, c('ID', 'TIME', 'check_TOT')]
names(GAD) <- c('ID', 'TIME', 'GAD7_TOT')

#'
#' # PHQ9 data
#'

PHQ9_ <- haven::read_sav(file.path(data_dir, 'SEA_DATA_SCORED_PHQ9_ALL_MONTHS.sav'))
PHQ9_ <- dplyr::select(dplyr::filter(PHQ9_, ID != '1017'),
                       -dplyr::matches('_impairment$'))

PHQ9_l <- tidyr::gather(PHQ9_, key = 'key', value = 'value', -ID)
PHQ9_l <- tidyr::extract(PHQ9_l, key, c('TIME', 'ITEM'), 'm(\\d+)_(?:phq9|PHQ9)_c*(\\d+|TOT)')
PHQ9_w <- tidyr::spread(PHQ9_l, ITEM, value)

PHQ9_check <- tidyr::gather(PHQ9_w, key = 'key', value = 'value' , matches('[1-9]'))
PHQ9_check_sum <- dplyr::summarize(dplyr::group_by(PHQ9_check, ID, TIME),
                                   TOT = unique(TOT),
                                   mean = mean(value, na.rm = TRUE),
                                   check_TOT = mean*9,
                                   n_NA = sum(is.na(value)))
PHQ9_Error <- dplyr::filter(PHQ9_check_sum, TOT != check_TOT)
warning(sprintf('Erroneous PHQ9 scores due to sum scores with missing values: %d', dim(PHQ9_Error)[[1]]))
print(PHQ9_Error)

PHQ9_w <- dplyr::left_join(PHQ9_w, PHQ9_check_sum[, c('ID', 'TIME', 'check_TOT')])
print(psych::multilevel.reliability(dplyr::filter(PHQ9_l, ITEM != 'TOT'), 
                                    lmer = TRUE,
                                    grp = 'ID', Time = 'TIME', long = TRUE, 
                                    items = 'ITEM', values = 'value',
                                    alpha=TRUE), 
      short = FALSE)

PHQ9 <- PHQ9_w[, c('ID', 'TIME', 'check_TOT')]
names(PHQ9) <- c('ID', 'TIME', 'PHQ9_TOT')

#'
#' # Rumination data
#'

#COMPUTE m0_CERQ_LONG_RUM = SUM(m0_CERQ_LONG_c3, m0_CERQ_LONG_c12, m0_CERQ_LONG_c21, m0_CERQ_LONG_c30).
#COMPUTE m1_CERQ_SHORT_RUM = SUM(m1_CERQ_LONG_c3, m1_CERQ_LONG_c12).
CERQ_ <- haven::read_sav(file.path(data_dir, 'SEA_DATA_SCORED_CERQ_LONG_AND_SHORT_ALL_MONTHS.sav'))
CERQ_ <- dplyr::select(dplyr::filter(CERQ_, ID != '1017'),
                       ID,
                       dplyr::matches('_SHORT_RUM$'),
                       dplyr::matches('_c(3|12)$'))
CERQ_l <- tidyr::gather(CERQ_, key = 'key', value = 'value', -ID)
CERQ_l <- tidyr::extract(CERQ_l, key, c('TIME', 'ITEM'), 'm(\\d+)_(?:CERQ|cerq)(?:_LONG|_long|_SHORT)_c*(\\d+|RUM)')
CERQ_w <- tidyr::spread(CERQ_l, ITEM, value)

CERQ_check <- tidyr::gather(CERQ_w, key = 'key', value = 'value' , matches('(3|12)'))
CERQ_check_sum <- dplyr::summarize(dplyr::group_by(CERQ_check, ID, TIME),
                                   RUM = unique(RUM),
                                   mean = mean(value, na.rm = TRUE),
                                   check_RUM = mean*2,
                                   n_NA = sum(is.na(value)))
CERQ_Error <- dplyr::filter(CERQ_check_sum, RUM != check_RUM)
warning(sprintf('Erroneous CEQR RUM scores due to sum scores with missing values: %d', dim(CERQ_Error)[[1]]))
print(CERQ_Error)

CERQ_w <- dplyr::left_join(CERQ_w, CERQ_check_sum[, c('ID', 'TIME', 'check_RUM')])
print(psych::multilevel.reliability(dplyr::filter(CERQ_l, ITEM != 'RUM'), 
                                    lmer = TRUE,
                                    grp = 'ID', Time = 'TIME', long = TRUE, 
                                    items = 'ITEM', values = 'value',
                                    alpha=TRUE), 
      short = FALSE)

CERQ <- CERQ_w[, c('ID', 'TIME', 'check_RUM')]
names(CERQ) <- c('ID', 'TIME', 'CERQ_RUM')
save(stress, GAD, PHQ9, CERQ, file = 'beh_data-cleaned_checked.Rda')
