library(tidyverse)
library(lubridate)
library(mice)
library(survival)
library(survminer)
library(corrplot)
library(splines)
library(car)
library(glmnet)
library(randomForestSRC)

# ---- Шаг 1: Загрузка и предобработка данных ----

# Загрузка данных
wl <- readRDS("wl.rds")
wl_v <- readRDS("wl_v.rds")
wl_adnan <- readRDS("wl_adnan.rds")
wl_bca <- readRDS("wl_bca.rds")

# Функция для предобработки дат и создания unique_id
process_data <- function(df) {
  df %>%
    mutate(across(contains("date"), ~ as.Date(as.character(.)))) %>%
    mutate(unique_id = paste(sex, date_of_birth, blood_type, sep = "_"))
}

wl <- process_data(wl)
wl_v <- process_data(wl_v)
wl_adnan <- process_data(wl_adnan)

# ---- Шаг 2: Объединение данных ----

wl_combined <- bind_rows(wl, wl_v) %>% distinct()
etnr_data <- wl_combined %>% select(unique_id, etnr)
wl_adnan <- wl_adnan %>% left_join(etnr_data, by = "unique_id")

# ---- Шаг 3: Фильтрация взрослых пациентов и объединение с данными body composition ----

wl_combined <- wl_adnan %>%
  mutate(age = as.numeric(difftime(Sys.Date(), date_of_birth, units = "days")) / 365.25) %>%
  filter(age >= 18) %>%
  left_join(wl_bca, by = "acsn_nr")

# ---- Шаг 4: Создание переменной выживаемости ----

wl_combined <- wl_combined %>%
  mutate(
    survival_time = case_when(
      !is.na(survival_days_until_ltx) ~ survival_days_until_ltx,
      !is.na(survival_days_from_wl_until_death) ~ survival_days_from_wl_until_death,
      TRUE ~ as.numeric(difftime(last_contact, date_of_wl, units = "days"))
    ),
    event = ifelse(death == 1, 1, 0)
  )

# ---- Шаг 5: Обработка пропущенных значений ----

vars_to_impute <- names(wl_combined)[sapply(wl_combined, function(x) sum(is.na(x)) / length(x) < 0.5 & is.numeric(x))]
vars_to_impute <- vars_to_impute[!vars_to_impute %in% c("patient_number", "acsn_nr", "survival_time", "event")]

set.seed(500)
imputed_data <- mice(wl_combined[vars_to_impute], m = 5, maxit = 50, method = 'pmm')
wl_combined_clean <- complete(imputed_data, 1)

# Возвращаем потерянные переменные
vars_to_exclude <- c("patient_number", "acsn_nr", "date_of_ct", "ct_accession_nr_2_intern", 
                     "ct_accession_nr_extern", "date_of_wl", "date_of_nt", "last_contact", 
                     "date_of_ltx", "date_of_birth", "date_of_death", "unique_id", "etnr")
vars_to_return <- setdiff(names(wl_combined), c(names(wl_combined_clean), vars_to_exclude))

for (var in vars_to_return) {
  wl_combined_clean[[var]] <- wl_combined[[var]]
}

# Заменяем NA в survival_time и event
wl_combined_clean$survival_time[is.na(wl_combined_clean$survival_time)] <- median(wl_combined_clean$survival_time, na.rm = TRUE)
wl_combined_clean$event[is.na(wl_combined_clean$event)] <- 0

# ---- Шаг 6: Обработка категориальных переменных ----

# Преобразуем числовые коды в факторы с понятными названиями
wl_combined_clean <- wl_combined_clean %>%
  mutate(
    hu_listung = factor(hu_listung, levels = c(0, 1), labels = c("no", "yes")),
    death = factor(death, levels = c(0, 1), labels = c("no", "yes")),
    death_on_the_list_without_ltx = factor(death_on_the_list_without_ltx, levels = c(0, 1), labels = c("no", "yes")),
    ltx = factor(ltx, levels = c(0, 1), labels = c("no", "yes")),
    blood_type = factor(blood_type, levels = 1:4, labels = c("A", "B", "AB", "O")),
    hcc = factor(hcc, levels = c(0, 1), labels = c("no", "yes")),
    child_pugh_score = factor(child_pugh_score, levels = 1:3, labels = c("A", "B", "C")),
    dialysis_cat = factor(dialysis_cat, levels = c(0, 1), labels = c("no", "yes")),
    icu_cat = factor(icu_cat, levels = c(0, 1), labels = c("no", "yes")),
    ventilation_cat = factor(ventilation_cat, levels = c(0, 1), labels = c("no", "yes")),
    catecholamine_cat = factor(catecholamine_cat, levels = c(0, 1), labels = c("no", "yes")),
    portal_vein_thrombosis = factor(portal_vein_thrombosis, levels = c(0, 1, 2), labels = c("no", "yes", "partial"))
  )

# Группируем primary_diagnosis
levels(wl_combined_clean$primary_diagnosis) <- c(
  "Alcoholic cirrhosis", "Hepatocellular carcinoma and cirrhosis", "NASH",
  "Primary sclerosing cholangitis", "Autoimmunhepatitis", "Others specify",
  "Virus related cirrhosis", "Budd Chiari", "Wilson disease", "Byler disease",
  "Extrahepatic biliary atresia", "Primary biliary cirrhosis", "Polycystic disease",
  "Alagille syndrome", "Drug-induced cirrhosis", "Secondary sclerosing cholangitis",
  "Secondary biliary cirrhosis", "Caroli disease", "Hepatoblastoma",
  "Congenital biliary fibrosis", "Post-operative", "Antitrypsin deficiency",
  "Schistosomiasis", "Primary hyperoxaluria", "Cystic fibrosis",
  "Epithelioid hemangioendothelioma"
)

wl_combined_clean$primary_diagnosis_grouped <- case_when(
  wl_combined_clean$primary_diagnosis %in% c("Alcoholic cirrhosis", "NASH", "Drug-induced cirrhosis") ~ "Metabolic",
  wl_combined_clean$primary_diagnosis %in% c("Hepatocellular carcinoma and cirrhosis", "Hepatoblastoma", "Epithelioid hemangioendothelioma") ~ "Neoplastic",
  wl_combined_clean$primary_diagnosis %in% c("Primary sclerosing cholangitis", "Primary biliary cirrhosis", "Secondary sclerosing cholangitis", "Secondary biliary cirrhosis") ~ "Cholestatic",
  wl_combined_clean$primary_diagnosis %in% c("Virus related cirrhosis") ~ "Viral",
  wl_combined_clean$primary_diagnosis %in% c("Autoimmunhepatitis") ~ "Autoimmune",
  wl_combined_clean$primary_diagnosis %in% c("Budd Chiari", "Wilson disease", "Byler disease", "Extrahepatic biliary atresia", "Polycystic disease", "Alagille syndrome", "Caroli disease", "Congenital biliary fibrosis", "Antitrypsin deficiency", "Schistosomiasis", "Primary hyperoxaluria", "Cystic fibrosis") ~ "Genetic/Congenital",
  TRUE ~ "Other"
)

wl_combined_clean$primary_diagnosis_grouped <- as.factor(wl_combined_clean$primary_diagnosis_grouped)

# Обрабатываем NA в категориальных переменных
cat_vars <- c("blood_type", "sex", "primary_diagnosis", "hcc", "child_pugh_score", "exceptional_meld",
              "primary_diagnosis_grouped", "hu_listung", "dialysis_cat", "icu_cat", "ventilation_cat", "catecholamine_cat", "portal_vein_thrombosis")
for (var in cat_vars) {
  if (sum(is.na(wl_combined_clean[[var]])) > 0) {
    wl_combined_clean[[var]] <- addNA(wl_combined_clean[[var]])
    levels(wl_combined_clean[[var]])[is.na(levels(wl_combined_clean[[var]]))] <- "Unknown"
  }
}


# ---- Шаг 7: Анализ выживаемости ----

# Создаем объект Surv
surv_obj <- Surv(wl_combined_clean$survival_time, wl_combined_clean$event)

# Кривая Каплана-Мейера
km_fit <- survfit(surv_obj ~ 1)
print(ggsurvplot(km_fit, data = wl_combined_clean, risk.table = TRUE, 
                 title = "Кривая выживаемости Каплана-Мейера"))

# Cox модель
cox_vars <- c("age", "sex", "blood_type", "lab_meld_listing", "hcc", "child_pugh_score")
cox_formula <- as.formula(paste("Surv(survival_time, event) ~", paste(cox_vars, collapse = " + ")))
cox_model <- coxph(cox_formula, data = wl_combined_clean)
print(summary(cox_model))
print(ggforest(cox_model, data = wl_combined_clean))

# ---- Шаг 8: Анализ по группам ----

# Анализ по возрасту и MELD score
wl_combined_clean$age_group <- cut(wl_combined_clean$age, breaks = 3, labels = c("Молодой", "Средний", "Пожилой"))
wl_combined_clean$meld_group <- cut(wl_combined_clean$lab_meld_listing, breaks = 3, labels = c("Низкий", "Средний", "Высокий"))

km_fit_age <- survfit(surv_obj ~ age_group, data = wl_combined_clean)
print(ggsurvplot(km_fit_age, data = wl_combined_clean, pval = TRUE, 
                 title = "Кривые выживаемости по возрастным группам"))

km_fit_meld <- survfit(surv_obj ~ meld_group, data = wl_combined_clean)
print(ggsurvplot(km_fit_meld, data = wl_combined_clean, pval = TRUE, 
                 title = "Кривые выживаемости по группам MELD score"))

# Анализ взаимодействия возраста и MELD score
cox_interaction <- coxph(surv_obj ~ age * lab_meld_listing, data = wl_combined_clean)
print(summary(cox_interaction))

# Визуализация взаимодействия
age_range <- range(wl_combined_clean$age)
meld_range <- range(wl_combined_clean$lab_meld_listing)

grid <- expand.grid(
  age = seq(age_range[1], age_range[2], length.out = 100),
  lab_meld_listing = seq(meld_range[1], meld_range[2], length.out = 100)
)

grid$risk <- predict(cox_interaction, newdata = grid, type = "risk")

print(ggplot(grid, aes(x = age, y = lab_meld_listing, fill = risk)) +
        geom_tile() +
        scale_fill_gradient(low = "blue", high = "red") +
        labs(title = "Взаимодействие возраста и MELD score",
             x = "Возраст", y = "MELD score", fill = "Риск") +
        theme_minimal())

# Анализ выживаемости для комбинаций групп
km_fit_interaction <- survfit(surv_obj ~ age_group + meld_group, data = wl_combined_clean)
print(ggsurvplot(km_fit_interaction, data = wl_combined_clean, 
                 title = "Кривые выживаемости по группам возраста и MELD score",
                 legend.title = "Группы",
                 risk.table = TRUE,
                 pval = TRUE))

# Анализ Child-Pugh score
km_fit_child <- survfit(surv_obj ~ child_pugh_score, data = wl_combined_clean)
print(ggsurvplot(km_fit_child, data = wl_combined_clean, pval = TRUE, 
                 title = "Кривые выживаемости по Child-Pugh score"))

# ---- Шаг 9: Корреляционный анализ ----
# Пример преобразования
# Преобразуем переменные в числовые
vars_to_convert <- c("inr_listing", "bili_listing", "crea_listing", "sodium_listing", "lab_meld_nt", "lap_meld_ltx", "lap_meld_death", "inr_removal", "bili_removal", "crea_removal", "natrium_removal", "platelets_removal", "days_of_icu", "days_of_ventilation", "amount_of_catecholamine")

for (var in vars_to_convert) {
  wl_combined_clean[[var]] <- as.numeric(wl_combined_clean[[var]])
}
for (var in vars_to_convert) {
  print(paste("NA в", var, ":", sum(is.na(wl_combined_clean[[var]]))))
}
# Повторно создаем numeric_data_filtered
# Выбираем только числовые переменные (без проблемных)
numeric_vars <- sapply(wl_combined_clean, is.numeric)
vars_to_exclude <- c("survival_days_until_nt", "survival_days_from_nt_until_last_contact", "survival_days_from_wl_until_death", "lab_meld_nt", "lap_meld_ltx", "lap_meld_death", "inr_removal", "bili_removal", "crea_removal", "natrium_removal", "platelets_removal")
numeric_data_filtered <- wl_combined_clean[, numeric_vars & !(names(wl_combined_clean) %in% vars_to_exclude)]

# Убираем переменные с нулевой дисперсией
zero_var <- apply(numeric_data_filtered, 2, function(x) var(x, na.rm = TRUE) == 0)
numeric_data_filtered <- numeric_data_filtered[, !zero_var]

# Преобразуем primary_diagnosis в числовой формат
numeric_data_filtered$primary_diagnosis <- as.numeric(as.character(numeric_data_filtered$primary_diagnosis))

# Строим корреляционную матрицу
cor_matrix <- cor(numeric_data_filtered, use = "pairwise.complete.obs")

# Визуализация
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Строим корреляционную матрицу
cor_matrix <- cor(numeric_data_filtered, use = "pairwise.complete.obs")

# Визуализация
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
wl_combined_clean$survival_time[wl_combined_clean$survival_time == 0] <- 0.1


library(car)
vif_model <- lm(survival_time ~ ., data = numeric_data_filtered)
print(vif(vif_model))

# Удаляем переменные
wl_combined_clean <- wl_combined_clean %>%
  select(-c(age_by_listung, weight, tat))

# Создаем индекс body composition
wl_combined_clean$body_composition <- wl_combined_clean$sat + wl_combined_clean$vat + wl_combined_clean$imat + wl_combined_clean$eat + wl_combined_clean$pat
outliers <- boxplot.stats(wl_combined_clean$lab_meld_listing)$out
print(paste("Количество выбросов:", length(outliers)))
print(outliers)
# Создаем base_model до исключения survival_time
base_model <- lm(survival_time ~ 1, data = wl_combined_clean)

vars_to_exclude <- c("lab_meld_nt", "lap_meld_ltx", "lap_meld_death", "inr_removal", "bili_removal", "crea_removal", "natrium_removal", "platelets_removal", "days_of_dialysis")
wl_combined_clean <- wl_combined_clean[, !(names(wl_combined_clean) %in% vars_to_exclude)]

# Обновляем numeric_vars
numeric_vars <- sapply(wl_combined_clean, is.numeric)
numeric_vars <- numeric_vars & !(names(wl_combined_clean) == "survival_time")


for (var in names(wl_combined_clean)[numeric_vars]) { 
  formula <- update(formula(base_model), paste(". ~ . +", var))
  model <- tryCatch({
    lm(formula, data = wl_combined_clean[, c("survival_time", names(wl_combined_clean)[numeric_vars])]) # Добавляем survival_time обратно в data
  }, error = function(e) {
    print(paste("Ошибка при добавлении переменной:", var))
    print(e)
    return(NULL)
  })
  
  if (!is.null(model)) {
    print(paste("Модель с добавлением переменной:", var))
    print(summary(model))
    # Рассчитываем VIF только если в модели больше одного предиктора
    if (length(coef(model)) > 2) { 
      print(vif(model))
    }
  }
}

# Пересчитываем VIF
numeric_vars <- sapply(wl_combined_clean, is.numeric)
vif_model <- lm(survival_time ~ ., data = wl_combined_clean[, numeric_vars])
print(vif(vif_model))

# ---- Шаг 10: Анализ категориальных переменных ----

cat_vars <- c("primary_diagnosis_grouped", "hu_listung", "blood_type", "hcc", "child_pugh_score", "dialysis_cat", "icu_cat", "ventilation_cat", "catecholamine_cat", "portal_vein_thrombosis")

for (var in cat_vars) {
  print(paste("Анализ переменной:", var))
  print(table(wl_combined_clean[[var]], useNA = "ifany"))
  
  cox_formula <- as.formula(paste("Surv(survival_time, event) ~", var))
  cox_model <- coxph(cox_formula, data = wl_combined_clean)
  print(summary(cox_model))
}

# ---- Шаг 11: Многофакторная модель Cox ----

multi_cox <- coxph(Surv(survival_time, event) ~ hu_listung + primary_diagnosis_grouped + blood_type + hcc + child_pugh_score + dialysis_cat + icu_cat + ventilation_cat + catecholamine_cat + portal_vein_thrombosis, data = wl_combined_clean)
print(summary(multi_cox))
print(ggforest(multi_cox, data = wl_combined_clean))

# ---- Шаг 12: Проверка пропорциональности рисков ----

test.ph <- cox.zph(multi_cox)
print(test.ph)
plot(test.ph)

# ---- Шаг 13: Дополнительные анализы ----

# Модель с временно-зависимыми коэффициентами
wl_combined_clean$survival_time_adj <- wl_combined_clean$survival_time + 0.1
time_dep_model <- coxph(Surv(survival_time_adj, event) ~ primary_diagnosis_grouped * log(survival_time_adj) + 
                          child_pugh_score + dialysis_cat + icu_cat + 
                          ventilation_cat + catecholamine_cat, 
                        data = wl_combined_clean)
print(summary(time_dep_model))

# Визуализация изменения риска со временем для разных диагнозов
newdata <- expand.grid(
  primary_diagnosis_grouped = levels(wl_combined_clean$primary_diagnosis_grouped),
  survival_time_adj = seq(0.1, max(wl_combined_clean$survival_time_adj), length.out = 100),
  child_pugh_score = "A",
  dialysis_cat = "no",
  icu_cat = "no",
  ventilation_cat = "no",
  catecholamine_cat = "no"
)
newdata$risk <- predict(time_dep_model, newdata = newdata, type = "risk")
print(ggplot(newdata, aes(x = survival_time_adj, y = risk, color = primary_diagnosis_grouped)) +
        geom_line() +
        scale_x_log10() +
        labs(title = "Изменение риска со временем для разных диагнозов",
             x = "Время выживания (log scale)", y = "Относительный риск") +
        theme_minimal())

# Сравнение моделей по AIC
AIC_time_dep <- AIC(time_dep_model)
AIC_strat <- AIC(strat_model)
print(data.frame(Model = c("Time-dependent", "Stratified"),
                 AIC = c(AIC_time_dep, AIC_strat)))

# Сплайновая модель
spline_model <- coxph(Surv(survival_time_adj, event) ~ 
                        primary_diagnosis_grouped * ns(log(survival_time_adj), df = 3) +
                        child_pugh_score + dialysis_cat + icu_cat + 
                        ventilation_cat + catecholamine_cat,
                      data = wl_combined_clean)
print(summary(spline_model))

# ---- Шаг 14: Сохранение и вывод информации ----

# Сохранение финального датасета
saveRDS(wl_combined_clean, "wl_combined_final.rds")

# Вывод основной информации о датасете
cat("Финальный датасет создан. Основная информация:\n")
cat("Количество строк:", nrow(wl_combined_clean), "\n")
cat("Количество столбцов:", ncol(wl_combined_clean), "\n")
cat("Количество пациентов с данными body composition:", sum(!is.na(wl_combined_clean$bone)), "\n")
cat("Диапазон возрастов:", min(wl_combined_clean$age), "-", max(wl_combined_clean$age), "\n")
cat("Распределение по полу:\n")
print(table(wl_combined_clean$sex))
cat("Распределение по группам крови:\n")
print(table(wl_combined_clean$blood_type))

