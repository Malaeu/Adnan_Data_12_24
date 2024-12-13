library(tidyverse)
library(survival)
library(survminer)
library(mice)
library(corrplot)
library(car)
library(funModeling)
source("~/Dokumente/Statistik/Adnan/Daten/DR_Adnan_Statistik/cl_adnan/Data_Sets/combining/Code/install_epi_packages.R")

setwd("~/Dokumente/Statistik/Adnan/Daten/DR_Adnan_Statistik/cl_adnan/Data_Sets/combining")
# Функция для предобработки дат и создания unique_id
process_data <- function(df) {
  df %>%
    mutate(across(contains("date"), ~ as.Date(as.character(.)))) %>%
    mutate(unique_id = paste(sex, date_of_birth, blood_type, sep = "_"))
}

# Загружаем и обрабатываем все датасеты
datasets <- list(
  wl = readRDS("wl.rds") %>% process_data(),
  wl_v = readRDS("wl_v.rds") %>% process_data(),
  wl_adnan = readRDS("wl_adnan.rds") %>% process_data(),
  wl_bca = readRDS("wl_bca.rds")
)

# Объединение данных и фильтрация взрослых пациентов
wl_combined <- bind_rows(datasets$wl, datasets$wl_v) %>%
  distinct() %>%
  select(unique_id, etnr) %>%
  left_join(datasets$wl_adnan, by = "unique_id") %>%
  mutate(age = as.numeric(difftime(Sys.Date(), date_of_birth, units = "days")) / 365.25) %>%
  filter(age >= 18) %>%
  left_join(datasets$wl_bca, by = "acsn_nr")

# Создание переменной выживаемости
wl_combined <- wl_combined %>%
  mutate(
    survival_time = coalesce(survival_days_until_ltx, survival_days_from_wl_until_death, as.numeric(difftime(last_contact, date_of_wl, units = "days"))),
    event = ifelse(death == 1, 1, 0)
  )

# Обработка пропущенных значений
vars_to_impute <- names(wl_combined)[sapply(wl_combined, function(x) sum(is.na(x)) / length(x) < 0.5 & is.numeric(x))]
vars_to_impute <- setdiff(vars_to_impute, c("patient_number", "acsn_nr", "survival_time", "event"))

set.seed(500)
imputed_data <- mice(wl_combined[vars_to_impute], m = 5, maxit = 50, method = 'pmm', print = FALSE)
wl_combined_clean <- complete(imputed_data, 1)

# Возвращаем потерянные переменные
vars_to_exclude <- c("patient_number", "acsn_nr", "date_of_ct", "ct_accession_nr_2_intern",
                     "ct_accession_nr_extern", "date_of_wl", "date_of_nt", "last_contact",
                     "date_of_ltx", "date_of_birth", "date_of_death", "unique_id", "etnr")
vars_to_return <- setdiff(names(wl_combined), c(names(wl_combined_clean), vars_to_exclude))

wl_combined_clean[vars_to_return] <- wl_combined[vars_to_return]

# Заменяем NA в survival_time и event
wl_combined_clean <- wl_combined_clean %>%
  mutate(
    survival_time = ifelse(is.na(survival_time), median(survival_time, na.rm = TRUE), survival_time),
    event = ifelse(is.na(event), 0, event)
  )

# Обработка категориальных переменных
cat_vars <- c("hu_listung", "death", "death_on_the_list_without_ltx", "ltx", "blood_type",
              "hcc", "child_pugh_score", "dialysis_cat", "icu_cat", "ventilation_cat",
              "catecholamine_cat", "portal_vein_thrombosis")

wl_combined_clean <- wl_combined_clean %>%
  mutate(across(all_of(cat_vars), ~factor(., levels = 0:1, labels = c("no", "yes")))) %>%
  mutate(
    blood_type = factor(blood_type, levels = 1:4, labels = c("A", "B", "AB", "O")),
    child_pugh_score = factor(child_pugh_score, levels = 1:3, labels = c("A", "B", "C")),
    portal_vein_thrombosis = factor(portal_vein_thrombosis, levels = 0:2, labels = c("no", "yes", "partial"))
  )

# Группируем primary_diagnosis
wl_combined_clean <- wl_combined_clean %>%
  mutate(primary_diagnosis_grouped = case_when(
    primary_diagnosis %in% c("Alcoholic cirrhosis", "NASH", "Drug-induced cirrhosis") ~ "Metabolic",
    primary_diagnosis %in% c("Hepatocellular carcinoma and cirrhosis", "Hepatoblastoma", "Epithelioid hemangioendothelioma") ~ "Neoplastic",
    primary_diagnosis %in% c("Primary sclerosing cholangitis", "Primary biliary cirrhosis", "Secondary sclerosing cholangitis", "Secondary biliary cirrhosis") ~ "Cholestatic",
    primary_diagnosis %in% c("Virus related cirrhosis") ~ "Viral",
    primary_diagnosis %in% c("Autoimmunhepatitis") ~ "Autoimmune",
    primary_diagnosis %in% c("Budd Chiari", "Wilson disease", "Byler disease", "Extrahepatic biliary atresia", "Polycystic disease", "Alagille syndrome", "Caroli disease", "Congenital biliary fibrosis", "Antitrypsin deficiency", "Schistosomiasis", "Primary hyperoxaluria", "Cystic fibrosis") ~ "Genetic/Congenital",
    TRUE ~ "Other"
  )) %>%
  mutate(primary_diagnosis_grouped = as.factor(primary_diagnosis_grouped))

# Анализ выживаемости
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

# Анализ по группам
wl_combined_clean <- wl_combined_clean %>%
  mutate(
    age_group = cut(age, breaks = 3, labels = c("Молодой", "Средний", "Пожилой")),
    meld_group = cut(lab_meld_listing, breaks = 3, labels = c("Низкий", "Средний", "Высокий"))
  )

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

# Корреляционный анализ
numeric_data <- wl_combined_clean %>%
  select(where(is.numeric)) %>%
  select_if(~ var(., na.rm = TRUE) != 0)

cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

# Многофакторная модель Cox
multi_cox <- coxph(Surv(survival_time, event) ~ hu_listung + primary_diagnosis_grouped + blood_type + hcc + child_pugh_score + dialysis_cat + icu_cat + ventilation_cat + catecholamine_cat + portal_vein_thrombosis, data = wl_combined_clean)
print(summary(multi_cox))
print(ggforest(multi_cox, data = wl_combined_clean))

# Проверка на мультиколлинеарность
vif_model <- lm(survival_time ~ hu_listung + primary_diagnosis_grouped + blood_type + hcc + child_pugh_score + dialysis_cat + icu_cat + ventilation_cat + catecholamine_cat + portal_vein_thrombosis, data = wl_combined_clean)
print(vif(vif_model))

# Проверка пропорциональности рисков
test.ph <- cox.zph(multi_cox)
print(test.ph)
plot(test.ph)

# Модель с временно-зависимыми коэффициентами
wl_combined_clean <- wl_combined_clean %>%
  mutate(survival_time_adj = survival_time + 0.1)

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

# Проверка на нелинейность
martingale_residuals <- residuals(multi_cox, type = "martingale")
plot(wl_combined_clean$age, martingale_residuals,
     xlab = "Возраст", ylab = "Мартингейловские остатки",
     main = "Проверка на нелинейность")
lines(lowess(wl_combined_clean$age, martingale_residuals), col = "red")

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
