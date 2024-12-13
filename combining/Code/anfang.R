library(funModeling)
library(gtsummary)
# Комментарий: Сначала объявим вектор с нашими охуенными колонками
columns_to_check <- c("bone", "muscle", "sat", "vat", "imat", "eat", "pat", "tat")

# Комментарий: Теперь применим нашу магию фильтрации
result <- cleaned_final_dataset[rowSums(!is.na(cleaned_final_dataset[, columns_to_check])) > 0, ]
df_reduced <- cleaned_final_dataset[rowSums(!is.na(cleaned_final_dataset[, columns_to_check])) > 0, ]
# Комментарий: Выведем результат, чтобы полюбоваться нашей работой
print(result)

# Комментарий: Если хочешь знать, сколько строк мы выловили
cat("Количество найденных строк:", nrow(result), "\n")
df_status(df_reduced)
# Комментарий: Загрузим библиотеку lubridate для работы с датами
library(lubridate)

# Комментарий: Предполагаем, что у тебя есть датафрейм df с колонками Geburtsdatum и Anmeldung_WL
# Если названия колонок другие, замени их на свои
Warteliste_verstorben_konietzko <- Warteliste_verstorben_konietzko %>% 
  janitor::clean_names()
wl <- wl %>% janitor::clean_names()
names(wl)
wl_adnan <- wl_adnan %>% janitor::clean_names()
names(wl_adnan)
wl_bca <- bca_acsn %>% janitor::clean_names()
# Комментарий: Преобразуем строки в даты, если они еще не в формате даты
Warteliste_verstorben_konietzko$gebdatum <- as.Date(Warteliste_verstorben_konietzko$gebdatum)
Warteliste_verstorben_konietzko$anmeldung_wl <- as.Date(Warteliste_verstorben_konietzko$anmeldung_wl)

# Комментарий: Вычисляем возраст
Warteliste_verstorben_konietzko$alter <- as.numeric(interval(Warteliste_verstorben_konietzko$gebdatum, Warteliste_verstorben_konietzko$anmeldung_wl) / years(1))

# Комментарий: Округляем возраст до целых лет
Warteliste_verstorben_konietzko$alter<- floor(Warteliste_verstorben_konietzko$alter)

# Комментарий: Выводим первые несколько строк, чтобы проверить результат
print(head(df))

wl_v <- Warteliste_verstorben_konietzko
wl <- Warteliste2_konietzko
saveRDS(wl,"wl.rds")
saveRDS(wl_adnan,"wl_adnan.rds")
saveRDS(wl_bca,"wl_bca.rds")
saveRDS(wl_v,"wl_v.rds")