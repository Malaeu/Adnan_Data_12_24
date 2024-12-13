
# Комментарий: Функция для переименования колонки в одном датасете
rename_gebdatum <- function(df) {
  if ("gebdatum" %in% names(df)) {
    names(df)[names(df) == "gebdatum"] <- "date_of_birth"
  }
  return(df)
}
# Комментарий: Получаем список всех объектов в глобальном окружении
all_objects <- ls(envir = .GlobalEnv)

# Комментарий: Проходимся по всем объектам и переименовываем колонку, если это датафрейм
for (obj_name in all_objects) {
  obj <- get(obj_name)
  if (is.data.frame(obj)) {
    assign(obj_name, rename_gebdatum(obj), envir = .GlobalEnv)
  }
}

# Комментарий: Выводим сообщение о завершении
cat("Ебать-колотить, переименование завершено! Проверь свои датасеты, сука!\n")

# Комментарий: Проверяем, что переименование прошло успешно
for (obj_name in all_objects) {
  obj <- get(obj_name)
  if (is.data.frame(obj) && "date_of_birth" %in% names(obj)) {
    cat(paste0("В датасете ", obj_name, " колонка успешно переименована, бля!\n"))
  }
}


# Save_all_rds_and_xlsx ---------------------------------------------------


# Комментарий: Загружаем необходимые библиотеки
library(openxlsx)

# Комментарий: Функция для сохранения датафрейма в RDS и Excel
save_dataframe <- function(df, name) {
  # Сохраняем как RDS
  rds_path <- paste0(name, ".rds")
  saveRDS(df, rds_path)
  cat(paste0("Охуенно! ", name, " сохранен как ", rds_path, "\n"))
  
  # Сохраняем как Excel
  excel_path <- paste0(name, ".xlsx")
  write.xlsx(df, excel_path, overwrite = TRUE)
  cat(paste0("Заебись! ", name, " также сохранен как ", excel_path, "\n"))
}

# Комментарий: Сохраняем каждый датафрейм
save_dataframe(wl, "wl")
save_dataframe(wl_adnan, "wl_adnan")
save_dataframe(wl_bca, "wl_bca")
save_dataframe(wl_v, "wl_v")

# Комментарий: Выводим сообщение о завершении
cat("Ебать-колотить, все датафреймы сохранены и как RDS, и как Excel! Проверяй, сука!\n")

# Комментарий: Выводим список сохраненных файлов
all_files <- list.files(pattern = "\\.(rds|xlsx)$")
cat("Список сохраненных файлов:\n")
cat(paste0("- ", all_files, "\n"))



# Var_names_in all_rds ----------------------------------------------------


# О, великий R, даруй нам силу кода,
library(openxlsx)  # Чтоб Excel покорился нашей воле!

# Список RDS-файлов, как строки в природе,
rds_files <- c("wl.rds", "wl_adnan.rds", "wl_bca.rds", "wl_v.rds")

# Функция-муза, что имена дарует,
get_var_names <- function(file) {
  df <- readRDS(file)
  return(data.frame(
    file = file,
    variable = names(df),
    stringsAsFactors = FALSE
  ))
}

# Все имена собрать - вот наша цель,
all_var_names <- do.call(rbind, lapply(rds_files, get_var_names))

# В Excel-сонет мы впишем их теперь,
write.xlsx(all_var_names, "variable_names_sonnet.xlsx")

# И на экран выведем, как в театре,
print(all_var_names)

cat("\nО, Ылша! Я создал для тебя Excel-сонет 'variable_names_sonnet.xlsx'!\n")
cat("Там все имена переменных, как звезды на небе, рассыпаны.\n")
cat("Теперь ты можешь их переименовать, как душе угодно, блять!\n")



# Angleichen die Names der Variables --------------------------------------


library(openxlsx)

# Функция для переименования переменных с проверкой
rename_vars <- function(df, old_names, new_names) {
  for (i in seq_along(old_names)) {
    if (old_names[i] %in% names(df)) {
      names(df)[names(df) == old_names[i]] <- new_names[i]
    } else {
      cat("Внимание, блять! Переменная", old_names[i], "не найдена в датафрейме.\n")
    }
  }
  return(df)
}

# Читаем RDS файлы
wl <- readRDS("wl.rds")
wl_adnan <- readRDS("wl_adnan.rds")
wl_v <- readRDS("wl_v.rds")

# Переименовываем в wl_adnan
wl_adnan <- rename_vars(wl_adnan, 
                        c("ct_accession_nr_1_intern"), 
                        c("acsn_nr"))

# Список для переименования
# Переименовываем в wl и wl_v
rename_list <- c(
  
  "geschlecht" = "sex",
  "bg" = "blood_type",
  "kg" = "weight",
  "cm" = "height",
  "diagnose_1" = "primary_diagnosis",
  "diagnose_relistung" = "relisting_diagnosis",
  "re_list" = "relisting_flag",
  "re_list_datum" = "relisting_date",
  "studie" = "study",
  "anmeldung_wl" = "date_of_wl",
  "urg" = "urgency",
  
  "tx_datum" = "date_of_ltx",
  "tx_nr" = "ltx_number",
  "diagnose" = "diagnosis",
  "abdominelle_opa_s" = "abdominal_surgeries",
  "bemerkungen" = "notes",
  "pfortader" = "portal_vein_thrombosis",
  "infektionen" = "infections",
  "letzter_kontakt" = "last_contact"
)

# Переименовываем в wl и wl_v
cat("Переименовываем переменные в wl:\n")
wl <- rename_vars(wl, names(rename_list), unname(rename_list))

cat("\nПереименовываем переменные в wl_v:\n")
wl_v <- rename_vars(wl_v, names(rename_list), unname(rename_list))

# Выводим текущие имена переменных
cat("\nТекущие имена переменных в wl:\n")
print(names(wl))
cat("\nТекущие имена переменных в wl_v:\n")
print(names(wl_v))

# Проверяем, какие переменные остались непереименованными
check_remaining <- function(df, original_names) {
  remaining <- intersect(names(df), original_names)
  if (length(remaining) > 0) {
    cat("Следующие переменные остались непереименованными:\n")
    print(remaining)
  } else {
    cat("Все переменные успешно переименованы, сука!\n")
  }
}

cat("\nПроверяем оставшиеся переменные в wl:\n")
check_remaining(wl, names(rename_list))

cat("\nПроверяем оставшиеся переменные в wl_v:\n")
check_remaining(wl_v, names(rename_list))


# Unique_ID_in_all_df -----------------------------------------------------

library(dplyr)
library(lubridate)

# Читаем RDS файлы
wl <- readRDS("wl.rds")
wl_v <- readRDS("wl_v.rds")
wl_adnan <- readRDS("wl_adnan.rds")

# Функция для создания уникального идентификатора
create_unique_id <- function(df) {
  df %>%
    mutate(
      unique_id = paste(sex, date_of_birth, blood_type, rh, date_of_wl, sep = "_"),
      date_of_birth = as.Date(date_of_birth),
      date_of_wl = as.Date(date_of_wl)
    )
}

# Применяем новую функцию
wl <- create_unique_id(wl)
wl_v <- create_unique_id(wl_v)
wl_adnan <- wl_adnan %>%
  mutate(
    unique_id = paste(sex, date_of_birth, blood_type, date_of_wl, sep = "_"),
    date_of_birth = as.Date(date_of_birth),
    date_of_wl = as.Date(date_of_wl)
  )

# Снова проверяем на дубликаты
etnr_data <- bind_rows(
  select(wl, etnr, unique_id),
  select(wl_v, etnr, unique_id)
) %>% distinct()

duplicates <- etnr_data %>% group_by(unique_id) %>% filter(n() > 1)
if (nrow(duplicates) > 0) {
  cat("Внимание, блять! Все еще есть дубликаты unique_id:\n")
  print(duplicates)
  cat("Нужно разобраться с этими дубликатами вручную!\n")
} else {
  cat("Заебись! Дубликатов больше нет!\n")
}


# Создаем уникальные идентификаторы в wl и wl_v
wl <- create_unique_id(wl)
wl_v <- create_unique_id(wl_v)

# Создаем уникальный идентификатор в wl_adnan
wl_adnan <- wl_adnan %>%
  mutate(
    unique_id = paste(sex, date_of_birth, blood_type, sep = "_"),
    date_of_birth = as.Date(date_of_birth)
  )

# Объединяем etnr из wl и wl_v
etnr_data <- bind_rows(
  select(wl, etnr, unique_id),
  select(wl_v, etnr, unique_id)
) %>% distinct()

# Проверяем, нет ли дубликатов unique_id
duplicates <- etnr_data %>% group_by(unique_id) %>% filter(n() > 1)
if (nrow(duplicates) > 0) {
  cat("Внимание, блять! Найдены дубликаты unique_id:\n")
  print(duplicates)
  cat("Нужно разобраться с этими дубликатами вручную!\n")
}

# Добавляем etnr в wl_adnan
wl_adnan <- wl_adnan %>%
  left_join(etnr_data, by = "unique_id")

# Проверяем, для всех ли строк нашелся etnr
missing_etnr <- wl_adnan %>% filter(is.na(etnr))
if (nrow(missing_etnr) > 0) {
  cat("Внимание, сука! Не удалось найти etnr для следующих пациентов:\n")
  print(select(missing_etnr, unique_id, sex, date_of_birth, blood_type))
}

# Выводим статистику
cat("\nСтатистика, нахуй:\n")
cat("Всего строк в wl_adnan:", nrow(wl_adnan), "\n")
cat("Строк с найденным etnr:", sum(!is.na(wl_adnan$etnr)), "\n")
cat("Строк без etnr:", sum(is.na(wl_adnan$etnr)), "\n")

# Сохраняем обновленный wl_adnan
saveRDS(wl_adnan, "wl_adnan_updated_with_etnr.rds")

cat("\nОбновленный wl_adnan сохранен, блять!\n")


# Razbor_duplikatov -------------------------------------------------------

# Получаем все данные для дубликатов
duplicates_full <- bind_rows(
  wl %>% filter(unique_id %in% duplicates$unique_id),
  wl_v %>% filter(unique_id %in% duplicates$unique_id)
) %>% 
  arrange(unique_id) %>%
  select(etnr, unique_id, sex, date_of_birth, blood_type, rh, everything())

print(duplicates_full, n = Inf)

# Применяем новую функцию
wl <- create_unique_id(wl)
wl_v <- create_unique_id(wl_v)
wl_adnan <- wl_adnan %>%
  mutate(
    unique_id = paste(sex, date_of_birth, blood_type, date_of_wl, sep = "_"),
    date_of_birth = as.Date(date_of_birth),
    date_of_wl = as.Date(date_of_wl)
  )

# Снова проверяем на дубликаты
etnr_data <- bind_rows(
  select(wl, etnr, unique_id),
  select(wl_v, etnr, unique_id)
) %>% distinct()

duplicates <- etnr_data %>% group_by(unique_id) %>% filter(n() > 1)
if (nrow(duplicates) > 0) {
  cat("Внимание, блять! Все еще есть дубликаты unique_id:\n")
  print(duplicates)
  cat("Нужно разобраться с этими дубликатами вручную!\n")
} else {
  cat("Заебись! Дубликатов больше нет!\n")
}

# Добавляем etnr в wl_adnan
wl_adnan <- wl_adnan %>%
  left_join(etnr_data, by = "unique_id")

# Проверяем, для всех ли строк нашелся etnr
missing_etnr <- wl_adnan %>% filter(is.na(etnr))
if (nrow(missing_etnr) > 0) {
  cat("Внимание, сука! Не удалось найти etnr для следующих пациентов:\n")
  print(select(missing_etnr, unique_id, sex, date_of_birth, blood_type, date_of_wl))
}

# Выводим статистику
cat("\nСтатистика, нахуй:\n")
cat("Всего строк в wl_adnan:", nrow(wl_adnan), "\n")
cat("Строк с найденным etnr:", sum(!is.na(wl_adnan$etnr)), "\n")
cat("Строк без etnr:", sum(is.na(wl_adnan$etnr)), "\n")

# Сохраняем обновленный wl_adnan
saveRDS(wl_adnan, "wl_adnan_updated_with_etnr.rds")

cat("\nОбновленный wl_adnan сохранен, блять!\n")


# Razborpoletov -----------------------------------------------------------


library(lubridate)

# Новая функция для исправления дат
fix_dates <- function(df) {
  df %>%
    mutate(
      date_of_birth = case_when(
        is.numeric(date_of_birth) ~ as.Date(as.numeric(date_of_birth), origin = "1970-01-01"),
        is.character(date_of_birth) ~ as.Date(date_of_birth),
        TRUE ~ date_of_birth
      ),
      date_of_wl = case_when(
        is.numeric(date_of_wl) ~ as.Date(as.numeric(date_of_wl), origin = "1970-01-01"),
        is.character(date_of_wl) ~ as.Date(date_of_wl),
        TRUE ~ date_of_wl
      )
    )
}

# Применяем функцию к нашим датафреймам
wl <- fix_dates(wl)
wl_v <- fix_dates(wl_v)
wl_adnan <- fix_dates(wl_adnan)

# Проверяем результаты
cat("Формат date_of_birth в wl:", class(wl$date_of_birth)[1], "\n")
cat("Формат date_of_wl в wl:", class(wl$date_of_wl)[1], "\n")
cat("Формат date_of_birth в wl_adnan:", class(wl_adnan$date_of_birth)[1], "\n")
cat("Формат date_of_wl в wl_adnan:", class(wl_adnan$date_of_wl)[1], "\n")

# Выводим несколько примеров дат для проверки
cat("\nПримеры date_of_birth в wl:\n")
print(head(wl$date_of_birth))
cat("\nПримеры date_of_wl в wl:\n")
print(head(wl$date_of_wl))
cat("\nПримеры date_of_birth в wl_adnan:\n")
print(head(wl_adnan$date_of_birth))
cat("\nПримеры date_of_wl в wl_adnan:\n")
print(head(wl_adnan$date_of_wl))

# Исправляем blood_type в wl_adnan
wl_adnan <- wl_adnan %>%
  mutate(blood_type = case_when(
    blood_type == 1 ~ "A",
    blood_type == 2 ~ "B",
    blood_type == 3 ~ "AB",
    blood_type == 4 ~ "0",
    TRUE ~ as.character(blood_type)
  ))

# Добавляем Rh фактор к blood_type в wl_adnan (предположим, что у всех Pos)
wl_adnan <- wl_adnan %>%
  mutate(blood_type = paste0(blood_type, "_Pos"))

# Пересоздаем unique_id
create_unique_id <- function(df) {
  df %>%
    mutate(
      unique_id = paste(sex, date_of_birth, blood_type, date_of_wl, sep = "_")
    )
}

wl <- create_unique_id(wl)
wl_v <- create_unique_id(wl_v)
wl_adnan <- create_unique_id(wl_adnan)

# Теперь снова пробуем найти соответствия
etnr_data <- bind_rows(
  select(wl, etnr, unique_id),
  select(wl_v, etnr, unique_id)
) %>% distinct()

wl_adnan <- wl_adnan %>%
  left_join(etnr_data, by = "unique_id")

# Проверяем результат
cat("\nСтрок с найденным etnr:", sum(!is.na(wl_adnan$etnr)), "\n")
cat("Строк без etnr:", sum(is.na(wl_adnan$etnr)), "\n")

# Выводим примеры совпадений и несовпадений
cat("\nПримеры строк с найденным etnr:\n")
print(head(wl_adnan %>% filter(!is.na(etnr)) %>% select(unique_id, etnr)))

cat("\nПримеры строк без etnr:\n")
print(head(wl_adnan %>% filter(is.na(etnr)) %>% select(unique_id)))

# Проверяем, есть ли теперь общие unique_id
common_ids <- intersect(wl_adnan$unique_id, c(wl$unique_id, wl_v$unique_id))
cat("\nКоличество общих unique_id:", length(common_ids), "\n")
if (length(common_ids) > 0) {
  cat("Примеры общих unique_id:\n")
  print(head(common_ids))
}
