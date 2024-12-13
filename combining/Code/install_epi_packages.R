# Функция для красивого вывода статуса
print_status <- function(msg) {
  cat(paste0("\n🔄 ", msg, "...\n"))
}

# Ставим базовые пакеты для управления
if (!require("pacman")) install.packages("pacman")
if (!require("remotes")) install.packages("remotes")

print_status("Устанавливаем и загружаем основные пакеты")
pacman::p_load(
  # Базовые инструменты
  tidyverse, lubridate, here, rio, openxlsx,
  
  # Статистика
  janitor, gtsummary, rstatix, broom, lmtest, easystats,
  
  # Эпидемиология
  epicontacts, EpiNow2, EpiEstim, projections, incidence2, 
  i2extras, epitrix, distcrete,
  
  # Визуализация
  cowplot, RColorBrewer, ggnewscale, ggcorrplot, DiagrammeR, 
  gghighlight, ggrepel, plotly, gganimate,
  
  # ГИС
  sf, tmap, OpenStreetMap, spdep,
  
  # Отчеты
  rmarkdown, reportfactory, officer, flexdashboard, 
  shiny, knitr, flextable,
  
  # Анализ
  mice, survival, survminer, corrplot, splines,
  car, glmnet, randomForestSRC,
  
  # Менеджеры
  BiocManager, remotes
)

print_status("Устанавливаем пакеты из Биокондуктора")
BiocManager::install(c("treeio", "ggtree", "ape"), 
                     update = FALSE, 
                     ask = FALSE)

print_status("Устанавливаем пакеты с GitHub")
remotes::install_github(
  c(
    "YuLab-SMU/treeio",
    "ropensci/aorsf"
  ),
  upgrade = "never",
  quiet = TRUE
)

print_status("Проверяем установленные пакеты")
installed_pkgs <- pacman::p_loaded()
cat("\n✅ Установлено пакетов:", length(installed_pkgs), "\n")

# Функция для проверки конкретных пакетов
check_packages <- function(pkgs) {
  missing <- pkgs[!pkgs %in% installed_pkgs]
  if (length(missing) > 0) {
    cat("\n⚠️ Не установлены:\n")
    print(missing)
  } else {
    cat("\n✅ Все критические пакеты установлены!\n")
  }
}

# Проверяем критически важные пакеты
critical_packages <- c("tidyverse", "aorsf", "treeio", "ggtree")
check_packages(critical_packages)
