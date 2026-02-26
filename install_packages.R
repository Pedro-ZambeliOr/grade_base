pkgs <- c(
  "httr",
  "jsonlite",
  "tidyverse",
  "openxlsx",
  "readxl",
  "reshape2",
  "urltools",
  "googlesheets4",
  "dotenv"
)

pkgs_faltando <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(pkgs_faltando) > 0) {
  message("Instalando pacotes faltando: ", paste(pkgs_faltando, collapse = ", "))
  install.packages(pkgs_faltando, dependencies = TRUE)
} else {
  message("Todos os pacotes já estão instalados!")
}

if (!"abjutils" %in% installed.packages()[, "Package"]) {
  if (!"remotes" %in% installed.packages()[, "Package"]) {
    install.packages("remotes")
  }
  message("Instalando abjutils do GitHub...")
  remotes::install_github("abjur/abjutils")
} else {
  message("abjutils já instalado!")
}

message("\n✅ Dependências prontas! Pode rodar o script principal.")
