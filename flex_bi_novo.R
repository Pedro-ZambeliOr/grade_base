library(googlesheets4)
library(lubridate)
library(scales)
library(tidyverse)

#auth gs4

json_content <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
tmp_json <- tempfile(fileext = ".json")
writeLines(json_content, tmp_json)
gs4_auth(path = tmp_json)


BASE_BI_ID=Sys.getenv("BASE_BI_ID")
base_ligacoes=Sys.getenv("base_ligacoes")
base_novo_bi=Sys.getenv("base_novo_bi")


##

start_date <- '2025-01-01' %>% as.Date()
end_date <- as.Date(Sys.time())

calendario <- seq(from = start_date, to = end_date, by = 'day')
calendario <- data.frame(Day = calendario)

google <- read_sheet(ss = BASE_BI_ID,
           sheet = 'Google - Atualizado')

google <- google %>% rename(cost = `Cost (Spend)`) %>%
          group_by(Day) %>% 
          summarise(cost = sum(cost)) %>%
          mutate(canal_ads = "GoogleAds")

meta <- read_sheet(ss = BASE_BI_ID,
                  sheet = 'Meta')

meta <- meta %>% rename(cost = `Amount Spent`) %>%
        group_by(Day) %>%
        summarise(cost = sum(cost)) %>%
        mutate(canal_ads  = "MetaAds")

debate_raiz <- calendario %>%
          mutate(
                    cost = case_when(
                              day(Day) == 10 & Day < as.Date("2025-04-01") ~ 25000,
                              day(Day) == 10 & Day >= as.Date("2025-04-01") ~ 35000,
                              TRUE ~ 0
                    ),
                    canal_ads = "Debate_Raiz")

cost_table <- rbind(google, meta, debate_raiz)

#### ticket + clientes

calendario <- expand.grid(
          Day = calendario$Day,
          canal_ads = c("GoogleAds", "MetaAds", "Debate_Raiz"))

comercial <- read_sheet(ss = base_ligacoes,
                        sheet = 'Consolidado')

comercial <- comercial %>%
  mutate(`TOTAL EM HONORÁRIOS` = map_dbl(`TOTAL EM HONORÁRIOS`, 
                                         ~ ifelse(is.null(.x), NA, 
                                                  suppressWarnings(as.numeric(.x[1])))))

comercial <- comercial %>%
          mutate(
                    across(
                              where(is.numeric),
                              ~ replace_na(., 0)
                    )
          )


unique(comercial$`CAPTAÇÃO (CLOSER)`)

comercial <- comercial %>% rename(Day = FECHAMENTO) %>% 
          mutate(m_fechamento = month(Day),
                 y_fechamento = year(Day),
                 canal_ads = case_when(
                           str_detect(`CANAL DE CAPTAÇÃO`, regex("site|google", ignore_case = TRUE)) ~ "GoogleAds",
                           str_detect(`CANAL DE CAPTAÇÃO`, regex("instagram|facebook|meta|meta ads", ignore_case = TRUE)) ~ "MetaAds",
                           str_detect(`CANAL DE CAPTAÇÃO`, regex("debate|debate raiz", ignore_case = TRUE)) ~ "Debate_Raiz",
                           TRUE ~ 'N/A'),
                 
                 UF = case_when(UF == "RS" ~ "RS",
                                UF == 'SC' ~ 'SC',
                                UF == 'SP' ~ 'SP',
                                UF == 'PR' ~ 'PR',
                                TRUE ~ 'OUTROS'
                 ))


comercial <- comercial %>% group_by(Day, canal_ads) %>%
          summarise(total_clientes = n(),
                    total_valor = sum(`TOTAL EM HONORÁRIOS`),
                    .groups = "drop") %>%
          mutate(ticket_medio = total_valor/total_clientes) %>%
          filter(canal_ads != "N/A")

base <- calendario %>%
          left_join(cost_table, by = c("Day", "canal_ads")) %>%
          left_join(comercial, by = c("Day", "canal_ads")) %>%
          mutate(across(-c(Day, canal_ads), ~ replace_na(.x, 0)))
          
colnames(base)

range_write(ss = base_novo_bi,
            sheet = "Página1",
            data = base)  
