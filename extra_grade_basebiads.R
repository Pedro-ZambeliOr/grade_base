
safe_rbind <- function(df1, df2) {
          tryCatch({
                    # Tentar fazer o rbind
                    result <- rbind(df1, as.data.frame(df2))
                    return(result)
          }, error = function(e) {
                    # Se houver erro, ajustar as colunas e tentar rbind novamente
                    warning("rbind falhou, ajustando colunas e tentando novamente")
                    
                    # Encontrar colunas que estão em df1, mas não em df2
                    cols_df1_not_in_df2 <- setdiff(names(df1), names(df2))
                    # Encontrar colunas que estão em df2, mas não em df1
                    cols_df2_not_in_df1 <- setdiff(names(df2), names(df1))
                    
                    # Adicionar colunas ausentes no df2
                    for (col in cols_df1_not_in_df2) {
                              df2[[col]] <- NA
                    }
                    
                    # Adicionar colunas ausentes no df1
                    for (col in cols_df2_not_in_df1) {
                              df1[[col]] <- NA
                    }
                    
                    # Reordenar as colunas para que ambos dataframes tenham a mesma ordem
                    df2 <- df2[names(df1)]
                    
                    # Tentar fazer o rbind novamente
                    result <- rbind(df1, df2)
                    return(result)
          })
}

library(httr)
library(jsonlite)
library(tidyverse)
library(openxlsx)
library(readxl)
library(reshape2)
library(abjutils)
library(urltools)
library(dotenv)
library(googlesheets4)


if (file.exists(".env")) {
  load_dot_env(file = ".env")
}

API_CRM = Sys.getenv("API_CRM")
deal_pipeline_id=Sys.getenv("deal_pipeline_id")

url <- "https://crm.rdstation.com/api/v1/deals"
has_more = TRUE
page = 1

ids = c()

start_date = "2024-12-31" %>%  as.Date()
end_date = start_date + days(30)

data_completa = data.frame()


while (end_date <= (today() + days(31))) {
          while (has_more) {
                    queryString <- list(
                              token = API_CRM,
                              page = page,
                              limit = "200",
                              deal_pipeline_id = deal_pipeline_id,
                              created_at_period = "true",
                              start_date = start_date,
                              end_date = end_date
                    )
                    
                    response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/json"))
                    sample = content(response, "text")
                    sample_data <- fromJSON(sample)
                    if(sample_data$total > 10000) {
                              end_date = start_date + days(1)
                              queryString <- list(
                                        token = API_CRM,
                                        page = page,
                                        limit = "200",
                                        deal_pipeline_id = deal_pipeline_id,
                                        created_at_period = "true",
                                        start_date = start_date,
                                        end_date = end_date
                                        
                              )
                              
                              response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/json"))
                              sample = content(response, "text")
                              sample_data <- fromJSON(sample)
                    }
                    
                    
                    has_more = sample_data$has_more
                    if(sample_data$total == 0){
                              print(paste(start_date, " Breake with 0 deals"))
                              next
                    } 
                    data = as.data.frame(sample_data)
                    
                    ids = c(ids, data$deals.id)
                    
                    #expandir
                    
                    products = sample_data$deals$deal_products
                    custom_fields = sample_data$deals$deal_custom_fields
                    organization = sample_data$deals
                    
                    
                    aux1 = matrix(NA, nrow = 1, ncol = 3)
                    prods = data.frame(aux1)
                    colnames(prods) = c("name", "price", "total")
                    
                    aux1 = matrix(NA, nrow = 1, ncol = 5)
                    custom_f = data.frame(aux1)
                    colnames(custom_f) =  c("utm_campaign", "utm_source" ,  "utm_medium" ,  "utm_term", "utm_content")
                    
                    ##pedro: nesse bloco abaixo, pq percorrendo linha a linha?
                    
                    
                    for (i in 1:nrow(data)) {
                              lista_customf = as.data.frame(custom_fields[[i]])
                              lista_prod = as.data.frame(products[[i]])
                              if(nrow(lista_customf) > 1){
                                        label = lista_customf$custom_field$label
                                        aux = lista_customf %>% select(value)
                                        aux = t(aux)
                                        colnames(aux) = label
                              } else{
                                        aux = matrix(NA, nrow = 1, ncol = 5)
                                        aux = as.data.frame(aux)
                                        colnames(aux) = c("utm_campaign", "utm_source" ,  "utm_medium" ,  "utm_term", "utm_content")
                              }
                              
                              if(nrow(lista_prod) > 0){
                                        lista_prod = lista_prod %>%  select(all_of(colnames(prods)))
                                        if(nrow(lista_prod) >1){
                                                  
                                                  lista_prod$total[1] = sum(lista_prod$total)
                                                  lista_prod = lista_prod[1,]
                                        }
                              }else{
                                        aux1 = matrix(NA, nrow = 1, ncol = 3)
                                        lista_prod = data.frame(aux1)
                                        colnames(lista_prod) = c("name", "price", "total")
                              }
                              
                              aux = aux %>%  as.data.frame()
                              aux[, setdiff(names(custom_f), names(aux))] <- NA
                              
                              # Reordenando as colunas de df2 para que fiquem na mesma ordem que df1
                              # aux <- aux[, names(custom_f)]
                              
                              # custom_f[i,] = aux
                              custom_f <- safe_rbind(custom_f, aux)
                              # prods[i,] = lista_prod
                              prods = rbind(prods, lista_prod)
                              
                    }
                    colnames(prods) = c("name", "price", "total_prod")
                    data = cbind(data, custom_f[-1,], prods[-1,])
                    
                    has_more = tail(data$has_more,1)
                    
                    if(is.null(has_more)){
                              has_more = FALSE
                    }
                    
                    if(nrow(data_completa) == 0){
                              data_completa = data
                    } else{
                              # data = data %>%  select(all_of(colnames(data_completa)))
                              if(ncol(data) < ncol(data_completa)){
                                        data_completa = full_join(data_completa, data)
                              } else{
                                        data_completa = full_join(data, data_completa)
                              }
                    }
                    page = page + 1
          }
          start_date = end_date +days(1)
          end_date = start_date + days(30)
          page = 1
          has_more = T
}


data_completa$deals.user = data_completa$deals.user$name
data_completa$deals.deal_stage = data_completa$deals.deal_stage$name
data_completa$deals.deal_source = data_completa$deals.deal_source$name
data_completa$deals.campaign = data_completa$deals.campaign$name

data_completa$deals.deal_lost_reason = data_completa$deals.deal_lost_reason$name


data_completa = data_completa %>%  select(-deals.stop_time_limit, -deals.contacts, - deals.deal_custom_fields, -deals.deal_products,
                                          -deals.organization, - deals.next_task)

print("################################## COMPLETO RD #################################")

Sys.sleep(10)


######### ENVIAR OS DADOS

json_content <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
tmp_json <- tempfile(fileext = ".json")
writeLines(json_content, tmp_json)
gs4_auth(path = tmp_json)


BASE_BI_ID=Sys.getenv("BASE_BI_ID")

chunk_size <- 2500
n <- nrow(data_completa)
starts <- seq(1, n, by = chunk_size)
ends <- pmin(starts + chunk_size - 1, n)

for (i in seq_along(starts)) {
          cat(paste0(" Enviando linhas ", starts[i], " a ", ends[i], "...\n"))
          chunk <- data_completa[starts[i]:ends[i], ]
          write_headers <- (i == 1)
          range_write(
                    ss = BASE_BI_ID,
                    sheet = 'RD',
                    reformat = FALSE,
                    data = chunk,
                    range = if (write_headers) "A1" else paste0("A", starts[i] + 1),
                    col_names = write_headers
          )
}


############# Parte 2 

#################################

#### montar ano completo


library(lubridate)

começa_date = "2024-12-31" %>% as.Date()
todia <- today()

dias_painel_sdrs <- seq(from = começa_date, to = todia, by = 'day')

painel_sdrs <- data.frame(dia = dias_painel_sdrs)


hot_leads <- c('Elaborar Cálculo', 'Cálculo Feito', 
               'Ligação Agendada', 'Em Negociação', 
               'Follow-up', 'Contrato na Rua')


unique(data_completa$deals.deal_source)

painel_base <- data_completa %>% 
          mutate(deals.user = recode(deals.user, "Lucas Lopes" = "Lucas Parodes"),
                 deals.user = recode(deals.user, "Eduardo  G." = "Eduardo Gomes"),
                 dia_ajustado = as_date(deals.created_at),
                 dia_update_ajustado = as_date(deals.updated_at),
                 lead_type = ifelse(deals.deal_stage %in% hot_leads, "lead quente", "lead fria"),
                 canalads = case_when(
                           str_detect(deals.deal_source, regex("google|googleads|google ads", ignore_case = TRUE)) ~ "GoogleAds",
                           str_detect(deals.deal_source, regex("meta|facebook|facebookads|instagram|insta|metaads|ig", ignore_case = TRUE)) ~ "MetaAds",
                           TRUE ~ "Outros"
                 )
          )

painel_sdrs_rd <- painel_base %>%
          group_by(dia_ajustado, canalads, deals.user) %>%
          summarise(leads_quentes = sum(lead_type == 'lead quente'),
                    leads_frias = sum(lead_type == 'lead fria')) %>%
          mutate(juntador = paste0(deals.user, canalads, dia_ajustado))


painel_closers_rd <- painel_base %>%
          group_by(dia_update_ajustado, canalads, deals.user) %>%
          summarise(leads_quentes = sum(lead_type == 'lead quente'),
                    leads_frias = sum(lead_type == 'lead fria')) %>%
          mutate(juntador = paste0(deals.user, canalads, dia_update_ajustado))


######planilha ligs + fechados

base_ligacoes=Sys.getenv("base_ligacoes")


base_ligacoes <- read_sheet(ss = base_ligacoes,
                            sheet = 'Acompanhamento ligações',
                            col_types = "?????????????????c???")


unique(base_ligacoes$ORIGEM)

base_ligs <- base_ligacoes %>%
          mutate(`RESP. AGEND.` = case_when(
                    `RESP. AGEND.` == "DUDU" ~ "Eduardo Gomes",
                    `RESP. AGEND.` == 'GUILHERME' ~ 'Guilherme Noroefé',
                    `RESP. AGEND.` == "FELIPE" ~ "Felipe Mendes",
                    `RESP. AGEND.` == "PIG" ~ "Pedro Henrique",
                    `RESP. AGEND.` == "STEF" ~ "Stefany Xavier",
                    `RESP. AGEND.` == "REBECCA" ~ "Rebecca Tolfo",
                    `RESP. AGEND.` == "GABRIELA" ~ "Gabriela Mattos",
                    `RESP. AGEND.` == "DANIEL" ~ "Daniel Benjoya",
                    `RESP. AGEND.` == "HÉRICA" ~ "Hérica  Cristina",
                    `RESP. AGEND.` == "CRIS" ~ "Cristiane Nascente",
                    `RESP. AGEND.` == "VITOR" ~ "Vitor Nunes",
                    TRUE ~ `RESP. AGEND.`
          ),
          canalads = case_when(
                    str_detect(ORIGEM, regex("META", ignore_case = TRUE)) ~ "MetaAds",
                    str_detect(ORIGEM, regex("SITE", ignore_case = TRUE)) ~ "GoogleAds",
                    TRUE ~ "Outros"
          )) %>% group_by(`RESP. AGEND.`, canalads, DATA) %>%
          rename(dia_ajustado = DATA,
                 deals.user = `RESP. AGEND.`) %>% 
          summarise(ligacoes_agendadas = n(),
                    fechadas = sum(`FECHOU?` == 'SIM')) %>% 
          mutate(juntador = paste0(deals.user, canalads, dia_ajustado))



full_basis <- full_join(painel_sdrs_rd, base_ligs, by = "juntador") %>%
          mutate(
                    across(
                              ends_with(".x"),
                              ~ {
                                        y_col <- get(sub("\\.x$", ".y", cur_column()))
                                        
                                        # Lógica adaptada pra não estragar colunas de data
                                        if (inherits(., "Date") || inherits(., "POSIXt")) {
                                                  # se .x for NA, usa .y
                                                  dplyr::coalesce(., y_col)
                                        } else {
                                                  # caso geral (texto, número etc.)
                                                  case_when(
                                                            (is.na(.) | . == "") & !(is.na(y_col) | y_col == "") ~ y_col,
                                                            TRUE ~ .
                                                  )
                                        }
                              }
                    )
          ) %>%
          select(-ends_with(".y")) %>%
          rename_with(~ sub("\\.x$", "", .x)) %>% select(-juntador)


lista_sdrs_devdd <- c(
          "Vitor Nunes",
          "Cristiane Nascente",
          "Hérica  Cristina",
          "Guilherme Noroefé",
          "Pedro Henrique",
          "Stefany Xavier"
)

painel_final_sdrs <- left_join(painel_sdrs, full_basis, by = c('dia' = 'dia_ajustado')) %>%
          filter(deals.user %in% lista_sdrs_devdd)

painel_final_sdrs <- painel_final_sdrs %>% 
          mutate(leads_frias = ifelse(is.na(leads_frias),0,leads_frias),
                 leads_quentes = ifelse(is.na(leads_quentes),0,leads_quentes),
                 ligacoes_agendadas = ifelse(is.na(ligacoes_agendadas),0,ligacoes_agendadas),
                 fechadas = ifelse(is.na(fechadas),0,fechadas))


write_sheet(ss = BASE_BI_ID,
            sheet = 'CONSOLIDADO SDRS V2',
            data = painel_final_sdrs)


####closers

base_ligs_cl <- base_ligacoes %>%
          mutate(RESPONSÁVEL = case_when(
                    RESPONSÁVEL == "DUDU" ~ "Eduardo Gomes",
                    RESPONSÁVEL == "DANIEL" ~ "Daniel Benjoya",
                    RESPONSÁVEL == "PARODES" ~ "Lucas Parodes",
                    RESPONSÁVEL == "ANDRÉ" ~ "André Viegas",
                    RESPONSÁVEL == "RONALDO" ~ "Ronaldo Traçante",
                    RESPONSÁVEL == "MASSA" ~ "Vinícius Gayer",
                    TRUE ~ RESPONSÁVEL
          ),
          canalads = case_when(
                    str_detect(ORIGEM, regex("META", ignore_case = TRUE)) ~ "MetaAds",
                    str_detect(ORIGEM, regex("SITE", ignore_case = TRUE)) ~ "GoogleAds",
                    TRUE ~ "Outros"
          )) %>% group_by(RESPONSÁVEL, canalads, DATA) %>%
          rename(dia_ajustado = DATA,
                 deals.user = RESPONSÁVEL) %>% 
          summarise(ligacoes_agendadas = n(),
                    fechadas = sum(`FECHOU?` == 'SIM')) %>% 
          mutate(juntador = paste0(deals.user, canalads, dia_ajustado))


full_basis_cl <- full_join(painel_closers_rd, base_ligs_cl, by = "juntador") %>%
          mutate(
                    across(
                              ends_with(".x"),
                              ~ {
                                        y_col <- get(sub("\\.x$", ".y", cur_column()))
                                        
                                        # Lógica adaptada pra não estragar colunas de data
                                        if (inherits(., "Date") || inherits(., "POSIXt")) {
                                                  # se .x for NA, usa .y
                                                  dplyr::coalesce(., y_col)
                                        } else {
                                                  # caso geral (texto, número etc.)
                                                  case_when(
                                                            (is.na(.) | . == "") & !(is.na(y_col) | y_col == "") ~ y_col,
                                                            TRUE ~ .
                                                  )
                                        }
                              }
                    )
          ) %>%
          select(-ends_with(".y")) %>%
          rename_with(~ sub("\\.x$", "", .x)) %>% select(-juntador, -dia_ajustado)


unique(data_completa$deals.user)

lista_closers_devdd <- c(
          "Eduardo Gomes",
          "Daniel Benjoya",
          "Lucas Parodes",
          "André Viegas",
          "Ronaldo Traçante",
          "Vinícius Gayer"
)

painel_final_closers <- left_join(painel_sdrs, full_basis_cl, by = c('dia' = 'dia_update_ajustado')) %>%
          filter(deals.user %in% lista_closers_devdd)

painel_final_closers <- painel_final_closers %>% 
          mutate(leads_frias = ifelse(is.na(leads_frias),0,leads_frias),
                 leads_quentes = ifelse(is.na(leads_quentes),0,leads_quentes),
                 ligacoes_agendadas = ifelse(is.na(ligacoes_agendadas),0,ligacoes_agendadas),
                 fechadas = ifelse(is.na(fechadas),0,fechadas))

write_sheet(ss = BASE_BI_ID,
            sheet = 'CONSOLIDADO CLOSERS',
            data = painel_final_closers)