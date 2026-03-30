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

start_date = "2025-09-01" %>% as.Date()

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


######### bases gastos

######### ENVIAR OS DADOS

json_content <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
tmp_json <- tempfile(fileext = ".json")
writeLines(json_content, tmp_json)
gs4_auth(path = tmp_json)

BASE_BI_ID=Sys.getenv("BASE_BI_ID")

########

start_date = "2025-09-01" %>% as.Date()
end_date <- as.Date(Sys.time())

calendario <- seq(from = start_date, to = end_date, by = 'day')
calendario <- data.frame(Day = calendario)

calendario <- expand.grid(
  Day = calendario$Day,
  canal_ads = c("GoogleAds", "MetaAds"))

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

cost_table <- rbind(google, meta)


lista_meta <- c(
  "Busca Paga | Facebook Ads",
  "Busca Paga | Facebook",
  "Social | ig",
  "Social | Instagram",
  "Social | Facebook",
  "Referência | wl.co",
  "Busca Paga | Instagram Ads",
  "Referência | linktr.ee"
)

lista_google <- c(
  "Busca Paga | google",
  "Busca Orgânica | Google",
  "Tráfego Direto",
  "Busca Paga | Google",
  "Busca Orgânica | Bing",
  "Social | YouTube",
  "Busca Paga | GoogleAds",
  "Busca Orgânica | Yahoo",
  "Referência | grupoflex.com.br",
  "Referência | rdstation.com.br",
  "Google e Outros Buscadores",
  "Outros | conv"
)

fechados = data_completa %>% 
  filter(deals.win == TRUE) %>%
  mutate(deals.closed_at = as.Date(deals.closed_at),
         canal_ads = case_when(deals.deal_source %in% lista_meta ~ "MetaAds",
                     deals.deal_source %in% lista_google ~ "GoogleAds",
         TRUE ~ "N/A")) %>%
  rename(Day = deals.closed_at) 

##com utm source
fechados_utm = data_completa %>% 
  filter(deals.win == TRUE) %>%
  mutate(deals.closed_at = as.Date(deals.closed_at),
         canal_ads = case_when(utm_source %in% c("Facebook", "ig", 'fb') ~ "MetaAds",
                               utm_source %in% c("google") ~ "GoogleAds",
                               TRUE ~ "N/A")) %>%
  rename(Day = deals.closed_at) 


fechados = fechados %>%
  group_by(Day, canal_ads)  %>%
  summarise(total_clientes = n(),
            total_valor = sum(deals.amount_total),
            .groups = "drop") %>%
  mutate(ticket_medio = total_valor/total_clientes) %>%
  filter(canal_ads != "N/A")

fechados_utm = fechados_utm %>%
  group_by(Day, canal_ads)  %>%
  summarise(total_clientes = n(),
            total_valor = sum(deals.amount_total),
            .groups = "drop") %>%
  mutate(ticket_medio = total_valor/total_clientes) %>%
  filter(canal_ads != "N/A")

colnames(cost_table)
colnames(fechados)

base <- calendario %>%
  left_join(cost_table, by = c("Day", "canal_ads")) %>%
  left_join(fechados, by = c("Day", "canal_ads")) %>%
  mutate(across(-c(Day, canal_ads), ~ replace_na(.x, 0)))


base_com_utm <- calendario %>%
  left_join(cost_table, by = c("Day", "canal_ads")) %>%
  left_join(fechados_utm, by = c("Day", "canal_ads")) %>%
  mutate(across(-c(Day, canal_ads), ~ replace_na(.x, 0)))


base_novo_bi = Sys.getenv("base_novo_bi")

range_write(ss = base_novo_bi,
            sheet = "Página3",
            data = base)  

range_write(ss = base_novo_bi,
            sheet = "novo_com_utm",
            data = base_com_utm)  
