library(data.table)
library(plotly)
library(dplyr)
library(zoo)

# Data
dates <- c(as.Date('2016-10-01'), Sys.Date())

# Índices -------------------------------------------------------------------
# Selic 
url_selic <- paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.11/dados?formato=csv&dataInicial=',
                    format(dates[1], '%d/%m/%Y'), '&dataFinal=', format(dates[2], '%d/%m/%Y'))
selic <- fread(url_selic)
selic$data <- as.Date(selic$data, format = '%d/%m/%Y')
selic$valor <- as.numeric(gsub(",", ".", gsub("\\.", "", selic$valor)))

# IPCA
url_IPCA <- paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=csv&dataInicial=',
                    format(dates[1], '%d/%m/%Y'), '&dataFinal=', format(dates[2], '%d/%m/%Y'))
IPCA <- fread(url_IPCA)
IPCA$data <- as.Date(IPCA$data, format = '%d/%m/%Y')
IPCA$valor <- as.numeric(gsub(",", ".", gsub("\\.", "", IPCA$valor)))

# CDI
url_CDI <- paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.12/dados?formato=csv&dataInicial=',
                    format(dates[1], '%d/%m/%Y'), '&dataFinal=', format(dates[2], '%d/%m/%Y'))
CDI <- fread(url_CDI)
CDI$data <- as.Date(CDI$data, format = '%d/%m/%Y')
CDI$valor <- as.numeric(gsub(",", ".", gsub("\\.", "", CDI$valor)))

# Tributos ------------------------------------------------------------------------
# Tabela Regressiva
tabela_regressiva <- function(dateInit, dateFinal) {
  dias <- as.numeric(difftime(dateFinal, dateInit, units = 'days'))
  
  if (dias <= 180) {
    ir <- 0.225
  } else if (dias > 180 & dias <= 360) {
    ir <- 0.20
  } else if (dias > 360 & dias <= 720) {
    ir <- 0.175
  } else if (dias > 720) {
    ir <- 0.15
  }
  
  return(ir)
}

# IOF
IOF <- function(dateInit, dateFinal) {
  dias <- as.numeric(difftime(dateFinal, dateInit, units = 'days'))
  
  iof <- data.frame("Dias" = c(1:30), 
                    "IOF" = c(0.96, 0.93, 0.90, 0.86, 0.83, 0.80, 0.76, 0.73, 0.70, 0.66,
                              0.63, 0.60, 0.56, 0.53, 0.50, 0.46, 0.43, 0.40, 0.36, 0.33,
                              0.30, 0.26, 0.23, 0.20, 0.16, 0.13, 0.10, 0.06, 0.03, 0.00))
  
  iof <- iof$IOF[iof$Dias == dias]
  if (dias > 30) iof = 0
  return(iof)
}

# Ativos ------------------------------------------------------------------
carteira <- read.csv2("carteira.csv", stringsAsFactors = F, header = TRUE,
                      colClasses = c("character", "numeric", "numeric", 
                                     "character", "Date", "Date"),
                      dec = ".")

rendimento_tempo_total <- data.frame(Data=as.Date(numeric()),
                                           stringsAsFactors=FALSE) 

for (i in 1:nrow(carteira)){
  switch(carteira$Indexado[i],
         "CDI" = {
           rendimento_tempo <- data.frame("Data" = CDI[data >= carteira$`Início`[i]]$data,
             "CDI" = carteira$`Aplicação`[i]*cumprod(1 + carteira$Rentabilidade[i]/100*as.numeric(CDI[data >= carteira$`Início`[i]]$valor)/100),
             stringsAsFactors = F)
           rendimento <- (tail(rendimento_tempo$CDI, n = 1) - carteira$`Aplicação`[i])
           
           }, 
         "IPCA" = {
           rendimento_tempo <- data.frame("Data" = IPCA[data >= carteira$`Início`[i]]$data,
                                          "IPCA" = carteira$`Aplicação`[i]*cumprod(1 + (((1 + carteira$Rentabilidade[i]/100)^(1/12) - 1)*100 + as.numeric(IPCA[data >= carteira$`Início`[i]]$valor))/100),
                                          stringsAsFactors = F)
           rendimento <- (tail(rendimento_tempo$IPCA, n = 1) 
                          - carteira$`Aplicação`[i])
         }, 
         "-" = {
           rendimento_tempo <- data.frame("Data" = seq(from=as.Date(carteira$`Início`[i]), to=Sys.Date(), by='month'),
                                          "Prefix" = carteira$`Aplicação`[i]*cumprod(1 + (((1 + carteira$Rentabilidade[i]/100)^(1/12) - 1)*100)*rep(1, length(seq(from=carteira$`Início`[i], to=Sys.Date(), by='month')))/100),
                                          stringsAsFactors = F)
           rendimento <- (tail(rendimento_tempo$Prefix, n = 1)
                           - carteira$`Aplicação`[i])
         },
         "Selic" = {
           rendimento_tempo <- data.frame("Data" = selic[data >= carteira$`Início`[i]]$data,
                                          "Selic" = carteira$`Aplicação`[i]*cumprod(1 + ((1 + carteira$Rentabilidade[i]/100)^(1/252) - 1) + as.numeric(selic[data >= carteira$`Início`[i]]$valor)/100),
                                          stringsAsFactors = F)
           rendimento <- (tail(rendimento_tempo$Selic, n = 1) 
                     - carteira$`Aplicação`[i])
         })
  
  rendimento_tempo_total <- merge(rendimento_tempo_total, rendimento_tempo, by = 'Data', all = T) %>% 
    do(na.locf(.))
  rendimento_tempo_total$Data <- as.Date(rendimento_tempo_total$Data)
  
  carteira$Rendimento[i] <- round(rendimento, digits = 2)
  carteira$Imposto[i] <- round((tabela_regressiva(carteira$`Início`[i], Sys.Date())
                     + IOF(carteira$`Início`[i], Sys.Date()))*carteira$Rendimento[i],
                     digits = 2)
  
  carteira$`Bruto`[i] <- carteira$Aplicação[i] + carteira$Rendimento[i]
  carteira$`Líquido`[i] <- carteira$Aplicação[i] + carteira$Rendimento[i] - carteira$Imposto[i]
}

rendimento_tempo_total[c("Selic", "IPCA", "CDI", "Prefix")] <- as.numeric(unlist(rendimento_tempo_total[c("Selic", "IPCA", "CDI", "Prefix")]))

rendimento_tempo_total <- rendimento_tempo_total %>%
  mutate("Patrimônio" = rowSums(.[2:5], na.rm = TRUE)) %>%
  select(Data, `Patrimônio`)

# Visualizações ----------------------------------------------------------------
# Distribuição
d <- plot_ly(carteira, labels = ~Ativo, values = ~`Líquido`, type = 'pie', 
             showlegend = T) %>%
  layout(title = "Distribuição da Carteira - Patrimônio Líquido")

# Maior rendimento
r <- plot_ly(carteira, x = ~Ativo, 
        y = ~(Bruto - `Aplicação`)*100/(`Aplicação`)/(as.numeric(difftime(Sys.Date(), `Início`, units = 'days'))),
        type = 'bar') %>%
  layout(title = "Rendimento Proporcional", yaxis = list(title = "Rendimento (%) / dias"))

# Rendimento ao longo do tempo
aporte <- carteira[c("Início", "Aplicação")] %>%
  arrange(`Início`) %>%
  mutate("Aporte" = cumsum(`Aplicação`)) %>%
  select("Início", "Aporte")
aporte <- rbind(aporte, data.frame("Início" = tail(rendimento_tempo_total$Data, n = 1),
                                   "Aporte" = tail(aporte$Aporte, n = 1)))

t <- plot_ly(rendimento_tempo_total, x = ~Data, y = ~`Patrimônio`, 
             type = 'scatter', mode = 'lines', name = "Patrimônio") %>%
  add_lines(x = aporte$`Início`, y = aporte$`Aporte`, 
            line = list(shape = "hv"), name = "Aporte") %>%
  layout(title = "Patrimõnio Líquido",
         xaxis = list(title = ""),
         yaxis = list(title = "R$"))



