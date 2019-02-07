library(data.table)
library(plotly)

# Data
dates <- c(as.Date('2017-01-02'), Sys.Date())

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

for (i in 1:nrow(carteira)){
  switch(carteira$Indexado[i],
         "CDI" = {
           rendimento <- (tail(carteira$`Aplicação`[i]*cumprod(1 + carteira$Rentabilidade[i]/100*as.numeric(CDI[data >= carteira$`Início`[i]]$valor)/100), n = 1) - carteira$`Aplicação`[i])
           
           }, 
         "IPCA" = {
           rendimento <- (tail(carteira$`Aplicação`[i]*cumprod(1 + (((1 + carteira$Rentabilidade[i]/100)^(1/12) - 1)*100 + as.numeric(IPCA[data >= format(as.Date(carteira$`Início`[i]), '%Y-%m-01')]$valor))/100), n = 1) 
                      - carteira$`Aplicação`[i])
         }, 
         "-" = {
           rendimento <- (tail(carteira$`Aplicação`[i]*cumprod(1 + (((1 + carteira$Rentabilidade[i]/100)^(1/12) - 1)*100)*rep(1, length(seq(from=as.Date(carteira$`Início`[i]), to=Sys.Date(), by='month')) - 1)/100), n = 1)
                           - carteira$`Aplicação`[i])
         },
         "Selic" = {
           rendimento <- (tail(carteira$`Aplicação`[i]*cumprod(1 + ((1 + carteira$Rentabilidade[i]/100)^(1/360) - 1) + as.numeric(selic[data >= carteira$`Início`[i]]$valor)/100), n = 1) 
                     - carteira$`Aplicação`[i])
         })
  
  carteira$Rendimento[i] <- round(rendimento, digits = 2)
  carteira$Imposto[i] <- round((tabela_regressiva(carteira$`Início`[i], Sys.Date())
                     + IOF(carteira$`Início`[i], Sys.Date()))*carteira$Rendimento[i],
                     digits = 2)

  carteira$Bruto[i] <- carteira$Rendimento[i]
  carteira$`Líquido`[i] <- carteira$Rendimento[i] - carteira$Imposto[i]
}
