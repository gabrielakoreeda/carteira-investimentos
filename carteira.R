library(data.table)

# Date
dates <- c(as.Date('2017-01-02'), Sys.Date())

# Rate Taxes -------------------------------------------------------------------
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


# Investments ------------------------------------------------------------------
CDB <- data.frame('CDB' = c('CDB Pan - AGO/2020', 'CDB Pan - SET/2020'),
                  'Aplicação' = c(4000, 1000), 
                  'Rentabilidade (%)' = c(117.50, 117.50), 
                  'Início' = c('2017-08-28', '2017-09-21'), 
                  'Vencimento' = c('2020-08-28', '2020-09-21'),
                  stringsAsFactors = F)

for (i in 1:nrow(CDB)) {
  rendimento_CDB <- tail(CDB$`Aplicação`[i]*cumprod(1 + CDB$Rentabilidade[i]/100*as.numeric(CDI[data >= CDB$`Início`[i]]$valor)/100), n = 1)
  CDB$Rendimento[i] <- rendimento_CDB
}

# Taxes ------------------------------------------------------------------------
# Tabela Regressiva
tabela_regressiva <- function(dateInit, dateFinal) {
  dias <- as.numeric(difftime(dateFinal, dateInit, units = 'days'))
  
  if (dias < 360) {
    ir <- 0.2
  } else if (dias < 720 & dias >= 360) {
    ir <- 0.175
  } else if (dias >= 720) {
    ir <- 0.15
  }
  
  return(ir)
}

# IOF
IOF <- function(dateInit, dateFinal) {
  dias <- as.numeric(difftime(dateFinal, dateInit, units = 'days'))
  
  if (dias < 30){
    return(0.0638)
  }
}



