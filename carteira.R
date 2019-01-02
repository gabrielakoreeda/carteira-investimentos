library(data.table)

# Date
dates <- c(as.Date('2017-01-02'), Sys.Date())

# Rate Taxes -------------------------------------------------------------------
# Selic 
url_selic <- paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.11/dados?formato=csv&dataInicial=',
                    format(datas[1], '%d/%m/%Y'), '&dataFinal=', format(datas[2], '%d/%m/%Y'))
selic <- fread(url_selic)
selic$data <- as.Date(selic$data, format = '%d/%m/%Y')

# IPCA
url_IPCA <- paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=csv&dataInicial=',
                    format(datas[1], '%d/%m/%Y'), '&dataFinal=', format(datas[2], '%d/%m/%Y'))
IPCA <- fread(url_IPCA)
IPCA$data <- as.Date(IPCA$data, format = '%d/%m/%Y')

# CDI
url_CDI <- paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.12/dados?formato=csv&dataInicial=',
                    format(datas[1], '%d/%m/%Y'), '&dataFinal=', format(datas[2], '%d/%m/%Y'))
CDI <- fread(url_CDI)
CDI$data <- as.Date(CDI$data, format = '%d/%m/%Y')

# Investments ------------------------------------------------------------------


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



