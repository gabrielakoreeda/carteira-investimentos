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
  } else {
    return(0)
  }
}

# Investments ------------------------------------------------------------------
# CDB
CDB <- data.frame('CDB' = c('CDB Pan - AGO/2020', 'CDB Pan - SET/2020'),
                  'Aplicação' = c(4000, 1000), 
                  'Rentabilidade' = c(117.50, 117.50), 
                  'Início' = c('2017-08-28', '2017-09-21'), 
                  'Vencimento' = c('2020-08-28', '2020-09-21'),
                  stringsAsFactors = F)

for (i in 1:nrow(CDB)) {
  rendimento_CDB <- (tail(CDB$`Aplicação`[i]*cumprod(1 + CDB$Rentabilidade[i]/100*as.numeric(CDI[data >= CDB$`Início`[i]]$valor)/100), n = 1) 
                     - CDB$`Aplicação`[i])
  CDB$Rendimento[i] <- round(rendimento_CDB, digits = 2)
  
  CDB$Imposto[i] <- round((tabela_regressiva(as.Date(CDB$`Início`[i]), Sys.Date()) 
                     + IOF(as.Date(CDB$`Início`[i]), Sys.Date()))*CDB$Rendimento[i],
                     digits = 2)
  
  CDB$`Líquido`[i] <- CDB$Rendimento[i] - CDB$Imposto[i]
}

# Tesouro Direto
tesouro <- data.frame('Tipo' = c('IPCA+', 'IPCA+', 'IPCA+', 'IPCA+', 'IPCA+', 'IPCA+', 'IPCA+',
                                 'Prefixado', 'Selic+', 'Selic+'),
                      'Aplicação' = c(499.88, 43.45, 78.85, 147.70, 190.18, 247.56, 741.98,
                                      63.04, 4369.33, 965.52),
                      'Rentabilidade' = c(5.72, 5.64, 5.69, 5.78, 5.50, 5.21, 5.14, 
                                          10.65, 0.04, 0.06),
                      'Início' = c('2018-07-10', '2018-08-06', '2018-08-14', 
                                   '2018-09-12', '2018-10-09', '2018-11-06', '2018-12-05',
                                   '2017-06-23', '2016-10-27', '2017-05-15'),
                      'Vencimento' = c('2045-05-15', '2045-05-15', '2045-05-15', 
                                       '2045-05-15', '2045-05-15', '2045-05-15', '2045-05-15',
                                       '2023-01-01', '2021-03-01', '2023-03-01'),
                      stringsAsFactors = F)

for (i in 1:nrow(tesouro)) {
  if (tesouro$Tipo[i] == 'IPCA+'){
    rendimento_tesouro <- (tail(tesouro$`Aplicação`[i]*cumprod(1 + (((1 + tesouro$Rentabilidade[1]/100)^(1/12) - 1)*100 + as.numeric(IPCA[data >= format(as.Date(tesouro$`Início`[i]), '%Y-%m-01')]$valor))/100), n = 1) 
                      - tesouro$`Aplicação`[i])
  } else if (tesouro$Tipo[i] == 'Prefixado') {
    rendimento_tesouro <- (tail(tesouro$`Aplicação`[i]*cumprod(1 + (((1 + tesouro$Rentabilidade[1]/100)^(1/12) - 1)*100)*rep(1, length(seq(from=as.Date(tesouro$`Início`[i]), to=Sys.Date(), by='month')) - 1)/100), n = 1)
                           - tesouro$`Aplicação`[i])
  } else if (tesouro$Tipo[i] == 'Selic+') {
    rendimento_tesouro <- (tail(tesouro$`Aplicação`[i]*cumprod(1 + ((1 + tesouro$Rentabilidade[1]/100)^(1/360) - 1) + as.numeric(selic[data >= tesouro$`Início`[i]]$valor)/100), n = 1) 
                     - tesouro$`Aplicação`[i])
  }
  
  if (length(rendimento_tesouro) == 0) rendimento_tesouro <- 0
  tesouro$Rendimento[i] <- round(rendimento_tesouro, digits = 2)
  
  tesouro$Imposto[i] <- round((tabela_regressiva(as.Date(tesouro$`Início`[i]), Sys.Date()) 
                     + IOF(as.Date(tesouro$`Início`[i]), Sys.Date()))*tesouro$Rendimento[i],
                     digits = 2)
  
  tesouro$`Líquido`[i] <- tesouro$Rendimento[i] - tesouro$Imposto[i]
}






