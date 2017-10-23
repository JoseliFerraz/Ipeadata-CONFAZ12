######################################################################
######################################################################
####### ------------------------------------------------------ #######
####### |   INSTITUTO DE PESQUISA ECONOMICA APLICADA - Ipea  | #######
####### |                 PROJETO IPEADATA                   | #######
####### |         COORDENADOR: ERIVELTON PIRES GUEDES        | #######
####### |       PROGRAMADOR: LUIZ EDUARDO DA SILVA GOMES     | #######
####### |  ROTINA R PARA OBTENCAO DO CONFAZ12 VIA WEBSCRAP.  | #######
####### ------------------------------------------------------ #######
######################################################################
######################################################################

#----------------------------------------------------------------------------------------------
# IMPORTANDO PACOTES NECESSARIOS
#----------------------------------------------------------------------------------------------

pacotes<-c("forecast","DBI","RCurl","XML","RPostgreSQL","httr","xml2","rvest","dplyr","RCurl")
for (i in 1:length(pacotes))
{
  if (length(names(installed.packages()[,1])[names(installed.packages()[,1])==pacotes[i]])==0){install.packages(pacotes[i], repos="http://cran.fiocruz.br/")}
  library(pacotes[i],character.only = TRUE) 
}
rm(pacotes,i)

#----------------------------------------------------------------------------------------------
# AJUSTES DOS PARAMETROS
#----------------------------------------------------------------------------------------------

#------ Configuracao de casas decimais ------#
options(scipen=999)

######################################################################
######################################################################
####### APAGAAAAAARRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR #######
######################################################################
######################################################################

############ COLOCAR SENHA AQUI
con <- NULL
######################################################################
######################################################################
####### APAGAAAAAARRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR #######
######################################################################
######################################################################

#----------------------------------------------------------------------------------------------
# LISTA DE PARAMETROS - SITE: https://www.confaz.fazenda.gov.br/legislacao/boletim-do-icms
#----------------------------------------------------------------------------------------------

#-----------------------------------------------------#
#------ NA PÁGINA HÁ 3 CAMPOS DE PREENCHIMENTO: ------#
#-----------------------------------------------------#
#   - ANO
#     De 1996 até presente ('1996, ..., 201X')
#   
#   - TIPO
#     Valores correntes ('valores_correntes')
#     Variação em relação ao mês anterior ('variacao_relacao_mes_anterior')
#     Participação relativa ('participacao_relativa')
#     
#   - CONTA (TIPO = Valores correntes)
#     Arrecadação do ICMS ('icms_total')
#     Arrecadação do IPVA ('outros_tributos_ipva')
#     Arrecadação do ITCD ('outros_tributos_itcd')
#     Arrecadação do TAXAS ('outros_tributos_taxas')
#     Arrecadação de OUTROS ('outros_tributos_outros')
#     Arrecadação de Outros Tributos ('outros_tributos_total')
#     ICMS - Setor Primário ('icms_primario')
#     ICMS - Setor Secundário ('icms_secundario')
#     ICMS - Setor Terciário ('icms_terciario')
#     ICMS - Setor Terciário - Com. Atacadista ('icms_terciario_atacadista')
#     ICMS - Setor Terciário - Com. Varejista ('icms_terciario_varejista')
#     ICMS - Setor Terciário - Serv. Transportes ('icms_terciario_transporte')
#     ICMS - Setor Terciário - Serv. Comunicação ('icms_terciario_comunicacao')
#     ICMS - Setor Terciário - Outros ('icms_terciario_outros')
#     ICMS - Energia Elétrica ('icms_energia')
#     ICMS - Energia Elétrica - Setor Secundário ('icms_energia_secundario')
#     ICMS - Energia Elétrica - Setor Terciário ('icms_energia_terciario')
#     ICMS - Petróleo, comb. e Lub. ('icms_combustiveis')
#     ICMS - Petróleo, comb. e Lub. - Setor Secundário ('icms_combustiveis_secundario')
#     ICMS - Petróleo, comb. e Lub. - Setor Terciário ('icms_combustiveis_terciario')
#     ICMS - Divida Ativa ('icms_divida_ativa')
#     ICMS - Outras Fontes de Receita ('icms_outros')
##############################################################################################

#----------------------------------------------------------------------------------------------
# WEBSCRAPPING - CONFAZ12
#----------------------------------------------------------------------------------------------

#-----------------------#
#------ METADADOS ------#
#-----------------------#

metadados <- expand.grid(Ano = 1996:(as.POSIXlt(Sys.Date())$year+1900),
                         Tipo = c("valores_correntes"),
                         Conta = c("icms_total",
                                   "outros_tributos_ipva",
                                   "outros_tributos_itcd",
                                   "outros_tributos_taxas",
                                   "outros_tributos_outros"))

#--------------------------#
#------ WEBSCRAPPING ------#
#--------------------------#

#------ Iniciando DF com resultados ------#
GERADO <- data.frame(NULL)

#------ HTTR configuracao global ------#
set_config(config(ssl_verifypeer = 0L))

#------ Link (URL) ------#
url <- 'https://www.confaz.fazenda.gov.br/legislacao/boletim-do-icms'

#------ Barra de progresso ------#
pb <- txtProgressBar(max = dim(metadados)[1], style = 3)

system.time(
for (i in 1:dim(metadados)[1])
{
  #------ Iniciando argumentos ------#
  exclinhas <- NULL
  aux <- NULL

  #------ Parametros ------#
  ### Encontrados na exploracao da webpage ###
  params <- list(`form.submitted` = "1",
                 `ano` = paste(metadados$Ano[i]),
                 `tipo_consulta` = paste(metadados$Tipo[i]),
                 `conta` = paste(metadados$Conta[i]),
                 `form.button.Search` = "Buscar")
  
  #------ Recuperando HTML ------#
  tabela <- rvest::html_table(xml2::read_html(httr::POST(url,
                                                         body = params,
                                                         encode = 'form')))
  
  #---------------------------------------------#
  #------ Organizando no formato IpeaData ------#
  #---------------------------------------------#
  
  ### Remover a coluna 1: Referente ao nome das regioes ###
  ### Remover a coluna 2: Referente a dezembro do ano anterior ###
  ### Remover a coluna 15: Referente ao acumulado ###
  ### Colunas de interesse: 3 a 14 ###
  
  #------ Linhas de interesse ------#
  ### Para ICMS, IPVA e ITCD: Todas as UFs -> Excluir linhas 1:5,13,23,28,32.
  ### Para outros: Somente Brasil -> Excluir linhas 1:36.
  if ((metadados$Conta[i]=="icms_total")|
      (metadados$Conta[i]=="outros_tributos_ipva")|
      (metadados$Conta[i]=="outros_tributos_itcd"))
  {exclinhas <- c(1:5,13,23,28,32)} else {exclinhas <- c(1:36)}
    
  #------ DF auxiliar ------#
  aux <- data.frame(valdata = as.character(seq(as.Date(paste0(metadados$Ano[i],"-01-15"), 
                                                     origin="1900-01-01"),
                                             as.Date(paste0(metadados$Ano[i],"-12-15"), 
                                                     origin="1900-01-01"),
                                             by="1 month")),
                    valvalor_temp = t(tabela[[1]][-exclinhas,3:14]),row.names=NULL)
  
  #------ Nomeando as colunas ------#
  names(aux)[-1] <- tabela[[1]][-exclinhas,1]
  
  #------ Salvando DF resultado - Padrao IpeaData ------#
  for (l in 2:dim(aux)[2])
  {
    GERADO <- rbind(GERADO,
                    data.frame(serid = as.integer(NA),
                               valdata = as.Date(aux[,1], origin = "1900-01-01"),
                               terid = as.integer(1),
                               valvalor = aux[,l],
                               ocrid = as.integer(NA),
                               sercodigotroll = names(aux)[l],
                               atualizdata = Sys.time(),
                               row.names = NULL))
  }
  
  #------ Substituindo o nome da regiao pelo codigo troll ------#
  if (metadados$Conta[i]=="icms_total")
  {
    GERADO$sercodigotroll <- gsub("Mato Grosso do Sul","CONFAZ12_ICMSMS12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Acre","CONFAZ12_ICMSAC12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Amazonas","CONFAZ12_ICMSAM12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Pará","CONFAZ12_ICMSPA12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rondônia","CONFAZ12_ICMSRO12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Amapá","CONFAZ12_ICMSAP12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Roraima","CONFAZ12_ICMSRR12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Tocantins","CONFAZ12_ICMSTO12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Maranhão","CONFAZ12_ICMSMA12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Piauí","CONFAZ12_ICMSPI12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Ceará","CONFAZ12_ICMSCE12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rio Grande do Norte","CONFAZ12_ICMSRN12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Paraíba","CONFAZ12_ICMSPB12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Pernambuco","CONFAZ12_ICMSPE12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Alagoas","CONFAZ12_ICMSAL12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Sergipe","CONFAZ12_ICMSSE12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Bahia","CONFAZ12_ICMSBA12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Minas Gerais","CONFAZ12_ICMSMG12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Espírito Santo","CONFAZ12_ICMSES12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rio de Janeiro","CONFAZ12_ICMSRJ12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("São Paulo","CONFAZ12_ICMSSP12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Paraná","CONFAZ12_ICMSPR12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Santa Catarina","CONFAZ12_ICMSSC12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rio Grande do Sul","CONFAZ12_ICMSRS12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Mato Grosso","CONFAZ12_ICMSMT12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Goiás","CONFAZ12_ICMSGO12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Distrito Federal","CONFAZ12_ICMSDF12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("BRASIL","CONFAZ12_ICMSN12", fixed = T, GERADO$sercodigotroll)
  }
  if (metadados$Conta[i]=="outros_tributos_ipva")
  {
    GERADO$sercodigotroll <- gsub("Mato Grosso do Sul","CONFAZ12_IPVAMS12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Acre","CONFAZ12_IPVAAC12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Amazonas","CONFAZ12_IPVAAM12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Pará","CONFAZ12_IPVAPA12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rondônia","CONFAZ12_IPVARO12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Amapá","CONFAZ12_IPVAAP12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Roraima","CONFAZ12_IPVARR12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Tocantins","CONFAZ12_IPVATO12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Maranhão","CONFAZ12_IPVAMA12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Piauí","CONFAZ12_IPVAPI12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Ceará","CONFAZ12_IPVACE12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rio Grande do Norte","CONFAZ12_IPVARN12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Paraíba","CONFAZ12_IPVAPB12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Pernambuco","CONFAZ12_IPVAPE12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Alagoas","CONFAZ12_IPVAAL12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Sergipe","CONFAZ12_IPVASE12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Bahia","CONFAZ12_IPVABA12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Minas Gerais","CONFAZ12_IPVAMG12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Espírito Santo","CONFAZ12_IPVAES12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rio de Janeiro","CONFAZ12_IPVARJ12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("São Paulo","CONFAZ12_IPVASP12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Paraná","CONFAZ12_IPVAPR12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Santa Catarina","CONFAZ12_IPVASC12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rio Grande do Sul","CONFAZ12_IPVARS12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Mato Grosso","CONFAZ12_IPVAMT12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Goiás","CONFAZ12_IPVAGO12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Distrito Federal","CONFAZ12_IPVADF12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("BRASIL","CONFAZ12_IPVA12", fixed = T, GERADO$sercodigotroll)
  }
  if (metadados$Conta[i]=="outros_tributos_itcd")
  {
    GERADO$sercodigotroll <- gsub("Mato Grosso do Sul","CONFAZ12_ITCDMS12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Acre","CONFAZ12_ITCDAC12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Amazonas","CONFAZ12_ITCDAM12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Pará","CONFAZ12_ITCDPA12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rondônia","CONFAZ12_ITCDRO12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Amapá","CONFAZ12_ITCDAP12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Roraima","CONFAZ12_ITCDRR12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Tocantins","CONFAZ12_ITCDTO12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Maranhão","CONFAZ12_ITCDMA12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Piauí","CONFAZ12_ITCDPI12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Ceará","CONFAZ12_ITCDCE12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rio Grande do Norte","CONFAZ12_ITCDRN12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Paraíba","CONFAZ12_ITCDPB12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Pernambuco","CONFAZ12_ITCDPE12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Alagoas","CONFAZ12_ITCDAL12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Sergipe","CONFAZ12_ITCDSE12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Bahia","CONFAZ12_ITCDBA12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Minas Gerais","CONFAZ12_ITCDMG12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Espírito Santo","CONFAZ12_ITCDES12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rio de Janeiro","CONFAZ12_ITCDRJ12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("São Paulo","CONFAZ12_ITCDSP12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Paraná","CONFAZ12_ITCDPR12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Santa Catarina","CONFAZ12_ITCDSC12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Rio Grande do Sul","CONFAZ12_ITCDRS12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Mato Grosso","CONFAZ12_ITCDMT12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Goiás","CONFAZ12_ITCDGO12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("Distrito Federal","CONFAZ12_ITCDDF12", fixed = T, GERADO$sercodigotroll)
    GERADO$sercodigotroll <- gsub("BRASIL","CONFAZ12_ITCD12", fixed = T, GERADO$sercodigotroll)
  }
  if (metadados$Conta[i]=="outros_tributos_taxas"){GERADO$sercodigotroll <- gsub("BRASIL","CONFAZ12_TAXAS12", fixed=T, GERADO$sercodigotroll)}
  if (metadados$Conta[i]=="outros_tributos_outros"){GERADO$sercodigotroll <- gsub("BRASIL","CONFAZ12_OUT12", fixed=T, GERADO$sercodigotroll)}
  
  #------ Barra de progresso na tela ------#
  setTxtProgressBar(pb, i)
})

#------ Fechando conexao da barra de progresso ------#
close(pb)

#-------------------------------------#
#------ DETALHES DE FINALIZACAO ------#
#-------------------------------------#

#------ Substituindo . por  ------#
### Milhares separados por . ###
GERADO$valvalor <- gsub("[.]", "", GERADO$valvalor)

#------ Substituindo * por  ------#
### Valores sobre observacao na fonte vem acompanhado de * ###
GERADO$valvalor <- gsub("[*]", "", GERADO$valvalor)

#------ Substituindo 0 por NA ------#
### Valores 0 = NA ###
GERADO$valvalor <- ifelse(GERADO$valvalor==0,NA,as.numeric(GERADO$valvalor))

#------ Removendo NA ------#
GERADO <- subset(GERADO,is.na(GERADO$valvalor)==F)

#------ Configurando data ------#
GERADO$valdata <- as.Date(GERADO$valdata, origin = "1900-01-01")

#------ Configurando codtroll ------#
GERADO$sercodigotroll <- as.character(GERADO$sercodigotroll)

#------ Ordenando por codtroll ------#
GERADO <- GERADO[order(GERADO$sercodigotroll),]

########################################################
########################################################
########################################################
########################################################
################## BLOCO TEMPORARIO ####################
########################################################
########################################################
########################################################
########################################################

#----------------------------------------------------------------------------------------------
# PLANILHA GENERICA - CONFAZ12 (SEM CORRECAO)
#----------------------------------------------------------------------------------------------

#------ Todas as series ------#
serie_codtroll <- as.character(unique(GERADO$sercodigotroll))

#------ Separando as series em objetos distintos ------#
for(i in 1:length(serie_codtroll))
{ 
  nomes <- paste0("serie", i)
  assign(nomes,subset(GERADO,GERADO$sercodigotroll==serie_codtroll[i])[,c(2,4,6)])
}

#------ Fundindo as series em um unico bloco de dados ------#
serie <- NULL
if (length(serie_codtroll)==1){serie <- merge(serie1,serie1,by="valdata",all=T)}
if ((length(serie_codtroll)>1) & (length(serie_codtroll)<=2)){serie <- merge(serie1,serie2,by="valdata",all=T)}
if (length(serie_codtroll)>2)
{
  serie <- merge(serie1,serie2,by="valdata",all=T)
  for (i in 3:length(serie_codtroll))
  {
    serie <- merge(serie,get(paste0("serie",i)),by="valdata",all=T)
    names(serie)[(2*i):((2*i)+1)] <- paste0(c("valvalor.","sercodigotroll."),i)
  }
}

#------ Organizando para salvamento ------#
serie <- serie[,c(1,seq(2,dim(serie)[2]-1,2))]

#------ Nomeando ------#
names(serie)[-1] <- serie_codtroll

#------ Salvando em .csv ------#
# write.csv2(serie,"GENERICA_CONFAZ12-NAO_CORRIGIDA.csv",row.names = F)

#------ Limpando o WD ------#
rm(list = ls()[ls()!=c("GERADO")])

########################################################
########################################################
########################################################
########################################################

#----------------------------------------------------------------------------------------------
# CORRECAO AUTOMATICA
#----------------------------------------------------------------------------------------------

a <- Sys.time()

#------ Todas as series ------#
input <- unique(GERADO$sercodigotroll)

#------ Iniciando DF com resultados 2 ------#
GERADO2 <- data.frame(NULL)

#------ Iniciando DF auxiliar ------#
VERIF <- data.frame(NULL)

#------ Barra de progresso ------#
pb <- txtProgressBar(max = length(input[-14]), style = 3)

#---------------------------------#
#------ SUAVIZACAO DINAMICA ------#
#---------------------------------#

#######################################
### A SERIE 14 E 69 NAO TA DANDO CERTO
### EXPLORAR
#######################################

for (j in 1:length(input)[-c(14,69)])
{
  serie1 <- subset(GERADO,GERADO$sercodigotroll==input[j])
  repeat
  {
    serie <- serie1$valvalor
    #------ Calculando a previsão juntamente com LI e LS ------#
    ### LI: Limite Inferior ###
    ### LS: Limite Superior ###
    ### ft: previsao no tempo t ###
    ### inic: qtde de valores iniciais minima ###
    LI <- NA
    LS <- NA
    ft <- NA
    inic <- 5
    for (i in inic:length(serie))
    {
      tryCatch({
        #------ Suavizacao exponencial ------#
        fit <- forecast(ets(serie[1:(i-1)]),level=c(95,99),1)
        ft[i] <- as.numeric(fit$mean)
        LI[i] <- as.numeric(fit$lower)[2]
        LS[i] <- as.numeric(fit$upper)[2]
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      if(length(ft)==1){inic <- inic + 1}
    }
    
    #--------------------------------------------------#
    #------ Calculando a razão das extrapolacoes ------#
    #--------------------------------------------------#
    ### Evitar possiveis divisoes improprias ###
    m <- 0
    if(length(serie[serie==0])){m <- m + 0.0001}

    RAZAO <- NA
    for (i in (sum(is.na(LI))+1):length(serie))
    {
      if ((serie[i]+m)<(LI[i]+m))
      {
        if((serie[i]+m)<0){RAZAO[i] <- abs((serie[i]+m)/(LI[i]+m))}
        if((serie[i]+m)>0){RAZAO[i] <- abs((LI[i]+m)/(serie[i]+m))}
      }
      if ((serie[i]+m)>(LS[i]+m))
      {
        if((LS[i]+m)<0){RAZAO[i] <- abs((LS[i]+m)/(serie[i]+m))}
        if((LS[i]+m)>0){RAZAO[i] <- (serie[i]+m)/(LS[i]+m)}
      }
      if (((serie[i]+m)>(LI[i]+m))&((serie[i]+m)<(LS[i]+m))){RAZAO[i] <- NA}
      if ((is.na(RAZAO[i])==F)&(RAZAO[i]<1)){RAZAO[i] <- 1/RAZAO[i]}
    }
    
    #---------------------------------------------#
    #------ DF auxiliar com as verificacoes ------#
    #---------------------------------------------#
    ### Caso a razao seja maior que r, entra a correcao ###
    r <- 3
    if (dim(VERIF)[2]>0){VERIF <- data.frame(serie1[,1:7],LI=LI,LS=LS,RAZAO=RAZAO,IND=ifelse(RAZAO>=r,1,0),ROW=1:length(serie))} else 
    {VERIF <- data.frame(serie1,LI=LI,LS=LS,RAZAO=RAZAO,IND=ifelse(RAZAO>=r,1,0),ROW=1:length(serie))}
    
    #------ Caso tenha extrapolacao, correcao automatica ------#
    ### Valor max da const de extrapolacao: k ###
    k <- 2
    if (sum(VERIF$IND,na.rm=T)>0)
    {
      #------ Indice da coluna ROW ------#
      linha <- subset(VERIF,VERIF$IND==1)[,12]
      
      #------ Printa a recorrencia atual ------#
      print(paste("Serie:",j,"Observacao(oes):",linha))
      
      for (l in 1:length(linha))
      {
        #------ Se tiver abaixo, multiplica por 10 ------#
        if(VERIF$valvalor[linha[l]]<(1/k)*VERIF$LI[linha[l]])
        {
          VERIF$valvalor[linha[l]] <- VERIF$valvalor[linha[l]]*10
          serie1 <- VERIF
        }
        
        #------ Se tiver acima, divide por 10 ------#
        if(VERIF$valvalor[linha[l]]>k*VERIF$LS[linha[l]])
        {
          VERIF$valvalor[linha[l]] <- VERIF$valvalor[linha[l]]/10
          serie1 <- VERIF
        }
      }
    } else break
  }
  #------ Grafico (JPEG) ------#
  # tryCatch({
  #   tam <- length(serie)
  #   jpeg(paste0(input[j],".jpg"),width = 800, height = 600)
  #   plot(1,type="n",ylim=c(0.8*min(serie),1.2*max(serie)),xlim=c(1,tam),axes=FALSE,ylab=input[j],xlab="")
  #   polygon(c(rev(inic:tam),inic:tam),c(rev(LI[-(1:(inic-1))]),LS[-(1:(inic-1))]),col="grey80",border=NA)
  #   lines(ft,lwd=2,lty=2)
  #   points(serie,col=2,pch=19,cex=.7)
  #   axis(1,seq(1,tam,by=100))
  #   axis(2,seq(0.8*min(serie),1.2*max(serie),length.out=25))
  #   dev.off()
  # }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  #------ Salvando DF resultado ------#
  GERADO2 <- rbind(GERADO2,VERIF[,1:7])
  
  #------ Limpando o DF auxiliar ------#
  VERIF <- data.frame(NULL)
  
  #------ Barra de progresso na tela ------#
  setTxtProgressBar(pb, j)
}

#------ Fechando conexao da barra de progresso ------#
close(pb)

Sys.time()-a

#----------------------------------------------------------------------------------------------
# PLANILHA GENERICA - CONFAZ12 (COM CORRECAO)
#----------------------------------------------------------------------------------------------

#------ Todas as series ------#
serie_codtroll <- as.character(unique(GERADO2$sercodigotroll))

#------ Separando as series em objetos distintos ------#
for(i in 1:length(serie_codtroll))
{ 
  nomes <- paste0("serie", i)
  assign(nomes,subset(GERADO2,GERADO2$sercodigotroll==serie_codtroll[i])[,c(2,4,6)])
}

#------ Fundindo as series em um unico bloco de dados ------#
serie <- NULL
if (length(serie_codtroll)==1){serie <- merge(serie1,serie1,by="valdata",all=T)}
if ((length(serie_codtroll)>1) & (length(serie_codtroll)<=2)){serie <- merge(serie1,serie2,by="valdata",all=T)}
if (length(serie_codtroll)>2)
{
  serie <- merge(serie1,serie2,by="valdata",all=T)
  for (i in 3:length(serie_codtroll))
  {
    serie <- merge(serie,get(paste0("serie",i)),by="valdata",all=T)
    names(serie)[(2*i):((2*i)+1)] <- paste0(c("valvalor.","sercodigotroll."),i)
  }
}

#------ Organizando para salvamento ------#
serie <- serie[,c(1,seq(2,dim(serie)[2]-1,2))]

#------ Nomeando ------#
names(serie)[-1] <- serie_codtroll

#------ Salvando em .csv ------#
# write.csv2(serie,"GENERICA_CONFAZ12-CORRIGIDA.csv",row.names = F)

#----------------------------------------------------------------------------------------------
# INSERCAO NO BANCO DE DADOS
#----------------------------------------------------------------------------------------------

######################################################################
######################################################################
####### APAGAAAAAARRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR #######
######################################################################
######################################################################

############ COLOCAR SENHA AQUI
con <- NULL
###########  NAO ESQUECER DE DESCONECTAR
######################################################################
######################################################################
####### APAGAAAAAARRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR #######
######################################################################
######################################################################