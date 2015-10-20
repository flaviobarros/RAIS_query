## Carregando os pacotes necessários
library(dplyr)
library(readr)

## Pegando os dados de ocupação para selecinar somente gerentes e diretores
ocupacao <- read_delim(file = 'ocupacao.csv', delim = ':', col_names = F)
colnames(ocupacao) <- c('codigo', 'ocupacao')

## Criando função que consulta o banco e retorna a mediana e média
media_salario <- function(tabela, cod_municipio) {
  
  ## Carrega o dplyr que é necessário
  library(dplyr)
  
  ## Consulta
  cidade <- tabela %>%
    select(município, cbo_ocupação_2002) %>%
    filter(município == cod_municipio) %>%
    summarise(total = n())
  
  cidade <- collect(cidade)
  
  return(cidade)
}

################################################################################
## Pegando as variáveis no Monet.DB
# Preparando um banco
## Carregando pacotes
library(MonetDB.R)

## Criando a conexão
con <- dbConnect(MonetDB(), host = 'localhost', 
                 dbname = 'rais2014',
                 user = 'monetdb',
                 password = 'monetdb')

## Cria conexão para o dplyr
monetdb_conn <- src_monetdb(dbname = 'rais2014')

## Busca e conecta a todas as tabelas
tabelas <- dbListTables(monetdb_conn$con)
fontes <- rep(0,length(tabelas))

for (i in 1:length(tabelas)) {
  
  fontes[i] <- tbl(monetdb_conn, tabelas[i])
}

## Cidades de interesse
cidades <- c('Se-Aracaju', 'Pa-Belem', 'Mg-Belo Horizonte', 'Sc-Blumenau',
             'Df-Brasilia', 'Sp-Campinas', 'Ms-Campo Grande', 'Rs-Caxias do Sul', 
             'Mt-Cuiaba', 'Pr-Curitiba', 'Sc-Florianopolis', 'Ce-Fortaleza',
             'Go-Goiania', 'Pb-Joao Pessoa', 'Sc-Joinville', 'Pr-Londrina',
             'Al-Maceio', 'Am-Manaus', 'Pr-Maringa', 'Rn-Natal', 'Rs-Porto Alegre',
             'Pe-Recife', 'Sp-Ribeirao Preto', 'Rj-Rio de Janeiro',
             'Ba-Salvador', 'Sp-Sao Jose dos Campos', 'Ma-Sao Luis', 'Sp-Sao Paulo',
             'Sp-Sorocaba', 'Pi-Teresina', 'Mg-Uberlandia', 'Es-Vitoria')

## Separando os códigos só das cidades de interesse
cod_cidades <- read_delim('codigo_cidades.csv', delim = ':')
cod_cidades <- subset(cod_cidades, subset = cod_cidades$municipio %in% cidades)

#################################################################################
## Pegando conexões para todos os estados
sergipe <- tbl(monetdb_conn, 'rais_se')
para <- tbl(monetdb_conn, 'rais_pa')
minas <- tbl(monetdb_conn, 'rais_mg')
sta_catarina <- tbl(monetdb_conn, 'rais_sc')
df <- tbl(monetdb_conn, 'rais_df')
sao_paulo <- tbl(monetdb_conn, 'rais_sp')
mato_grosso <- tbl(monetdb_conn, 'rais_mt')
mato_grosso_sul <- tbl(monetdb_conn, 'rais_ms')
rio_grande_sul <- tbl(monetdb_conn, 'rais_rs')
parana <- tbl(monetdb_conn, 'rais_pr')
ceara <- tbl(monetdb_conn, 'rais_ce')
goias <- tbl(monetdb_conn, 'rais_go')
paraiba <- tbl(monetdb_conn, 'rais_pb')
alagoas <- tbl(monetdb_conn, 'rais_al')
amazonas <- tbl(monetdb_conn, 'rais_am')
rio_grande_norte <- tbl(monetdb_conn, 'rais_rn')
pernambuco <- tbl(monetdb_conn, 'rais_pe')
rj <- tbl(monetdb_conn, 'rais_rj')
bahia <- tbl(monetdb_conn, 'rais_ba')
maranhao <- tbl(monetdb_conn, 'rais_ma')
piaui <- tbl(monetdb_conn, 'rais_pi')
espirito_santo <- tbl(monetdb_conn, 'rais_es')

#################################################################################
## Resultado da consulta da RAIS
resultado <- data.frame(cidades = cidades, renda = rep(0,length(cidades)))

# Aracaju
resultado$renda[1] <- media_salario(sergipe, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[1]])

# Belem 
resultado$renda[2] <- media_salario(para, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[2]])

# BH
resultado$renda[3] <- media_salario(minas, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[3]])

# Blumenau
resultado$renda[4] <- media_salario(sta_catarina, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[4]])

# Brasilia
resultado$renda[5] <- media_salario(df, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[5]])

# Campinas
resultado$renda[6] <- media_salario(sao_paulo, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[6]])

# Campo grande
resultado$renda[7] <- media_salario(mato_grosso_sul, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[7]])

# Caxias do Sul
resultado$renda[8] <- media_salario(rio_grande_sul, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[8]])

# Cuiaba
resultado$renda[9] <- media_salario(mato_grosso, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[9]])

# Curitiba
resultado$renda[10] <- media_salario(parana, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[10]])

# Floripa
resultado$renda[11] <- media_salario(sta_catarina, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[11]])

# Fortaleza
resultado$renda[12] <- media_salario(ceara, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[12]])

# Goiania
resultado$renda[13] <- media_salario(goias, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[13]])

# Joao Pessoa
resultado$renda[14] <- media_salario(paraiba, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[14]])

# Joinville
resultado$renda[15] <- media_salario(sta_catarina, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[15]])

# Londrina
resultado$renda[16] <- media_salario(parana, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[16]])

# Maceio
resultado$renda[17] <- media_salario(alagoas, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[17]])

# Manaus
resultado$renda[18] <- media_salario(amazonas, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[18]])

# Maringa
resultado$renda[19] <- media_salario(parana, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[19]])

# Natal
resultado$renda[20] <- media_salario(rio_grande_norte, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[20]])

# Porto Alegre
resultado$renda[21] <- media_salario(rio_grande_sul, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[21]])

# Recife
resultado$renda[22] <- media_salario(pernambuco, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[22]])

# Ribeirão Preto
resultado$renda[23] <- media_salario(sao_paulo, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[23]])

# Rio de Janeiro
resultado$renda[24] <- media_salario(rj, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[24]])

# Salvador
resultado$renda[25] <- media_salario(bahia, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[25]])

# Sao Jose dos Campos
resultado$renda[26] <- media_salario(sao_paulo, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[26]])

# Sao Luis
resultado$renda[27] <- media_salario(maranhao, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[27]])

# Sao Paulo
resultado$renda[28] <- media_salario(sao_paulo, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[28]])

# Sorocaba
resultado$renda[29] <- media_salario(sao_paulo, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[29]])

# Teresina
resultado$renda[30] <- media_salario(piaui, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[30]])

# Uberlandia
resultado$renda[31] <- media_salario(minas, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[31]])

# Vitoria
resultado$renda[32] <- media_salario(espirito_santo, cod_municipio = cod_cidades$codigo[cod_cidades$municipio == cidades[32]])

## Mudando o nome das colunas
colnames(resultado) <- c('cidade', 'total_funcionarios')
save(resultado, file = 'total_funcionarios_por_municipio.rda')
library(xlsx)
write.xlsx(resultado, file = 'total_funcionarios_por_municipio.xlsx')

