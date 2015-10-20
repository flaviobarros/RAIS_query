## Carregando os pacotes necessários
library(dplyr)
library(readr)
library(MonetDB.R)

## Cria conexão para o dplyr
monetdb_conn <- src_monetdb(dbname = 'rais2014')

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

## Criando uma conexão com a cidade
estab <- tbl(monetdb_conn, from = 'rais_estb')

## Consultando o número de estabelecimentos por município
estabelecimentos <- estab %>%
  select(município, ind_atividade_ano) %>%
  filter(ind_atividade_ano == 1) %>%
  filter(município %in% cod_cidades$codigo) %>%
  group_by(município) %>%
  summarise(total = n())

## Salvando a consulta em um data.frame
estabelecimentos <- collect(estabelecimentos)
cidade <- collect(cidade)
colnames(estabelecimentos) <- c('codigo', 'total')

## Juntando com os nomes das cidades
estabelecimentos <- merge(estabelecimentos, cod_cidades, by = 'codigo')
estabelecimentos <- subset(estabelecimentos, select = c('municipio', 'total'))
