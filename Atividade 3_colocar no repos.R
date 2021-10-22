###################### ATIVIDADE 3 ##########################

##Acessando o banco de dados abertos
#Utiliza-se o pacote tidyverse para fazer a manipulacao dos dados
##Carregar pacotes

library(tidyverse)

#Para pesquisar e recuperar dados do GBIF. O pacote utiliza o codigo no R em torno da API GBIF para permitir a comunicacao entre os dois (R e GBIF), assim da pra acessar metadados, nome de especies e ocorrencias.
#Para extrair dados do GBIF:

library(rgbif)

#Agora utilizamos a funcao occ_data para buscar as ocorrencias no repositorio do GBIF atraves do nome cientifico, numero de identificacao, pais e outros. Assim, para checar as funcoes do pacote, temos:

?occ_data


# Para baixar ocorrencias
murici_gbif <- occ_data(scientificName = "Byrsonima sericea", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)
#Obs: selecionar apenas ocorrencias com coordenadas e sem problemas geoespaciais.

# verificar as dimensoes:
dim(murici_gbif)

#Verificar novamente o tamanho do dataset:

dim(murici_gbif$data)

#Obs: Foram encontrados 3320 records/registros, mas com a filtragem de coordenadas e problemas geoespaciais, a quantidade desce para 500 (Ou seja, muita coisa com problema na coordenada e com issues!).

# Para checar campos
murici_gbif$data %>% names

##Problemas reportados
#Para checar os issues reportados no algoritmo:

gbif_issues()

#Para individualizar os issues:
issues_gbif <- murici_gbif$data$issues %>%
   # unique() %>%
    strsplit(., "[,]") %>% #usado pra dividir os issues (quando se tem dois ou mais issues associados a mesma ocorrencia)
  unlist() %>% #Pra colocar tudo em um vetor so:
  unique()
#Obs: O unique anterior foi suprimido para verificar se tem menos issues, o que de fato ocorreu.

#Para verificar quais problemas existem no dataset baixado:
gbif_issues() %>% head
  data.frame() %>%
  filter(code%in%issues_gbif)
#Verifica-se que o valor da coordenada dada foi invalida e o GBIF nao soube interpretar ela.

gbif_issues() %>%
  data.frame() %>%
  filter(code%in%issues_gbif)
  
#Para selecionar variaveis que serao uteis para a validacao dos dados (como coordenadas, profundidade, nome da base de dados etc.):
  
murici_gbif1 <- murici_gbif$data %>%
    dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                  issues, basisOfRecord, occurrenceStatus, rightsHolder, 
                  datasetName, recordedBy, continent, locality, habitat, institutionCode, elevation, stateProvince)

#Obs: Foram retirados o "waterBody" e "depth" pois nao se aplicam. Foi inserido o "continent","institutionCode", "elevation" e "stateProvince".

#Para verificar dados unicos:

murici_gbif1 <- murici_gbif1 %>% 
  distinct()

#Checar niveis dos fatores:

lapply(murici_gbif1, unique)
#Obs: Para cada 13 variaveis, e aplicado O lapply, um loop para verificar em todas as colunas quais sao os valores unicos. 
#Observei que uma das localidades e a RPPN Fazenda Caruara, onde trabalhei com essa especie, legal!
  
##Problemas nao reportados
#Para investigar niveis suspeitos:

murici_gbif1 %>% 
  distinct(institutionCode) %>% 
  pull()

#Obs: Foi trocado "waterBody" por "institutionCode". (Tentei "locality", mas apareceram muitas ocorrencias e nao deu pra visualizar o grafico direito, entao troquei por institutionCode)

# institutionCode
murici_gbif1 %>%
  group_by(institutionCode) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=institutionCode)) +
  geom_bar(stat = 'identity')
  #visualizacao das ocorrencias por instituicao

# fonte das ocorrencias nessas 5 instituicoes
murici_gbif1 %>% 
  filter(institutionCode %in% c("VIES", "Universidade Federal de Sergipe", "Universidade Federal da Bahia", "JBRJ", "ALCB")) %>% 
  distinct(datasetName)
#Obs: Coloquei as 5 instituicoes com maiores ocorrencias.
#Visualizar quantos datasets distintos existem para essas instituicoes

#Foi observado que 2 datasets se referem a mesma instituicao. Segue:
murici_gbif1 %>% 
  filter(datasetName %in% c("RB - Rio de Janeiro Botanical Garden Herbarium Collection"))

#Foram observadas 33 ocorrencias para RB - Rio de Janeiro Botanical Garden Herbarium Collection

#Agora vou verificar as ocorrencias para Caatinga Biome - RB - Rio de Janeiro Botanical Garden Herbarium Collection

murici_gbif1 %>% 
  filter(datasetName %in% c("Caatinga Biome - RB - Rio de Janeiro Botanical Garden Herbarium Collection"))
#Foi observado apenas 1 ocorrencia para Caatinga Biome - RB - Rio de Janeiro Botanical Garden Herbarium Collection. Pode ser uma ambiguidade para a instituicao.

#Nao vou definir "Caatinga Biome - RB - Rio de Janeiro Botanical Garden Herbarium Collection" como dataset suspeito pois se trata da coleta da instituicao com apenas a denominacao da instituicao diferente, ou seja, continua valida. Entretanto, para retirar essa ocorrencia basta:

# filtrar todas do dataset suspeito
murici_gbif_ok <- murici_gbif1 %>% 
  filter(!datasetName %in% c("Caatinga Biome - RB - Rio de Janeiro Botanical Garden Herbarium Collection"))

##Plotar ocorrencias

#Instalar alguns pacotes primeiro:
install.packages("ggmap")
install.packages("mapdata")

#Agora sim:

library(ggmap)
library(maps)
library(mapdata)

world <- map_data('world')
brasil <-map_data('world', region="Brazil")

# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = murici_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Byrsonima sericea")))


# checar elevacao por estado:
murici_gbif_ok %>% 
  ggplot(aes(x = elevation, fill = stateProvince)) +
  geom_histogram() 

#A partir deste histograma, podemos ver que boa parte das ocorrencias se concentram a uma elevacao entre 0 e 1000 metros.No Rio de Janeiro, por exemplo, as maiores ocorrencias em elevacoes entre 0 e 1000 metros pode estar presente desde restingas a florestas estacionais (transicao entre areas costeiras e florestas mais adensadas).
 
##OBIS
#Para oceanografia.Nao se aplica ao caso deste script.
#Pode ser utilizado outro pacote especifico para especies vegetais. Apos a filtragem das ocorrencias, plotar o histograma novamente e depois unir as ocorrencias do GBIF e do pacote para especies vegetais.


#Para diferenciar os pontos por cores/elevation:

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = murici_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude, color = elevation)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Byrsonima sericea")))

#Percebe-se que, atraves da diferenciacao de cores dos pontos no mapa, onde ha ocorrencia da especie na costa brasileira, a elevacao e maior. Talvez pela cadeia de montanhas nessas areas que, da costa para o interior do pais, vao sendo substituidas por planicies mais baixas, ainda que ocorram certos cumes, podendo ser afloramentos rochosos.
########

#Salvar os dados tratados:

write.csv(murici_gbif_ok, "C:/Users/vanes/OneDrive/Documentos/UENF/Doutorado - PPGERN/Disciplinas/Ferramentas em ci?ncia colaborativa e bancos de dados abertos/Atividades/Atividade 3/Murici_dados_salvos.csv", row.names = FALSE)


##EXTRA: Classificacao automatica de pontos
#Funcao 'caseira'

# funcao para classificar ocorrencias suspeitas
flag_outlier <- function(df, species)
  # baseada no calculo do centroide de todas ocorrencias
  # indica como 'check' as ocorrencias que tem distancias ate o centroide
  # acima do 90th quantil (default) das distancias calculadas
#Entao, o que ficar acima de 90% seria os outliers da distribuicao, sendo entao possiveis dados suspeitos
  
  {dados <- df %>% 
    dplyr::filter(scientificName == species); 
  
  dados2 <- geosphere::distVincentyEllipsoid(
    dados %>%
      summarise(centr_lon = median(decimalLongitude),
                centr_lat = median(decimalLatitude)),
    dados %>% 
      dplyr::select(decimalLongitude, decimalLatitude)
  ) %>% 
    bind_cols(dados) %>% 
    rename(dist_centroid = '...1') %>% 
    mutate(flag = ifelse(dist_centroid < quantile(dist_centroid, probs = 0.9), "OK",
                         ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.90) & dist_centroid < quantile(dist_centroid, probs = 0.95), "check > Q90",
                                ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.95), "check > Q95", "OK"))))
  
  # mutate(flag = ifelse(dist_centroid > quantile(dist_centroid, probs = prob), "check", "OK"))
  
  print(dados2)}

# classificar ocorrencias
marcados <- murici_gbif$data %>% 
  data.frame() %>% 
  dplyr::select(scientificName, decimalLongitude, decimalLatitude, datasetName) %>% 
  distinct() %>% 
  flag_outlier(., "Byrsonima sericea DC.")

# mapa
ggplot() +
  geom_polygon(data = brasil, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados, 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = flag)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Byrsonima sericea")))

#Pelo mapa plotado, podemos ver que o centro de distribuicao da especie e mesmo na costa (por ex. restingas), enquanto que muito mais ao interior (por ex. Mato Grosso), os dados comecam a passar o percentil de 95%.

#Tambem podemos utilizar o pacote scrubr para outras funcoes de filtragem de dados:

install.packages("scrubr")
remotes::install_github("ropensci/scrubr")


library(scrubr)

# usando os dados com flag
data_scrubr <- marcados %>% 
  dframe() %>% 
  coord_impossible() %>% #coordenadas impossiveis
  coord_incomplete() %>% #coordenadas incompletas
  coord_unlikely() %>% 
  dedup() #coordenadas duplicadas


# mapa
ggplot() +
  geom_polygon(data = brasil, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = data_scrubr, 
             aes(x = decimalLongitude, y = decimalLatitude), 
             color = "red") +
  geom_point(data = marcados %>% 
               filter(flag != "OK"), 
             aes(x = decimalLongitude, y = decimalLatitude), 
             color = "blue", shape = 3) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Byrsonima sericea")))

#O mapa informa as ocorrencias com cruzinha que sao provaveis de serem suspeitas.Como mencionado anteriormente, as ocorrencias mais distantes do centro de distribuicao (dados que passaram do percentil de 95%) foram marcadas.

#Pacote obistools: nao se aplica (p/ dados marinhos)

#Testar o pacote CoordinateCleaner. Nesse pacote, ocorre uma averiguacao das coordenadas que estao fora do banco de dados.

install.packages("CoordinateCleaner")

library(CoordinateCleaner)
?CoordinateCleaner

flags <- clean_coordinates(
    x = marcados,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    tests = c("equal", "gbif",
              "zeros", "seas"))
#Testes: coordenadas iguais, se tem zeros, se esta em terra ou no mar, comparando com coordenadas do gbif.
#Foram verificados 46 registros suspeitos para "seas".

################################

summary(flags)
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")


#Exclude problematic records
data_cleaned <- marcados[flags$.summary,]
#The flagged records
data_flagged <- marcados[!flags$.summary,]

ggplot() +
  geom_polygon(data = brasil, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = data_cleaned, 
             aes(x = decimalLongitude, y = decimalLatitude), 
             color = "red") +
  geom_point(data = marcados %>% 
               filter(flag != "OK"), 
             aes(x = decimalLongitude, y = decimalLatitude), 
             color = "blue", shape = 3) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Byrsonima sericea")))

##########################################################
