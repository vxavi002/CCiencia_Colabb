############################ ATIVIDADE 2 ##############################

#DarwinCore (DwC)

#3 planilhas: 1. eventCore, 2.ocurrenceExtension, 3. extendedMeasurementsOrFacts

##Carregar pacotes
library(tidyverse)
library(taxize)

##Carregar dados
#Utilizacao de dados prontos

iris<-read.csv("iris_mod.csv", header=TRUE)
view(iris)
lapply(iris, unique)

##Checar validez dos taxons

species <- iris %>% 
  distinct(Species) %>% 
  pull() %>% 
  get_tsn() %>% 
  data.frame() %>% 
  bind_cols(iris %>% 
              distinct(Species))
#Foi utilizado o pipe acima para evitar a criacao de varios objetos. Criacao de varios objetos virtuais que vao passando de uma funcao p/ outra.

species
#Nome das especies associados eh "uri":
#Campos onde tem "match" e "pattern_match" significa que as especies sao validas. O campo "uri" indica o endereco unico para a checagem das spss no ITIS. O campo "uri" sera utilizado na planilha "occurrence" do sistema "event core".

#Manipulacao dos dados#
#Planilha base - renomear as variaveis de acordo com o DwC
#Criacao de informacoes unicas que vao remeter a cada ocorrencia


iris_1 <- iris %>% 
  dplyr::mutate(eventID = paste(site, date, sep = "_"), # create indexing fields/ Usa-se a funcao mutate do pacote dplyr, pra criar um campo novo que vai ser o site e a data separados por _ pra criar o eventID.
                occurrenceID = paste(site, date, amostra, sep = "_")) %>% #site, data e amostra concatenados separados pra criar occurrenceID.
  left_join(species %>% #repetir os valores do primeiro dataframe no segundo
              select(Species, uri)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = lon, # rename fields according to DwC 
                decimalLatitude = lat,
                eventDate = date,
                scientificName = Species,
                scientificNameID = uri) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")

head(iris_1)
#Realizar a construcao das tres matrizes necessarias para inserir os dados em repositorios baseados em ocorrencias e dados acessorios.

#Campos obrigatorios na planilha de eventos (eventCore/planilha core): eventID = eventos unicos de amostragem, eventDate (Ano/m?s/dia), decimalLongitude (graus decimais), decimalLatitude (graus decimais), verbatimCoordinateSystem = tipo de coordenadas, geodeticDatum = Datum das coordenadas.

#Termos padronizados em: https://dwc.tdwg.org/terms/

#Planilha de eventos - separacao em 3 niveis.
## create eventCore
#selecionar o que tem que ter dentro do eventCore - tempo e espaco das ocorrencias

eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
#percebe-se que ha amostras repetidas, necessita-se apenas os valores unicos porque ja temos o "eventID".
  distinct()

#O distinct ve os valores unicos nesse conjunto de dados.

head(eventCore)

#Planilha de ocorr?ncias 

## create occurrence
#Aqui coloca-se dados das especies e o eventID eh o que vai relacionar as planilhas
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, scientificNameID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 

#Percebe-se que continua tendo 25 linhas (cada ocorrencia eh uma amostra).

head(occurrences)

#Planilha de atributos eMOF (dados extendidos)

## create measurementsOrFacts - dados relacionados eh cada spp

eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Length:Petal.Width) %>%  
  pivot_longer(cols = Sepal.Length:Petal.Width, #Pega-se as 4 colunas e coloca-se em uma outra (measumentType e measurementValue)
               names_to = "measurementType",
               values_to = "measurementValue") %>% #transformacao em formato mais longo
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))
#Renomear os valores, colocando tudo em minusculo, etc

head(eMOF)

##Controle de qualidade
#Checar se todas as planilhas tem os mesmos valores de eventID.

# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)

#Nao ha diferencas

setdiff(eventCore$eventID, eMOF$eventID)

#Nao ha diferencas

setdiff(occurrences$eventID, eMOF$eventID)

#Tb nao ha nenhuma diferenca

# check NA values em eventID
eMOF %>% #medidas
  filter(is.na(eventID))

occurrences %>% #em ocorrencias
  filter(is.na(eventID))

#Tudo ok, proximo passo transformar a planilha em 3 arquivos.

#Salvar dados
#Limpar a memoria e deixar apenas os arquivos que interessam:

rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

#transformar em 3 arquivos:
#Ler arquivos
files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")

#Loop pra criar arquivos *csv
for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}

write.csv(eMOF, "eMOF.csv", row.names=FALSE)


###################### ATIVIDADE 3 ##########################
  
##Acessando o banco de dados abertos
#Utiliza-se o pacote tidyverse para fazer a manipula??o dos dados
##Carregar pacotes

library(tidyverse)

#Para pesquisar e recuperar dados do GBIF. O pacote utiliza o c?digo no R em torno da API GBIF para permitir a comunica??o entre os dois (R e GBIF), assim d? pra acessar metadados, nome de esp?cies e ocorr?ncias.
#Para extrair dados do GBIF:

library(rgbif)

#Agora utilizamos a fun??o occ_data para buscar as ocorr?ncias no reposit?rio do GBIF atrav?s do nome cient?fico, n?mero de identifica??o, pa?s e outros. Assim:

?occ_data




