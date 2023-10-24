# Cyclistics_study-case

---
title: "Cyclistic - study case"
author: "Caio"
date: "`r Sys.Date()`"
output: html_document
---

## Fase 1: Perguntar
  
#### Principais perguntas a serem respondidas:

* Como os membros anuais e os ciclistas casuais usam as bicicletas da Cyclistic de forma diferente?
* Por que os passageiros casuais iriam querer adquirir planos anuais da Cyclistic?
* Como a Cyclistic pode usar a mídia digital para influenciar os passageiros casuais a se tornarem membros?
    
#### Identificar a tarefa de negócios

A principal tarefa de negócio é mapear o perfil do usuário casual e usuário membro e diferenciá-los. Assim poderemos descobrir porquê os usuários casuais preferem permanecer no plano casual e depois mudar a abordagem para tentar fazer os usuários casuais assinaram o plano anual.

#### Partes interessadas

As partes interessadas são a diretora Lyla Moreno, equipe de análise de marketing e a equipe executiva.

##Fase 2: Preparar

#### Local dos dados
Eu peguei os dados [desse](https://divvy-tripdata.s3.amazonaws.com/index.html) local, onde estão disponíveis todos os dados de 2013.
#### Organização dos dados
Os dados estão organizados através do ano e mês, mas escritos de forma não uniforme.

#### Credibilidade dos dados
Os dados são atuais, já que todo mês são coletados novos dados sobre o compartilhamento de bicicletas. Os dados são devidamente coletados pela empresa Lyft Bikes and Scooters, eles apoiam o compartilhamento de bicicletas e permitiram acesso público através da licensa que pode ser acessada [aqui](https://divvybikes.com/data-license-agreement). Para utilizar os dados é necessário seguir uma série de regras.


## Fase 3: Processar

```{r loading librarys, message=FALSE, warning=FALSE}
library(tidyverse)
library(skimr)
library(janitor)
library(dplyr)
library(lubridate)
library(geosphere)
```


```{r loading data, message=FALSE, warning=FALSE}

Cyclistic2022_01 <- read.csv('202201-divvy-tripdata.csv')
Cyclistic2022_02 <- read.csv('202202-divvy-tripdata.csv')
Cyclistic2022_03 <- read.csv('202203-divvy-tripdata.csv')
Cyclistic2022_04 <- read.csv('202204-divvy-tripdata.csv')
Cyclistic2022_05 <- read.csv('202205-divvy-tripdata.csv')
Cyclistic2022_06 <- read.csv('202206-divvy-tripdata.csv')
Cyclistic2022_07 <- read.csv('202207-divvy-tripdata.csv')
Cyclistic2022_08 <- read.csv('202208-divvy-tripdata.csv')
Cyclistic2022_09 <- read.csv('202209-divvy-tripdata.csv')
Cyclistic2022_10 <- read.csv('202210-divvy-tripdata.csv')
Cyclistic2022_11 <- read.csv('202211-divvy-tripdata.csv')
Cyclistic2022_12 <- read.csv('202212-divvy-tripdata.csv')


```

Binding all the data
```{r binding all the data, message=FALSE, warning=FALSE}

all_cyclistic2022 <- bind_rows(Cyclistic2022_01, Cyclistic2022_02, Cyclistic2022_03, Cyclistic2022_04, Cyclistic2022_05, Cyclistic2022_06, Cyclistic2022_07, Cyclistic2022_08, Cyclistic2022_09, Cyclistic2022_10, Cyclistic2022_11, Cyclistic2022_12)
```


Inspecting the data
```{r Inspecting, warning=FALSE}
nrow(all_cyclistic2022)
skim(all_cyclistic2022)
colnames(all_cyclistic2022)
str(all_cyclistic2022)
summary(all_cyclistic2022)
```


Cleaning Data
```{r cleaning the data}
#preciso trocar as variaveis depois
#removing duplicates
all_cyclistic2022without_na <- distinct(all_cyclistic2022, ride_id, rideable_type, started_at, ended_at, start_station_name, start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng, member_casual)

#removing N/A
all_cyclistic2022clean <- drop_na(all_cyclistic2022without_na)
nrow(all_cyclistic2022clean)
```

Transforming Data
```{r transforming, message=FALSE, warning=FALSE}
#diferença de tempo
all_cyclistic2022clean <- all_cyclistic2022clean %>%
  mutate(ride_duration = difftime(ended_at, started_at))

all_cyclistic2022clean$date <- as.Date(all_cyclistic2022clean$started_at)
#ano
all_cyclistic2022clean$year <- format(as.Date(all_cyclistic2022clean$date), '%y')

#mes
all_cyclistic2022clean$month <- format(as.Date(all_cyclistic2022clean$date), '%m')

#dia do mes
all_cyclistic2022clean$day <- format(as.Date(all_cyclistic2022clean$date), '%d')

#dia da semana
all_cyclistic2022clean$week_day <- format(as.Date(all_cyclistic2022clean$date), '%A')

#convertentdo para numero
all_cyclistic2022clean$ride_duration <- as.numeric(as.character(all_cyclistic2022clean$ride_duration))

#mudando nome da coluna member_casual
all_cyclistic2022clean <- all_cyclistic2022clean %>%
  rename('client_type' = member_casual)

#creating ride_distance
all_cyclistic2022clean$ride_distance <- distGeo(matrix(c(all_cyclistic2022clean$start_lng, all_cyclistic2022clean$start_lat), ncol=2), matrix(c(all_cyclistic2022clean$end_lng, all_cyclistic2022clean$end_lat), ncol = 2))

all_cyclistic2022clean$ride_distance = all_cyclistic2022clean$ride_distance/1000

```

## Fase 4: Análise

```{r message=FALSE, warning=FALSE}
#média/max/min
avg_ride_duration <- all_cyclistic2022clean %>%
  summarise(duracao_média_sec = mean(all_cyclistic2022clean$ride_duration), duracao_maxima_sec = max(all_cyclistic2022clean$ride_duration), duracao_minima_sec = min(all_cyclistic2022clean$ride_duration))


# porcentagem de clientes
all_cyclistic2022clean %>%
  group_by(client_type) %>%
  summarise(qtd_tipo_cliente = n()) %>%
  mutate(tipo_clientes_pct = qtd_tipo_cliente / sum(qtd_tipo_cliente) * 100) %>%
  ggplot(aes(client_type, qtd_tipo_cliente, fill = client_type, label = round(tipo_clientes_pct, digits = 0))) + geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label = paste0(round(tipo_clientes_pct, 2), '%')), vjust = -0.3) +
  labs(title = 'Quantidade de clientes', x = 'Tipo de cliente (%)', y = 'Quantity')


# Qual estação as pessoas mais começam a pedalar(member)?
all_cyclistic2022clean %>%
  group_by(start_station_name) %>%
  filter(start_station_name != '', client_type == 'member') %>%
  summarise(station_rides = n()) %>%
  head() %>%
  ggplot(mapping = aes(x = start_station_name, y = station_rides, fill = start_station_name)) + geom_col() + coord_flip() +
  labs(title = 'Top estações', x = 'Estações', y = 'Quantidade de passeios')

# Qual estação as pessoas mais começam a pedalar(casual)?
all_cyclistic2022clean %>%
  group_by(start_station_name) %>%
  filter(start_station_name != '', client_type == 'casual') %>%
  summarise(station_rides = n()) %>%
  head() %>%
  ggplot(mapping = aes(x = start_station_name, y = station_rides, fill = start_station_name)) + geom_col() + coord_flip() +
  labs(title = 'Top estações', x = 'Estações', y = 'Quantidade de passeios')


#Mês do ano em que os passeios mais aumentam(por tipo de membro)
all_cyclistic2022clean %>%
  group_by(month, client_type)%>%
  summarise(num_rides = n())%>%
  ggplot(mapping = aes(x = month, y = num_rides, fill = client_type)) + geom_col(position = 'dodge') +
  labs(title = 'Quantidade de passeios em cada mês', x = 'Mês', y = 'Quantidade de passeios')

#Qual dia da semana as pessoas mais pedalam?
all_cyclistic2022clean$week_day <- factor(all_cyclistic2022clean$week_day, levels = c('segunda-feira', 'terça-feira', 'quarta-feira', 'quinta-feira', 'sexta-feira', 'sábado', 'domingo'))

all_cyclistic2022clean %>%
  group_by(week_day, client_type) %>%
  summarise(ride_count = n()) %>%
  arrange(desc(ride_count)) %>%
  ggplot(mapping = aes(week_day, ride_count, fill = client_type)) +
  geom_col(stat = 'identity', position = 'dodge') +
  labs(title = 'Quantidade de passeios nos dias da semana', x = 'Dias da semana', y = 'Quantidade de passeios')


#Qual é o horário mais popular?
ggplot(data = all_cyclistic2022clean, aes(x = hour(started_at), fill = client_type)) + geom_bar(position = 'dodge')



#Qual tipo de bicicleta é mais popular?

all_cyclistic2022clean %>%
  group_by(client_type, rideable_type) %>%
  filter(rideable_type != 'docked_bike') %>%
  summarise(tipo_preferido = n()) %>%
  ggplot(mapping = aes(x = rideable_type, y = tipo_preferido, fill = client_type)) + geom_col(position = 'dodge') +
  labs(title = 'Popularidade do tipo de bicicleta', x = 'Tipo de bicicleta', y = 'Quantidade')

  
#Média distancia percorrida por tipo de usuário
all_cyclistic2022clean %>%
  group_by(client_type) %>%
  filter(ride_distance < 1000) %>%
  summarise(count = n())


#Qual o tempo médio de viagem para membros e casuais nos dias da semana?
all_cyclistic2022clean %>%
  group_by(week_day, client_type) %>%
  summarise(avg_ride_duration = mean(ride_duration)) %>%
  arrange(desc(avg_ride_duration)) %>%
  ggplot(mapping = aes(x = week_day, y = avg_ride_duration, fill = client_type)) + geom_col(position = 'dodge') +
  labs(title = 'Tempo médio de viagens durante a semana', x = 'Dias da semana', y = 'Duração média')



```

## Fase 5: Compartilhar

Como podemos ver através dos gráficos, os clientes que utilizam o modelo anual são maioria em relação aos clientes que utilizam o modelo casual. O número de usuários aumenta quando a primavera começa, tem seu pico durante o verão e quando a temperatura começa a cair e o inverno começa a chegar o número de usuários cai drasticamente, mas principalmente o número de usuários casuais, a diferença entre os membros e casuais aumenta. Durante a semana, a diferença de usuários é grande, porém, durante o final de semana o número de usuários casuais aumenta chegando até a ultrapassar os usuários assinantes. Durante a semana, as horas mais populares para os membros são as horas do horário comercial, os usuários casuais possuem um aumento no fim da tarde. Os usuários casuais têm uma preferência por bicicletas elétricas, enquanto os membros não possuem uma preferência.

* Os passeios de usuários casuais tendem a demorar mais.
* Enquanto os membros utilizam a bicicleta no decorrer da semana, os casuais  tendem a utilizar mais no fim de semana.
* A duração dos trajetos é maior para os usuários casuais, justamente por conta do uso para lazer, então eles aproveitam mais o passeio.

A partir destes 3 pontos, podemos inferir que os membros casuais tendem a utilizar a bicicleta mais como passeio enquanto os usuários que assinam tendem a usar mais como meio de transporte para se locomover e realizar suas atividades.

## Fase 6: Agir

Agora voltando para as perguntas apresentadas no início do documento:

* Como os membros anuais e os ciclistas casuais usam as bicicletas da Cyclistic de forma diferente?
Os ciclistas casuais utilizam as bicicletas mais para relaxar, lazer. Enquanto os membros assinantes utilizam mais para se locomover durante a semana e realizar suas atividades.

* Por que os passageiros casuais iriam querer adquirir planos anuais da Cyclistic?
O uso dos usuários casuais é mais esporádico, então para fazê-los adquirir o plano anual seria necessário fazer com que o uso desses usuários se tornasse mais recorrente.

* Como a Cyclistic pode usar a mídia digital para influenciar os passageiros casuais a se tornarem membros?
A Cyclistic pode utilizar outdoors digitais localizados nas estações onde usuários casuais costumam pegar bicicletas. Para impulsionar o uso recorrente a Cyclistic poderia incentivar programas de exercício durante a semana para atrair as pessoas para se exercitar, assim, aumentaria o uso recorrente de usuários casuais.
