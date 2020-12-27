#Zadanie 1.
#Utworz funkcje:
#rankAccount <- function(dataFrame, colName, groupName, valueSort, num) 
#ktora bedzie zwracala dla danej tabeli(dataFrame) n wierszy posiadajace najwieksze wartosci(sortowanie po kolumnie valueSort) 
#dla wybranej grupy (konkretna wartosc komorki, np. "NAUCZYCIEL") z kolumny (colName, np. occupation-zawod).

#install.packages("dplyr")
library(dplyr)

plik <- read.csv("usersAccounts.csv")
df <- as.data.frame(plik)
View(df)

#dfs <- df[order(-df$saldo),]

dff <- filter(df,occupation=="NAUCZYCIEL")
dfs <- arrange(dff,desc(saldo))
top <- head(dfs,10)
top

rankAccount <- function(dataFrame, colName, groupName, valueSort, num){
  dff <- filter(dataFrame, occupation==groupName); class(dff)
  dfs <- arrange(dff, desc(valueSort))
  top <- head(dfs, num)
  top
}
rankAccount(dataFrame=df, colName=occupation, groupName="NAUCZYCIEL", valueSort=saldo, num=10)

#Zadanie 2.
#Tak jak w zad. 1 tylko z uzyciem datachunku. 
#Przyklad naglowka:
#rankAccountBigDatatoChunk(filename = "usersAccounts.csv", 1000, "occupation", "NAUCZYCIEL", "saldo", 10)

#Zadanie 3.
#Sprawdzic czy da sie zrobic to samo w zapytaniu SQL dla takich wartosci jak: tabelaZbazyDanych, occupation, nauczyciel, saldo

library(RSQLite)

dbpath <- "banki.sqlite"
dbconn <- dbConnect(SQLite(),dbpath)
salda_occ_top10 <- dbGetQuery(conn = dbconn, "select * from konta where occupation = 'NAUCZYCIEL' order by saldo desc limit 10")
salda_occ_top10
dbDisconnect(dbconn)



#Zaczytania plikow

#transakcjeSmall
transakcjeSmall <- read.csv("transakcjeSmall.csv",nrows=10)
View(transakcjeSmall)

#konta
konta <- read.csv("konta.csv",nrows=10)
View(konta)

#usersAccounts
usersAccounts <- read.csv("usersAccounts.csv",nrows=10)
View(usersAccounts)


srednia <- function(filePath, columnName, size, sep=","){
  fileConnection <- file(description=filePath, open="r")
  counter <- 0
  suma <- 0
  data <- read.table(fileConnection, nrows=size, header=TRUE, fill=TRUE, sep=sep)
  columnNames <- names(data)
  repeat{
    if(nrow(data)==0){
      break
    }
    data <- na.omit(data)
    suma <- suma+sum(data[[columnName]])
    print(paste0("suma:",suma))
    counter <- counter+nrow(data)
    data <- read.table(fileConnection, nrows=size, header=FALSE, col.names=columnNames, fill=TRUE, sep=sep)
  }
  close.connection(fileConnection)
  suma/counter
}

srednia(filePath = "transakcjeSmall.csv",columnName = "amount", size=100000)
srednia(filePath = "konta.csv",columnName = "saldo", size=100000)
srednia(filePath = "usersAccounts.csv",columnName = "saldo", size=100000)