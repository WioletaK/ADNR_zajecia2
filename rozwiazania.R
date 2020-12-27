#Zadanie 1.
#Utworz funkcje:
#rankAccount <- function(dataFrame, colName, groupName, valueSort, num) 
#ktora bedzie zwracala dla danej tabeli(dataFrame) n wierszy posiadajace najwieksze wartosci(sortowanie po kolumnie valueSort) 
#dla wybranej grupy (konkretna wartosc komorki, np. "NAUCZYCIEL") z kolumny (colName, np. occupation-zawod).

#install.packages("dplyr")
library(dplyr)

#Stworzenie data frame'u
plik <- read.csv("konta.csv")
df <- as.data.frame(plik)
#View(df)
names(df)
#df["age"]

#dff <- filter(df,occupation=="NAUCZYCIEL")
#dfs <- df[order(-df$saldo),]
#dfs <- arrange(dff,desc(saldo))
#top <- head(dfs,10);top

rankAccount <- function(dataFrame, colName, groupName, valueSort, num){
  dff <- filter(dataFrame, dataFrame[colName]==groupName)
  dfs <- arrange(dff, desc(dff[valueSort]))
  top <- head(dfs, num)
  top
}
rankAccount(dataFrame=df, colName="occupation", groupName="NAUCZYCIEL", valueSort="saldo", num=10)
rankAccount(dataFrame=df, colName="occupation", groupName="PROGRAMISTA_JAVA", valueSort="saldo", num=10)
rankAccount(dataFrame=df, colName="name", groupName="Leon", valueSort="saldo", num=10)
rankAccount(dataFrame=df, colName="occupation", groupName="PROGRAMISTA_JAVA", valueSort="age", num=10)


#Zadanie 2.
#Tak jak w zad. 1 tylko z uzyciem datachunku. 
#Przyklad naglowka:
#rankAccountBigDatatoChunk(filename = "usersAccounts.csv", 1000, "occupation", "NAUCZYCIEL", "saldo", 10)

rankAccountBigDatatoChunk <- function(filename, sep=",", size, colName, groupName, valueSort, num){
  fileConnection <- file(description=filename, open="r")
  counter <- 0
  data <- read.table(fileConnection, nrows=size, header=TRUE, fill=TRUE, sep=sep)
  columnNames <- names(data)
  repeat{
    if(nrow(data)==0){
      break
    }
    data <- na.omit(data)
    dff <- filter(data, data[colName]==groupName)
    dfs <- arrange(dff, desc(dff[valueSort]))
    if(counter==0){
      top <- head(dfs, num)
      top10 <- head(dfs, num)
    }
    else{
      top10 <- head(dfs, num)
      top <- rbind(top,top10)
      top10 <- head(arrange(top, desc(top[valueSort])), num)
    }
    counter <- counter+nrow(data)
    print(paste0("Zaczytano ",counter," wierszy."))
    data <- read.table(fileConnection, nrows=size, header=FALSE, col.names=columnNames, fill=TRUE, sep=sep)
  }
  close.connection(fileConnection)
  top10
}

rankAccountBigDatatoChunk(filename="usersAccounts.csv", sep=",", size=10000, colName="occupation", groupName="NAUCZYCIEL", valueSort="saldo", num=10)
rankAccountBigDatatoChunk(filename="konta.csv", sep=",", size=10000, colName="occupation", groupName="NAUCZYCIEL", valueSort="saldo", num=10)
rankAccountBigDatatoChunk(filename="transakcjeSmall.csv", sep=",", size=10000, colName="title", groupName="Wyplata", valueSort="amount", num=10)
#rankAccountBigDatatoChunk(filename="transakcjeSmall.csv", sep=",", size=10000, colName="title", groupName="CZYNSZ", valueSort="amount", num=10)
#rankAccountBigDatatoChunk(filename="transakcjeSmall.csv", sep=",", size=10000, colName="title", groupName="ABONAMENT", valueSort="amount", num=10)
#rankAccountBigDatatoChunk(filename="transakcjeSmall.csv", sep=",", size=10000, colName="title", groupName="RATA", valueSort="amount", num=10)
#rankAccountBigDatatoChunk(filename="transakcjeSmall.csv", sep=",", size=10000, colName="title", groupName="FAKTURA", valueSort="amount", num=10)
#rankAccountBigDatatoChunk(filename="transakcje.csv", sep=",", size=100000, colName="title", groupName="Wyplata", valueSort="amount", num=10)


#Zadanie 3.
#Sprawdzic czy da sie zrobic to samo w zapytaniu SQL dla takich wartosci jak: tabelaZbazyDanych, occupation, nauczyciel, saldo

library(RSQLite)

dbpath <- "banki.sqlite"
dbconn <- dbConnect(SQLite(),dbpath)
salda_occ_top10 <- dbGetQuery(conn = dbconn, "select * from konta where occupation = 'NAUCZYCIEL' order by saldo desc limit 10")
salda_occ_top10
dbDisconnect(dbconn)