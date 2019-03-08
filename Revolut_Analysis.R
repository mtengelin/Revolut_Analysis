# Parameters
EUR <- 1.14
GBP <- 1.28
folder <- "Statements"

# Libraries
library(data.table)
library(stringr)
library(plotly)

# Read the Revolut statement files, use file names of format [person]-[currency].csv
Filenames <- list.files(folder)
Statements <- list()
Persons <- rep(NA, length(Filenames))

for (i in 1:length(Filenames)) {
  Statements[[i]] <- fread(paste(folder,Filenames[i], sep = "/"))
  Persons[i] <- str_replace_all(Filenames[i],".csv","")
  Statements[[i]]$Card <- Persons[i]
  Statements[[i]]$Currency <- substr(names(Statements[[i]][,3]),11,13)
  
  # Remove currency from column names (3,4,7)
  setnames(Statements[[i]], 3 ,"PaidOut")
  setnames(Statements[[i]], 4 ,"PaidIn")
  setnames(Statements[[i]], 7 ,"Balance")
}

# Merge into one
Transactions <- do.call(rbind,Statements)

# Remove strange characters
Transactions$`PaidIn` <- str_replace_all(Transactions$`PaidIn`, "[^0-9.]","")
Transactions$`PaidOut` <- str_replace_all(Transactions$`PaidOut`, "[^0-9.]","")
Transactions$`Balance` <- str_replace_all(Transactions$`Balance`, "[^0-9.]","")

# Reformat date column, add year if missing (newer version of the app)
Transactions$Date <- as.Date(Transactions$`Completed Date`, format = "%d %B %Y",tz = "GMT") # Replace day month year
Transactions[is.na(Transactions$Date),]$Date <- as.Date(Transactions[is.na(Transactions$Date),]$`Completed Date`, format = "%Y %B %d",tz = "GMT") # Replace year month day
Transactions[is.na(Transactions$Date),]$Date <- as.Date(Transactions[is.na(Transactions$Date),]$`Completed Date`, format = "%B %d",tz = "GMT") # Replace year month day

# Reformat columns
Transactions$`Completed Date` <- NULL
Transactions$PaidOut <- as.numeric(Transactions$PaidOut)
Transactions$PaidIn <- as.numeric(Transactions$PaidIn)
Transactions$Balance <- as.numeric(Transactions$Balance)


# Add column for month (yyyy-mm)
Transactions$Month <- substr(Transactions$Date, 1, 7)

# Add column for CHF equivalent
Transactions$PaidOutCHF <- Transactions$PaidOut
Transactions[Currency=="EUR"]$PaidOutCHF <- Transactions[Currency=="EUR"]$PaidOut*EUR
Transactions[Currency=="GBP"]$PaidOutCHF <- Transactions[Currency=="GBP"]$PaidOut*GBP

# Save CSV file with all transactions
fwrite(Transactions, "All_Transactions.csv")

# Create Expenses (Transactions minus transfers)
Expenses <- Transactions[Category != 'transfers']

# Save CSV file with all transactions
fwrite(Expenses, "All_Expenses.csv")

