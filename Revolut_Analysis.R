# Libraries
library(data.table)
library(stringr)
library(plotly)

ReadStatements <- function(folder){
    # Read the Revolut statement files, use file names of format [person].csv
    Filenames <- list.files(folder)
    Statements <- list()
    Persons <- rep(NA, length(Filenames))
    
    for (i in 1:length(Filenames)) {
        Statements[[i]] <- fread(paste(folder,Filenames[i], sep = "/"))
        Persons[i] <- str_replace_all(Filenames[i],".csv","")
        Statements[[i]]$Card <- Persons[i]
    }
    
    # Merge into one
    Transactions <- do.call(rbind,Statements)
    
    # Remove strange characters
    Transactions$`Paid In (CHF)` <- str_replace_all(Transactions$`Paid In (CHF)`, "[^0-9.]","")
    Transactions$`Paid Out (CHF)` <- str_replace_all(Transactions$`Paid Out (CHF)`, "[^0-9.]","")
    Transactions$`Balance (CHF)` <- str_replace_all(Transactions$`Balance (CHF)`, "[^0-9.]","")
    
    # Reformat columns
    Transactions$Date <- as.Date(Transactions$`Completed Date`, format = "%d %B %Y",tz = "GMT")
    Transactions$PaidOut <- as.numeric(Transactions$`Paid Out (CHF)`) 
    Transactions$PaidIn <- as.numeric(Transactions$`Paid In (CHF)`) 
    Transactions$Balance <- as.numeric(Transactions$`Balance (CHF)`) 
    Transactions$`Completed Date` <- NULL
    Transactions$`Paid Out (CHF)` <- NULL
    Transactions$`Paid In (CHF)` <- NULL
    Transactions$`Balance (CHF)` <- NULL
    
    # Add column for month (yyyy-mm)
    Transactions$Month <- substr(Transactions$Date, 1, 7)
    
    return(Transactions)
}

PlotExpenses <- function(Expenses) {
    # Plot the expenses, either total or per person
    Persons <- unique(Expenses$Card)
    p <- plot_ly(Expenses, x = ~Month, y = ~PaidOut, type = 'bar', 
                 name = ~Category, color = ~Category,
                 marker = list(line = list(color = 'rgb(8,48,107)',width = 0.5))) %>%
        layout(yaxis = list(title = 'CHF'), barmode = 'stack', title = Persons)
    print(p)
}

# Read all statements in "Statements" folder
Transactions <- ReadStatements("Statements")

# Save CSV file with all transactions
write.csv2(Transactions, "All_Transactions.csv", row.names = FALSE)

# Create Expenses (Transactions minus transfers)
Expenses <- Transactions[Category != 'transfers']

# Aggregate expenses by category, month and card for plotting
Expenses <- Expenses[, sum(PaidOut), .(Category, Month, Card)]
names(Expenses)[4]<-"PaidOut"

# Plot monthly summary
PlotExpenses(Expenses)

# Plot monthly summary by card
Persons <- unique(Expenses$Card)
for (i in 1:length(Persons)) {
    PlotExpenses(Expenses[Card == Persons[i]])
}

