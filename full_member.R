library(data.table)
library(ggplot2)
library(plotly)
library(reshape)
library(stringr)
library(plyr)

setwd("../compare_data/")
file.b <- "member-16.1.csv"
sourcename.b <- "CDH 16.1"
ncol.member.b <- 116

system.time(
  dt.b <- fread(
    file.b,
    sep = ",",
    header = TRUE,
    na.strings = NULL,
    encoding = "UTF-8",
    colClasses = rep("character", ncol.member.b)
  )
)

#View(data.frame(colnames(dt.b)))

address.columns.b <- c(1, 2, 4, 62, 83)
dt.address.all.b <- dt.b[, address.columns.b, with = FALSE]

nrow.all <- nrow(dt.address.all.b)
#View(data.frame(data.frame(colnames(dt.address.all.b))))

f <- factor(dt.address.all.b$postal_address.status)
table(f)

none <- dt.address.all.b[, postal_address.status, with = TRUE] == ""
valids <- dt.address.all.b[, postal_address.status, with = TRUE] == "valid"
invalids <- dt.address.all.b[, postal_address.status, with = TRUE] == "invalid"

df <- data.frame(all = nrow.all, none = sum(none),
                 valids = sum(valids), invalids = sum(invalids))

dt.address.b <- dt.address.all.b[!none]

# we have to split the "," separated validation messages

dt.address.valids.b <- dt.address.all.b[valids]
dt.address.invalids.b <- dt.address.all.b[invalids]

df.temp <- 
  as.data.frame(
    tstrsplit(dt.address.invalids.b$postal_address.validation_messages,
              split = ",")
    )

colnames(df.temp) <- paste("ValMsg", c(1:ncol(df.temp)), sep = "")

#View(df.temp)

dt <- cbind(dt.address.invalids.b, df.temp)

# max.sep.b <- 
#   max(str_count(dt.address.invalids.b$postal_address.validation_messages, ","))
# valmsgs.b <- 
#   colsplit(dt.address.invalids.b$postal_address.validation_messages, ",",
#            paste("ValMsg", c(1:max.sep.b), sep = ""))
# dt <- cbind(dt.address.invalids.b, 
#             str_count(dt.address.invalids.b$postal_address.validation_messages, 
#                       ","),
#             valmsgs.b)

dt <- dt[, colnames(dt)[3:5] := NULL]
dt <- melt(dt, id.vars = c("record.source", "record.key"))
dt <- unique(dt)

df.valmsgs.b <- data.frame(table(factor(dt$value)))
colnames(df.valmsgs.b) <- c("valmsg", "frequency")
df.valmsgs.b <- arrange(df.valmsgs.b, desc(frequency))
plot.valmsgs <- plot_ly(df.valmsgs.b, 
                        x = valmsg,
                        y = frequency,
                        name = "Postal Address Validation Messages", 
                        type = "bar")
m <- list(l = 50, r = 150, b = 200, t = 50)
y <- list(title = "Häufigkeit")
x <- list(title = "")
layout(plot.valmsgs, barmode = "stack", margin = m, xaxis = x, yaxis = y)

View(df.valmsgs.b)     

     
