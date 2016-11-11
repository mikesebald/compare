library(data.table)
library(ggplot2)
library(plotly)

# ----------------------------------------------------------------------------
# reading both files and compare column names to identify differences in the
# CSV export structure. So we are creating a data frame with the header names
# from the CSV exports. The data frame helps us to identify differences and
# to rearrange if necessary


#setwd("e:/R/compare_data/")
setwd("../compare_data/")
file.golden.a <- "golden-record-15.4.csv"
file.golden.b <- "golden-record-16.1.csv"

# determine the number of columns per file using
# head -n 1 <filename> |grep -o "\," |wc -l
ncol.golden.a <- 113
ncol.golden.b <- 116

system.time(
  dt.golden.all.a <- fread(
    file.golden.a, sep = ",", header = TRUE,
    na.strings = NULL, encoding = "UTF-8",
    colClasses = rep("character", ncol.golden.a)
  )
)

system.time(
  dt.golden.all.b <- fread(
    file.golden.b, sep = ",", header = TRUE,
    na.strings = NULL, encoding = "UTF-8",
    colClasses = rep("character", ncol.golden.b)
  )
)


colnames.golden.all.a <- colnames(dt.golden.all.a)
colnames.golden.all.b <- colnames(dt.golden.all.b)

length(colnames.golden.all.a) <- max(length(colnames.golden.all.a), 
                                 length(colnames.golden.all.b))
length(colnames.golden.all.b) <- max(length(colnames.golden.all.a), 
                                 length(colnames.golden.all.b))

column.names.golden.all <- data.frame(
  colnames.golden.all.a, colnames.golden.all.b,
  colnames.golden.all.a == colnames.golden.all.b,
  stringsAsFactors = FALSE
)


# this is the point where we should look at the source and rearrange,
# if necessary. Both data sets whould have the same columns
View(column.names.golden.all)

sum(!column.names.golden.all[,3])
sum(!column.names.golden.all[,3], na.rm = TRUE)


# let's delete the columns we do not need. RStudio's "View" has a limit on 
# 100 columns
#
# TODO: needs to be adjusted for 15.4 because of missing member columns in CSV
#

dt.golden.a <- dt.golden.all.a[, c(1:2, 4:5, 9:32, 62:87, 92:98), 
                               with = FALSE]
dt.golden.b <- dt.golden.all.b[, c(1:2, 4:5, 9:32, 62:87, 92:98, 114:116), 
                               with = FALSE]

colnames.golden.a <- colnames(dt.golden.a)
colnames.golden.b <- colnames(dt.golden.b)

#column.names.golden <- data.frame(colnames.golden.a, colnames.golden.b, 
#                                  colnames.golden.a == colnames.golden.b,
#                                  stringsAsFactors = FALSE)
column.names.golden <- data.frame(colnames.golden.b, stringsAsFactors = FALSE)
View(column.names.golden)

# determine the number of golden records per system
total.golden.a <- length(unique(dt.golden.a[[2]]))
total.golden.b <- length(unique(dt.golden.b[[2]]))

# since we do not have the member keys in a 15.4 CSV export, there is probably
# not much more we can do here...

# ...but we can prepare a few things
# determine the number of member records per GR, get the maximum and identify
# the record which the maximum amount of members
member.keys.b <- strsplit(dt.golden.b$member.key, "\\,")
num.members.b <- sapply(member.keys.b, length)

dt.member.keys.b <- data.table(keys = dt.golden.b$member.key, 
                               num = as.factor(num.members.b))

# remove the singletons (that's a lot)
dt.member.keys.wo1.b <- subset(dt.member.keys.b, num != 1)
members.fact.b <- data.frame(table(dt.member.keys.wo1.b$num))
plot.members.fact.wo1.b <- plot_ly(members.fact.b, x = Var1, y = Freq, 
                               name = "Members per Golden Record", type = "bar")
layout(plot.members.fact.wo1.b)

# remove the ones with 2 members (also a lot)
dt.member.keys.wo2.b <- subset(dt.member.keys.wo1.b, num != 2)
members.fact.b <- data.frame(table(dt.member.keys.wo2.b$num))
plot.members.fact.wo2.b <- plot_ly(members.fact.b, x = Var1, y = Freq, 
                               name = "Members per Golden Record", type = "bar")
layout(plot.members.fact.wo2.b)

#now which is the guy which has 20 members? or 5?
which(dt.member.keys.b$num == 20)
which(dt.member.keys.b$num == 5)

View(dt.golden.b[which(dt.member.keys.b$num == 20), ])
View(dt.golden.b[which(dt.member.keys.b$num == 5), ])


#next step would be to compare 15.4 with 16.1 but not sure we will get the 
# members in a 15.4 CSV export
