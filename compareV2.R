library(data.table)
library(ggplot2)
library(plotly)
library(dplyr)
library(xlsx)

# ----------------------------------------------------------------------------
# we assume that we compare valid records from both data sets only. Invalid 
# records haven't been touched by the system, so there is no point in 
# comparing them
# ===> reworking this one currently!
# !!! 15.4 does not provide us with filled status columns in the CSV export per
# entity

# ----------------------------------------------------------------------------
# reading both files and compare column names to identify differences in the
# CSV export structure. So we are creating a data frame with the header names
# from the CSV exports. The data frame helps us to identify differences and
# to rearrange if necessary

#setwd("e:/R/compare_data/")
setwd("../compare_data/")
file.member.a <- "valid-15.4.csv"
file.member.b <- "valid-16.1.csv"
file.all.b <- "member-16.1.csv"

sourcename.a <- "CDH 15.4"
sourcename.b <- "CDH 16.1"

# determine the number of columns per file using
# head -n 1 <filename> |grep -o "\," |wc -l
ncol.member.a <- 113
ncol.member.b <- 116

system.time(
  dt.member.all.a <- fread(
    file.member.a,
    sep = ",",
    header = TRUE,
    na.strings = NULL,
    encoding = "UTF-8",
    colClasses = rep("character", ncol.member.a)
  )
)
system.time(
  dt.member.all.b <- fread(
    file.member.b,
    sep = ",",
    header = TRUE,
    na.strings = NULL,
    encoding = "UTF-8",
    colClasses = rep("character", ncol.member.b)
  )
)

colnames.member.a <- colnames(dt.member.all.a)
colnames.member.b <- colnames(dt.member.all.b)

length(colnames.member.a) <- max(length(colnames.member.a), 
                                 length(colnames.member.b))
length(colnames.member.b) <- max(length(colnames.member.a), 
                                 length(colnames.member.b))

column.member.names <- data.frame(colnames.member.a,
                                  colnames.member.b,
                                  colnames.member.a == colnames.member.b,
                                  stringsAsFactors = FALSE)

# ----------------------------------------------------------------------------
# this is the point where we should look at the source and rearrange,
# if necessary. Both data sets whould have the same columns

# View(column.member.names)
sum(!column.member.names[,3])
sum(!column.member.names[,3], na.rm = TRUE)


# ----------------------------------------------------------------------------
# lets eliminate the columns we don't need. RStudio's "View" has a limit of 100
# columns...
# ... and let's take a look at it again
#
# we subset on record source and key as well as postal_address.
#
# POSTAL ADDRESS only from now on !!!

relevant.member.columns <- c(1:2, 63:82)
dt.member.a <- dt.member.all.a[, relevant.member.columns, 
                               with = FALSE]
dt.member.b <- dt.member.all.b[, relevant.member.columns, 
                               with = FALSE]

rm(dt.member.all.a, dt.member.all.b)

colnames.member.a <- colnames(dt.member.a)
colnames.member.b <- colnames(dt.member.b)

column.member.names <- data.frame(colnames.member.a,
                                  colnames.member.b,
                                  colnames.member.a == colnames.member.b,
                                  stringsAsFactors = FALSE)
# View(column.member.names)


# ----------------------------------------------------------------------------
# - which IDs are not unique?
# - the number of unique IDs (incl. source system)
#
# In this data model, CDH allows multiple instances of the same entity type, 
# e.g. multiple "work" email addresses

count.a <- data.table(count(dt.member.a, record.key))
nrow(count.a[n > 1])

count.b <- data.table(count(dt.member.b, record.key))
nrow(count.b[n > 1])

unique.keys.a <- unique(dt.member.a,
                        by = c("record.source", "record.key"))$record.key
unique.keys.b <- unique(dt.member.b,
                        by = c("record.source", "record.key"))$record.key

# just an example here
#View(dt.member.a[record.key == "1001006632"])

# ----------------------------------------------------------------------------
# lets check for the number of rows and compare them

nrow.a <- nrow(dt.member.a)
nrow.b <- nrow(dt.member.b)
max.rows <- max(nrow.a, nrow.b)
total.rows <- data.frame(c(sourcename.a, sourcename.b),
                         c(nrow.a, nrow.b),
                         c((max.rows - nrow.a), (max.rows - nrow.b)),
                         stringsAsFactors = FALSE)
colnames(total.rows) <- c("system", "rows", "difference")

# plot.rows <- plot_ly(total.rows, x = system, y = rows, 
#                      name = "Valid Rows", type = "bar") %>%
#   add_trace(x = system, y = difference, name = "Difference") %>%
#   config(displayModeBar = F)
# layout(plot.rows, barmode = "stack", legend = list(x = 1.0, y = 0.5))


# ----------------------------------------------------------------------------
# so now we know if there is a difference in the number of valid rows It is
# just the total number. There might be different records validated in system A 
# and system B so there might be different sets of data valid.
# So which records are valid in BOTH systems and how many of them are there?
# However: a record can be valid in both system A and B but with a different 
# validation result.
# Before we start joining, we have to keep in mind that each party can have 
# multiple addresses, phone numbers and other entities. So the records get 
# "exploded" i.e. we have a lot of redundant data per party.
#
# TODO: If we have become very clever with R and JSON we may switch from the 
#       CSV/line based apporoach to an unstructured/document style.
#
# Also we want to measure the difference in address validation and name 
# validation, so it might be the right time now to split the data sets. In order
# to do so, we review the column.names data frame created above and select the 
# relevant columns for subsetting
#
# TODO: Investigate how the selection of the relevant columns to subset can be 
# automated i.e for different CDH data models

# ----------------------------------------------------------------------------
# let's start with addresses and exclude the rows without an address type and 
# then let's get rid of duplicates

dt.member.a[with = FALSE] %>%
  subset(postal_address.type != "") %>%
  unique() -> address.a

dt.member.b[with = FALSE] %>%
  subset(postal_address.type != "") %>%
  unique() -> address.b

nrow.address.a <- nrow(address.a)
nrow.address.b <- nrow(address.b)

max.rows <- max(nrow.address.a, nrow.address.b)
address.rows <- data.frame(c(sourcename.a, sourcename.b),
                           c(nrow.address.a, nrow.address.b),
                           c((max.rows - nrow.address.a),
                             (max.rows - nrow.address.b)),
                           stringsAsFactors = FALSE)
colnames(address.rows) <- c("system", "rows", "difference")

# plot.address.rows <- plot_ly(address.rows, x = system, y = rows, 
#                              name = "Records with Addresses", type = "bar") %>%
#   add_trace(x = system, y = difference, name = "Difference")
# layout(plot.address.rows, barmode = "stack")


# ----------------------------------------------------------------------------
# then we should do the same for names and throw away the original input to 
# make some memory available
# TODO: name extraction from files
# TODO: investigate how much sense a call to rm() makes
rm(dt.member.a, dt.member.b)

# ----------------------------------------------------------------------------
# now we have deduplicated unique addresses per source system and are ready to 
# (INNER) join both data sets

setkeyv(address.a, c("record.source", "record.key", "postal_address.type"))
setkeyv(address.b, c("record.source", "record.key", "postal_address.type"))
system.time(address.ab <- merge(address.a, address.b))

# cleaning up the column names which have been messed up during the merge, make
# sure they get a correct suffix
colnames(address.ab) <- gsub("\\.x$", ".a", colnames(address.ab))
colnames(address.ab) <- gsub("\\.y$", ".b", colnames(address.ab))

nrow.address.ab <- nrow(address.ab)

# ----------------------------------------------------------------------------
# it would also be interesting to identify the records which are either valid in 
# system A or B, but not in both systems
# LEFT OUTER join excluding everything on the right. Adding column with X to 
# have something to filter on
#
# To answer the "why" question we should now look into the respective CDH 
# system. Alternatively we could check the full export which includes the 
# invalid records.

temp <- cbind(address.b, x = "X")
merge(address.a, temp, all.x = TRUE) %>%
  subset(is.na(x)) -> uniques.a

temp <- cbind(address.a, x = "X")
merge(address.b, temp, all.x = TRUE) %>%
  subset(is.na(x)) -> uniques.b

rm(temp)

differents <- data.frame(system = c(sourcename.a, sourcename.b),
                         counts = c(nrow(uniques.a), nrow(uniques.b)))

# plot.differents <- plot_ly(differents, x = system, y = counts, 
#                            name = "Records which are valid in just one system", 
#                            type = "bar")
# layout(plot.differents, barmode = "stack")

# optionally write to Excel to look at these records
#write.xlsx(uniques.a, file = "uniques.a.xls")
#write.xlsx(uniques.b, file = "uniqies.b.xls")

#sanity.check <- merge(uniques.a, uniques.b)
#nrow(sanity.check) # -> must return 0

# ----------------------------------------------------------------------------
# If we compare these IDs we see that records fail validation because of 
# different reasons. Record X fails phone validation in system A but passes it
# in system B - regardless of the address.
#
# TODO: what about valid addresses in invalid records? Should we also export
#       invalid records and compare them? How can we export all records with a
#       validated address, regardless of the other entities? Of course, we would
#       have to do the same for names.
#
# For the time being, let's continue with addresses in valid records
#
# Okay, now we want to find out, if we have different address validation results
# in A and B
#
# The following creates a boolean vector per field comparison. Each vector 
# indicates if there is a difference in the respective field, so each vector 
# identifies the records with differences in given name, gender, salutation and 
# so on. Each vector is cbound to the right of the data frame
# columns 1 to 3 just contain source, key and type so nothing to compare here
# so the magic numbers here are 2, 3 and 4
# 2 = times x = the distance between the columns to compare
# 3 = the three columns on the left we don't have to compare (source, key, type)
# 4 = because we start at column 4 to compare fields
# Thanks to the post in http://datascienceplus.com/strategies-to-speedup-r-code/
# for helping on the performance side

# View(data.table(colnames(address.ab)))

ncol.address <- ncol(address.a) - 3
df.address.ab <- as.data.frame(address.ab)
is.equal <- vector(length = nrow(address.ab))
system.time(
  for (j in 4:(ncol.address + 3)) {
    is.equal <- df.address.ab[j] == df.address.ab[j + ncol.address]
    df.address.ab <- cbind(df.address.ab, is.equal)
  }
)

# assign proper names to the comparison columns
col.start <- ncol.address * 2 + 4
col.end <- ncol(df.address.ab)

colnames(df.address.ab)[col.start:col.end] <-
  gsub("\\.a$", ".equal", colnames(df.address.ab[col.start:col.end]))

# some sample comparisons
street.compare <- df.address.ab[, c(1:3, c(0,
                                           ncol.address,
                                           ncol.address * 2) + 4)]
zip.compare <- df.address.ab[, c(1:3, c(0,
                                        ncol.address,
                                        ncol.address * 2) + 6)]
city.compare <- df.address.ab[, c(1:3, c(0,
                                         ncol.address,
                                         ncol.address * 2) + 7)]

# or combined
combined.compare <- df.address.ab[, c(1:3,
                                    c(0, ncol.address, ncol.address * 2) + 4,
                                    c(0, ncol.address, ncol.address * 2) + 6,
                                    c(0, ncol.address, ncol.address * 2) + 7)]

# View(as.data.frame(colnames(df.address.ab)))

# Lets identify the rows where there aren't any differences ...
system.time(same <- which(
  rowSums(!df.address.ab[col.start : col.end]) == 0))

# ... and where we are having differences
system.time(different <- which(
  rowSums(!df.address.ab[col.start : col.end]) > 0))

# Let's separate the good from the bad
system.time(df_different <- df.address.ab[different,])
system.time(df_same <- df.address.ab[same,])

# sanity check: should both be 0
#max(colSums(!df_same[col.start: col.end]))
#max(rowSums(!df_same[col.start: col.end]))


# we can now calculate the number of differences per column i.e. how many 
# difference do we have in ZIP, in city, in hno and so on

col.sums <- colSums(!df_different[col.start:col.end])
col.names <- gsub(".equal", "", colnames(df_different[col.start:col.end])) %>%
  gsub(pattern = "postal_address.", replacement = "") 
df_columns <- data.frame(attributes = col.names, occurences = col.sums)

plot.differents <- plot_ly(df_columns, x = attributes, y = occurences, 
                           name = "Number of Differences per Attributes", 
                           type = "bar")
layout(plot.differents, barmode = "stack")



View(df_different[, c(
  1:3,
  c(0, ncol.address, ncol.address * 2) + 4,
  c(0, ncol.address, ncol.address * 2) + 5,
  c(0, ncol.address, ncol.address * 2) + 6,
  c(0, ncol.address, ncol.address * 2) + 7
)])

# and let's do the rowsums as well (number of different fields per reocrd)
row.sums <- rowSums(!df_different[col.start: col.end])
row.sums







# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# old stuff below


# Let's free up some memory now
remove(isEqual, df_join, dt_merged, dt_raw)

# this one is probably right. HOWEVER: this just creates a new vector which is unrelated to the original data frame. On the other side,
# it doesn't make sense to calculate the distance for equal strings
system.time(d2 <- df_bad[, 10] == df_bad[, 27])

# the following will kill R...
# d <- adist(df_bad[, 10], df_bad[, 27])
# ... so we use a different function
system.time(d <- stringdist(df_bad[, 10], df_bad[, 10 + ncols - 1]))
f <- factor(d, exclude = 0)
plot(f)

# this was just one column, let's concatenate the most imprtant name fields
parts_1 <- c(6:10)
parts_2 <- parts_1 + ncols -1

system.time(a <- apply(df_bad[, parts_1], 1, function (x) { paste(x, collapse = "")}))
system.time(b <- apply(df_bad[, parts_2], 1, function (x) { paste(x, collapse = "")}))

system.time(c <- stringdist(a, b))

View(cbind(a, b, c))

f <- factor(c, exclude = 0)
plot(f)

# we can now calculate the number of differences per column, more efficient than above!
# it'll be ugly, but let's plot them
col_sums <- colSums(!df_bad[(ncols * 2): ncol(df_bad)])
plot(col_sums)

# and lets do the rowsums as well (number of different fields per reocrd)
row_sums <- rowSums(!df_bad[(ncols * 2): ncol(df_bad)])
rf <- factor(row_sums)
plot(rf)

# the following is a list of both raw and validated records, ordered by source system key
dt_all <- rbindlist(list(dt_merged, dt_raw))
setorder(dt_all, merged.key)


print(paste("Number of validated records", kpi1))
print(paste("Number of raw records", kpi2))
print(paste("Number of records joining validated and raw (sanity check only)", kpi1))
