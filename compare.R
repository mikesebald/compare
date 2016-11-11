library(data.table)
library(stringdist)
library(rCharts)

setwd("~/R/compare")
#setwd("e:/R/compare")

# assumption: merged and raw record files have the same number of columns
cols <- read.table("../compare_data/fieldfile_merged.txt", stringsAsFactors = FALSE)
cols_merged <- gsub("person.0.", "", as.vector(cols$V1))
cols <- read.table("../compare_data/fieldfile_raw.txt", stringsAsFactors = FALSE)
cols_raw <- gsub("person.0.", "", as.vector(cols$V1))

ncols <- nrow(cols)

# the data used here is a mongoexport from t02
system.time(dt_merged <- fread("../compare_data/record_actual_merged_subs.csv", sep = ",", header = TRUE, na.strings = NULL, encoding = "UTF-8", colClasses = rep("character", ncols)))
system.time(dt_raw <- fread("../compare_data/record_actual_raw_subs.csv", sep = ",", header = TRUE, na.strings = NULL, encoding = "UTF-8", colClasses = rep("character", ncols)))

kpi1 <- nrow(dt_merged)
kpi2 <- nrow(dt_raw)

system.time(df_join <- as.data.frame(merge(dt_merged, dt_raw, by.x = "merged.key", by.y = "raw.key")))

colnames(df_join)[1:ncols] <- cols_merged
colnames(df_join)[(ncols + 1):(ncols * 2 - 1)] <- cols_raw[c(1, 3:ncols)]

kpi3 <- nrow(df_join)


# the following creates a boolean vector per field comparison. Each vector indicates if there 
# is a difference in the respective field, so each vector identifies the records with differences in given name, 
# gender, salutation and so on. Each vector is cbound to the data frame, starting at 2*ncols.
# columns 1 to 3 just contain source, key and type so nothing to compare here
# thanks to the post in http://datascienceplus.com/strategies-to-speedup-r-code/ this piece is now 150x faster than 
# using apply which was unnecessary anyway

isEqual <- vector(length = nrow(df_join))
system.time(
for (j in 4:ncols) {
  isEqual <- df_join[j] == df_join[j + ncols - 1]
  df_join <- cbind(df_join, isEqual)
})


# This is the distance between a merged column and its corresponding same/different vector
col_dist <- 2 * ncols - 4

# assign proper column names
system.time(for (j in 4:ncols) {
  colnames(df_join)[j + col_dist] <- paste("equal_", gsub("raw.", "", cols_raw[j]), sep = "")
})

# print number of differences
# calculation can be done simpler, see below!
for (j in 4:ncols) {
  num_diff <- sum(!df_join[j + col_dist])
  print(paste("Number of differences for column", j, gsub("merged.", "", colnames(df_join)[j]), ":", num_diff))
}

# Lets identify the rows where there aren't any differences ...
system.time(goods <- which(rowSums(!df_join[(ncols * 2): ncol(df_join)]) == 0))
system.time(bads <- which(rowSums(!df_join[(ncols * 2): ncol(df_join)]) > 0))

# ... and separate both data sets
system.time(df_bad <- df_join[bads,])
system.time(df_goods <- df_join[goods,])

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
col_names <- c("Gender", "Form of Address", "Qualification Preceeding", "Given Names Initial", "Given Names Full", "Surname Prefix First",
               "Surname First", "Qualification Intermediate First", "Middle Name", "Qualification Intermediate Second", "Surname Prefix Second",
               "Surname Second", "Qualification Succeeding", "Indicator", "Name Qualifier")

df_col_sums <- as.data.frame(col_sums)
df_col_sums <- cbind(df_col_sums, col_names)

colnames(df_col_sums) <- c("Attribute", "Difference")
attrib_diff <- nPlot(Attribute ~ Difference, df_col_sums)
attrib_diff$save("attrib_diff.html", cdn = TRUE)
attrib_diff$show(cdn = TRUE)
#attrib_diff$show("server")
attrib_diff$show(static = FALSE)
cat('<iframe src="attrib_diff.html" width=100%, height=600></iframe>')

#barplot(col_sums)


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
