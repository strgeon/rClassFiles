# TODO: Add comment
# 
# Author: Scott
###############################################################################


for(i in 1:10) {
	print(i)
}

print(i)

#To get only the 7th column of each data frame in the list, you can use

lapply(mydatalist, "[", 7)  ## you can also get multiple columns with c(1,5,etc)
#for the column as a data frame, and

lapply(mydatalist, "[[", 7)
#to view it as a vector. To reference it/them by name, you can use the name just like you would the column number

lapply(mydatalist, "[", "qsec")
#To use substr on the column, first you have to access the column. Then you apply can use an anonymous function to do the work with substr.

lapply(mydatalist, function(x) substr(x[[7]], 0, 2)) ## or x[["qsec"]]
# [[1]]
#  [1] "16" "17" "18" "19" "17" "20" "15" "20" "22" "18"
# [11] "18" "17" "17" "18" "17" "17" "17" "19" "18" "19"
# [21] "20" "16" "17" "15" "17" "18" "16" "16" "14" "15"
# [31] "14" "18"
# 
# [[2]]
#  [1] "32" "34" "37" "38" "34" "40" "31" "40" "45" "36"
# [11] "37" "34" "35" "36" "35" "35" "34" "38" "37" "39"
# [21] "40" "33" "34" "30" "34" "37" "33" "33" "29" "31"
# [31] "29" "37"