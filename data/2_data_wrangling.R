#Match() Function
#Indexing with Logical Vectors
#Which() Function
#Duplicated() Function
class_data <- read.csv('classes.csv', header = T, stringsAsFactors = F)
head(class_data)
match(class_data$A, class_data$B, nomatch = 0)
class_data$A[14] 
class_data$B[29]
class_data$A[23]
class_data$B[20]
matches = class_data$A %in% class_data$B
matches
class_data$A[matches]
quiz_class_data <- read.csv('classes_test.csv', header = T, stringsAsFactors = F)
head(quiz_class_data)
quiz_class_data$A.Year[quiz_class_data$A == 'Trina']
A_B_matches = quiz_class_data$A %in% quiz_class_data$B
quiz_class_data$B[A_B_matches]
B_A_matches = quiz_class_data$B %in% quiz_class_data$A
quiz_class_data$B[B_A_matches]
my_vector = c(1,2,3,4,5)
my_logical_vector = c(TRUE, TRUE, FALSE, FALSE, FALSE)
my_vector[my_logical_vector]
my_vector[!my_logical_vector]
quiz_matches = quiz_class_data$B %in% quiz_class_data$A
quiz_class_data$B[!quiz_matches]
small_class = quiz_class_data$B[1:5] # rows 1 to 5
small_class
logical_vector = c(FALSE, FALSE, TRUE, TRUE, FALSE)
small_class[logical_vector]
class_A = class_data[ ,c(1,2)] # all rows cols 1 and 2
class_A
soph_rows = which(class_A$A.Year == 'Sophomore')
soph_rows
sophomores = class_A[soph_rows, ]
sophomores
# or 
class_A[class_A$A.Year == 'Sophomore',]
which(class_A$A.Year == 'Junior' | class_A$A.Year == 'Senior')
length(quiz_class_data$A[quiz_class_data$A.Year == 'Fifth'])
length(quiz_class_data$A[quiz_class_data$A.Year == 'Junior'])
View(class_data)
class_data$A.Year[1:10]
duplicates = duplicated(class_data$A.Year)
duplicates[1:10]
class_data$A.Year[!duplicates]
sum(duplicates+c(0))
quiz_data_name_dup = duplicated(quiz_class_data$A)
sum(quiz_data_name_dup+c(0))
quiz_data_name_dup
new_class <- c(quiz_class_data$A, "Carl", "Alexis", "Jeffrey", "Ben", "Joshua")
new_class
new_class_dup = duplicated(new_class)
new_class_dup
new_class[new_class_dup]
