for(i in 1:5) {
  print(i)
}
evens = c(2,4,5,6,8,10)
for (j in evens) {
  print(j)
}
if(5 > 3) {
  print("Statement 1 is true")
} else if(234 > 100) {
  print("Statement 2 is true")
} else {
  print("None of the statements are true")
}
bad_numbers = c(4,23, 55, 89)
1 %in% bad_numbers
4 %in% bad_numbers
for(i in 1:100) {
  if(i %in% bad_numbers) {
    #do nothing so we'll just leave it blank
  } else {
    print(i)
  }
}
new_startup_data <- read.csv('second_crunchbase.csv', header = T, stringsAsFactors = F)
years = new_startup_data$founded_year[1:50]
for(i in years) {
  if(is.na(i)) {
    print('Information Not Available')
  } else if(i > 2009) {
    print('Early Stage')
  } else {
    print('Late Stage')
  }
}
year_tag = function(dataset, rows=1:50) {
  years = dataset$founded_year[rows]
  for(i in years) {
    if(is.na(i)) {
      print('Information Not Available')
    } else if(i > 2009) {
      print('Early Stage')
    } else {
      print('Late Stage')
    }
  }
}
year_tag(new_startup_data) #since we are using the default rows we don't need to include the argument
summer = function(N) {
  sum = 0
  for (i in 1:N) {
    sum = sum + i
  }
  return(sum)
}
summer(3455)
vector_three = c(1, "a", "b", 3)
vector_four = c("a","b",T,F,TRUE)
as.logical(c(0,1,343,132,0,34))
as.character(c(1,2,3))
as.character(c(1,TRUE, FALSE, 26))
a = c(1,2,3,4,5)
b = c("dog", "cat", "mouse")
c = c(T, F, T, T)
mylist = list(a,b,c)
class(mylist[1])
mylist[[1]]
mylist[[1]][1]
class(mylist[[1]])
mylist[[1]][2:3]
for(i in mylist) {
  print(i[1])
}
startup_data <- read.csv('crunchbase_monthly_export.csv', header = T, stringsAsFactors = F)
startup_list = list(startup_data$region, startup_data$funding_rounds, startup_data$market)
startup_list[[2]][1:50]
startup_list[2]
install.packages('datasets')
library('datasets')
head(iris)
class(iris$Species)
iris$Species
help(apply)
iris[ ,1:4]
apply(iris[ ,1:4], 2, mean)
mean(iris$Sepal.Length)
#The apply function can make more sense when you work with each individual observation in a dataset by setting the margin to c(1,2). Suppose we wanted to add 100 to every measurement in iris.
apply(iris[ ,1:4], c(1,2), function(x) x + 100)
head(mtcars)
View(mtcars)
apply(mtcars, 2, mean)
cities = c("New York", "San Francisco", "Los Angeles", "Boston", "London")
head(startup_data)
ny = startup_data[which(startup_data$city == "New York"), ]
sf = startup_data[which(startup_data$city == "San Francisco"), ]
la = startup_data[which(startup_data$city == "Los Angeles"), ]
bo = startup_data[which(startup_data$city == "Boston"), ]
uk = startup_data[which(startup_data$city == "London"), ]
mean(sf$funding_rounds)
mean(sf$founded_year, na.rm = TRUE)
rounds = c(mean(ny$funding_rounds), mean(sf$funding_rounds), mean(la$funding_rounds), mean(bo$funding_rounds), mean(uk$funding_rounds))
year = c(mean(ny$founded_year, na.rm = TRUE), mean(sf$founded_year, na.rm = TRUE), mean(la$founded_year, na.rm = TRUE), mean(bo$founded_year, na.rm = TRUE), mean(uk$founded_year, na.rm = TRUE))
bad_city_data = data.frame(rounds, year)
rownames(bad_city_data) = cities
bad_city_data
city_subsets = list()
for(i in cities) {
  city_subsets[[i]] = startup_data[which(startup_data$city == i), ]
}
city_data = sapply(city_subsets, function(x) c(mean(x$funding_rounds), mean(x$founded_year, na.rm = TRUE)))
