#source('F:/USF/SDM/Leada/chk.R')

#Function to check matching combinations of different fields
chkFull <- function(x,y){
  library(stringr)
  checkFull = vector()
  for(i in x){
    if(str_sub(i,-2)=="NA" | str_sub(i,1,2)=="NA" | str_sub(i,-2)=="  " |str_sub(i,-1)==" "){
      checkFull = c(checkFull,0)}
    else{
      checkFull = c(checkFull,match(i,y,nomatch=0))}
  }
  checkFull
}

#Function to check individual variables.
chk <- function(x,y){
  z = vector()
  for(i in x){
    if(is.na(i) | i=="")
    {z = c(z,0)}  
    else
      z = c(z,match(i,y,nomatch=0))
  }
  z }

#Read both sheets
aban = read.csv("Abandoned_Data_Seed.csv",header = T)

rsvn = read.csv("Reservation_Data_Seed.csv")#,na.strings = "")

#Check on email match
emailMatch = chk(rsvn$Email,aban$Email)

#Subset
emailMatch[emailMatch>1]



#Check on Contact Phone Match
#phoneMatch = match(rsvn$Contact_Phone,aban$Contact_Phone,nomatch=0)
phoneMatch = chk(rsvn$Contact_Phone,aban$Contact_Phone)
#Subset - Potentially matching customers
phoneMatch[phoneMatch>1]

#Replace all missing values to NA in both datasets
aban[aban==""]=NA
rsvn[rsvn==""]=NA

#Creating Concatenated Vectors of FirstName, LastName and Zip
rsvn.Full = paste(rsvn$First_Name,rsvn$Last_Name,rsvn$Zipcode)
aban.Full = paste(aban$First_Name,aban$Last_Name,aban$Zipcode)

#Comparing Full
#source('F:/USF/SDM/Leada/chkFull.R')
FullMatch  = chkFull(rsvn.Full,aban.Full)

#Subesetting to find hits
FullMatch[FullMatch>0] #Gives no Records
#Subset
FullMatch[FullMatch>1]

#Creating Concatenated Vectors LastName,Incomming Phone, considering the fact that 2 different 
#people from the same family making a reservation using the same phone line
rsvn.incph = paste0(rsvn$Last_Name,rsvn$Incoming_Phone)
aban.incph = paste0(aban$Last_Name,aban$Incoming_Phone)



checkInc = chkFull(rsvn.incph,aban.incph)
checkInc[checkInc>0]

#We find one record through this and use which to check it in the reservation sheet
which(checkInc>0)
#To confirm the same record in abandoned sheet

aban[4276,]

#We will now remove duplicates from all of the 4 above
#1. checkInc
#2. 
m = matrix(data = which(emailMatch>1),ncol=1)
m = cbind(m,emailMatch[emailMatch>1])

phone_mat = matrix(which(phoneMatch>1),ncol = 1)
phone_mat= cbind(phone_mat,phoneMatch[phoneMatch>1])

m = rbind(m,phone_mat)

#Adding the checkInc (LastName + Incomming Phone) pair to our matrix m - 264 rows
m = rbind(m,matrix(c(which(checkInc>0),checkInc[checkInc>1]),nrow=length(which(checkInc>0))))

colnames(m) = c("Reservation Sheet Index","Abandoned Sheet Index")
m = unique(m) #Deletes duplicate entries remain with 249 records
m = cbind(m,duplicated(m[,2])) #Delete duplicate values in abandoned column
m = m[m[,3]==FALSE,,drop=FALSE]
m= m[,1:3]



#61 records that have no last name and incomming phone detail - ignore this.

duplicated(phoneMatch[phoneMatch!=103 & phoneMatch !=0],emailMatch[emailMatch>1])


#Cleaned DataSet 
finalds = aban[m[,2],]

#
m = cbind(m,duplicated(m[,2]))

#Inititalize new column in aban which indicates buy / din't buy
aban$buy = 0
for(i in m[,2]) aban$buy[i] = 1

#Subsetting corr Statewise
subset(aban$Test_Control,aban$Address=="FL")

#Number of rows in Abandoned sheet separated as Test and Control
tests = nrow(aban[aban$Test_Control=="test",]) #4065 tests
controls = nrow(aban[aban$Test_Control=="control",]) #4128 controls



#We check the number of test and control groups in this Final Dataset
ntest = nrow(finalds[finalds$Test_Control=="test",]) 
#181 test

ncontrol = nrow(finalds[finalds$Test_Control=="control",])
#42 test



#test_prop = ntest/nrow(finalds)
conv_rate_test = ntest/tests

#control_prop = ncontrol/nrow(finalds)
conv_rate_control = ncontrol/controls


test_statistic = conv_rate_test- conv_rate_control
pooled_prop = (ntest+ncontrol)/nrow(aban)

std_error = sqrt(pooled_prop*(1-pooled_prop)*(1/tests + 1/controls))
z_score = test_statistic/std_error

p_value = pnorm(z_score,lower.tail = F)

control_prop
test_prop