DATA_DIR<-"~/Desktop/zantiks/zantiks_test" #specify directory of the zantiks datafile
setwd(DATA_DIR) #set it as working directory
list.files(pattern= "*.csv") #check that your file is in the directory

#install.packages(readr) #if you do not have readr installed
library(readr) #load readr package 
predam <- read_csv("ladybug.csv", col_names = FALSE) #import the zantiks datafile into R (only first 4 columns are imported)
#install.packages(data.table) #if you do not have data.table installed
library(data.table) #load data.table package
setDT(predam) #convert the file into data.table
#class(predam) 

datetime<-as.POSIXct(predam$X3,format="%Y-%m-%d %H:%M:%S") #extract datetimes from the header of zantiks data
datetime<-na.omit(unique(datetime))
datetime
class(datetime)

timerows<-predam[, .N-1] #find the last row of experimental data
timetoadd<-predam[5:timerows,1] #collect runtime for the rows containing experimental data
timetoadd<-unlist(timetoadd) #unlist the runtime values
timetoadd<-as.numeric(timetoadd) #convert the values to numeric
timetoadd
#class(timetoadd)
addedtimedate<-datetime[1]+timetoadd #add runtime values in s to the first datetime (start of experiment)
#addedtimedate
#class(addedtimedate)

dateonly<-strftime(addedtimedate, format="%d-%m-%y") #extract date from the timedate+runtime
head(dateonly)
class(dateonly)
timeonly<-strftime(addedtimedate, format = "%H:%M:%S") #extract time from the timedate+runtime
class(timeonly)

predamcut <- read_csv("ladybug.csv", skip = 3) #read the zxantiks file again but ignore the header
#and include all the columns
setDT(predamcut) #convert the file into a data.table
lastrow<-predamcut[, .N-1] #find the last row of experimental data
predamcut<-predamcut[1:lastrow,] #cut the last row
#the following is specific to the format recorded by the ladybird experiment, wikll adjust to fit the standardized format
predamcut[,"TIME"]<-predamcut[,"SERIES"] #replace "TIME" column with "SERIES" (counter)
predamcut[, "TEMPERATURE"]<-dateonly #replace the "TEMPERATURE" column with the date values
predamcut[, "VARIABLE"]<-timeonly #replace the "VARIABLE" column with the time values
predamcut[, "TREATMENT"]<-1 #replace "TREATMENT" column with 1s to indicate monitor activity as required by the DAM format
predamcut[, "SERIES"]<-0 #replace "SERIES" column with 0s to fill a utility column as required by the DAM format
#View(predamcut)

firstpart<- predamcut[,1:5] #extract the first 5 cplumns (counter, date, time, 1, 0)
setDF(firstpart) #convert to data.frame
firstpart[c("D1","D2","D3","D4","D5")]<-0 #add extra 5 columns of 0s to get to 10 utility columns
secondpart<-predamcut[,6:17] #extract the last 12 columns of data (distance travelled)
setDF(secondpart) #convert to data.frame
secondalt<-predamcut[,19:30] #extract the last 12 columns of data (arena activity)


#View(secondalt)
completealt<-cbind(firstpart,secondalt) #bind utility columns to data columns (for arena activity)
#View(completealt)
complete<-cbind(firstpart,secondpart) #bind utility columns to data columns (for distance travelled)
complete[c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")]<-0 
#complete the frame to 42 columns with 0s
# View(complete)
# View(predam)


write.table(completealt, file = "postdamalt.txt", sep = "\t", row.names = FALSE, col.names = FALSE) #save the file as
#tab delimited .txt for arena activity
write.table(completealt, file = "postdamt.txt", sep = "\t", row.names = FALSE, col.names = FALSE) #save the file as
#tab delimited .txt for distance travelled


#had some issues with formatting, hopefully this is a temporary fix
#for arena activity
postdam <- readLines("postdamalt.txt") #load the new .txt file
postdam <- gsub(pattern = '"', replace= "", x= postdam) #get rid of quotes
postdam <- gsub(pattern = '-01-', replace= " Jan ", x= postdam) #convert month from mm to mmm
postdam <- gsub(pattern = '-02-', replace= " Feb ", x= postdam)
postdam <- gsub(pattern = '-03-', replace= " Mar ", x= postdam)
postdam <- gsub(pattern = '-04-', replace= " Apr ", x= postdam)
postdam <- gsub(pattern = '-05-', replace= " May ", x= postdam)
postdam <- gsub(pattern = '-06-', replace= " Jun ", x= postdam)
postdam <- gsub(pattern = '-07-', replace= " Jul ", x= postdam)
postdam <- gsub(pattern = '-08-', replace= " Aug ", x= postdam)
postdam <- gsub(pattern = '-09-', replace= " Sep ", x= postdam)
postdam <- gsub(pattern = '-10-', replace= " Oct ", x= postdam)
postdam <- gsub(pattern = '-11-', replace= " Nov ", x= postdam)
postdam <- gsub(pattern = '-12-', replace= " Dec ", x= postdam)
postdam <- gsub("([0-9])([A-Z])", "\\1 \\2", postdam, ignore.case = TRUE) #add spaces between numbers and letters
postdam <- gsub("([a-z])([0-9])", "\\1 \\2", postdam, ignore.case = TRUE) #to separate yymmmdd to yy mmm dd
#head(postdam)

writeLines(postdam, con="postdamalt1.txt") #save the .txt monitor file

#same but for distance travelled
postdam <- readLines("postdam.txt") #load the new .txt file
postdam <- gsub(pattern = '"', replace= "", x= postdam) #get rid of quotes
postdam <- gsub(pattern = '-01-', replace= " Jan ", x= postdam) #convert month from mm to mmm
postdam <- gsub(pattern = '-02-', replace= " Feb ", x= postdam)
postdam <- gsub(pattern = '-03-', replace= " Mar ", x= postdam)
postdam <- gsub(pattern = '-04-', replace= " Apr ", x= postdam)
postdam <- gsub(pattern = '-05-', replace= " May ", x= postdam)
postdam <- gsub(pattern = '-06-', replace= " Jun ", x= postdam)
postdam <- gsub(pattern = '-07-', replace= " Jul ", x= postdam)
postdam <- gsub(pattern = '-08-', replace= " Aug ", x= postdam)
postdam <- gsub(pattern = '-09-', replace= " Sep ", x= postdam)
postdam <- gsub(pattern = '-10-', replace= " Oct ", x= postdam)
postdam <- gsub(pattern = '-11-', replace= " Nov ", x= postdam)
postdam <- gsub(pattern = '-12-', replace= " Dec ", x= postdam)
postdam <- gsub("([0-9])([A-Z])", "\\1 \\2", postdam, ignore.case = TRUE) #add spaces between numbers and letters
postdam <- gsub("([a-z])([0-9])", "\\1 \\2", postdam, ignore.case = TRUE) #to separate yymmmdd to yy mmm dd
#head(postdam)

writeLines(postdam, con="postdam1.txt") #save the .txt monitor file

#install rethomics packages if not installed already
#install.packages("behavr")
#install.packages("damr")
#install.packages("sleepr")
#install.packages("zeitgebr")
#install.packages("ggetho")
library(damr) #load damr package

metad2<-data.table(file = rep("postdamalt.txt", times = 12), start_datetime = rep(addedtimedate[1], times = 12),
                   stop_datetime = rep(addedtimedate[lasttime], times = 12), region_id = rep(1:12))
#create a metadata data.table (fil, start_datetime,stop_datetime, region_id) for 12 subjects
#more variables can be added: condition, genotype, etc
#metadata can include subjects from multiple monitor files and times
#practically it is probably easier to either create it in excel along with the experiment or specify
#the metavariables in the zantiks output and extract them in R

metad2<-link_dam_metadata(metad2, result_dir = DATA_DIR) #link the metadata to the monitor file
ladybugalt<-load_dam(metad2) #create a behavr table 


#same for diastance travelled
metad<-data.table(file = rep("postdam1.txt", times = 12), start_datetime = rep(addedtimedate[1], times = 12),
                   stop_datetime = rep(addedtimedate[lasttime], times = 12), region_id = rep(1:12))
metad<-link_dam_metadata(metad, result_dir = DATA_DIR)
ladybug<-load_dam(metad)


#TODO: alter for standardised format
#merge the 2 observations through metadata
