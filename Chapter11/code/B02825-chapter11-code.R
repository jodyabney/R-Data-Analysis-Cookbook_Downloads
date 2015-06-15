# Using Java objects in R

install.packages(‘rJava’)
library(rJava)
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")

.jaddClassPath(getwd())
.jclassPath()

s <- .jnew("java/lang/String", "Hello World!")
print(s)
.jstrVal(s)
.jcall(s,"S","toLowerCase")
.jcall(s,"S","replaceAll","World","SV")


javaVector <- .jnew("java/util/Vector")
months <- month.abb
sapply(months, javaVector$add)
javaVector$size()
javaVector$toString()

monthsArray <- .jarray(month.abb)
yearsArray <- .jarray(as.numeric(2010:2015))
calArray <- .jarray(list(monthsArray,yearsArray))
print(monthsArray)
.jevalArray(monthsArray)
print(l <- .jevalArray(calArray))
lapply(l, .jevalArray)

hw <- .jnew("javasamples.HelloWorld")
hello <- .jcall(hw,"S", "getString")
hello

greet <- .jnew("javasamples.Greeting")
print(greet)
g <- .jcall(greet,"S", "getString", "Shanthi")
print(g)
.jstrVal(g)

jvm = .jnew("java.lang.System")
jvm.props = jvm$getProperties()$toString()
jvm.props <- strsplit(gsub("\\{(.*)}", "\\1", jvm.props), ", ")[[1]]
jvm.props

.jmethods(s,"trim")
.jmethods(s)

export R_HOME=/Library/Frameworks/R.framework/Resources
export PATH=$PATH:/Library/Frameworks/R.framework/Resources/bin/

# Using JRI to call R functions from Java

From javasamples directory::

javac -cp .:../lib/* *java

java  -Djava.library.path=/Library/Frameworks/R.framework/Resources/library/rJava/jri -cp ..:../lib/* javasamples.SimpleJRIStat

java -Djava.library.path=/Library/Frameworks/R.framework/Resources/library/rJava/jri/ -cp ..:../lib/* javasamples.SimplePlot /Users/sv/book/Chapter11

# Using Rserv to call R functions from Java

install.packages(“Rserve”)
library(Rserve)
Rserve(args="--no-save")  — on Mac, Rserve() on windows

# in the following command, be sure to change the last command argument to point
# to where you have the auto-mpg.csv file
java -cp ..:../lib/* javasamples.SimpleGGPlot /Users/sv/book/Chapter11

java -cp ..:../lib/* javasamples.SimpleRservStat 

# Executing R script from Java

java -Djava.library.path=/Library/Frameworks/R.framework/Resources/library/rJava/jri/ -cp ..:../lib/* javasamples.InvokeRScript mpg weight /Users/sv/book/Chapter11

# Using xlsx to connect to Excel

library(xlsx)   —we are not discussing XLConnect package here.
auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)

write.xlsx(auto, file = "auto.xlsx", sheetName = "autobase", row.names = FALSE)

auto$kmpg <- auto$mpg * 1.6
auto$mpg_deviation <- (auto$mpg - mean(auto$mpg))/auto$mpg

auto.wb <- createWorkbook()
sheet1 <- createSheet(auto.wb,"auto1")
rows <- createRow(sheet1, rowIndex=1)
cell.1 <- createCell(rows, colIndex=1)[[1,1]]     
setCellValue(cell.1, "Hello Auto Data!")

addDataFrame(auto, sheet1, startRow=3, row.names=FALSE)

cs <- CellStyle(auto.wb) + Font(auto.wb, isBold=TRUE, color="red")
setCellStyle(cell.1, cs)
saveWorkbook(auto.wb,"auto_wb.xlsx")

auto.wb <- loadWorkbook("auto_wb.xlsx")
sheet2 <- createSheet(auto.wb,"auto2")
addDataFrame(auto[,1:9], sheet2, row.names=FALSE)
saveWorkbook(auto.wb, "auto_wb.xlsx")

wb <- loadWorkbook("auto_wb.xlsx")
sheets <- getSheets(wb)
sheet <- sheets[[2]]
addDataFrame(auto[,10:11], sheet, startColumn=10, row.names=FALSE)
saveWorkbook(wb, "newauto.xlsx")

#Recipe: Reading data from relational databases
#----------------------------------------------
customer <- c("John", "Peter", "Jane")
orddt <- as.Date(c('2014-10-1','2014-1-2','2014-7-6'))
ordamt <- c(280, 100.50, 40.25)
order <- data.frame(customer,orddt,ordamt)

library(RODBC)
con <- odbcConnect("order_dsn", uid="user", pwd="pwd")
sqlSave(con,order,"orders",append=FALSE)
custData <- sqlQuery(con, "select * from orders")
close(con)

library(RMySQL)
con <- dbConnect("MySQL", dbname="Customer",
                 host="127.0.0.1", port=8889, username="root", 
                 password="root")
dbWriteTable(con,"orders", order)
dbReadTable(con,"Orders")
dbGetQuery(con,"select * from orders")

rs <- dbSendQuery(con, "select * from orders")
while(!dbHasCompleted(rs)) {
  fetch(rs,n=2)
}
dbClearResult(rs)
dbDisconnect(con)
dbListConnections(dbDriver("MySQL"))

library(RJDBC)
# In the followi g command, be sure to point to where the downloaded jar file resides
driver <- JDBC("com.mysql.jdbc.Driver", 
               classpath=
                 "/etc/jdbc/mysql-connector-java-5.1.34-bin.jar", "`")
con <- dbConnect(driver,"jdbc:mysql://host:port/Customer"
                 ,"username","password")
# The remaining operations are identical to RMySQL

fetch(rs,n=-1) 

dbSendQuery(con, statement=paste(
  "select ordernumber, orderdate, customername",
  "from orders o, customers c",
  "where o.customer = c.customer",
  "and c.state = 'NJ'",
  "ORDER BY ordernumber"))


# Recipe: Read data from NoSQL databases -- mongoDB
------------------------------
# Execute the following 4 lines of code in MongoDB
use customer
db.orders.save({customername:"John",
                orderdate:ISODate("2014-11-01"),orderamount:1000})
db.orders.find()
db.save 

install.packages("rmongodb")
library(rmongodb)
mongo <- mongo.create()
mongo.create(host = "127.0.0.1", db = "customer")
mongo.is.connected(mongo)

coll<- mongo.get.database.collections(mongo,"customer")

json <- "{\"orderamount\":{\"$lte\":25000}, 
      \"orderamount\":{\"$gte\":1000}}"
dat <- mongo.find.all(mongo,coll,json)

library(jsonlite)
json <- "{\"orderamount\":{\"$lte\":25000}, 
      \"orderamount\":{\"$gte\":1500}}"
validate(json)



