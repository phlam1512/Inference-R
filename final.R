library(readODS) # for read_ods()
library(tidyr)   # for fill()
library(dplyr)   # for filter()

# Import data
path <- "dvsa1203.ods" # Assuming this file is placed in the same directory
sheets <- list_ods_sheets(path) # Get list of sheet names
sheets <- sheets[-1] #The first sheet is Notes
col_names <- c("Test_centre", "Age", "Male_count", "Male_pass", "Male_pass_rate", "Female_count", "Female_pass","Female_pass_rate","Total_count","Total_pass","Total_pass_rate")
data <- c()

for (sheet in sheets){
  print(sheet) #Progress bar
  dat <- c()
  dat <- read_ods(path, sheet=sheet, col_names=TRUE, na="", skip=6) #Read sheet
  empty_columns <- sapply(dat, function(x) all(is.na(x) | x=="")) #Find NA columns
  dat <- dat[,!empty_columns] #Remove NA columns
  colnames(dat) <- col_names #Rename columns
  dat <- fill(dat, Test_centre, .direction="down") #Fill in test centre
  dat <- filter(dat, Test_centre=="Guildford" | Test_centre=="Wood Green (London)" | Test_centre=="Wood Green") #Subset only the two relevant test centres
  dat <- filter(dat, !is.na(Age), Age!="Total") #Remove empty rows and the total row
  y <- unlist(strsplit(sheet, split="-", fixed=TRUE))[1] 
  yr <- rep(y, 18) 
  dat <- cbind(yr, dat) #Add column for years
  data <- rbind(data, dat) #Merge data frames
}

# Convert data type to numeric
data[,c(1,3:12)] <- sapply(data[,c(1,3:12)], function(x) as.numeric(x)) 

# Make names of test centres consistent
data$Test_centre[data$Test_centre == "Wood Green"] <- "Wood Green (London)" 

# Convert percentages to decimal 
data$Male_pass_rate <- data$Male_pass_rate / 100                      
data$Female_pass_rate <- data$Female_pass_rate / 100
data$Total_pass_rate <- data$Total_pass_rate / 100

# Subset dataframe by relevant test centre, age and gender
dfg <- filter(data, Test_centre=="Guildford", Age==23)
dfw <- filter(data, Test_centre=="Wood Green (London)", Age==23)
dfg_1 <- dfg[,c("yr","Female_pass_rate")]
dfw_1 <- dfw[,c("yr","Female_pass_rate")]

# Rename columns and merge the two dataframes
colnames(dfg_1)[colnames(dfg_1)=="Female_pass_rate"] <- "Guildford"
colnames(dfw_1)[colnames(dfw_1)=="Female_pass_rate"] <- "Wood Green (London)"
dff <- merge(dfg_1, dfw_1)

# Line plot
png(filename="line.png", width=800, height=400, bg="white")
par(cex=1.5)
xlim <- c(2007, 2021)
ylim <- c(0.3,0.7)
plot(dff$yr, dff$Guildford, "b", col=2, xlim=xlim, ylim=ylim, xlab="Year", ylab="Passing rate", xaxt="n", main="Passing rate of 23 year old females")
lines(dff$yr, dff$`Wood Green (London)`, "b", col=4, xlim=xlim, ylim=ylim)
grid(nx=NA, ny=NULL, col="lightgray", lty="dotted", lwd=0.5)
legend("topright", legend=c("Guildford", "Wood Green"), col=c(2,4), lty=1)
axis(1, at = seq(2007,2022,by=1), las=2)

# Box plot
png(filename="box.png", width=800, height=400, bg="white")
par(cex=1.5)
boxplot(dfg_1$Guildford, dfw_1$`Wood Green (London)`, horizontal=TRUE, xlab="Passing rate", ylab="Testing centre", main="Distribution of passing rate of 23 year old females", col=c(2,4), names=c("Guildford", "Wood Green"))

# Summary statistics
standard_error <- function(x){sd(x)/sqrt(length(x))}
median_g <- median(dfg_1$Guildford)                   # 0.4925373
median_w <- median(dfw_1$`Wood Green (London)`)       # 0.3803419
mean_g <- mean(dfg_1$Guildford)                       # 0.4834553
mean_w <- mean(dfw_1$`Wood Green (London)`)           # 0.3828296
var_g <- var(dfg_1$Guildford)                         # 0.005834305
var_w <- var(dfw_1$`Wood Green (London)`)             # 0.002997427
se_g <- standard_error(dfg_1$Guildford)               # 0.01972191
se_w <- standard_error(dfw_1$`Wood Green (London)`)   # 0.01413607

# t-test
sqrt(28/((2/15)))*((mean_g-mean_w)/(sqrt(14*(var_g+var_w)))) # 4.146977
ts <- qt(0.025, 28, lower.tail=FALSE) # t-value: 2.048407
# Confidence interval
(mean_g-mean_w)+ts*sqrt((2/15)/28*(14*(var_g+var_w))) # 0.15033
(mean_g-mean_w)-ts*sqrt((2/15)/28*(14*(var_g+var_w))) # 0.05092145
# Alternatively, we can use the t.test() function to obtain the same results
t.test(dfg_1$Guildford, dfw_1$`Wood Green (London)`, var.equal=T)
################################################################################
## Two Sample t-test
## 
## data:  dfg_1$Guildford and dfw_1$`Wood Green (London)`
## t = 4.147, df = 28, p-value = 0.000283
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   0.05092144 0.15032995
## sample estimates:
##   mean of x mean of y 
## 0.4834553 0.3828296 
################################################################################

# F-test 
var_g / var_w # 1.946438
qf(0.025, 14, 14, lower.tail=FALSE) # F-value: 2.978588
# Alternatively, we can use the var.test() function to obtain the same results
var.test(dfg_1$Guildford, dfw_1$`Wood Green (London)`)
################################################################################
## F test to compare two variances
## 
## data:  dfg_1$Guildford and dfw_1$`Wood Green (London)`
## F = 1.9464, num df = 14, denom df = 14, p-value = 0.2251
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##   0.6534768 5.7976356
## sample estimates:
##   ratio of variances 
## 1.946438 
################################################################################

## QQ-plot
png(filename="qq.png", width=1200, height=400, bg="white")
par(mfrow=c(1,3), cex=1.5)
qqnorm(dfg_1$Guildford, main="Normal Q-Q Plot \n (Guildford)")
qqline(dfg_1$Guildford, col=2)
qqnorm(dfw_1$`Wood Green (London)`, main="Normal Q-Q Plot \n (Wood Green)")
qqline(dfw_1$`Wood Green (London)`, col=4)
qqnorm(c(dfg_1$Guildford, dfw_1$`Wood Green (London)`))
qqline(c(dfg_1$Guildford, dfw_1$`Wood Green (London)`))