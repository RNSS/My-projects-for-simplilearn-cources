#Library
library(lubridate)
library(dplyr)
library(ggpubr)
library(stringi)
library(ggplot2)
#directory
setwd("/Users/rynadalswyd/Documents/LEARN/Level 8/summer training/MSIT/Path1_step1/Project_1")
getwd()
#Q1
data_file = read.csv("Comcast Telecom Complaints data.csv")
View(data_file)
str(data_file)
#Manipulating column names
names(data_file)<- stri_replace_all(regex = "\\.",replacement = "",str =names(data_file))
head(data_file)
na_vector <- is.na(data_file)
length(na_vector[na_vector==T])
str(data_file)
#Q2
###Date format
date_format = as.Date(parse_date_time(x = data_file$Date,
orders = c("d-m-y","d/m/y"),
locale = "en_US"))
data_file = transform(data_file, Date = date_format)
### daily
daily_count<- summarise(group_by(data_file,Date),Count =n())
ggplot(data = daily_count,aes(as.POSIXct(Date),Count))+
geom_line()+
geom_point(size = 1)+
scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")+
theme(axis.text.x = element_text(angle = 75),
plot.title = element_text(hjust = 0.5))
### monthly
monthly_count<- summarise(group_by(data_file,Month =as.integer(month(Date))),Count = n())
monthly_count<-arrange(monthly_count,Month)
ggplot(data = monthly_count,aes(Month,Count,label = Count))+
geom_line()+
geom_point(size = 0.8)+
geom_text()+
scale_x_continuous(breaks = monthly_count$Month)+
labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")+
theme(plot.title = element_text(hjust = 0.5))
#Q3
network_tickets<- contains(data_file$CustomerComplaint,match = 'network',ignore.case = T)
internet_tickets<- contains(data_file$CustomerComplaint,match = 'internet',ignore.case = T)
billing_tickets<- contains(data_file$CustomerComplaint,match = 'bill',ignore.case = T)
email_tickets<- contains(data_file$CustomerComplaint,match = 'email',ignore.case = T)
charges_ticket<- contains(data_file$CustomerComplaint,match = 'charge',ignore.case = T)
data_file$ComplaintType[internet_tickets]<- "Internet"
data_file$ComplaintType[network_tickets]<- "Network"
data_file$ComplaintType[billing_tickets]<- "Billing"
data_file$ComplaintType[email_tickets]<- "Email"
data_file$ComplaintType[charges_ticket]<- "Charges"
data_file$ComplaintType[-c(internet_tickets,network_tickets,
billing_tickets,charges_ticket,email_tickets)]<- "Others"
network_tickets
complaibts_type_tot=table(data_file$ComplaintType)
barplot(complaibts_type_tot)
#Q4
status=c()
n=1
for (x in data_file$Status) {
if(x == "Open" || x == "Pending")
{ status[n] = "Open"}
else {status[n] = "Closed"}
print(x)
n=n+1
}
status
data_file = transform(data_file, Status = status)
# Q5
states_complaints= table( data_file$State, data_file$Status) #MAIN TABLE OF COMPLAINTS
states_complaints_2= table( data_file$Status, data_file$State) #MAIN TABLE OF COMPLAINTS
states_comp_tot<- apply(states_complaints,c(1),sum)#TABLE FOR EACH STATE TOTAL
#states = row.names(states_complaints)#LIST OF STATES NAMES
barplot(states_complaints_2,
main="state-wise status of complaints",
xlab="State",
col=c("grey","cornflowerblue"),
legend = rownames(states_complaints_2),)
v =1
len_tot = length(states_comp_tot)
while (v <= len_tot) { # FIND THE HIGHST COMPLAINT STATE
if (states_comp_tot[v] == max(states_comp_tot)){
print(states_comp_tot[v])
break;
}
v=v+1
}
# Q6
### TOTAL OF ALL COMPLAINTS
total_of_complaints = 0
x =1
while (x <=len_tot) {
total_of_complaints = as.integer(total_of_complaints + states_comp_tot[x])
x = x+1
}
total_of_complaints
### TOTAL OF ALL CLOSED COMPLAINTS
x=1
total_of_closed = 0
while (x <=len_tot) {
total_of_closed = total_of_closed + states_complaints[x,1]
x = x+1
}
total_of_closed
closed_precentage = as.integer((total_of_closed/total_of_complaints )*100)
closed_precentage
Open_precentage =100 - closed_precentage
slice =c(Open_precentage,closed_precentage)
pct<-round(slice/sum(slices)*100)
lbls = paste(c("Open","Closed")," ",as.integer(slice) ,"%", sep= "")
pie(slice,labels = lbls,
main="Complaints Status")
### TOTAL OF ALL CLOSED COMPLAINTS BY Customer Care Call
x=1
tot_closed_by_call = 0
while (x <= length(data_file$Status)) {
if(data_file$Status[x] == "Closed" && data_file$ReceivedVia[x] == "Customer Care Call")
{tot_closed_by_call = tot_closed_by_call + 1}
x = x+1
}
tot_closed_by_call
### Total for closed by internet
tot_closed_by_internet = total_of_closed - tot_closed_by_call
tot_closed_by_internet
slices = c(tot_closed_by_call,tot_closed_by_internet)
pct<-round(slices/sum(slices)*100)
lbls = paste(c("Care Call","Internet")," ",pct ,"%", sep= "")
pie( slices,
labels = lbls,
main="Closed by")