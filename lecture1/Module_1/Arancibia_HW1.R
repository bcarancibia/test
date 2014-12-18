## Benjamin Arancibia
## Homework 1
## Principles of Data Visualization and Intro to ggplot 2

###Load required packages and read in data
require(plyr)
require(ggplot2)
require(data.table)


#this will need to change based on where you running the script since this is my local machine
data.file <- "/lecture1/data/inc5000_data.csv"
raw.data <- read.csv (file = data.file, header = TRUE, sep = ",")


####Question 1####
#first need to get a count of the data 
inc_state <- ddply(raw.data, .(State), summarize, count = length(Rank))
inc_state <- inc_state[order(inc_state$count),] #count
dtinc <- data.table(raw.data)

#plot using bars and counts as the x-axis
fig1<-ggplot(data=inc_state, aes(x=State, y=count))+
  geom_bar(stat="identity", fill="white", colour="darkgreen")+
  coord_flip()+
  ggtitle("Number of Companies in Each State")

fig1

#save
ggsave(fig1, file=paste("/Users/bcarancibia/CUNY_IS_608/Module_1/data/", "figure1_Arancibia.png", sep=""), height=11, width=8, dpi=100, scale=0.75)

####Question 2####
#by looking at the data list grouping inc_state find that New York has 3rd most companies in the dataset
#look to see how many people employed companies in different industries
#exclude outliers
#isolate New York
ny <- dtinc[dtinc$State == "NY"]
#look for outliers
qplot(Name, Employees, data = ny, geom ="jitter")

#exclude outliers (which is anything greater than 300 Employees)
outlier = 300
ny <- ny[ny$Employees < outlier,]
#visual show change
qplot(Name, Employees, data = ny, geom ="jitter")

#average employment by industry in New York state
ny_average_ind <- ddply(ny, .(Industry), summarize,
                         average_employees = mean(Employees),
                         num_companies = length(Employees))
ny_average_ind<- ny_average_ind[order(ny_average_ind$average_employees, decreasing=TRUE),]

fig2<-ggplot(data=ny_average_ind, aes(x=Industry, y=average_employees))+
  geom_bar(stat="identity", colour="darkgreen")+
  coord_flip()+
  ggtitle("Average Employment by Industry in the State of New York")

fig2

#save figure 2 to folder
ggsave(fig2, file=paste("/Users/bcarancibia/CUNY_IS_608/Module_1/data/", "figure2_Arancibia.png", sep=""), height=11, width=8, dpi=100, scale=0.75)

####Question 3####
#Want to see which industries generate the most revenue per employee
#use the same ddply above just add revenue per employee
#since I am an investor I want to know growth rate, doesn't make sense to invest in something that has high revenue for employee but not growing
ny_rev_employee <- ddply(ny, .(Industry), summarize,
                         average_employees = mean(Employees),
                         num_companies = length(Employees),
                         total_employed = sum(Employees),
                         average_revenue = mean(Revenue/Employees),
                         growth = mean(Growth_Rate)
                         )
ny_rev_employee<- ny_rev_employee[order(ny_rev_employee$average_revenue, decreasing=TRUE),]

#plot it using scatter plot
fig3 <- ggplot(ny_rev_employee, aes(average_revenue, Industry))+ geom_point(aes(size = growth))+
  ggtitle("Average Revenue and Growth per Industry")

fig3

#save figure 3 to folder
ggsave(fig3, file=paste("/Users/bcarancibia/CUNY_IS_608/Module_1/data/", "figure3_Arancibia.png", sep=""), height=11, width=8, dpi=100, scale=0.75)








