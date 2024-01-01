#libraries:
library(tidyverse)
library(dplyr)
library(effectsize)
library(ggplot2)

#these are the VME errors and the most frequent non-VME errors:
most_freq_errors =
 c("If.so..does.VME.error.exist.", "No.Pen", "Shape",
  "Incorrect.Missing.Rune", "Wrong.Shape", "No.Tie",
   "No.Stripes",  "No.Clothes")  

#the non-VME images where the most frequent errors occur:
seven_relevant_non_VME_images =
 c("Bic", "Biohaz", "Bluetooth", "Kitty", "Fred", "Garfield", "Sponge")
#the columns relevant for the ANOVA:
relevant_columns = c("Image", "Task.Type", "VME.or.not.")
#these will help me later:
columns_to_keep = c(relevant_columns, most_freq_errors)
columns_to_keep2 = c(relevant_columns, "Error")


#file read:
path = "C:\Users\ariel\Dropbox\studies\research methods\Drawing_Scores.csv"
data = read.csv(path)

#two columns had originally te same name:
colnames(data)[31] <- "Wrong.Shape2"

#keeping only drawings where VME errors could have occured, and non-VME images:
data = filter(data, 
    VME.or.not. == "VME"&Possible.for.VME.Error.to.exist. == 1 |
    Image %in% seven_relevant_non_VME_images)

#indicating in which drawaing an error occured:
data = data[columns_to_keep]
data$If.so..does.VME.error.exist. <- as.numeric(data$If.so..does.VME.error.exist.)
data <- replace(data, is.na(data), 0)
data$Error = rowSums(data[most_freq_errors])
data = data[columns_to_keep2]
View(data)

#grouping the data according to image and the independent variables:
new_df =data %>%
  group_by(Image, VME.or.not., Task.Type) %>%
    summarize(mean_error = mean(Error))
new_df$VME.or.not. = as.factor(new_df$VME.or.not.) 
new_df$Task.Type = as.factor(new_df$Task.Type)

View(new_df)

#results:
aov.out= aov(data=new_df, mean_error~VME.or.not.*Task.Type)
summary(aov.out)
effectsize::eta_squared(aov.out, alternative = "two.sided")

#graph:
graph_df =new_df
graph_df$mean_error = 100*graph_df$mean_error

p <- ggplot(data = graph_df, mapping = aes(x = Task.Type, y = mean_error))
p + geom_point(aes(color = factor(VME.or.not.)), size=5) + 
 labs(y = "Error Percentage", x = "Task Type: Short vs. Long Term Memory" ,colour = "Is it VME?", size =10)+
  geom_line(aes(group = Image)) +
   facet_grid(cols=vars(VME.or.not.)) + geom_hline(yintercept=0)+
   geom_hline(yintercept=100)
