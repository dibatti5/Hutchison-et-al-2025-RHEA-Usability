## The following is the code accompanying the manuscript
## "Assessing the Usability and Satisfaction of a Concussion Rehabilitation 
## App: A Pilot Study" by Hutchison et al 2025 ##

#Relevant libraries
library(tidyverse)
library(gt)
library(dplyr)
library(gtsummary)
library(viridis)
library(hrbrthemes)
library(likert)
library(scales)
library(ggrepel)
library(forcats)
library(scales)
library(cowplot)
library(bayesplot)
library(grid)
library(gridExtra)
library(lattice)
library(latticeExtra)
library(RColorBrewer)

#load dataset
df <- read.csv("rhea_usability.csv", stringsAsFactors = FALSE)

##clean up for tables

#remove all periods in column titles and replace with spaces
colnames(df) <- gsub("\\.", " ", colnames(df))

#Prep of a demographics table
colnames(df)
t1_df <- df[c(2, 3, 5, 7, 12, 20, 21, 23)]

#change PA column
colnames(t1_df)[7] <- "Physical Activity (Times/Week)"

#change column title
colnames(t1_df)[3] <- "History of Concussion"

#change order of mechanism
unique(t1_df$Mechanism)
t1_df$Mechanism <- factor(t1_df$Mechanism, 
                              levels = c("Sport/PA", "MVC", "Work", "Fall", "Other"))

#rearrange for table
colnames(t1_df)
t1_df2 <- t1_df[c(1, 2, 5, 4, 3, 6, 8, 7)]

#create demo table
t1 <- 
  tbl_summary(t1_df2, missing = "no",
              statistic = all_continuous() ~ "{median} ({p25},{p75})",
              digits = all_continuous() ~ 1)%>%
  bold_labels()%>%
  modify_caption("**Table 1. Participant Characteristics**") %>%
  as_gt() %>%
  tab_source_note("PA, Physical Activity")%>%
  gt::gtsave(filename = "t1.html")

#prep symptoms table
colnames(df)
t2_df <- df[c(15:18)]

#change column title for Feeling normal
colnames(t2_df)[3] <- "Percent of Normal"

#change symptom trend order for table
t2_df$`Symptom Trend Since Injury` <- factor(t2_df$`Symptom Trend Since Injury`,
                                              levels = c('No change','Steadily better',
                                                         'Steadily worse',
                                                         'Worse, then better',
                                                         'Better, then worse'))

#create demo table
tbl_summary(t2_df, missing = "no",
            statistic = all_continuous() ~ "{median} ({p25},{p75})",
            type = list(
                          `Symptom Number` ~ "continuous",
                          `Symptom Severity` ~ "continuous",
                          `Percent of Normal` ~ "continuous"),
         digits = all_continuous() ~ 1) %>%
  bold_labels() %>%
  modify_caption("**Table 2. Concussion Characteristics**") %>%
  as_gt() %>%
  gt::gtsave(filename = "t2.html") 

###Likert Data Plotting###

#clean up df for likert data
colnames(df)
likert_df <- df[c(25:42, 44:47)]

#change error in column title
colnames(likert_df)[15] <- "The app would be useful for my health and well being."

#MUAQ specific
likert_df_muaq <- likert_df[c(1:16)]

#factor across the df so all columns have equal levels
likert_plot_df <- as.data.frame(lapply(likert_df_muaq, function(x) {
  factor(x, levels = c("Strongly Disagree", "Disagree",
                       "Somewhat Disagree",  "Neither Disagree nor Agree", 
                       "Somewhat Agree", "Agree", "Strongly Agree"))
}))

#change column names again after listing
colnames(likert_plot_df) <- gsub("\\.", " ", colnames(likert_plot_df))
str(likert_plot_df)

#separate out into categories and make into likert object
p1 <- likert::likert(likert_plot_df[c(1, 8, 7, 6, 3, 4, 2, 5)])
p2 <- likert::likert(likert_plot_df[c(9:14)])
p3 <- likert::likert(likert_plot_df[c(15:16)])

# Extract the legend from one of the plots
legend <- get_legend(
  plot(p2) + theme(legend.position = "right", 
  legend.title = element_text(face = "bold")) +
  scale_fill_manual("Response", values = brewer.pal(n = 7, "RdYlBu"), 
  breaks = c("Strongly Disagree", "Disagree",
             "Somewhat Disagree",  "Neither Disagree nor Agree", 
          "Somewhat Agree", "Agree", "Strongly Agree")))

#create plot
plot1 <- plot(p1, legend.position = "none") + ggtitle("Ease of use and satisfaction") +
  theme(axis.title.x = element_blank(), 
  axis.text.y = element_text(face = "bold"), 
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), 
  title = element_text(face = "bold", size = 9)) + 
  scale_fill_manual(values = brewer.pal(n = 7, "RdYlBu"), 
  breaks = c("Strongly Disagree","Disagree", "Somewhat Disagree", 
             "Neither Disagree nor Agree", 
          "Somewhat Agree", "Agree", "Strongly Agree"))
  
plot2 <- plot(p2, legend.position = "none") + ggtitle("System information arrangement") +
  theme(axis.title.x = element_blank(),
  axis.text.y = element_text(face = "bold"),
   plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), 
  title = element_text(face = "bold", size = 9)) + 
  scale_fill_manual(values = brewer.pal(n = 7,"RdYlBu"), 
  breaks = c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neither Disagree nor Agree", 
          "Somewhat Agree", "Agree", "Strongly Agree"))

plot3 <- plot(p3, legend.position = "none") + ggtitle("Usefulness") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
  axis.text.y = element_text(face = "bold", size = 9),
  title = element_text(face = "bold")) + 
  scale_fill_manual(values = brewer.pal(n = 7,"RdYlBu"), 
  breaks = c("Strongly Disagree", "Disagree", 
             "Somewhat Disagree", "Neither Disagree nor Agree", 
          "Somewhat Agree", "Agree", "Strongly Agree"))

# Align the plots using cowplot
aligned_plots <- align_plots(plot1, plot2, plot3, align = 'v', axis = 'lr')

# Combine the aligned plots
p_final <- plot_grid(aligned_plots[[1]], aligned_plots[[2]], aligned_plots[[3]], 
                     ncol = 1, 
                     rel_heights = c(8, 6, 2))

# Add the legend to the right of the combined plot
p_with_legend <- plot_grid(p_final, legend, ncol = 2, rel_widths = c(3, 0.7))

# Save the final plot with the legend on the right
ggsave(plot = p_with_legend, filename = "fig1.jpg", dpi = 600, height = 12, width = 12)

#additional questions (other than overall satisfaction question)
likert_df_add <- likert_df[c(18:22)]
colnames(likert_df_add)

likert_plot_df_add <- as.data.frame(lapply(likert_df_add, function(x) {
  factor(x, levels = c("Strongly Disagree", "Disagree", "Somewhat Disagree",
                       "Neither Disagree nor Agree", 
                       "Somewhat Agree", "Agree", "Strongly Agree"))
}))

#change column names again after listing
colnames(likert_plot_df_add) <- gsub("\\.", " ", colnames(likert_plot_df_add))

#create likert object
p4 <- likert::likert(likert_plot_df_add)

plot4 <- plot(p4, legend.position = "right") + ggtitle("Additional Custom Questions") +
  theme(axis.text.y = element_text(face = "bold"), 
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), 
  title = element_text(face = "bold", size = 9)) + 
  scale_fill_manual("Response", values = brewer.pal(n = 7, "RdYlBu"), 
  breaks = c("Strongly Disagree", "Somewhat Disagree", "Disagree", "Neither Disagree nor Agree", 
          "Somewhat Agree", "Agree", "Strongly Agree"))

ggsave(plot = plot4, filename = "fig2.jpg", dpi = 600, height = 8, width = 12)

##Creating likert tables with GT##

# Convert to long format
df_long <- likert_plot_df %>%
  pivot_longer(cols = everything(), 
               names_to = "Question", 
               values_to = "Response")

#Factor questions
df_long$Question <- factor(df_long$Question, levels = 
                             c(paste(colnames(likert_plot_df))))

#make table
summary_table <- df_long %>%
  tbl_summary(by = Response, # Create columns for each response level
              statistic = all_categorical() ~ "{n} ({p}%)", # Show counts and row percentages
              percent = "row",
              label = Question ~ "") %>%
  modify_header(update = all_stat_cols() ~ "**{level}**") %>%
  as_gt() %>%
  tab_row_group(label = "Ease of use and satisfaction (N = 23)", rows = c(2:9)) %>%
  tab_row_group(label = "System information arrangement (N = 23)", rows = c(10:15)) %>%
  tab_row_group(label = "Usefulness (N = 23)", rows = 16:17) %>%
  row_group_order(groups = c("Ease of use and satisfaction (N = 23)",
                             "System information arrangement (N = 23)",
                             "Usefulness (N = 23)")) %>%
  tab_style(style = list(cell_text(weight = 'bold')),
    locations = cells_row_groups()) %>%
  tab_style(style = list(cell_text(indent = px(15))),
    locations = cells_body()) %>%
gtsave("t3.html")

#table for additional questions
# Convert to long format
df_long2 <- likert_plot_df_add %>%
  pivot_longer(cols = everything(), 
               names_to = "Question", 
               values_to = "Response")

#Factor questions
df_long2$Question <- factor(df_long2$Question, levels = 
                             c(paste(colnames(likert_plot_df_add))))

#make table
summary_table <- df_long2 %>%
  tbl_summary(by = Response, # Create columns for each response level
              statistic = all_categorical() ~ "{n} ({p}%)", # Show counts and row percentages
              percent = "row",
              label = Question ~ "") %>%
  modify_header(label = "**Question (N = 23)**") %>%
  modify_header(update = all_stat_cols() ~ "**{level}**") %>%
  bold_labels() %>%
  as_gt() %>%
  gtsave("t4.html")
  
  

  
