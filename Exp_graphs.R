



plot <- ggplot(record) +
  geom_bar(aes(x = STATUS, fill = Status#, weight = -1*MONTHS_BALANCE
               )) +
  # GROUP for GEOM line fixes error instead of trying to use 2 different x columns for factor and date
  #geom_line(  aes(x = Timeline, y = Cycles.peak2peak.trough2trough),size = 1L, colour = "black", group = 1) +
  labs(title = "Spread of status with binary color groupings", 
       x = 'Status Rating groups', y = 'Frequency') + 
  scale_fill_brewer(palette = "Dark2") +
  
  # 90 degree to dates on x axis
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
plot

# make interactive
ggplotly(plot)




Plot <- ggplot(record, aes(x = -1*MONTHS_BALANCE, fill = Status)) +
  geom_bar( position = "fill") +
  scale_fill_manual("legend", values = c("Current" = "darkblue", "Past Due" = "darkorange")) +
labs(title = "Customer Status percentages based on Months Balance",
     x = 'Months Balance is Carried', y = 'Proportions') 
Plot

# make interactive
ggplotly(Plot)


# longer months balance carried more people will end up past due 
# jump in no balance once at over 60, this is due to collection transfers
Plot1 <- ggplot(record, aes(x = -1*MONTHS_BALANCE, fill = STATUS)) +
  geom_bar( position = "fill") +
  
  #for counts
  #geom_bar( position = "identity") +
  
  labs(title = "Customer Multi level Status percentages based on Months Balance",
       x = 'Months balance is Carried', y = 'Proportions') 
Plot1

# make interactive
ggplotly(Plot1)

#ggplot(record,aes(x = STATUS, fill = Status)) + 
#  geom_bar(position = "fill")



box <- record %>%
  filter(!(OCCUPATION_TYPE %in% "")) %>%
  ggplot() +
  aes(x = STATUS, y = -1*MONTHS_BALANCE, fill = Status) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()+ 
  labs(title = "Customer Multi level Status spread of months",
                      x = 'Status level', y = 'Months balance is Carried') 
box
ggplotly(box)

box1 <- record %>%
  filter(!(OCCUPATION_TYPE %in% "")) %>%
  ggplot() +
  aes(x = STATUS, y = yrs_old, fill = Status) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()+ 
  labs(title = "Customer Multi level Status spread of age",
       x = 'Status level', y = 'Age in years') 
box1
ggplotly(box1)

box2 <- record %>%
  filter(!(OCCUPATION_TYPE %in% "")) %>%
  ggplot() +
  aes(x = STATUS, y = yrs_employed, fill = Status) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() + 
  labs(title = "Customer Multi level Status spread of employment time",
       x = 'Status level', y = 'Current employment time in yrs') 
box2
ggplotly(box2)


#### compare with /without inc filter
box3 <- record %>%
  filter(!(OCCUPATION_TYPE %in% "")) %>%
  filter(AMT_INCOME_TOTAL <= 400000 ) %>%
  ggplot() +
  aes(x = STATUS, y = AMT_INCOME_TOTAL, fill = Status) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()+ 
  labs(title = "Customer Multi level Status spread of Total Income",
       x = 'Status level', y = 'Income') 
box3
ggplotly(box3)






# plots <- record %>%
#   filter(!(OCCUPATION_TYPE %in% "")) %>%
#   ggplot() +
#   aes(x = STATUS, y = DAYS_BIRTH, fill = Status) +
#   geom_boxplot() +
#   scale_fill_distiller(palette = "RdYlBu") +
#   theme_minimal()
# plots
# ggplotly(plots)
# 
# plots1 <- record %>%
#   filter(!(OCCUPATION_TYPE %in% "")) %>%
#   ggplot() +
#   aes(x = STATUS, y = DAYS_EMPLOYED, fill = Status_binary) +
#   geom_boxplot() +
#   scale_fill_distiller(palette = "RdBu") +
#   theme_minimal()
# plots1
# ggplotly(plots1)


scatter <- record %>%
  filter(AMT_INCOME_TOTAL >= 27000L & AMT_INCOME_TOTAL <= 837608L) %>%
  filter(!(OCCUPATION_TYPE %in% 
             "")) %>%
  filter(CNT_FAM_MEMBERS >= 1L & CNT_FAM_MEMBERS <= 3L) %>%
  ggplot() +
  aes(x = yrs_old, y = yrs_employed, colour = AMT_INCOME_TOTAL
      #
      , size = AMT_INCOME_TOTAL
      ) +
  geom_point() +
  scale_color_distiller(palette = "RdYlBu") +
  theme_minimal()
# add threshold line ?
scatter


# CLEAR divide with income jumps
# older doesnt mean more income, longer time at imployment shows potentially may go along with higher pay
record3d <- record %>%
  filter(AMT_INCOME_TOTAL >= 150500 
         & AMT_INCOME_TOTAL <= 200000 ) %>%
  filter(Status_binary == 1) %>%
  filter(yrs_employed >= 6 & yrs_employed >= 8)%>%
  filter(yrs_old >= 35 & yrs_old >= 54)

record3 <- plot_ly(x = record3d$ yrs_old, y = record3d$yrs_employed, z = record3d$AMT_INCOME_TOTAL, type = "scatter3d", mode = "markers", color = record3d$AMT_INCOME_TOTAL)
record3

# bubble plot

# Reorder so big bubbles on top

p <- ggplot(record3d, aes(x = yrs_employed, y = yrs_old)) + 
  geom_point(aes(color = AMT_INCOME_TOTAL, size = AMT_INCOME_TOTAL), alpha = 0.25) +
  scale_size(range = c(1, 6)) +
  scale_color_distiller(palette = "RdYlGn")  + 
  labs(title = "Income sizes with age and employment timeframe",
       x = 'Years of employment', y = 'Age in yrs') 

p
ggplotly(p)

# save the widget
# library(htmlwidgets)
# saveWidget(pp, file=paste0( getwd(), "/HtmlWidget/ggplotlyBubblechart.html"))






#### good age comparison
hist <- record %>%
  filter(!(OCCUPATION_TYPE %in% "")) %>%
  ggplot() +
  aes(x = yrs_old, fill = STATUS) +
  geom_histogram(bins = 30L) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title = "Customer account status shown by age", 
       x = 'Age in Years', y = 'Frequency') 

hist
ggplotly(hist)
