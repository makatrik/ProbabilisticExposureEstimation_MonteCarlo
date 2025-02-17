SumConcChemical <- as.data.frame(SumConcChemical)
SumFoodIntake <- as.data.frame(SumFoodIntake)


###### 1st way of calculating the exposure pr Person and food category
set.seed(123)

#### Join the dataframes and keep only the columns of interest
join_dataframes <-inner_join(SumFoodIntake, SumConcChemical, by = "FoodCat") %>% 
  select(FoodCat, IDKode, Amount, Loc, Shape)
View(join_dataframes)

#### Expression to be replicated - Exposure estimates in μg/day
## divide by 1000 to express it in μg/day
exposure_expr <- ddply(join_dataframes, .(FoodCat, IDKode), mutate, value = rlnorm(1, Loc, Shape)) %>% 
  mutate(New_val = (Amount * value)/1000) %>%  select(New_val, Amount, FoodCat, IDKode)
View(exposure_expr) 

#### Order by IDKode and FoodCat
exposure_exprI <- exposure_expr[with(exposure_expr, order("IDKode","FoodCat"))]
View(exposure_exprI) 

exposure_expr <-  exposure_expr[order(exposure_expr$IDKode),]
View(exposure_expr)


### MC simulations of 1000 times#### 
#############
MCout2 <- replicate(1000, ddply(join_dataframes, .(FoodCat, IDKode), mutate, value = rlnorm(1, Loc, Shape))%>% 
                      mutate(New_val = (Amount * value)/1000) %>%  select(New_val, FoodCat, IDKode), simplify = FALSE)

##View lists as data.frames
df_MC <- as.data.frame(do.call(cbind, MCout2))
View(df_MC)

#make all the column names unique is by using the make.names() function and the unique=TRUE option 
names(df_MC) <- make.names(names(df_MC), unique=TRUE)
View(df_MC)

#####Order by IDKode
df_MC <-  df_MC[order(df_MC$IDKode),]
View(df_MC)

#### remove FoodCat and IDKode repeating columns
df_MC <- df_MC %>% 
  select(-matches(c("Food", "ID"))) 
View(df_MC)

##Add the FoodCat and IDKode columns
df_MCI <- cbind(SumFoodIntake$IDKode, SumFoodIntake$FoodCat, df_MC)
View(df_MCI)
#change the name of the columns of the df_MCI
colnames(df_MCI)[colnames(df_MCI) == 
c('SumFoodIntake$IDKode', 'SumFoodIntake$FoodCat')] <- c('IDKode', 'FoodCat')
View(df_MCI)

#######Probabilistic exposure estimates from the diaries - lnorm distribution ####
############# 
####Add chemical exposure (in ug) group for all IDs per FoodCat (goup_by IDKode)
Sum_IDKode <- df_MCI%>% 
  group_by(IDKode) %>%
  summarise(across(New_val:New_val.999, ~ sum (.x, na.rm = TRUE)))
View(Sum_IDKode)

###Summary data of total chemical exposure for each ID
# create a row-wise dataframe and per IDKode
row_Sum_IDKode <- Sum_IDKode %>% rowwise(IDKode) %>% 
  summarise(N = length(c_across(New_val:New_val.999)),
            sd = sd(c_across(New_val:New_val.999)),
            mean = mean(c_across(New_val:New_val.999)),
            min = min(c_across(New_val:New_val.199)),
            P05= quantile(c_across(New_val:New_val.999), .05),
            P50= quantile(c_across(New_val:New_val.999), .50),
            P95= quantile(c_across(New_val:New_val.999), .90),
            max= max(c_across(New_val:New_val.999)))
View(row_Sum_IDKode)


# Basic ECDF plot of total Chemical exposure using ggplot
ggplot(row_Sum_IDKode, aes(mean))+
  stat_ecdf(geom = "point", size = 1, col = "blue")+
  xlab("Total Chemical exposure (ug)")+
  ylab("Cumulative probability")

# Multiple ECDF colored by group
df <- data.frame(x=c(row_Sum_IDKode$mean, row_Sum_IDKode$P95), g = gl(2, 141))
View(df)
df %>% 
  ggplot(aes(x=x, col = g))+
  stat_ecdf(geom = "point", size = 1)+
  scale_colour_hue(name="Statistics", labels=c('mean','P95'))+
  xlab("Chemical exposure (ug)")+
  ylab("Cumulative probability")

############# 
###Average Chemical exposure (in ug) over all MC (divide by 1000) per FoodCat & IDKode### 
Sum_FoodCat <- df_MCI%>% 
  group_by(FoodCat, IDKode) %>%
  summarise(sum_value = sum(c_across(New_val:New_val.999)/1000))
View(Sum_FoodCat)

library(ggplot2)
#### plot the exposure for all IDs per FoodCat
ggplot(data = Sum_FoodCat, aes(x=FoodCat, y=sum_value, color = FoodCat))+
  geom_boxplot(lwd=0.9)+
  theme_minimal()+
  scale_colour_hue()+
  theme(text = element_text(size = 25))+
  scale_y_log10(limits=c(0.5,60), labels = scales::comma)+
  labs(
    title = "Average chemical exposure (in µg) over Monte Carlo simulations per Food Category",
    x = "Food category",
    y = "Chemical exposure (ug/day)")

