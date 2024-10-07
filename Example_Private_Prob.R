SumConcAcrylamide <- as.data.frame(SumConcAcrylamide)
SumFoodIntakeBothDays <- as.data.frame(SumFoodIntakeBothDays)


###### 1st way of calculating the exposure pr Person and food category
set.seed(123)

#### Join the dataframes and keep only the columns of interest
join_dataframes <-inner_join(SumFoodIntakeBothDays, SumConcAcrylamide, by = "FoodCat") %>% 
  select(FoodCat, IDKode, Amount, Loc, Shape)
View(join_dataframes)


#### Expression to be replicated - Exposure estimates in μg/day
## divide by 1000 to express it in μg/day
### ddply function: Split data frame (df), apply function, and return results in a df
##https://www.rdocumentation.org/packages/plyr/versions/1.8.7/topics/ddply
exposure_expr <- ddply(join_dataframes, .(FoodCat, IDKode), mutate, value = rlnorm(1, Loc, Shape)) %>% 
  mutate(New_val = (Amount * value)/1000) %>%  select(New_val, Amount, FoodCat, IDKode)
View(exposure_expr) 

#### Order by IDKode and FoodCat
exposure_exprI <- exposure_expr[with(exposure_expr, order("IDKode","FoodCat"))]
View(exposure_exprI) 

exposure_expr <-  exposure_expr[order(exposure_expr$IDKode),]
View(exposure_expr)