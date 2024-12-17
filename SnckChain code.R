install.packages("tidyr")
install.packages("reshape2")

library(readxl)
library(dplyr)
library(lme4)
library(ggplot2)
library(car)
library(tidyr)
library(stargazer)

# Read the Excel sheets into data frames

stores_df <- read_excel("C:/Users/Akshara/Desktop/SDM/finalproject/SnackChain.xlsx", sheet = "stores")

products_df <- read_excel("C:/Users/Akshara/Desktop/SDM/finalproject/SnackChain.xlsx", sheet = "products")
transactions_df <- read_excel("C:/Users/Akshara/Desktop/SDM/finalproject/SnackChain.xlsx", sheet = "transactions")

temp <- merge(transactions_df, products_df, by.x=c("UPC"), by.y=c("UPC"))  #join by UPCs between transactions and products
df   <- merge(temp, stores_df, by.x=c("STORE_NUM"), by.y=c("STORE_ID"))  #join by adding the stores.
str(df)
View(df)

rm(transactions_df)                              # Memory management is important with big data, remove temporary dfs
rm(stores_df)

# Data Preprocessing
# Remove oral hygiene products and filter for complete cases
df <- df %>%
  filter(CATEGORY != "ORAL HYGIENE PRODUCTS") %>%
  drop_na(c("PRICE", "BASE_PRICE", "FEATURE", "DISPLAY", "TPR_ONLY"))

# Convert columns to factors and relevel
df <- df %>%
  mutate(
    STORE_NUM = as.factor(STORE_NUM),
    UPC = as.factor(UPC),
    CATEGORY = factor(CATEGORY, levels = c("BAG SNACKS", "COLD CEREAL", "FROZEN PIZZA")),
    SEGMENT = factor(SEGMENT, levels = c("VALUE", "MAINSTREAM", "UPSCALE")),
    MONTH = factor(format(WEEK_END_DATE, "%b"), levels = month.abb),
    YEAR = as.numeric(format(WEEK_END_DATE, "%Y")),
    WEEKNUM = as.numeric(difftime(WEEK_END_DATE, min(WEEK_END_DATE), units = "weeks")) + 1,
    DISCOUNT_AMOUNT = BASE_PRICE - PRICE,
    DISCOUNT_RATE = DISCOUNT_AMOUNT / BASE_PRICE
  )

# Feature Engineering: Time-based features
df$MONTH_NUM <- as.numeric(format(df$WEEK_END_DATE, "%m"))
df$DAY_OF_WEEK <- as.numeric(format(df$WEEK_END_DATE, "%u"))

# Exploratory Data Analysis (EDA)
ggplot(df, aes(x = DISCOUNT_RATE, y = SPEND)) + geom_point() + geom_smooth(method = "lm")
ggplot(df, aes(x = DISCOUNT_RATE, y = UNITS)) + geom_point() + geom_smooth(method = "lm")
ggplot(df, aes(x = DISCOUNT_RATE, y = HHS)) + geom_point() + geom_smooth(method = "lm")

#Transformation: Log transformation for SPEND, UNITS, HHS to meet normality assumption
df <- df %>%
  mutate(
    LOG_SPEND = log(SPEND + 1),
    LOG_UNITS = log(UNITS + 1),
    LOG_HHS = log(HHS + 1)
  )
#Data Visualization
#bar chart to visualize sales distribution  by CATEGORY and LOG_SPEND
ggplot(df, aes(x = CATEGORY, y = LOG_SPEND)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Sales Distribution by Category", x = "Category", y = "Sales Volume")

#scatter plot to explore the relationship between discount rate and log Unit Sales
ggplot(df, aes(x = DISCOUNT_RATE, y = LOG_UNITS)) +
  geom_point(aes(color = CATEGORY)) +
  labs(title = "Discount Rate vs. Log Unit Sales",
       x = "Discount Rate",
       y = "log Unit Sales") +
  theme_minimal()

#Time Series Graph: Sales Trends Over Time
df$WEEK_END_DATE <- as.Date(df$WEEK_END_DATE)

ggplot(df, aes(x = WEEK_END_DATE, y = LOG_UNITS)) +
  geom_line(aes(color = CATEGORY), size = 1) +
  labs(title = "Sales Trends Over Time",
       x = "Time",
       y = "Log Unit Sales") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
df$WEEK_END_DATE <- as.Date(df$WEEK_END_DATE)

#Heatmap-Monthly Sales Performance of Product Categories
library(ggplot2)
library(reshape2)  # For melting data frames
# Created a summary table for the heatmap
sales_summary <- df %>%
  group_by(MONTH_NUM, CATEGORY) %>%
  summarise(TotalUnits = sum(LOG_UNITS)) %>%
  ungroup() %>%
  spread(MONTH_NUM, TotalUnits, fill = 0)
melted_sales_summary <- melt(sales_summary, id.vars = "CATEGORY")
ggplot(melted_sales_summary, aes(x = variable, y = CATEGORY, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Monthly Sales Performance by Product Category",
       x = "Month",
       y = "Category") +
  theme_minimal()

#Correlation Matrix Visualization
library(corrplot)
numerical_df <- df %>% select(LOG_UNITS, LOG_SPEND, PRICE, BASE_PRICE, DISCOUNT_RATE)

cor_matrix <- cor(numerical_df, use = "complete.obs")

corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Numerical Variables")
#Models
# Linear Models with promotion indicators and store/category features
model_sales_value <- lm(LOG_SPEND ~ FEATURE + DISPLAY + TPR_ONLY + CATEGORY + SEGMENT, data = df)
model_unit_sales <- lm(LOG_UNITS ~ FEATURE + DISPLAY + TPR_ONLY + CATEGORY + SEGMENT, data = df)
model_household_count <- lm(LOG_HHS ~ FEATURE + DISPLAY + TPR_ONLY + CATEGORY + SEGMENT, data = df)

# Check model assumptions with diagnostic plots
par(mfrow = c(2, 2))
plot(model_sales_value)
plot(model_unit_sales)
plot(model_household_count)

#Random Effects Models to account for store and product level effects
re_sales_value <- lmer(LOG_SPEND ~ FEATURE + DISPLAY + TPR_ONLY + CATEGORY + SEGMENT + (1 | STORE_NUM) + (1 | UPC), data = df)
re_unit_sales <- lmer(LOG_UNITS ~ FEATURE + DISPLAY + TPR_ONLY + CATEGORY + SEGMENT + (1 | STORE_NUM) + (1 | UPC), data = df)
re_household_count <- lmer(LOG_HHS ~ FEATURE + DISPLAY + TPR_ONLY + CATEGORY + SEGMENT + (1 | STORE_NUM) + (1 | UPC), data = df)

# Summaries of the mixed effects models
summary(re_sales_value)
summary(re_unit_sales)
summary(re_household_count)

# Models with Interaction Terms
model_sales_interaction1 <- lm(LOG_SPEND ~ FEATURE * CATEGORY + DISPLAY * SEGMENT + DISCOUNT_RATE, data = df)
model_units_interaction1 <- lm(LOG_UNITS ~ FEATURE * CATEGORY + DISPLAY * SEGMENT + DISCOUNT_RATE, data = df)
model_hhs_interaction1 <- lm(LOG_HHS ~ FEATURE * CATEGORY + DISPLAY * SEGMENT + DISCOUNT_RATE, data = df)

# Summary of models with interaction terms
summary(model_sales_interaction1)
summary(model_units_interaction1)
summary(model_hhs_interaction1)

# Diagnostic plots for models with interaction terms
par(mfrow = c(2, 2))
plot(model_sales_interaction1)
plot(model_units_interaction1)
plot(model_hhs_interaction1)

#Question2

#Linear model with interaction terms for sales value
model_sales_interaction <- lm(LOG_SPEND ~ FEATURE * CATEGORY + DISPLAY * CATEGORY + TPR_ONLY * CATEGORY + 
                                FEATURE * SEGMENT + DISPLAY * SEGMENT + TPR_ONLY * SEGMENT, data=df)
summary(model_sales_interaction)

# Linear model with interaction terms for unit sales
model_units_interaction <- lm(LOG_UNITS ~ FEATURE * CATEGORY + DISPLAY * CATEGORY + TPR_ONLY * CATEGORY + 
                                FEATURE * SEGMENT + DISPLAY * SEGMENT + TPR_ONLY * SEGMENT, data=df)
summary(model_units_interaction)

# Linear model with interaction terms for household count
model_hhs_interaction <- lm(LOG_HHS ~ FEATURE * CATEGORY + DISPLAY * CATEGORY + TPR_ONLY * CATEGORY + 
                              FEATURE * SEGMENT + DISPLAY * SEGMENT + TPR_ONLY * SEGMENT, data=df)
summary(model_hhs_interaction)

#bestmodels
#Summarize best models using stargazer
#model_sales_interaction
#re_sales_value
#model_hhs_interaction
stargazer(model_sales_interaction, re_sales_value, model_hhs_interaction, type = "html", out = "C:/Users/Akshita khazane/Desktop/SDM/finalproject/summary.html")

#Assumption testing
# Diagnostic plots for models with interaction terms
par(mfrow = c(2, 2))
plot(model_sales_interaction)
plot(re_sales_value)
plot(model_hhs_interaction)

# Compare models using AIC, BIC
AIC(model_sales_interaction,re_sales_value,model_hhs_interaction)
BIC(model_sales_interaction,re_sales_value,model_hhs_interaction)

#Multicollinearity     
library(car)
vif(model_sales_interaction)
vif(re_sales_value)
vif(model_hhs_interaction)

# Autocorrelation check using ACF plot for RE model
# Extract residuals from the mixed-effects model
residuals_re_sales_value <- residuals(re_sales_value)
acf(residuals_re_sales_value)

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(model_sales_interaction))
shapiro.test(residuals(re_sales_value))
shapiro.test(residuals(model_hhs_interaction))

# Q-Q plots for visual inspection of normality
qqnorm(residuals(model_sales_interaction)); qqline(residuals(model_sales_interaction))
qqnorm(residuals(re_sales_value)); qqline(residuals(re_sales_value))
qqnorm(residuals(model_hhs_interaction)); qqline(residuals(model_hhs_interaction))

#test of homoscedasticity
library(lmtest)
bptest(model_sales_interaction)
# Breusch-Pagan test may not be directly applicable to mixed models like re_sales_value
bptest(model_hhs_interaction)

#question 3
# Filter out rows where UNITS or PRICE are less than or equal to zero, which can't be log-transformed
df <- df %>%
  filter(UNITS > 0, PRICE > 0)
# Calculate log of UNITS and PRICE
df$log_UNITS <- log(df$UNITS)
df$log_PRICE <- log(df$PRICE)

# Fit a linear model for each product to estimate price elasticity
elasticities <- df %>%
  group_by(UPC) %>%
  do(model = lm(log_UNITS ~ log_PRICE, data = .)) %>% #add more controls
  summarise(elasticity = coef(model)[["log_PRICE"]])

# Sort the products by elasticity to find the most and least price-elastic products
most_elastic <- elasticities %>%
  arrange(desc(elasticity)) %>%
  top_n(5, elasticity)

least_elastic <- elasticities %>%
  arrange(elasticity) %>%
  top_n(-5, elasticity)

# Display the results
print("Most Price-Elastic Products:")
print(most_elastic)

print("Least Price-Elastic Products:")
print(least_elastic)

#Question4
# Calculate price elasticity for each product
df$log_UNITS <- log(df$UNITS + 1)  # Adding 1 to avoid log(0)
df$log_PRICE <- log(df$PRICE)

elasticity_models <- df %>%
  group_by(UPC) %>%
  do(model = lm(log_UNITS ~ log_PRICE, data = .)) %>%
  summarize(elasticity = coef(model)[["log_PRICE"]])

# Extract products with the highest (absolute) negative elasticity
most_elastic_products <- elasticity_models %>%
  arrange(elasticity) %>%
  top_n(-5, elasticity)  #5 most elastic products

# Print the UPCs and their elasticities for the most elastic products
print("Products to consider for price reduction to maximize sales:")
print(most_elastic_products)
