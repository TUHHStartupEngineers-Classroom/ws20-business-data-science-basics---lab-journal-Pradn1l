# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library("writexl")

# 2.0 Importing Files ----
bikes_tbl      <- read_excel(path = "~/Data science/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel(path = "~/Data science/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel(path = "~/Data science/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
View(orderlines_tbl)
View(bikeshops_tbl)
View(bikes_tbl)

# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>% left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>% left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>% separate(col    = location, into   = c("city", "State"), sep    = ",") %>% 
mutate(total.price = price * quantity) %>% rename(bikeshop = name) %>% set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----

# 6.1 Sales by State ----

# Step 1 - Manipulate
sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>% select(State, total_price) %>% group_by(State) %>% summarize(sales = sum(total_price)) %>%
mutate(sales_text = scales::dollar(sales, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"))
View(sales_by_state_tbl)

sales_by_city_tbl <- bike_orderlines_wrangled_tbl %>% select(city, total_price) %>% group_by(city) %>% summarize(sales = sum(total_price)) %>%
mutate(sales_text = scales::dollar(sales, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"))
View(sales_by_city_tbl)

# Step 2 - Visualize


sales_by_state_tbl %>%
ggplot(aes(x = State, y = sales)) +
geom_col(fill = "#2DC6D6") + 
geom_label(aes(label = sales_text)) +
geom_smooth(method = "lm", se = FALSE) + 
scale_y_continuous(labels = scales::dollar_format(big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")) +
labs(title    = "Revenue by state", subtitle = "Upward Trend", x = "", y = "Revenue")+
theme(axis.text.x = element_text(angle = 45, hjust = 1))


sales_by_city_tbl %>%
ggplot(aes(x = city, y = sales)) +
geom_col(fill = "#2DC6D6") + 
geom_label(aes(label = sales_text)) +
geom_smooth(method = "lm", se = FALSE) + 
scale_y_continuous(labels = scales::dollar_format(big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")) +
labs(title    = "Revenue by city", subtitle = "Upward Trend", x = "", y = "Revenue")+
theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 6.2 Sales by Year and location ----

# Step 1 - Manipulate
sales_by_location_year_tbl <- bike_orderlines_wrangled_tbl %>% select(State, order_date, total_price) %>%  mutate(year = year(order_date)) %>%
group_by(State ,year) %>% summarize(sales = sum(total_price)) %>% mutate(sales_text = scales::dollar(sales, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"))
View(sales_by_location_year_tbl)

# Step 2 - Visualize
sales_by_location_year_tbl %>%
ggplot(aes(x = year, y = sales)) +
geom_col(fill = "#2DC6D6") + 
geom_label(aes(label = sales_text)) +
geom_smooth(method = "lm", se = FALSE) + 
scale_y_continuous(labels = scales::dollar_format(big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")) +
labs(title    = "Revenue by state and year", subtitle = "Upward Trend", x = "", y = "Revenue")+
theme(axis.text.x = element_text(angle = 45, hjust = 1))+
facet_wrap(vars(State))