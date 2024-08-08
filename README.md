# RFM, K-Means, and Hierarchical Clustering Analysis

## Introduction
This project involves analyzing gym usage data on a university campus using various data analysis and machine learning techniques. We employ Recency, Frequency, and Monetary (RFM) analysis, K-Means clustering, and Hierarchical clustering to segment and profile gym visitors. 

- RMarkdown file found [here](https://github.com/darensoh/R_Customer_Analytics/blob/main/RFM%2C%20K-Means%20and%20Hierarchical%20Clustering.Rmd).
- PDF file found [here](https://github.com/darensoh/R_Customer_Analytics/blob/main/RFM%2C%20K-Means%20and%20Hierarchical%20Clustering.pdf).
- Data source found [here](https://github.com/darensoh/R_Customer_Analytics/blob/main/gym_staff.xlsx).

## Background
Understanding customer behavior through data analytics can help improve service offerings and customer satisfaction. This project aims to analyze gym usage data to answer key business questions about customer visits and revenue generation, and to profile different segments of gym visitors.

## R Libraries I used:
  - `readxl`: For reading Excel files.
  - `lubridate`: For date manipulation.
  - `tidyverse`: For data manipulation and visualization.
  - `ggplot2`: For data visualization.
  - `treemapify`: For creating treemaps.
  - `rfm`: For RFM analysis.
  - `caret`: For data preprocessing.
  - `cluster`: For clustering algorithms.
  - `factoextra`: For visualizing clustering results.
  - `plotly`: For creating interactive plots.

## Analysis

### 1. Recency, Frequency, and Monetary (RFM) Model
**Objective**: To review the gym usage on campus.

**Questions**:
1. What are the average frequency and duration of gym visits across gym visitors?
2. How can the RFM model help segment and profile our gym visitors?
3. How has the profile of gym users changed over time?
4. Explore other attributes associated with different gym visitor segments.

#### Load the Required Libraries
```
pacman::p_load(readxl, lubridate, tidyverse, ggplot2, treemapify, rfm, caret, cluster, factoextra, plotly)
```
#### Load and Inspect the Data
```
df <- read_excel("..gym_staff.xlsx")  - Copy file path to link the excel file
str(df)
summary(df)
df <- df %>% mutate_if(is.character, as.factor) 
df$Date <- ymd(df$Date)
```
#### Data Cleaning
```
sum(is.na(df))
df[rowSums(is.na(df)) > 0, ] 
sum(duplicated(df))
```
### 2. Latency Analysis
**Objective:** To determine the typical time span between gym visits.

#### Number of Visits per Customer
```
df_visits <- df %>% 
  group_by(ID) %>%
  mutate(Visit = n()) %>%
  distinct(ID, .keep_all = TRUE)
summary(df_visits$Visit)
summary(df_visits)
```
#### Days Between Visits
```
df_days <- df %>%
  group_by(ID) %>%
  mutate(Date_Diff = as.numeric(difftime(Date, lag(Date), units = "days"))) %>%
  mutate(counter = seq_along(ID)) %>%
  arrange(ID)
view(df_days)
df_days <- df_days %>% select(ID, counter, Date_Diff)
```
#### Average Number of Days Between Visits
```
latency <- pivot_wider(df_days, names_from = counter, values_from = Date_Diff)
latency <- as.data.frame(latency)
rownames(latency) <- latency[,"ID"] 
latency$`ID` <- NULL
latency$`1` <- NULL
lmean <- round(colMeans(latency, na.rm = TRUE), digit = 2)
lmedian <- apply(latency, 2, median, na.rm = TRUE)
lcount <- apply(latency, 2, function(x) sum(!is.na(x)))
lsummary <- rbind(lcount, lmean, lmedian)
rownames(lsummary) <- c("Count", "Mean", "Median")
```
### 3. Duration of Each Visit
**Objective:** To calculate the duration of each gym visit.
```
df$duration <- difftime(df$Checkout, df$Checkin, units = "mins")
df$duration <- round(as.numeric(df$duration), 2)
summary(df$duration)
hist(df$duration)
```

### 4. Customer Lifetime Value (CLV) Analysis
**Objective:** To compare revenue generated from single entries versus memberships.

#### Revenue Calculation
```
summary(as.factor(df_visits$Passtype))
df_visits$Membership <- ifelse(grepl("Fitness Gym Membership", df_visits$Passtype) == TRUE, 100, 0) 
df_visits$Single <- ifelse(grepl("Per-Entry Ticket", df_visits$Passtype) == TRUE, df_visits$Visit * 4.50, 0)
Revenue_membership <- sum(df_visits$Membership)
Revenue_single <- sum(df_visits$Single)
Total_revenue <- Revenue_membership + Revenue_single
df_visits$year5 <- ifelse(df_visits$Membership == 100, df_visits$Membership * 5, df_visits$Single * 5)
hist(df_visits$year5)
```

#### Identify Potential Members
```
Potential_member <- df_visits %>% filter(Single > 100)
View(Potential_member)
Potential_member$Savings <- Potential_member$Single - 100
```
### 5. Exploratory Data Analysis (EDA)
**Objective:** To visually explore the dataset and identify patterns.

#### Data Visualization
```
ggplot(df) + geom_bar(aes(y = Passtype)) 
ggplot(df) + geom_boxplot(aes(x = Gender, y = duration)) 
ggplot(df) + geom_bar(aes(y = Age, fill = Passtype), position = "fill") 
ggplot(df) + geom_bar(aes(y = Gender, fill = Passtype), position = "fill") 
ggplot(df, aes(y = ULU, fill = Passtype)) + geom_bar(aes(y = ULU, fill = Passtype)) 
```

### 6. K-Means Clustering
**Objective:** To cluster gym visitors based on their visit frequency, recency, and duration.

#### Data Preprocessing
```
preProcValues <- preProcess(u_df[,c("recency_days", "transaction_count","avg_duration")], method = c("center", "scale"))
u_df_std <- predict(preProcValues, u_df[,c("recency_days", "transaction_count","avg_duration")])
```
#### Determine Optimal Number of Clusters
```
fviz_nbclust(u_df_std[,c("recency_days", "transaction_count","avg_duration")], kmeans, method = "wss")
fviz_nbclust(u_df_std[,c("recency_days", "transaction_count","avg_duration")], kmeans, method = "silhouette") 
```
#### K-Means Clustering with k = 4
```
set.seed(10)
kmeans_4 <- kmeans(u_df_std, 4)
u_df <- u_df %>% mutate(kcluster4 = kmeans_4$cluster)
plot_ly(u_df, x = ~transaction_count, y = ~recency_days, z = ~avg_duration, type = "scatter3d", mode = "markers", color = ~kcluster4)
fviz_cluster(kmeans_4, u_df_std[,c("recency_days", "transaction_count","avg_duration")], ellipse.type = "norm")
sil <- silhouette(kmeans_4$cluster, dist(u_df_std[,c("recency_days", "transaction_count","avg_duration")]))
fviz_silhouette(sil)
```
### 7. Hierarchical Clustering
**Objective:** To perform hierarchical clustering and compare with K-Means clustering results.

#### Hierarchical Clustering with k = 4
```
hc_dist <- dist(u_df_std, method = "euclidean")
hc_cluster <- hclust(d = hc_dist, method = "average")
plot(hc_cluster)
rect.hclust(hc_cluster, k = 4, border = 1:9)
```
#### Hierarchical Clustering with Sample Data
```
set.seed(44)
u_df1 <- u_df[sample(nrow(u_df), 40),]
preProcValues <- preProcess(u_df1[,c("recency_days", "transaction_count","avg_duration")], method = c("center", "scale"))
u_df1_std <- predict(preProcValues, u_df1[,c("recency_days", "transaction_count","avg_duration")])
u_df1_dist <- dist(u_df1_std, method = "euclidean")
u_df1_hc <- hclust(d = u_df1_dist, method = "average")
u_df1$hcluster4 <- cutree(tree = u_df1_hc, k = 4)
plot(u_df1_hc)
rect.hclust(u_df1_hc, k = 4, border = 2:5)
```

## Analysis Steps Summary
1. **RFM Analysis:** We segment gym visitors based on their Recency (last visit time), Frequency (number of visits), and Monetary value (revenue generated).

2. **Latency Analysis:** We calculate the average and median time between visits, uncovering patterns in member engagement.

3. **Customer Lifetime Value (CLV):** We estimate the revenue potential of single-entry users versus members, highlighting the value of memberships.

4. **Exploratory Data Analysis (EDA):** We visualize key metrics like visit frequency, duration, and demographics to identify potential relationships.

5. **K-Means Clustering:** We group gym visitors based on their RFM characteristics, revealing distinct customer segments with varying behavior.

6. **Hierarchical Clustering:** We perform hierarchical clustering for comparison with K-Means results, solidifying our segmentation approach.



## Technical Skills Demonstrated

* **R Programming:** Proficient use of R for data manipulation, analysis, and visualization.
* **Data Cleaning and Preprocessing:** Techniques for handling missing values, outliers, and data transformation.
* **RFM Analysis:** Expertise in customer segmentation using RFM model.
* **Clustering Algorithms:** Implementation of K-Means and Hierarchical clustering for customer segmentation.
* **Data Visualization:** Creation of insightful visualizations using ggplot2 and plotly.

## Insights
* **Membership Drives Revenue Stability:**  Memberships provide a more consistent income stream compared to single-entry ticket sales.
* **Identifying Potential Members:**  By analyzing visit frequency, we pinpoint users who could benefit from memberships, offering targeted promotions.
* **Segment-Specific Insights:**  Clustering techniques reveal distinct customer segments with varying visit patterns and durations, enabling tailored marketing strategies.


## Conclusions
This project underscores the pivotal role of data-driven insights in optimizing business strategies. By employing **RFM analysis**, **K-Means**, and **Hierarchical clustering**, we have successfully segmented gym members into distinct groups based on their behavior patterns. These insights offer a robust foundation for targeted marketing campaigns, personalized services, and optimized resource allocation. Additionally, the analysis of latency and customer lifetime value provides valuable metrics for assessing customer engagement and identifying revenue opportunities.

## Closing Thoughts
In an increasingly competitive landscape, businesses must leverage data to understand their customers deeply. This project demonstrates how R can be a powerful tool for extracting actionable insights from customer data. By effectively segmenting customers and understanding their behavior, organizations can enhance customer satisfaction, increase revenue, and foster long-term loyalty. As data continues to grow in volume and complexity, the ability to analyze and interpret it will become even more critical for business success.
