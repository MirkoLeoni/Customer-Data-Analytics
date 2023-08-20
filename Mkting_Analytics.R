# LOAD ----

# library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(arules)
library(arulesViz)
library(rfm)
library(lubridate)

setwd("C:/Users/Mirko/Desktop/mkt_project")
output_path = "C:/Users/Mirko/Desktop/mkt_project/Output"
# df_raw = read_excel("Master_DB.xlsx", sheet = "Database_Master", col_names = T)
# saveRDS(df_raw, "df_raw.rds")
df_raw = readRDS("df_raw.rds")

segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

# CREATE DF ----

df = df_raw %>% select(store, ticket_id, id_cust, qty, tot_price, 
                       prod_description, date_wt_hour) %>%
  mutate(store = as.factor(store), ticket_id = as.factor(ticket_id),
         id_cust = as.factor(id_cust), prod_description = as.factor(prod_description), unit_price = abs(tot_price/qty)) %>%
  filter(unit_price > 0.10 & prod_description != "Home delivery") %>% mutate(prod_description = factor(prod_description), 
                                                                             date = date(date_wt_hour))


# id_transaction
df$id_transaction_aux = paste(as.character(df$id_cust), 
                              as.character(df$date_wt_hour))
j = 1
for (i in unique(df$id_transaction_aux)) {
  df[df$id_transaction_aux == i,"id_transaction"] = j
  j = j+1
}

df = df %>% select(-id_transaction_aux) %>%
      mutate(id_transaction = factor(id_transaction))

# df$store_description = ifelse(df$store == "576", "900",
#        ifelse(df$store ==  "519" | df$store ==  "552", "600", "400" ))
# df$store_description = as.factor(df$store_description)

writexl::write_xlsx(df, file.path(output_path, "ClenaedDF.xlsx"), col_names = T)
rm(list = c('j','i'))

# PROMOTION cleaning ----

# example boxplot befor cleaning
aux = (df %>% filter(prod_description == "Pasta"))
bplot01 = ggplot(aux, aes( y = unit_price)) +
            geom_boxplot(fill = "lightblue", col = "blue", outlier.color = "red") +
            labs(title = 'BoxPlot item "Pasta" before cleaning', y = "Unitary price")+
            ylim(0, NA)+
            theme_classic()



price_limit = data.frame(prod_description = levels(df$prod_description), 
                         limit = rep(0,nlevels(df$prod_description)))
row_to_rm = NULL

for (i in unique(price_limit$prod_description)) {
  aux01 = (boxplot.stats((df %>% filter(prod_description == i))$unit_price))$stats[1]
  price_limit[price_limit$prod_description == i, "limit"] = aux01
  aux02 = which(df$prod_description == i & df$unit_price < aux01)
  row_to_rm = c(row_to_rm, aux02)
}

df = df[-row_to_rm,]

# example boxplot after cleaning
bplot02 = ggplot((df %>% filter(prod_description == "Pasta")), aes( y = unit_price)) +
  geom_boxplot(fill = "lightblue", col = "blue", outlier.color = "red") +
  labs(title = 'BoxPlot item "Pasta" after cleaning', y = "Unitary price")+
  ylim(0, NA)+
  theme_classic()

ggpubr::ggarrange(bplot01, bplot02, ncol = 2, nrow = 1)

rm(aux)

# CASHIERS cleaning ----

df_RFM = aggregate(tot_price ~ id_transaction + date_wt_hour + id_cust, df, FUN = "sum")%>%
  filter(tot_price > 0) %>% mutate(id_cust = as.character(id_cust), 
                                   date_wt_hour = as.Date(date_wt_hour)) %>%
  select(-id_transaction)

analysis_date = max(df_RFM$date_wt_hour)+1

rfm_result = rfm_table_order(df_RFM, id_cust, date_wt_hour , tot_price, analysis_date)
rfm_dataframe = rfm_result$rfm
outlier_customers = rfm_dataframe[rfm_dataframe$transaction_count > 90, "customer_id"]

# outlier plot
ggplot(rfm_dataframe, aes(x = customer_id,y = transaction_count)) + 
  geom_bar(stat = "identity", fill = "lightblue", col = "blue")+
  geom_hline(yintercept = 90, linetype = "dashed", color = "blue", size = 1.2)+
  labs(title = "Number of transaction per client", x = "Customer ID", y = "Transaction count")+
  theme(plot.title = element_text(size=25))+
  theme_classic()


df_adj = filter(df, !(id_cust %in% outlier_customers$customer_id))
df_outlier = filter(df, (id_cust %in% outlier_customers$customer_id))

writexl::write_xlsx(df_adj, file.path(output_path, "DF_Adjusted.xlsx"), col_names = T)


# DF STRUCTURE ----

table(is.na(df)) #-> NO NAs
length(unique(df$id_cust)) #-> 225 customers
length(unique(df$store)) #-> 8 stores
length(unique(df$prod_description)) #-> 286 product sold + "Home delivery"
length(unique(df$id_transaction)) #-> 41834 tickets done
summary(df)

nrow(df_outlier)/nrow(df) #-> 97% dataset built by cashiers

sum(rfm_dataframe[rfm_dataframe$transaction_count >= 90, "transaction_count"])/sum(rfm_dataframe$transaction_count)
#-> 97% transaction done by cashiers

sum(rfm_dataframe[rfm_dataframe$transaction_count >= 90, "amount"])/sum(rfm_dataframe$amount)
#-> 97% revenue in cashiers transactions

plot(density((filter(.data = df, tot_price > 0) %>% 
                aggregate(tot_price ~ id_transaction, ., FUN = "sum"))$tot_price))

# CUSTOMER MOVEMENTS ----

df_adj = filter(df, !(id_cust %in% outlier_customers$customer_id))
cust_store_i = data.frame(id_cust = unique((filter(df, !(id_cust %in% outlier_customers$customer_id)))$id_cust))
cust_store_i$id_cust = factor(cust_store_i$id_cust)

j = 2
for (i in levels(df$store)) {
  aux = select(df_adj, id_cust, store) %>% filter(store == i) %>% 
    distinct() %>% filter(!(id_cust %in% outlier_customers$customer_id))
  aux[,2] = 1
  cust_store_i = plyr::join(cust_store_i, aux, by = "id_cust")
  colnames(cust_store_i)[j] = i
  j = j+1
}
cust_store_i$sum = rowSums(cust_store_i[,2:9], na.rm = T)

table(cust_store_i$sum)

rm(list = c('i', 'j'))

writexl::write_xlsx(cust_store_i, file.path(output_path, "CustomerMovements.xlsx"), col_names = T)

# RECENCY FREQUENCY MONETARY ANALYSIS ----

rfm_result_adj = rfm_table_order(df_RFM[!(df_RFM$id_cust %in% outlier_customers$customer_id), ],
                                 id_cust, date_wt_hour , tot_price, analysis_date)
rfm_dataframe_adj = rfm_result_adj$rfm

rfm_heatmap(rfm_result_adj)
rfm_bar_chart(rfm_result_adj)
rfm_histograms(rfm_result_adj) #-> distribution of RFM

segments <- rfm_segment(rfm_result_adj, segment_names, recency_lower,
                        recency_upper, frequency_lower, frequency_upper, monetary_lower,
                        monetary_upper)

rfm_plot_median_recency(segments)
rfm_plot_median_frequency(segments)
rfm_plot_median_monetary(segments)

rfm_order_dist(rfm_result_adj) + theme_classic()

n_cust_segment =data.frame(segment = c(segment_names,'Others'),
                           n_cust = rep(0, length(c(segment_names,'Others'))))

for(i in c(segment_names,'Others')){
  n_cust_segment[n_cust_segment$segment == i, "n_cust"] = (nrow(filter(segments, segment == i)))
}

writexl::write_xlsx(rfm_result_adj$rfm, file.path(output_path, "RFM.xlsx"), col_names = T)
writexl::write_xlsx(n_cust_segment, file.path(output_path, "N_CustPerSegment.xlsx"), col_names = T)

# MOST SOLD PRODUCTS ----

# tot_qty_sold = data.frame(prod_description = character(), qty = numeric())
# j=1
# for (i in as.character(unique(df$prod_description))) {
#   aux = aggregate(qty  ~ prod_description, df, FUN = "sum") %>% filter(prod_description == i)
#   tot_qty_sold[j, "prod_description"] = as.character(aux$prod_description)
#   tot_qty_sold[j, "qty"] = aux$qty
#   j=j+1
# }
# tot_qty_sold$prod_description = factor(tot_qty_sold$prod_description)
# 
# tot_qty_sold = tot_qty_sold %>% arrange(desc(qty)) %>% mutate(perc_tot = round(qty/sum(qty)*100, digits = 2))
# 
# tot_qty_sold_store = data.frame(store = numeric(), prod_description = character(), qty = numeric())
# for (i in as.character(unique(df$prod_description))) {
#   aux = aggregate(qty  ~ store + prod_description, df, FUN = "sum") %>% filter(prod_description == i)
#   tot_qty_sold_store = rbind(tot_qty_sold_store, aux)
# }
# 
# as.character(tot_qty_sold[which.max(tot_qty_sold$qty), "prod_description"])
# tot_qty_sold[which.max(tot_qty_sold$qty), "qty"]
# 
# as.character(tot_qty_sold[which.min(tot_qty_sold$qty), "prod_description"])
# tot_qty_sold[which.min(tot_qty_sold$qty), "qty"]

best_prod_store = data.frame(prod_description = levels(df$prod_description))
j = 2
for (i in levels(df$store)) {
  aux = (filter(df, store == i) %>% aggregate(qty  ~ prod_description, ., FUN = "sum") %>% arrange(-qty))[1:25,]
  aux$rank = 1:25
  best_prod_store = plyr::join(best_prod_store, aux, by = "prod_description")
  colnames(best_prod_store)[j] = paste("qty", i, sep = "_")
  colnames(best_prod_store)[j+1] = paste("rank", i, sep = "_")
  j = j+2 
}


best_prod_store = best_prod_store[rowSums(best_prod_store[2:17], na.rm = T) != 0, ]
overall_best_prod = (aggregate(qty  ~ prod_description, df, FUN = "sum") %>% arrange(-qty))
best_prod_store = plyr::join(best_prod_store, overall_best_prod, by = "prod_description")
colnames(best_prod_store)[18] = "qty_tot"

avg_qty_prod = data.frame(prod_description = levels(df$prod_description), 
                          avg = rep(0,nlevels(df$prod_description)),
                          var = rep(0,nlevels(df$prod_description)) )
                          
for (i in levels(df$prod_description)) {
  avg_qty_prod[avg_qty_prod$prod_description == i, "avg"] = round(mean((filter(df, prod_description == i) %>% 
                                                               aggregate(qty ~ id_transaction, ., FUN = "sum") %>% select(qty))$qty), 
                                                               digits = 2)
  avg_qty_prod[avg_qty_prod$prod_description == i, "var"] = round(var((filter(df, prod_description == i) %>% 
                                                               aggregate(qty ~ id_transaction, ., FUN = "sum") %>% select(qty))$qty),
                                                             digits = 2)
}

nrow(na.omit(best_prod_store)) 
#-> 10 products in top 25 for all stores
#-> 3 products in top 10 for all stores: Cookies, Pasta, Paper and disposable tools 

rm(list = c('i','j'))

writexl::write_xlsx(best_prod_store, file.path(output_path, "BestProducts.xlsx"), col_names = T)

# MARKET BASKET ANALYSIS ----

#-> high support means that there will be a large number of future transaction to
#   which will be applicable the cross selling
#-> confidence = num of transaction with beer+diapers / number of transaction with beer

list_CBA = split(as.character(df$prod_description), df$id_transaction)
for (i in names(list_CBA)) {
  list_CBA[[i]] = unique(list_CBA[[i]])
}

transaction_list = as(list_CBA, 'transactions')

basket_rules = apriori(transaction_list, 
                        parameter = list(supp = 0.01, conf = 0.01, 
                                         target = 'rules', minlen = 2))

plot(transaction_rules, method="graph")

itemFrequencyPlot(transaction_list, topN = 10)
itemFrequencyPlot(transaction_list, support = 0.1)

BasketAnalysis = DATAFRAME((basket_rules))

plot(basket_rules[1:20], jitter = 0)

writexl::write_xlsx(BasketAnalysis, file.path(output_path, "BasketAnalysis.xlsx"), col_names = T)
rm(i)

# number of transaction per store
num_transaction = NULL
for(i in levels(df$store)){
  df_aux = df %>% filter(store == i) %>% select(-store)
  df_aux$ticket_id = factor(df_aux$ticket_id)
  df_aux$id_cust = factor(df_aux$id_cust)
  df_aux$prod_description = factor(df_aux$prod_description)
  df_aux$id_transaction = factor(df_aux$id_transaction)
  list_aux = split(as.character(df_aux$prod_description), df_aux$id_transaction)
  
  transaction_list_aux = as(list_aux, 'transactions')
  aux02 = length(transaction_list_aux)
  num_transaction = c(num_transaction, aux02)
}

#### STORE 516 ####

df_516 = df %>% filter(store == '516') %>% select(-store)
df_516$ticket_id = factor(df_516$ticket_id)
df_516$id_cust = factor(df_516$id_cust)
df_516$prod_description = factor(df_516$prod_description)
df_516$id_transaction = factor(df_516$id_transaction)
#str(df_516)

# num of transactions 3 months 
tot_transaction_516 = nlevels(df_516$id_transaction)
# tot sales 3 months
tot_revenue_516 = sum(df_516$tot_price)

# num of customers 3 months
n_of_customer_516 = levels(df_516$id_cust)

# euro/transaction 
summary01_516 = summary(aggregate(tot_price ~ id_transaction, df_516, FUN = "sum")$tot_price)

#plot type 1
plot(density(aggregate(tot_price ~ id_transaction, df_516, FUN = "sum")$tot_price),
                  main = "expense per transaction store 516")
abline(v = summary01_516[4], col = "blue")
legend("topright", legend = paste("MEAN VALUE = ",round(summary01_516[4], digits = 2)),
       col = "blue", lty = 1, cex=0.8)

#plot type 2
ggplot(aggregate(tot_price ~ id_transaction, df_516, FUN = "sum"), aes(x=tot_price)) + 
  geom_density(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(tot_price)),color="blue", linetype="dashed", size=1)+
  theme_classic()

# revenue per day
summary02_516 = summary(aggregate(tot_price ~ date, df_516, FUN = "sum")$tot_price)

plot(density(aggregate(tot_price ~ date, df_516, FUN = "sum")$tot_price),
     main = "revenue per day store 516")
abline(v = summary02_516[4], col = "blue")
legend("topright", legend = paste("MEAN VALUE = ",round(summary02_516[4], digits = 2)),
       col = "blue", lty = 1, cex=0.8)

ggplot(aggregate(tot_price ~ date, df_516, FUN = "sum"), aes(x=tot_price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="lightblue") +
  geom_vline(aes(xintercept=mean(tot_price)),color="blue", linetype="dashed", size=1) +
  labs(title = "Revenue per day distribution store 516", x = "revenue per day")+
  theme(plot.title = element_text(size=20))+
  theme_classic()

# MBA

list_CBA_516 = split(as.character(df_516$prod_description), df_516$id_transaction)

transaction_list_516 = as(list_CBA_516, 'transactions')
basket_rules_516 = apriori(transaction_list_516,
                           parameter = list(supp = 0.03, conf = 0.3, target = 'rules', minlen = 2))

plot(basket_rules_516, method="graph", engine = "htmlwidget")

BasketAnalysis_516 = DATAFRAME((basket_rules_516))

# save data

output_path_516 = "C:/Users/Mirko/Desktop/mkt_project/Output/516"

writexl::write_xlsx(BasketAnalysis_516, file.path(output_path_516, "BasketAnalysis_516.xlsx"), col_names = T)
writexl::write_xlsx(rfm_dataframe_adj_516$rfm, file.path(output_path_516, "RFM_516.xlsx"), col_names = T)
