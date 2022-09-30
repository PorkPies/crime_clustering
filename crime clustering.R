##### packages #####
require('ggplot2')
require('dplyr')
require('cluster')
require('tidyr')
##### read in data #####

file <- '\\\\US-Oakland\\DORMANT6$\\sb16\\Personal Projects\\PoliceUKSep2021.zip'
zipped_csv_names <- grep('\\.csv$', unzip(file, list=TRUE)$Name, 
                         ignore.case=TRUE, value=TRUE)

gloucestershire <- read.csv(unzip(file, file = zipped_csv_names[15]))

#write.csv(gloucestershire, '\\\\US-Oakland\\DORMANT6$\\sb16\\Personal Projects\\unzipped\\gloucester-2020-09.csv')
gloucestershire <- read.csv(('\\\\US-Oakland\\DORMANT6$\\sb16\\Personal Projects\\unzipped\\gloucester-2020-09.csv'))
##### manip #####

df <- gloucestershire %>%
  mutate(Count = as.numeric(1)) %>%
  select(`LSOA.name`, Crime.type, Count) %>%
  group_by(`LSOA.name`, Crime.type) %>%
  across(list(sum)) %>%
  filter(`LSOA.name` != "") %>%
  spread(key = Crime.type, value = Count)

df[is.na(df)] = 0

##### cluster #####

clus <- df[, -c(1,1)]
clus <- clus[complete.cases(clus),]

m <- apply(clus, 2, mean)
s <- apply(clus, 2, sd)
scaled <- scale(clus, m, s)

wss <- (nrow(scaled)-1) * sum(apply(scaled, 2, var))
for (i in 2:20) wss[i] <- sum(kmeans(scaled, centers=i)$withiness)
plot(1:20, wss, type='b', xlab='Number of Clusters', ylab='Within groups sum of squares')

kc <- kmeans(scaled, 2)
kc

z1 <- data.frame(scaled, kc$cluster)
clusplot(z1, kc$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis')



z2 <- data.frame(df, kc$cluster)
z2 <- z2 %>% 
  mutate(Total_crime = select(.,Anti.social.behaviour:Violence.and.sexual.offences) %>% rowSums(na.rm = TRUE))


head(z2[order(-z2$Total_crime), ], 15)
