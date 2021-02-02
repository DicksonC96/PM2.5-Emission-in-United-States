---
title: 'PM2.5 Emission in United States from 1999 to 2008'
author: 'DicksonC'
output: html_document
---

# PM2.5 Emission in United States from 1999 to 2008
Data source: [EPA National Emissions Inventory](https://www.epa.gov/technical-air-pollution-resources)  
Project description adapted from https://datascience-enthusiast.com/R/EPA_R.html.  

Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong evidence that it is harmful to human health. In the United States, the Environmental Protection Agency (EPA) is tasked with setting national ambient air quality standards for fine PM and for tracking the emissions of this pollutant into the atmosphere. Approximatly every 3 years, the EPA releases its database on emissions of PM2.5. This database is known as the National Emissions Inventory (NEI). You can read more information about the NEI at the EPA National Emissions Inventory web site.  

For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that source over the course of the entire year.  

* PM2.5 Emissions Data (summarySCC_PM25.rds) contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year.  
* Source Classification Code Table (Source_Classification_Code.rds) provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source, categorized into various ways.  

### Reading in data

```r
NEI <- readRDS("../data/summarySCC_PM25.rds")
```

```
## Warning in gzfile(file, "rb"): cannot open compressed
## file '../data/summarySCC_PM25.rds', probable reason 'No
## such file or directory'
```

```
## Error in gzfile(file, "rb"): cannot open the connection
```

```r
str(NEI)
```

```
## Error in str(NEI): object 'NEI' not found
```

```r
SCC <- readRDS("../data/Source_Classification_Code.rds")
```

```
## Warning in gzfile(file, "rb"): cannot open compressed
## file '../data/Source_Classification_Code.rds', probable
## reason 'No such file or directory'
```

```
## Error in gzfile(file, "rb"): cannot open the connection
```

```r
str(SCC)
```

```
## Error in str(SCC): object 'SCC' not found
```

### Total PM2.5 emission between 1999 to 2008 in United States

```r
totalus <- with(NEI, tapply(Emissions, year, sum))
```

```
## Error in with(NEI, tapply(Emissions, year, sum)): object 'NEI' not found
```

```r
par(mfrow = c(1,1))
plot(names(totalus), totalus, type = "l", 
     ylim = c(min(totalus)-10^5, max(totalus)+10^6),
     xlim = c(1998, 2009),
     xlab = "Years", 
     ylab = "PM2.5 Emission (in tonnes)", 
     main = "Total PM2.5 emission between 1999 to 2008 in United States")
```

```
## Error in plot(names(totalus), totalus, type = "l", ylim = c(min(totalus) - : object 'totalus' not found
```

```r
text(names(totalus), totalus, 
     labels = paste(as.integer(totalus), "+"), pos = 3)
```

```
## Error in text(names(totalus), totalus, labels = paste(as.integer(totalus), : object 'totalus' not found
```
  
Obviously, we can observe a significant downtrend on total PM2.5 emission in US.  
  
### Total PM2.5 emission between 1999 to 2008 in Baltimore City, Maryland

```r
totalbc <- with(NEI[NEI$fips == "24510",], tapply(Emissions, year, sum))
```

```
## Error in with(NEI[NEI$fips == "24510", ], tapply(Emissions, year, sum)): object 'NEI' not found
```

```r
par(mfrow = c(1,1))
plot(names(totalbc), totalbc, type = "l", 
     ylim = c(min(totalbc)-10^3, max(totalbc)+10^3),
     xlim = c(1998, 2009),
     xlab = "Years", 
     ylab = "PM2.5 Emission (in tonnes)", 
     main = "Total PM2.5 emission between 1999 to 2008 in Baltimore City, Maryland")
```

```
## Error in plot(names(totalbc), totalbc, type = "l", ylim = c(min(totalbc) - : object 'totalbc' not found
```

```r
text(names(totalbc), totalbc, 
     labels = paste(as.integer(totalbc), "+"), pos = 3)
```

```
## Error in text(names(totalbc), totalbc, labels = paste(as.integer(totalbc), : object 'totalbc' not found
```
  
Similarly, total PM2.5 emission in Baltimore City shows a gradual drop in comparison with total emission in whole.  
  
### Emissions from 1999 to 2008 for Baltimore City by source type

```r
library(dplyr)
emisbc <- NEI %>% filter(fips == "24510") %>%
        select(Emissions, type, year)
```

```
## Error in filter(., fips == "24510"): object 'NEI' not found
```

```r
classify <- emisbc %>% 
        group_by(type, year) %>%
        summarize(emissions = sum(Emissions))
```

```
## Error in group_by(., type, year): object 'emisbc' not found
```

```r
library(ggplot2)
with(classify, qplot(year, emissions, color = type,
                     main = "Emissions from 1999 to 2008 for Baltimore City by source type",
                     xlab = "Years",
                     ylab = "PM2.5 Emissions (in tonnes)") + 
             geom_smooth(method = "lm", se = FALSE))
```

```
## Error in with(classify, qplot(year, emissions, color = type, main = "Emissions from 1999 to 2008 for Baltimore City by source type", : object 'classify' not found
```
    
Generally, all except point emission source types exhibits a downward trend.  
  
### Emissions from coal combustion-related sources across United States from 1999 to 2008

```r
coalbool <- grepl("Coal", SCC$EI.Sector)
```

```
## Error in grepl("Coal", SCC$EI.Sector): object 'SCC' not found
```

```r
coaldata <- SCC[coalbool, ]
```

```
## Error in eval(expr, envir, enclos): object 'SCC' not found
```

```r
merg <- merge(coaldata, NEI, by = "SCC")
```

```
## Error in merge(coaldata, NEI, by = "SCC"): object 'coaldata' not found
```

```r
library(dplyr)
df <- merg %>%
        select(EI.Sector, year, Emissions) %>%
        group_by(EI.Sector, year) %>%
        summarize(emission = sum(Emissions))
```

```
## Error in select(., EI.Sector, year, Emissions): object 'merg' not found
```

```r
library(ggplot2)
with(df, qplot(year, emission, color = EI.Sector,
               main = "Emissions from coal combustion-related sources across United States from 1999 to 2008",
               xlab = "Years",
               ylab = "PM2.5 Emissions (in tonnes)") +
        geom_smooth(method = "lm", se = FALSE))
```

```
## Error in eval(substitute(expr), data, enclos = parent.frame()): invalid 'envir' argument of type 'closure'
```
  
The plotting shows differences in trend among coal-related emission, with electric generation stand out from the rest.  
  
### Emissions from motor vehicle sources in Baltimore City from 1999 to 2008

```r
motorbool <- grepl("Vehicle", SCC$EI.Sector)
```

```
## Error in grepl("Vehicle", SCC$EI.Sector): object 'SCC' not found
```

```r
motordata <- SCC[motorbool, ]
```

```
## Error in eval(expr, envir, enclos): object 'SCC' not found
```

```r
merg <- merge(motordata, NEI[NEI$fips == "24510", ], by = "SCC")
```

```
## Error in merge(motordata, NEI[NEI$fips == "24510", ], by = "SCC"): object 'motordata' not found
```

```r
library(dplyr)
df <- merg %>%
        select(EI.Sector, year, Emissions) %>%
        group_by(EI.Sector, year) %>%
        summarize(emission = sum(Emissions))
```

```
## Error in select(., EI.Sector, year, Emissions): object 'merg' not found
```

```r
library(ggplot2)
with(df, qplot(year, emission, color = EI.Sector,
               main = "Emissions from motor vehicle sources in Baltimore City from 1999 to 2008",
               xlab = "Years",
               ylab = "PM2.5 Emissions (in tonnes)") +
             geom_smooth(method = "lm", se = FALSE))
```

```
## Error in eval(substitute(expr), data, enclos = parent.frame()): invalid 'envir' argument of type 'closure'
```
  
Similarly, the plot shows differences in trend among emissions from different motor vehicle source. The vast changes in diesel heavy duty and gasoline light duty vehicle emission may be due to their popularity and usage.  
  
### Emissions from motor vehicle sources in Baltimore City from 1999 to 2008

```r
motorbool <- grepl("Vehicle", SCC$EI.Sector)
```

```
## Error in grepl("Vehicle", SCC$EI.Sector): object 'SCC' not found
```

```r
motordata <- SCC[motorbool, ]
```

```
## Error in eval(expr, envir, enclos): object 'SCC' not found
```

```r
merg <- merge(motordata, NEI[grepl("24510|06037", NEI$fips), ], by = "SCC")
```

```
## Error in merge(motordata, NEI[grepl("24510|06037", NEI$fips), ], by = "SCC"): object 'motordata' not found
```

```r
merg$fips <- factor(merg$fips, levels = c("24510", "06037"), labels = c("Baltimore City", "Los Angeles"))
```

```
## Error in factor(merg$fips, levels = c("24510", "06037"), labels = c("Baltimore City", : object 'merg' not found
```

```r
library(dplyr)
df <- merg %>%
        select(fips, year, Emissions) %>%
        group_by(fips, year) %>%
        summarize(emission = sum(Emissions))
```

```
## Error in select(., fips, year, Emissions): object 'merg' not found
```

```r
bcdiff <- as.integer(df[4,3]-df[1,3])
```

```
## Error in df[4, 3]: object of type 'closure' is not subsettable
```

```r
ladiff <- as.integer(df[8,3]-df[5,3])
```

```
## Error in df[8, 3]: object of type 'closure' is not subsettable
```

```r
library(ggplot2)
ggplot(df, aes(year, emission)) +
        labs(title = "Emissions from motor vehicle sources in Baltimore City from 1999 to 2008",
              x = "Years",
              y = "PM2.5 Emission (in tonnes)") +
        geom_point(aes(color = fips)) +
        geom_smooth(aes(color = fips), method = "lm", se = FALSE) +
        annotate("text", x = 2006, y = 500, label = paste("1999-2008 Diff =", bcdiff)) +
        annotate("text", x = 2006, y = 3900, label = paste("1999-2008 Diff =", ladiff))
```

```
## Error:   You're passing a function as global data.
##   Have you misspelled the `data` argument in `ggplot()`
```
  
Again, different cities illustrate huge different in their value and trend on PM2.5 vehicle emission. This could also reflects how developed the city is (at least from the plotting).
  
