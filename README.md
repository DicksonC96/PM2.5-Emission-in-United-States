PM2.5 Emission in United States from 1999 to 2008
=================================================

Data source: [EPA National Emissions
Inventory](https://www.epa.gov/technical-air-pollution-resources)  
Project description adapted from
<a href="https://datascience-enthusiast.com/R/EPA_R.html" class="uri">https://datascience-enthusiast.com/R/EPA_R.html</a>.

Fine particulate matter (PM2.5) is an ambient air pollutant for which
there is strong evidence that it is harmful to human health. In the
United States, the Environmental Protection Agency (EPA) is tasked with
setting national ambient air quality standards for fine PM and for
tracking the emissions of this pollutant into the atmosphere.
Approximatly every 3 years, the EPA releases its database on emissions
of PM2.5. This database is known as the National Emissions Inventory
(NEI). You can read more information about the NEI at the EPA National
Emissions Inventory web site.

For each year and for each type of PM source, the NEI records how many
tons of PM2.5 were emitted from that source over the course of the
entire year.

-   PM2.5 Emissions Data (summarySCC\_PM25.rds) contains a data frame
    with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008.
    For each year, the table contains number of tons of PM2.5 emitted
    from a specific type of source for the entire year.  
-   Source Classification Code Table (Source\_Classification\_Code.rds)
    provides a mapping from the SCC digit strings in the Emissions table
    to the actual name of the PM2.5 source, categorized into various
    ways.

### Reading in data

``` r
NEI <- readRDS("../data/summarySCC_PM25.rds")
str(NEI)
```

    ## 'data.frame':    6497651 obs. of  6 variables:
    ##  $ fips     : chr  "09001" "09001" "09001" "09001" ...
    ##  $ SCC      : chr  "10100401" "10100404" "10100501" "10200401" ...
    ##  $ Pollutant: chr  "PM25-PRI" "PM25-PRI" "PM25-PRI" "PM25-PRI" ...
    ##  $ Emissions: num  15.714 234.178 0.128 2.036 0.388 ...
    ##  $ type     : chr  "POINT" "POINT" "POINT" "POINT" ...
    ##  $ year     : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...

``` r
SCC <- readRDS("../data/Source_Classification_Code.rds")
str(SCC)
```

    ## 'data.frame':    11717 obs. of  15 variables:
    ##  $ SCC                : Factor w/ 11717 levels "10100101","10100102",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Data.Category      : Factor w/ 6 levels "Biogenic","Event",..: 6 6 6 6 6 6 6 6 6 6 ...
    ##  $ Short.Name         : Factor w/ 11238 levels "","2,4-D Salts and Esters Prod /Process Vents, 2,4-D Recovery: Filtration",..: 3283 3284 3293 3291 3290 3294 3295 3296 3292 3289 ...
    ##  $ EI.Sector          : Factor w/ 59 levels "Agriculture - Crops & Livestock Dust",..: 18 18 18 18 18 18 18 18 18 18 ...
    ##  $ Option.Group       : Factor w/ 25 levels "","C/I Kerosene",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Option.Set         : Factor w/ 18 levels "","A","B","B1A",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ SCC.Level.One      : Factor w/ 17 levels "Brick Kilns",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ SCC.Level.Two      : Factor w/ 146 levels "","Agricultural Chemicals Production",..: 32 32 32 32 32 32 32 32 32 32 ...
    ##  $ SCC.Level.Three    : Factor w/ 1061 levels "","100% Biosolids (e.g., sewage sludge, manure, mixtures of these matls)",..: 88 88 156 156 156 156 156 156 156 156 ...
    ##  $ SCC.Level.Four     : Factor w/ 6084 levels "","(NH4)2 SO4 Acid Bath System and Evaporator",..: 4455 5583 4466 4458 1341 5246 5584 5983 4461 776 ...
    ##  $ Map.To             : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Last.Inventory.Year: int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Created_Date       : Factor w/ 57 levels "","1/27/2000 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Revised_Date       : Factor w/ 44 levels "","1/27/2000 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Usage.Notes        : Factor w/ 21 levels ""," ","includes bleaching towers, washer hoods, filtrate tanks, vacuum pump exhausts",..: 1 1 1 1 1 1 1 1 1 1 ...

### Total PM2.5 emission between 1999 to 2008 in United States

``` r
totalus <- with(NEI, tapply(Emissions, year, sum))
par(mfrow = c(1,1))
plot(names(totalus), totalus, type = "l", 
     ylim = c(min(totalus)-10^5, max(totalus)+10^6),
     xlim = c(1998, 2009),
     xlab = "Years", 
     ylab = "PM2.5 Emission (in tonnes)", 
     main = "Total PM2.5 emission between 1999 to 2008 in United States")
text(names(totalus), totalus, 
     labels = paste(as.integer(totalus), "+"), pos = 3)
```

![](README_files/figure-markdown_github/plot1-1.png)

Obviously, we can observe a significant downtrend on total PM2.5
emission in US.

### Total PM2.5 emission between 1999 to 2008 in Baltimore City, Maryland

``` r
totalbc <- with(NEI[NEI$fips == "24510",], tapply(Emissions, year, sum))
par(mfrow = c(1,1))
plot(names(totalbc), totalbc, type = "l", 
     ylim = c(min(totalbc)-10^3, max(totalbc)+10^3),
     xlim = c(1998, 2009),
     xlab = "Years", 
     ylab = "PM2.5 Emission (in tonnes)", 
     main = "Total PM2.5 emission between 1999 to 2008 in Baltimore City, Maryland")
text(names(totalbc), totalbc, 
     labels = paste(as.integer(totalbc), "+"), pos = 3)
```

![](README_files/figure-markdown_github/plot2-1.png)

Similarly, total PM2.5 emission in Baltimore City shows a gradual drop
in comparison with total emission in whole.

### Emissions from 1999 to 2008 for Baltimore City by source type

``` r
library(dplyr)
emisbc <- NEI %>% filter(fips == "24510") %>%
        select(Emissions, type, year)
classify <- emisbc %>% 
        group_by(type, year) %>%
        summarize(emissions = sum(Emissions))
library(ggplot2)
with(classify, qplot(year, emissions, color = type,
                     main = "Emissions from 1999 to 2008 for Baltimore City by source type",
                     xlab = "Years",
                     ylab = "PM2.5 Emissions (in tonnes)") + 
             geom_smooth(method = "lm", se = FALSE))
```

![](README_files/figure-markdown_github/plot3-1.png)

Generally, all except point emission source types exhibits a downward
trend.

### Emissions from coal combustion-related sources across United States from 1999 to 2008

``` r
coalbool <- grepl("Coal", SCC$EI.Sector)
coaldata <- SCC[coalbool, ]
merg <- merge(coaldata, NEI, by = "SCC")
library(dplyr)
df <- merg %>%
        select(EI.Sector, year, Emissions) %>%
        group_by(EI.Sector, year) %>%
        summarize(emission = sum(Emissions))
library(ggplot2)
with(df, qplot(year, emission, color = EI.Sector,
               main = "Emissions from coal combustion-related sources across United States from 1999 to 2008",
               xlab = "Years",
               ylab = "PM2.5 Emissions (in tonnes)") +
        geom_smooth(method = "lm", se = FALSE))
```

![](README_files/figure-markdown_github/plot4-1.png)

The plotting shows differences in trend among coal-related emission,
with electric generation stand out from the rest.

### Emissions from motor vehicle sources in Baltimore City from 1999 to 2008

``` r
motorbool <- grepl("Vehicle", SCC$EI.Sector)
motordata <- SCC[motorbool, ]
merg <- merge(motordata, NEI[NEI$fips == "24510", ], by = "SCC")
library(dplyr)
df <- merg %>%
        select(EI.Sector, year, Emissions) %>%
        group_by(EI.Sector, year) %>%
        summarize(emission = sum(Emissions))
library(ggplot2)
with(df, qplot(year, emission, color = EI.Sector,
               main = "Emissions from motor vehicle sources in Baltimore City from 1999 to 2008",
               xlab = "Years",
               ylab = "PM2.5 Emissions (in tonnes)") +
             geom_smooth(method = "lm", se = FALSE))
```

![](README_files/figure-markdown_github/plot5-1.png)

Similarly, the plot shows differences in trend among emissions from
different motor vehicle source. The vast changes in diesel heavy duty
and gasoline light duty vehicle emission may be due to their popularity
and usage.

### Emissions from motor vehicle sources in Baltimore City from 1999 to 2008

``` r
motorbool <- grepl("Vehicle", SCC$EI.Sector)
motordata <- SCC[motorbool, ]
merg <- merge(motordata, NEI[grepl("24510|06037", NEI$fips), ], by = "SCC")
merg$fips <- factor(merg$fips, levels = c("24510", "06037"), labels = c("Baltimore City", "Los Angeles"))
library(dplyr)
df <- merg %>%
        select(fips, year, Emissions) %>%
        group_by(fips, year) %>%
        summarize(emission = sum(Emissions))
bcdiff <- as.integer(df[4,3]-df[1,3])
ladiff <- as.integer(df[8,3]-df[5,3])
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

![](README_files/figure-markdown_github/plot6-1.png)

Again, different cities illustrate huge different in their value and
trend on PM2.5 vehicle emission. This could also reflects how developed
the city is (at least from the plotting).
