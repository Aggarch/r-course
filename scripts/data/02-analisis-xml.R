install.packages("XML")
library(XML)

url <- "../data/tema1/cd_catalog.xml"
url <- "C:/Users/ANALITICA/Desktop/AG/calendar_statement.xml"


xmldoc <- xmlParse(url) #XMLInternalDocument
rootnode <- xmlRoot(xmldoc)
rootnode[2]

cds_data <- xmlSApply(rootnode, function(x) xmlSApply(x, xmlValue) )
cds.catalog <- data.frame(t(cds_data), row.names = NULL)
head(cds.catalog, 2)
cds.catalog[1:5,]

#xpathSApply()
#getNodeSet()

population_url <- "https://www.myfxbook.com/forex-economic-calendar"

population_url <- "../data/tema1/WorldPopulation-wiki.htm"
tables <- readHTMLTable(population_url)

most_populated <- tables[[6]]
head(most_populated, 3)

custom_table <- readHTMLTable(population_url, which = 6)

# as_tibble(cds_data) %>% mutate(Time = substr(events, 0, 19),
#                                Country = str_extract(events,"()"))