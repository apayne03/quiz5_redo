library(tidyverse)
library(apaTables)

my.data <- read_csv("reg_quiz2_data.csv")

# APA Correlation Table...
apa.cor.table(my.data, filename = "Table1.doc", table.number = 1)


# Make sure data isn't curvilinear, so regression makes sense...
psych::pairs.panels(as.data.frame(my.data))


# Block Regressions...

## SE - PAS 
block1 = lm(aSuc~PAS, data = my.data)
block2 = lm(aSuc~PAS + selfEsteem, data = my.data)
apa.reg.table(block1, block2, filename = "Table2.doc", table.number = 2)

## SE - NAS
block3 = lm(aSuc~NAS, data = my.data)
block4 = lm(aSuc~NAS + selfEsteem, data = my.data)
apa.reg.table(block3, block4, filename = "Table3.doc", table.number = 3)

## SE - PAS + NAS
block5 = lm(aSuc ~ NAS + PAS, data = my.data)
block6 = lm(aSuc ~ NAS + PAS + selfEsteem, data = my.data)
apa.reg.table(block5, block6, filename = "Table4.doc", table.number = 4)
