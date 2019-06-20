library(jsonlite)
library(httr)
library(reshape2)
library(tidyr)
options(stringsAsFactors = F)

test <- readLines("https://ddowiki.com/page/Item:Agony,_the_Knife_in_the_Dark")
test <- gsub("")

testline <- unlist(strsplit(test, "[<>]"))
testline <- testline[testline != ""]
testline <- testline[testline != "/div"]


testing <- as.data.frame(testline, stringsAsFactors = F)


https://ddowiki.com/api.php

raw.result <- GET(url = "https://ddowiki.com/page/Item:Agony,_the_Knife_in_the_Dark")

this.raw <- rawToChar(raw.result$content)
this.raw2 <- gsub("[\t]+", "\t", this.raw)
this.rawsplit <- unlist(strsplit(this.raw2, "\n"))

# Remove tags
test <- gsub("<(.|\n)*?>","",this.rawsplit)
test3 <- do.call('rbind', strsplit(test, "\t"))

test <- gsub("\t", "", test)
test2 <- test[test != ""]
test2 <- as.data.frame(test2)




this.raw <- rawToChar(raw.result$content)
this.raw2 <- gsub("[\t]+", "\t", this.raw)
this.rawsplit <- unlist(strsplit(this.raw2, "\n"))
working <- sapply(this.rawsplit, function(x) {
  x <- substr(x, 0, el(gregexpr("Icon tooltip.png", x)))
})
working2 <- working[working != ""]
working2 <- gsub("[\t]+", "\t", working2)
working2 <- gsub("<(.|\n)*?>","",working2)
working3 <- data.frame(working2, row.names = NULL)

test <- gsub("<(.|\n)*?>","",this.rawsplit)
test2 <- do.call('rbind', strsplit(test, "\t"))
test3 <- gsub("\t", "", test2)
test3 <- as.data.frame(test3[test3 != ""])








## This bit is working



data <- GET(url = "https://ddowiki.com/page/Category:Hand_items")
data.raw <- rawToChar(data$content)
# data.raw2 <- gsub("[\t]+", "\t", data.raw)
data.rawsplit <- unlist(strsplit(data.raw, "\t"))
data.rawsplit <- gsub("<(.|\n)*?>","",data.rawsplit)
test <- data.rawsplit[grep("Special Abilities", data.rawsplit)]
test2 <- gsub("\n", "@", test)
test2 <- substr(test2, el(gregexpr("Item", test2))[1],
                el(gregexpr("The above was automatically compiled", test2))[1]-1)


test3 <- data.frame(do.call('rbind', data.frame(unlist(strsplit(test2, "@{3,}")))))
columnnames <- c(unlist(strsplit(test3[1, 1], "@@")))
names(columnnames) = NULL
test3 <- test3[1, 2:ncol(test3)]

# t4 <- test3[1, c(1:405)%%3-2 == 0]
# t5 <- test3[1, c(1:405)%%3-1 == 0]
# t6 <- test3[1, c(1:405)%%3 == 0]

test4 <- data.frame(cbind(t(t5), t(t6), t(t4)))

nones <- which(c(unlist(gregexpr("^ None@@", test3))) > 0)
mls <- which(c(unlist(gregexpr("^ [0-9]+@@", test3))) > 0)
mls <- c(nones, mls)

MLs <- test3[1, mls]

working <- which(c(unlist(regexpr(":", test3[1, ]))) > 0)

Effects <- test3[1, setdiff(working, mls)]

# mergeUpgrade <- which(c(unlist(regexec("Upgradeable - Tier 3", as.character(Effects)))) == 2)

Eff2 <- as.data.frame(t(Effects))
colnames(Eff2) <- "Effs"
Augless <- mutate(Eff2, Effs = ifelse(c(unlist(regexpr("Augment Slot", Effs))) > 1,
                                   substr(Effs, 0, c(unlist(regexpr("Augment Slot", Effs)))+11),
                                    Effs))

upp3 <- which(c(unlist(regexec("Upgradeable - Tier 3", as.character(Effects)))) == 2)

test <- cbind(Augless[upp3-1, ], Augless[upp3, ])


# augslots <- which(c(unlist(regexec("Augment Slot", as.character(Effects[1, ])))) > 1)
# noaugs <- Effects[1, ]
# 
# Augless <- data.frame(substr(as.character(Effects[1, augslots]), 0,
#                   c(unlist(regexec("Augment Slot", as.character(Effects[1, augslots])))) +11))
# augment <- data.frame(as.character(Effects)[which(c(unlist(gregexpr("Augment Slot", Effects[1, ]))) >= 0)])

# noAt <- which(c(unlist(regexec("@", as.character(test3[1, ])))) == -1)
# Names <- test3[1, setdiff(mls, noAt)]

# colnames(test3) <- NULL
# rownames(test3) <- NULL

# test4 <- data.frame(t(rbind(test3, ceiling(as.numeric(cut(1:ncol(test3), breaks = ncol(test3)/3))))))
# test5 <- spread(test4, key = "X2", value = "X1")
