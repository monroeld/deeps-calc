library(jsonlite)
library(httr)
library(reshape2)
library(tidyr)
library(tidyverse)
options(stringsAsFactors = F)
# 
# test <- readLines("https://ddowiki.com/page/Item:Agony,_the_Knife_in_the_Dark")
# test <- gsub("")
# 
# testline <- unlist(strsplit(test, "[<>]"))
# testline <- testline[testline != ""]
# testline <- testline[testline != "/div"]
# 
# 
# testing <- as.data.frame(testline, stringsAsFactors = F)
# 
# 
# https://ddowiki.com/api.php
# 
# raw.result <- GET(url = "https://ddowiki.com/page/Item:Agony,_the_Knife_in_the_Dark")
# 
# this.raw <- rawToChar(raw.result$content)
# this.raw2 <- gsub("[\t]+", "\t", this.raw)
# this.rawsplit <- unlist(strsplit(this.raw2, "\n"))
# 
# # Remove tags
# test <- gsub("<(.|\n)*?>","",this.rawsplit)
# test3 <- do.call('rbind', strsplit(test, "\t"))
# 
# test <- gsub("\t", "", test)
# test2 <- test[test != ""]
# test2 <- as.data.frame(test2)
# 
# 
# 
# 
# this.raw <- rawToChar(raw.result$content)
# this.raw2 <- gsub("[\t]+", "\t", this.raw)
# this.rawsplit <- unlist(strsplit(this.raw2, "\n"))
# working <- sapply(this.rawsplit, function(x) {
#   x <- substr(x, 0, el(gregexpr("Icon tooltip.png", x)))
# })
# working2 <- working[working != ""]
# working2 <- gsub("[\t]+", "\t", working2)
# working2 <- gsub("<(.|\n)*?>","",working2)
# working3 <- data.frame(working2, row.names = NULL)
# 
# test <- gsub("<(.|\n)*?>","",this.rawsplit)
# test2 <- do.call('rbind', strsplit(test, "\t"))
# test3 <- gsub("\t", "", test2)
# test3 <- as.data.frame(test3[test3 != ""])
# 
# 
# 
# 




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

# test4 <- data.frame(cbind(t(t5), t(t6), t(t4)))

nones <- which(c(unlist(gregexpr("^ None@@", test3))) > 0)
mls <- which(c(unlist(gregexpr("^ [0-9]+@@", test3))) > 0)
mls <- c(nones, mls)

MLs <- test3[1, mls]

working <- which(c(unlist(regexpr(":", test3[1, ]))) > 0)

Effects <- test3[1, setdiff(working, mls)]


Eff2 <- as.data.frame(t(Effects))
colnames(Eff2) <- "Effs"


# Concatenate all "Tier 3" effects into the columns with "Tier 1 & Tier 2" effects
upp3 <- which(c(unlist(regexec("Upgradeable - Tier 3", as.character(Effects)))) == 2)
Effects2 <- data.frame(cbind(t(Effects[, upp3-1]), t(Effects[, upp3])))
colnames(Effects2) <- c("E12", "E3")
Effects2 <- mutate(Effects2, Eall = paste0(E12, E3))
Effects2$Effs[match(Effects2$E12, Effects2$Effs)] <- Effects2$Eall
Effects3 <- data.frame(t(Effects[1, ])[-c(which(startsWith(t(Effects[1, ]), " Upgradeable - Tier 3"))), ])



allEffs <- lapply(Effects3[, 1], function(x) c(unlist(str_split(x, "@{2,}"))))
Effects3$splits <- allEffs

# Might need to change Effects3's elements to arrays now
test2 <- lapply(Effects3$splits, function(x)
          unname(sapply(unlist(x), function(y) {
            
            # If "Feat:", split on second one
            y <- c(unlist(ifelse(unlist(gregexpr("Feat:", y)) > 1,
                        trimws(substring(y, 0,
                                c(unlist(gregexpr("Feat:",
                                        y[which(regexec("Feat:",y) > -1)])))
                                ),
                              which = "left")[2],
                        
            # Otherwise, split on the colon
                    ifelse(unlist(regexec(":", y)) > 1,
                          trimws(substring(y, 0,
                                   unlist(regexec(":", y))),
                          which = "left"),
                   y)
            # For some reason the feat generates a second copy...
                ))[1])
            
          # Remove final character, halve length because href tags
            #  (now removed) cause repetition
                  y <- substring(y, 0, nchar(y)-1)
                  y <- trimws(ifelse(startsWith(y, "Feat"), y,
                                     substring(y, 0, nchar(y)/2)), which = "right")
                  
          # This may be the place to do all of the filtering?
                  # Seems to work pretty well except for certain named things
                
          }
          
        ))
)

# Still gonna be dicey later, but there are no commas so I can separate
# different effects with those


# Works up until here for sure



# This works on a dummy vector so far
# Substring at ":" or "Feat:"
test2 <- sapply(c(unlist(strsplit(Effects3[, 1], "@@"))), function(x)
  ifelse(is.na(c(unlist(gregexpr("Feat:", x)))[2]),
         substring(x, 0, c(unlist(regexpr(":", x)))),
         substring(x, 0, c(unlist(gregexpr("Feat:", x)))[2])))
names(test2) <- NULL

# Remove left whitespace, final character
test2 <- trimws(test2, which = "left")
test2 <- substring(test2, 0, nchar(test2)-1)

# Halve the string unless it starts with "Feat"
test3 <- trimws(ifelse(startsWith(test2, "Feat"), test2,
                substring(test2, 0, nchar(test2)/2)), which = "right")




# Embed weapon effects in a vector instead of a character string
#    Don't do this until the end because we effectively won't be able to mutate the product, I think...
allEffs <- lapply(Eff2$Effs, function(x) c(unlist(str_split(x, "@{2,}"))))
Eff2$splits <- allEffs
allEffs <- list(c(unlist(strsplit(Eff2$Effs, "@{2,}"))))

# This line cuts off anything after an augment, which does happen
#   Run it later on allEffs
Augless <- mutate(Eff2, Effs = ifelse(c(unlist(regexpr("Augment Slot", Effs))) > 1,
                                   substr(Effs, 0, c(unlist(regexpr("Augment Slot", Effs)))+11),
                                    Effs))


#  Stepping back a minute, what's in the "href" tags?
# testing <- which(regexpr("Special Abilities", data.rawsplit) > 0)
# testing <- data.rawsplit[testing]
# testing2 <- substr(testing, el(gregexpr("Item", testing))[1],
#                 el(gregexpr("The above was automatically compiled", testing))[1]-1)
# 
# # Remove things between "a href" tags?
# hrefstart <- c(unlist(gregexpr("<a href=", testing2)))
# hrefend <- c(unlist(gregexpr("</a>", testing2)))
# 
# newstart <- c(1, hrefend+1)[1:length(hrefend)]
# newend <- c(hrefstart-1)
# 
# testing4 <- sapply(length(newstart):1, function(x)
#                   substr(testing2, newstart[x]+3, newend[x]))

# Try gsubbing out tags now?  Didn't seem to work
# testing5 <- gsub("<(.|\n)*?>","",testing4)
# 




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
