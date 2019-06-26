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
fulltable<- data.rawsplit[grep("Special Abilities", data.rawsplit)]
fulltable <- gsub("\n", "@", fulltable)
fulltable <- substr(fulltable, el(gregexpr("Item", fulltable))[1],
                el(gregexpr("The above was automatically compiled", fulltable))[1]-1)


fulltable <- data.frame(do.call('rbind', data.frame(unlist(strsplit(fulltable, "@{3,}")))))
columnnames <- c(unlist(strsplit(fulltable[1, 1], "@@")))
names(columnnames) = NULL
fulltable <- fulltable[1, 2:ncol(fulltable)]


mlInds <- which(c(unlist(gregexpr("(^ None@@)|(^ [0-9]+@@)", fulltable)))>0)


# Effects have colons
colons <- grep(":", fulltable[1, ])
Effects <- fulltable[1, setdiff(colons, mlInds)]
Effects <- t(Effects)




# Concatenate all "Tier 3" effects into the columns with "Tier 1 & Tier 2" effects
upp1 <- which(c(unlist(regexec("Upgradeable - Tier 1", Effects))) == 2)
upp3 <- which(c(unlist(regexec("Upgradeable - Tier 3", Effects))) == 2)
Effects2 <- data.frame(cbind(Effects[upp3-1], Effects[upp3]))
colnames(Effects2) <- c("E12", "E3")
Effects2 <- mutate(Effects2, Eall = paste0(E12, E3))


Effects[upp1] <- Effects2$Eall
Effects <- Effects[-c(upp3)]

nEffects <- lengths(str_split(Effects, "@@"))
allEffects <- c(unlist(str_split(Effects, "@@")))


# Clean this up a bit
MLs <- as.character(fulltable[1, mlInds])
MLs <- trimws(substr(MLs, 0, c(unlist(regexpr("@@", MLs)))-1), which = "left")
MLs <- as.numeric(gsub("None", 0, MLs))
MLs <- rep(MLs, times = nEffects)

# Names never have colons in them!
gearNames <- as.character(fulltable[1, setdiff(c(1:ncol(fulltable)),
                                               colons)])
gearNames <- rep(gearNames, times = nEffects)


gloves <- data.frame(cbind(gearNames, MLs, allEffects))


# Working dataset complete!  Time for some dplyr
gloves$augs <- unlist(regexec("Augment Slot", gloves$allEffects))
gloves$feats <- unlist(regexec("[a-z]Feat:", gloves$allEffects))
gloves$charges <- unlist(regexec("Charges:", gloves$allEffects))
gloves$CL <- unlist(regexec("Caster level:", gloves$allEffects))
gloves$set <- unlist(regexec("Set", gloves$allEffects))



gloves$upgrades <- unlist(regexec("^ Upgradeable", gloves$allEffects))
gloves$historic <- unlist(gregexpr("(historic)", gloves$gearNames))

# Just leave duplicated, outdated crystal cove gear...


gloves$colons <- unlist(regexec(":", gloves$allEffects))
gloves <- gloves %>% filter(upgrades < 0, historic < 0) %>%
                    mutate(allEffects = ifelse(augs > 0,
                           substr(allEffects, 0, augs+11),
                          allEffects)) %>%
                    mutate(allEffects = ifelse(feats > 0,
                            substr(allEffects, 0, feats),
                          allEffects)) %>%
                    mutate(allEffects = ifelse(charges > 0,
                          paste0(":Spell", substr(allEffects, 0, CL-1)),
                          allEffects)) %>%
                    mutate(set = ifelse(set > 0,
                            allEffects,
                          "None"))

gloves$junction <- unlist(regexpr("([a-z]|[0-9]|[%]|[IVX])[A-Z]", gloves$allEffects))



test <- gloves %>% mutate(allEffects = ifelse(junction > 0,
                                              substr(allEffects, 0, junction),
                          allEffects))


# test <- gloves %>% mutate(allEffects = ifelse(charges+feats+augs < 0,
#                                               substr(allEffects, 0, colons),
#                                               allEffects))

test <- gloves %>% str_which(allEffects, "Feat:")
  
  mutate(Effs = ifelse(str_which(allEffects, "Feat:"), 1, 0))




# Before we do something crazy like below, see if we can get the other columns as vectors
  # If so, can we repeat each one by the lengths of splits' sub-arrays and bind those?


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








# This line cuts off anything after an augment, which does happen
#   Run it later on allEffs
Augless <- mutate(Eff2, Effs = ifelse(c(unlist(regexpr("Augment Slot", Effs))) > 1,
                                   substr(Effs, 0, c(unlist(regexpr("Augment Slot", Effs)))+11),
                                    Effs))



