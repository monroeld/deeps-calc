library(jsonlite)
library(httr)
library(reshape2)
library(tidyr)
library(tidyverse)
library(utils)
options(stringsAsFactors = F)




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
ML <- rep(MLs, times = nEffects)

# Names never have colons in them!
gearName <- as.character(fulltable[1, setdiff(c(1:ncol(fulltable)),
                                               colons)])
gearName <- rep(gearName, times = nEffects)


gloves <- data.frame(cbind(gearName, ML, allEffects))
# Just leave duplicated, outdated crystal cove gear for now
#   When effects are finalized, maybe compare toupper(gearName)

# Working dataset complete!  Time for some kludging and dplyr

# Swap out weird characters after Insightful, Quality, etc
gloves$allEffects <- gsub("[&][^a-zA-Z]+;", " ", gloves$allEffects)

gloves$augs <- unlist(regexec("Augment Slot", gloves$allEffects))
gloves$feats <- unlist(regexec("[a-z]Feat:", gloves$allEffects))
gloves$charges <- unlist(regexec("Charges:", gloves$allEffects))
gloves$CL <- unlist(regexec("Caster level:", gloves$allEffects))
gloves$set <- unlist(regexec("Set", gloves$allEffects))
gloves$upgrades <- unlist(regexec("^ Upgradeable", gloves$allEffects))
gloves$historic <- unlist(gregexpr("(historic)", gloves$gearName))

# Doesn't work for DR. Can't mod junction to work with capital letters
  # Tooltip says "Damage Reduction" so it's not a repeat.  Maybe hard-code?

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
                    mutate(set = ifelse(set > 0, allEffects, "None"))


# Deduplicate effects with junctions
gloves$junction <- unlist(regexpr("([a-z]|[0-9]|[%]|[IVX]|[)])[A-Z]", gloves$allEffects))
gloves <- gloves %>% mutate(details = ifelse(junction > 0,
                                             substring(allEffects, 2*junction+1),
                                             "None")) %>%
                     mutate(allEffects = ifelse(junction > 0,
                                                substr(allEffects, 0, junction),
                                                allEffects))

# Some things didn't have junctions because of extra spaces. They have colons though.  Split at the colon
gloves$colons <- unlist(regexec(":", gloves$allEffects))
gloves$setcolons <- unlist(regexec(":", gloves$set))
gloves$setjunction <- unlist(regexpr("([a-z]|[0-9]|[%]|[IVX]|[)])[A-Z]", gloves$set))

# Make the remaining junction changes and remove extra columns
gloves <- gloves %>%
  mutate(deets = ifelse(charges+feats < 0 & colons > 0, 1, 0)) %>%
  mutate(details = ifelse(deets == 1 & details == "None",
                             substring(allEffects, colons+1),
                             details)) %>%
  mutate(allEffects = ifelse(charges+feats < 0 & colons > 0,
                            substr(allEffects, 0, floor(colons)/2),
                            allEffects)) %>%
                     mutate(set = ifelse(setcolons > 0,
                            substr(set, 0, floor(setcolons)/2),
                            set)) %>%
                    mutate(set = ifelse(setjunction > 0,
                                        substr(set, 0, setjunction),
                                        set)) %>%
                    select(-c(augs, feats, charges, CL, junction,
                              setcolons, setjunction, upgrades, historic, colons, deets))

gloves$colons <- unlist(regexec(":", gloves$details))
gloves <- gloves %>% mutate(details = ifelse(colons > 0,
                                             substring(details, colons+1),
                                             details)) %>% select(-colons)


# Create a new one to mess with.  This can get culled later
newgloves <- gloves
newgloves$allEffects <- trimws(newgloves$allEffects, which = "both")

newgloves$bonus <- unlist(regexpr("( [+]+[0-9]+)|( [IVX]+$)|( [0-9]+)",
                                  newgloves$allEffects))


newgloves <- newgloves %>% mutate(Effect = ifelse(bonus > 0,
                                  trimws(substr(allEffects, 0, bonus), which = "both"),
                                                  allEffects)) %>%
                           mutate(Bonus = ifelse(bonus > 0,
                                   trimws(substring(allEffects, bonus), which = "both"),
                                   "None")) %>% select(-c(allEffects, bonus))
                           

newgloves$Bonus <- gsub("[+]|[%]", "", newgloves$Bonus)
newgloves$Effect <- gsub("( [-]$)|([-]$)", "", newgloves$Effect)
suppressWarnings({ newgloves <- newgloves %>% mutate(Bonus = ifelse(!is.na(as.numeric(as.roman(Bonus))),
                                                as.numeric(as.roman(Bonus)),
                                                Bonus)) })

final <- newgloves

# Time to classify effects, I guess...

types <- c("Enhancement", "Equipment", "Profane", "Exceptional", "Insight", "Quality", "Competence")
test <- data.frame(do.call(cbind, lapply(types, function(x) regexec(x, final$Effect))))
colnames(test) <- types
test2 <- data.frame(do.call(cbind, lapply(types, function(x) regexec(x, final$details))))
colnames(test2) <- types
final <- cbind(final, test)
