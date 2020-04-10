# Loading necessary libraries

library(doParallel)
registerDoParallel(makeCluster(4))
library(stringr)
library(dplyr)
library(caret)
library(tau)
library(data.table)
library(quanteda)

# Setting a working directory and opening the database files

setwd("C:/UZYTKI/DATA SCIENCE/Cap_SwiftKey/final/en_US/")

# Definitions of database file names and paths

twit_org_path <- "en_US.twitter.txt"
blog_org_path <- "en_US.blogs.txt"
news_org_path <- "en_US.news.txt"

twit_cln_path <- "en_US.twitter_cl.rds"
blog_cln_path <- "en_US.blogs_cl.rds"
news_cln_path <- "en_US.news_cl.rds"

# Opening and initial cleaning of files

Load_File <- function(file, loaded_file) {
    con <- file(file, open="rb")
    filrd <- readLines(con,  encoding="UTF-8", skipNul = TRUE, warn = FALSE)
    filrd <- iconv(filrd, from="UTF-8", to="ASCII", sub=" ")
    filrd <- tolower(filrd)
    filrd <- str_replace_all(filrd, "([iu]n)-([a-z])", "\\1\\2")
    filrd <- str_replace_all(filrd, "([0-9])(st|nd|rd|th)", "\\1")
    filrd <- str_replace_all(filrd, " \\'|\\' ", " ")
    filrd <- str_replace_all(filrd, "[^a-z.' ]", " ")
    filrd <- str_replace_all(filrd, "([abiep])\\.([cdegm])\\.", "\\1\\2")
    filrd <- str_replace_all(filrd, "([a-z])\\.([a-z])", "\\1 \\2")
    filrd <- str_replace_all(filrd, "( [a-z])\\. ", "\\1 ")
    filrd <- str_replace_all(filrd, " (m[rs]|mrs)\\.", " \\1 ")
    filrd <- str_replace_all(filrd, " (dr|st|rd|av|ave|blvd|ct)\\.", " \\1 ")
    filrd <- str_replace_all(filrd, "\\.$", "")
    filrd <- str_replace_all(filrd, "^ +| +$|", "")
    filrd <- str_replace_all(filrd, " {2,}", " ")
    filrd <- str_replace_all(filrd, " *\\. *","\\.")
    filrd <- str_split(filrd, "\\.")
    filrd <- unlist(filrd)
    filrd <- filrd[filrd != ""]
    saveRDS(filrd, file=loaded_file)
    close(con)
}

twit_base <- Load_File(twit_org_path, twit_cln_path)
blog_base <- Load_File(blog_org_path, blog_cln_path)
news_base <- Load_File(news_org_path, news_cln_path)

# Creation of base for n-grams

twit_base <- readRDS(twit_cln_path)
blog_base <- readRDS(blog_cln_path)
news_base <- readRDS(news_cln_path)

full_base <- c(twit_base, blog_base, news_base)
rm(twit_base, blog_base, news_base)

set.seed(3456)
inFrame <- createDataPartition(y = 1:length(full_base), p = 0.05, list = F)
sample_base <- full_base[inFrame]
rm(full_base, inFrame)

# Creation of n-grams

uni_base <- textcnt(sample_base, method = "string", split = "[[:space:]]", n = 1L, decreasing = T)
bii_base <- textcnt(sample_base, method = "string", split = "[[:space:]]", n = 2L, decreasing = T)
tri_base <- textcnt(sample_base, method = "string", split = "[[:space:]]", n = 3L, decreasing = T)
rm(sample_base)

saveRDS(uni_base, "uni_base.rds")
saveRDS(bii_base, "bii_base.rds")
saveRDS(tri_base, "tri_base.rds")
uni_base <- readRDS("uni_base.rds")
bii_base <- readRDS("bii_base.rds")
tri_base <- readRDS("tri_base.rds")

uni_gram_dt <- data.table(text = names(uni_base), as.matrix(uni_base))
setnames(uni_gram_dt, "V1", "count")
setnames(uni_gram_dt, "text", "word1")
uni_total <- sum(uni_gram_dt$count)
uni_gram_dt <- mutate(uni_gram_dt, freq = round(count/uni_total, 7))
uni_gram_dt$count <- NULL
uni_gram_dt <- data.table(uni_gram_dt)
setkeyv(uni_gram_dt, c("word1", "freq"))
saveRDS(uni_gram_dt, "uni_gram_dt.rds")
rm(uni_total, uni_gram_dt)

bii_base_dt <- data.table(text = names(bii_base), as.matrix(bii_base))
setnames(bii_base_dt, "V1", "count")
bii_gram_dt <- bii_base_dt
bii_gram_dt[, c("word2", "word1")  := do.call(Map, c(f = c, strsplit(text, " ")))]
bii_gram_dt <- mutate(bii_gram_dt, freq = round(count/uni_base[word1][[1]], 7))
bii_gram_dt$text <- NULL
bii_gram_dt$count <- NULL
bii_gram_dt <- data.table(bii_gram_dt)
setkey(bii_gram_dt, word1)
bii_gram_dt <- bii_gram_dt[,lapply(.SD, function(x) head(x, 5)), by = key(bii_gram_dt)]
setkeyv(bii_gram_dt, c("word2", "freq", "word1"))
saveRDS(bii_gram_dt, "bii_gram_dt.rds")
rm(bii_base_dt, uni_base, bii_gram_dt)

tri_base_dt <- data.table(text = names(tri_base), as.matrix(tri_base))
setnames(tri_base_dt, "V1", "count")
tri_gram_dt <- subset(tri_base_dt, count > 1)
tri_gram_dt[, c("word3", "word2", "word1")  := do.call(Map, c(f = c, strsplit(text, " ")))]
tri_gram_dt <- mutate(tri_gram_dt, freq = round(count/bii_base[paste(word3, word2)][[1]], 7))
tri_gram_dt$text <- NULL
tri_gram_dt$count <- NULL
tri_gram_dt <- data.table(tri_gram_dt)
setkeyv(tri_gram_dt, c("word3", "word2"))
tri_gram_dt <- tri_gram_dt[,lapply(.SD, function(x) head(x, 5)),by = key(tri_gram_dt)]
setkeyv(tri_gram_dt, c("word3", "word2", "freq", "word1"))
saveRDS(tri_gram_dt, "tri_gram_dt.rds")
rm(tri_base_dt, bii_base, tri_base, tri_gram_dt)

ban_words <- readLines("banned_words.txt", warn = F)
ban_words <- tolower(ban_words)
ban_words <- str_replace_all(ban_words, "\\(", "\\\\(")
saveRDS(ban_words, "ban_words.rds")
rm(ban_words)