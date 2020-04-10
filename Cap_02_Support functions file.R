library(stringr)
library(data.table)
library(openNLP)
library(NLP)

annot_tok_sent <- Maxent_Sent_Token_Annotator()
annot_tok_word <- Maxent_Word_Token_Annotator()
annot_tag_posi <- Maxent_POS_Tag_Annotator()

setwd("C:/UZYTKI/DATA SCIENCE/Cap_SwiftKey/final/en_US/")

uni_gram_dt <- readRDS("uni_gram_dt.rds")
bii_gram_dt <- readRDS("bii_gram_dt.rds")
tri_gram_dt <- readRDS("tri_gram_dt.rds")
ban_words <- readRDS("ban_words.rds")

txt_clr <- function(text) {
    txt_in <- tolower(text)    
    txt_in <- str_replace_all(txt_in, "([iu]n)-([a-z])", "\\1\\2")
    txt_in <- str_replace_all(txt_in, "([0-9])(st|nd|rd|th)", "\\1")
    txt_in <- str_replace_all(txt_in, "[^a-z.' ]", " ")
    txt_in <- str_replace_all(txt_in, "www\\.[a-z]+\\.[a-z]+", "")
    txt_in <- str_replace_all(txt_in, "\\.", " ")
    txt_in <- str_replace_all(txt_in, " ([a-z])\\1+ |^([a-z])\\1+ | ([a-z])\\1+$|^([a-z])\\1+$", " ")
    txt_in <- str_replace_all(txt_in, "([a-z])\\1{2,}", "\\1\\1")
    txt_in <- str_replace_all(txt_in, "\\'+([a-z]+)\\'+", "\\1")
    txt_in <- str_replace_all(txt_in, "\\'+ \\'+", " ")
    txt_in <- str_replace_all(txt_in, "(\\'+ )+|( \\'+)+|^\\'+|\\'+$", " ")
    txt_in <- str_replace_all(txt_in, "^[a-z]+$", "")
    txt_in <- str_replace_all(txt_in, "( [^ai])+ |^([^ai] )+|( [^ai])+$", " ")
    txt_in <- str_replace_all(txt_in, "^ +| +$|", "")
    txt_in <- str_replace_all(txt_in, " {2,}", " ")
    txt_in <- str_replace_all(txt_in, " +$|^ +", "")
    return(txt_in)
}

txt_flt <- function(text) {
    tmp_txt <- text
    if (length(tmp_txt) > 0) {
        words_in <- txt_prs(tmp_txt)
        words_in_qty <- length(words_in)
        if (words_in_qty > 0) {
            for (i in 1:words_in_qty) {
                if (words_in[i] %in% ban_words) words_in[i] <- paste(substring(words_in[i], 1, 1), "***", sep = "")
            }
            tmp_word <- paste(words_in[1]) 
            if (words_in_qty > 1) {
                for (i in 2:words_in_qty) tmp_word <- paste(tmp_word, words_in[i])
            }
            return(tmp_word)
        }
    }
    return(tmp_txt)
}

txt_get_df <- function(text) {
    if (length(text) > 0) {
        annot_1 <- annotate(as.String(text), list(annot_tok_sent, annot_tok_word))
        annot_2 <- annotate(as.String(text), annot_tag_posi, annot_1)
        annot_2_wrd <- subset(annot_2, type == "word")
        tags <- sapply(annot_2_wrd$features, `[[`, "POS")
        if (tags %like% "NN") {
            return("in")
        } else if (tags %like% "VB") {
            return("a")
        } else if (tags %like% "JJ") {
            return("time")
        } else if (tags %like% "PRP") {
            return("first")
        } else if (tags %like% "CC") {
            return("i")
        } else if (text == "the") {
            return("first")
        }
    }
    return("the")
}

txt_prs <- function(text) {
    tmp_txt2 <- unlist(str_split(text, " "))
    tmp_txt2 <- tmp_txt2[tmp_txt2 != ""]
    return(tmp_txt2)
}

wrd_catch <- function(text) {
    if (text != " ") { 
        words_catch <- txt_prs(tolower(text))
        words_catch_qty <- length(words_catch)
        if (words_catch_qty > 0) {
            filter <- paste("^", words_catch[words_catch_qty], sep = "")
            tmp_dt <- uni_gram_dt[word1 %like% filter]
            pred_word <- dim(tmp_dt)[1]
            if (pred_word > 0) {
                tmp_dt <- tmp_dt[order(rank(-freq))]
                pred <- tmp_dt[1]$word1
                if (words_catch_qty > 2) {
                    tmp_w <- paste(words[1])
                    for (i in 2:(words_catch_qty - 1)) tmp_w <- paste(tmp_w, words_catch[i])
                    return(paste(tmp_w, txt_flt(pred)))
                } else if (words_catch_qty > 1) {
                    tmp_w <- paste(words_catch[1])
                    return(paste(tmp_w, txt_flt(pred)))
                }
            }
        }
    }
    return(text)
}

pred_catch <- function(text) {
    if (text != " ") { 
        user_txt <- txt_prs(txt_clr(text))
        lgt_user_txt <- length(user_txt)
        
        if (lgt_user_txt > 1) {
            wrd1 <- user_txt[lgt_user_txt]
            wrd2 <- user_txt[lgt_user_txt - 1]
        } else if (lgt_user_txt > 0) {
            wrd1 <- user_txt[lgt_user_txt]
            wrd2 <- "NA"
        } else return("N/A")
        
        l1 <- .95
        l2 <- .04
        l3 <- .01
        
        lgt_user_txt_3 <- length(tri_gram_dt[tri_gram_dt[word3 == wrd2 & word2 == wrd1]]$freq)
        lgt_user_txt_2 <- length(bii_gram_dt[bii_gram_dt[word2 == wrd1]]$freq)
        pred_match <- matrix(nrow = lgt_user_txt_3 + lgt_user_txt_2, ncol = 2)
        pred_match[,1] <- ""
        pred_match[,2] <- 0
        
        if (lgt_user_txt_3 > 0) {
            for (i in 1:lgt_user_txt_3) {
                pred_match[i, 1] <- tri_gram_dt[tri_gram_dt[word3 == wrd2 & word2 == wrd1]]$word1[i]
                pred_cnt2 <- length(bii_gram_dt[bii_gram_dt[word2 == wrd1 & word1 == pred_match[i, 1]]]$freq)
                pred_cnt1 <- length(uni_gram_dt[uni_gram_dt[word1 == pred_match[i, 1]]]$freq)
                if (pred_cnt2 > 0) freq2 <- bii_gram_dt[bii_gram_dt[word2 == wrd1 & 
                                                                        word1 == pred_match[i, 1]]]$freq else freq2 <- 0
                if (pred_cnt1 > 0) freq1 <- uni_gram_dt[uni_gram_dt[word1 == pred_match[i, 1]]]$freq else freq1 <- 0
                pred_match[i, 2] <- tri_gram_dt[tri_gram_dt[word3 == wrd2 & word2 == wrd1]]$freq[i] * 
                    l1 + freq2 * l2 + freq1 * l3     
            }
        }
        if (lgt_user_txt_2 > 0) {
            for (i in sum(lgt_user_txt_3, 1):sum(lgt_user_txt_3, lgt_user_txt_2)) {
                pred_match[i, 1] <- bii_gram_dt[bii_gram_dt[word2 == wrd1]]$word1[i - lgt_user_txt_3]
                pred_cnt1 <- length(uni_gram_dt[uni_gram_dt[word1 == pred_match[i, 1]]]$freq)
                if (pred_cnt1 > 0) freq1 <- uni_gram_dt[uni_gram_dt[word1 == pred_match[i, 1]]]$freq else freq1 <- 0
                pred_match[i, 2] <- bii_gram_dt[bii_gram_dt[word2 == wrd1]]$freq[i - lgt_user_txt_3] * l2 + freq1 * l3   
            }
        }
        pred_match_lgt <- length(pred_match[which.max(pred_match[,2])])
        if (pred_match_lgt > 0) return(pred_match[which.max(pred_match[,2])])
        return(txt_get_df(wrd1))
    }
    return(" ")
}