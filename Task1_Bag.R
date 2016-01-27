setwd("/Users/shenguangmin/Desktop/TextMining/final")
# 1. Tokenization: identifying appropriate tokens such as words, punctuation, 
# and numbers. Writing a function that takes a file as input and returns a 
# tokenized version of it.
library(tm)
library(RWeka)
bags <- function(chunk){
    chunk <- paste(chunk, collapse=' ')
    chunk <- paste0(chunk, ' ')
    name_pattern <- "( [a-z]{1,} )+((([A-Z][a-z]{1,})+ ){2,3})+([a-z]|[[:punct:]])"
    m <- gregexpr(name_pattern, chunk)
    names <- regmatches(chunk, m)
    df_names <- as.data.frame(table(names))
    df_names = t(df_names)
    sub_pattern <- "((([A-Z][a-z]{1,})+ ){1,2})+([A-Z][a-z]{1,})"
    names <- df_names[1,]
    m <- gregexpr(sub_pattern, names)
    names <- unlist(regmatches(names, m))
    df_names[1,] <- names
    values = df_names[2,]
    col_names = df_names[1,]
    nd = rbind(values)
    colnames(nd) = col_names

    chunk = tolower(chunk)
    words = gsub("([[:punct:]]){1,}(+ )", " ", chunk)
    words = gsub("( +)([[:punct:]]){1,}", " ", words)
    words = gsub("( +)([[:punct:]]){1,}(+ )", " ", words)
    words = gsub("( +)([0-9]){1,}(+ )", " ", words)
    corpus = Corpus(VectorSource(words))
    corpus = tm_map(corpus, PlainTextDocument)
#     corpus = tm_map(corpus, stemDocument)
    frequencies = DocumentTermMatrix(corpus)
    df = as.data.frame(as.matrix(frequencies))
    
    tokenizer_2 <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    tokenizer_3 <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    a = Corpus(VectorSource(chunk))
    gram_2 = DocumentTermMatrix(a, control=list(tokenize=tokenizer_2))
    df_2 = as.data.frame(as.matrix(gram_2))
    gram_3 = DocumentTermMatrix(a, control=list(tokenize=tokenizer_3))
    df_3 = as.data.frame(as.matrix(gram_3))


    chunk = gsub("[a-z]", " ", chunk) 
    num_chunk = gsub("[[:punct:]]", " ", chunk)
    corpus = Corpus(VectorSource(num_chunk))
    cp_number = tm_map(corpus, PlainTextDocument)
#     cp_number = tm_map(cp_number, stemDocument)
    freq = DocumentTermMatrix(cp_number)
    df_number = as.data.frame(as.matrix(freq))
    
    chunk = gsub("[0-9]", " ", chunk)
    corpus = Corpus(VectorSource(chunk))
    cp_punc = tm_map(corpus, PlainTextDocument)
#     cp_punc = tm_map(cp_punc, stemDocument)
    freqp = DocumentTermMatrix(cp_punc)
    df_punc = as.data.frame(as.matrix(freqp))
    
    freq <- function(df){
        r <- cbind(df)
        r[1,] <- as.character(r[1,])
        r[1,] <- as.numeric(r[1,])
        sort_r <- r[,order(as.numeric(r), decreasing=TRUE)]
        value <- as.numeric(sort_r)
        names <- names(sort_r)
        df <- as.data.frame(rbind(value))
        names(df) <- names
        df
    }
    df <- freq(df)
    nd <- freq(nd)
    df_number <- freq(df_number)
    df_punc <- freq(df_punc)
    df_2 <- freq(df_2)
    df_3 <- freq(df_3)
    df_2 <- df_2[,df_2>=3]
    df_3 <- df_3[,df_3>=3]

    list('proper_n'=nd, 'number'=df_number, 'punc'=df_punc, 'word'=df,
         'gram_2'=df_2, 'gram_3'=df_3)
}


# 2. Profanity filtering: removing profanity and other words you do not want 
# to predict.