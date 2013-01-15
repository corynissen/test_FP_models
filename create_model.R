
library(textcat)
library(stringr)
library(RTextTools)

replace.links <- function(text){
  # extract urls from string
  return(str_replace_all(text, ignore.case("http://[a-z0-9].[a-z]{2,3}/[a-z0-9]+"), "urlextracted"))
}

remove.word <- function(string, starts.with.char){
  # INPUT:  string is a string to be edited,
  #         starts.with.char is a string or partial string to search and remove
  # OUTPUT: string with words removed
  # USAGE:  remove.word(string, "@") removes words starting with "@"
  #         remove.word(string, "RT") removes RT from string
  word.len <- nchar(starts.with.char)
  list.of.words <- strsplit(string, " ")[[1]]
  # remove ones that start with "starts.with.char"
  list.of.words <- list.of.words[!substring(list.of.words,1,word.len)==starts.with.char]
  ret.string <- paste(list.of.words, collapse=" ")
  return(ret.string)
}

df <- read.csv("/media/windrive/personal/111512_fp/fp_tweets.csv", stringsAsFactors=F)

df$text.cleansed <- tolower(df$text)
#df$text.cleansed <- iconv(df$text.cleansed, "latin1", "ASCII", sub="")
# remove the string "food poisoning" because every tweet has this in it...
df$text.cleansed <- sapply(df$text.cleansed, function(x)gsub("food poisoning", "", x))
df$text.cleansed <- as.character(sapply( df$text.cleansed, replace.links))
df$text.cleansed <- as.character(sapply( df$text.cleansed, function(x)remove.word(x, "@")))
df$text.cleansed <- as.character(sapply( df$text.cleansed, function(x)remove.word(x, "rt")))
# replace non-letters with spaces
df$text.cleansed <- as.character(sapply(df$text.cleansed, function(x)gsub("[^[:alnum:]]", " ", x)))
# remove leading and trailing spaces
df$text.cleansed <- as.character(sapply(df$text.cleansed, function(x)gsub("^\\s+|\\s+$", "", x)))
# replace multiple spaces next to each other with single space
df$text.cleansed <- as.character(sapply(df$text.cleansed, function(x)gsub("\\s{2,}", " ", x)))


df <- subset(df, text.cleansed!="")

fp.model <- textcat_profile_db(df$text.cleansed, df$manual_class)
save(list=c("fp.model", "replace.links", "remove.word"), file="/media/windrive/personal/111512_fp/fp_classifier/fp_model.Rdata")
textcat("i have the food poisoning", fp.model)

# try it out...
test <- subset(df, is.na(manual_class))
samp <- sample(1:nrow(test), 30)
test2 <- test[samp,]
paste(sapply(test2$text.cleansed, function(x)textcat(x, fp.model)), test2$text, sep="__")

# create csv file with predicted categories...
samp <- sample(1:nrow(df), 500)
df2 <- df[samp,]
df2$predicted_class <- sapply(df2$text.cleansed, function(x)textcat(x, model))
df2 <- df2[,c("predicted_class", "text")]
write.csv(df2, "/media/windrive/personal/111512_fp/some_fp_predicted_classifications.csv", row.names=F)

# test
df2 <- subset(df, !is.na(manual_class))
train.samp <- sample(1:nrow(df2), .70*nrow(df2))
df3 <- df2[train.samp,]
df4 <- df2[-train.samp,]
model.subset <- textcat_profile_db(df3$text.cleansed, df3$manual_class)
df4$predicted_class <- sapply(df4$text.cleansed, function(x)textcat(x, model.subset))
paste(df4$predicted_class, df4$text, sep="__")
# % correct overall
sum(df4$predicted_class==df4$manual_class, na.rm=T) / sum(!is.na(df4$predicted_class))
# % correct 1's
sum(df4$predicted_class[df4$manual_class==1]==df4$manual_class[df4$manual_class==1], na.rm=T) / sum(!is.na(df4$predicted_class[df4$manual_class==1]))
# % correct 0's
sum(df4$predicted_class[df4$manual_class==0]==df4$manual_class[df4$manual_class==0], na.rm=T) / sum(!is.na(df4$predicted_class[df4$manual_class==0]))
# look at the wrong ones
paste("pred:  ", df4$predicted_class[df4$predicted_class!=df4$manual_class], "  manual:  ", df4$manual_class[df4$predicted_class!=df4$manual_class], " | ", df4$text[df4$predicted_class!=df4$manual_class], sep="")

# repeat test n times
get.percent.correct <- function(){
  df2 <- subset(df, !is.na(manual_class))
  train.samp <- sample(1:nrow(df2), .70*nrow(df2))
  df3 <- df2[train.samp,]
  df4 <- df2[-train.samp,]
  model.subset <- textcat_profile_db(df3$text.cleansed, df3$manual_class)
  df4$predicted_class <- sapply(df4$text.cleansed, function(x)textcat(x, model.subset))
  paste(df4$predicted_class, df4$text, sep="__")
  # % correct overall
  ov.perc <- sum(df4$predicted_class==df4$manual_class, na.rm=T) / sum(!is.na(df4$predicted_class))
  # % correct 1's
  one.perc <- sum(df4$predicted_class[df4$manual_class==1]==df4$manual_class[df4$manual_class==1], na.rm=T) / sum(!is.na(df4$predicted_class[df4$manual_class==1]))
  # % correct 0's
  zero.perc <- sum(df4$predicted_class[df4$manual_class==0]==df4$manual_class[df4$manual_class==0], na.rm=T) / sum(!is.na(df4$predicted_class[df4$manual_class==0]))
  return(list(ov.perc=ov.perc, one.perc=one.perc, zero.perc=zero.perc))
}
n <- 100
ov.perc <- one.perc <- zero.perc <- NULL
for(i in 1:n){
  result.list <- get.percent.correct()
  ov.perc <- c(ov.perc, result.list$ov.perc)
  one.perc <- c(one.perc, result.list$one.perc)
  zero.perc <- c(zero.perc, result.list$zero.perc)
}
mean(ov.perc)
mean(one.perc)
mean(zero.perc)

# try RTextTool
df2 <- subset(df, !is.na(manual_class))

matrix <- create_matrix(df2$text.cleansed, language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)
container <- create_container(matrix,as.numeric(df2$manual_class),trainSize=1:300, testSize=301:334,virgin=FALSE)
# models...  c("SVM","GLMNET","MAXENT","SLDA","BOOSTING","BAGGING","RF","NNET","TREE")
#models <- train_models(container, algorithms=c("GLMNET","MAXENT","SVM"))
models <- train_models(container, algorithms=c("BAGGING","BOOSTING","GLMNET","MAXENT","TREE", "SLDA", "RF", "SVM"))
#models <- train_models(container, algorithms=c("GLMNET", "SVM"))  #works:  MAXENT
results <- classify_models(container, models)
create_analytics(container, results)

df3 <- subset(df, is.na(manual_class))[1:50,]
new.data <- create_matrix(df3$text.cleansed, language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf, originalMatrix=matrix)
new.container <- create_container(new.data,as.numeric(df3$manual_class),trainSize=1:50, testSize=1:50, virgin=TRUE)
results <- classify_models(new.container, models)
paste(results$SLDA_LABEL, "__", df3$text, sep="")
paste(results$SLDA_LABEL, "__", df3$text.cleansed, sep="")
