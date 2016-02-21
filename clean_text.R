## Inspired by Matthew James Denny - http://www.mjdenny.com/Text_Processing_In_R.html

library(stringr)

clean_text <- function(text){
  # Remove special characters
  text <- str_replace_all(text,"[[:punct:]]", " ")
  # Remove all non-alphanumeric characters - just as backup to the special chars
  text <- str_replace_all(text,"[[:punct:]]", " ")
  # Remove camelCase
  text <- gsub("([a-z])([A-Z])", "\\1 \\L\\2", text, perl = TRUE) 
  # Lowercase
  text <- tolower(text)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  text <- str_replace_all(text,"[^a-zA-Z0-9\\s]", " ")
  # Shrink down to just one white space
  text <- str_replace_all(text,"[\\s]+", " ")
  # Split it
  #text <- str_split(text, " ")[[1]]
  # Stemmer
  text <- wordStem(text, language = 'english')
  # Get rid of trailing "" if necessary
  indexes <- which(text == "")
  if(length(indexes) > 0){
    text <- text[-indexes]
  } 
  return(text)
}
