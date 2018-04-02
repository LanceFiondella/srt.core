language <<- "jp"
dictionary <<- read.csv("dictionary.csv", encoding = 'UTF-8')
    rownames(dictionary) <- dictionary$variable
translate <- function(text){
    toString(dictionary[text, language])
}

