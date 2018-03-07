language <<- "jp"
dictionary <<- read.csv("dictionary.csv")
    rownames(dictionary) <- dictionary$variable
translate <- function(text){
    toString(dictionary[text, language])
}

