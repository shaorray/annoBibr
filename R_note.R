

library(R6)
library(cli)
library(RISmed)
library(crayon)

library(DBI)
library(RSQLite)

# DBI::dbConnect(RSQLite::SQLite(), path = file)

# allow: 
# 1. add new quotation with the associated questions
# 2. search in the quotations and questions
# 3. hint for the question while typing
# 

# Interface style
#
# 1. One paper in console:
# note(
#   title = ,
#   group = ,
#   tag = ,
#   quote = ,
#   question = ,
#   question = , (a quota can be followed by many consecutive questions)
#
#   quote = ,
#   question = ,
# )
#
# The collection
# note_album <- c(note1, note2, ...)
#
#
# 2. Multiple paper notes from Rmarkdown
# 
# 
# 
# 
#
#
#
# Functionalities
# sort(note_album) (remove redundant notes)
# delete()
# search(note_album, key_words)
# print(note_album)
# search_reference(query_paper, subject_paper): to check if a paper has been cited in another paper


# User interface can be:
# 1. console
# 2. Rmarkdown


# class ---------------------------------------------------------------------------
note_R6 <- R6Class("Note", 
                   
                   public = list(
                     
                     title = NULL,
                     group = NULL,
                     tag = NULL,
                     summary = NULL,
                     quote = NULL,
                     question = NULL,
                     date = NULL,
                     
                     initialize = function(title = NULL) {
                       if (is.character(title) & length(title) == 1) {
                         self$title = title
                         invisible(self)
                       } else {
                         message("Please add a title.")
                       }
                       self$date = Sys.time()
                     },
                     
                     add_summary = function(text = NULL) {
                       if (is.character(text) & length(text) == 1) {
                         self$summary = text
                         invisible(self)
                       } else {
                         message("Please append a brief summary.")
                       }
                     },
                     
                     add_quote = function(quote, question = "NA") { # many questions?
                       if (length(self$quote) == 0) {
                         self$quote = quote
                         self$question = question
                       } else {
                         self$quote = c(self$quote, quote)
                         self$question = c(self$question, question)
                       }
                       invisible(self)
                     },
                     
                     print = function(...) {
                       cat(rep("-", cli::console_width()), "\n", sep = "")
                       cat("Group: ", sapply(self$group, function(x) paste0(crayon::bgWhite(crayon::black(x)), " ")), "\n", sep = "")
                       cat("Tag: ", sapply(self$tag, function(x) paste0(crayon::bgWhite(crayon::black(x)), " ")), "\n\n", sep = "")
                       cat(crayon::bgGreen("[Title]"), " ", crayon::bold(self$title), "\n\n", sep = "")
                       for (i in seq_along(self$quote)) {
                         cat(crayon::bgCyan('[Quote]'), ' "', self$quote[i], '"', "\n", sep = "")
                         cat(crayon::bgBlue("[?]"), " ", self$question[i], "\n\n", sep = "")
                       }
                       cat(as.character(self$date), "\n", sep = "")
                       cat(rep("-", cli::console_width()), "\n\n", sep = "")
                     }
                   )
                   )

# functions -----------------------------------------------------------------------

translate_rmd_note <- function(file) {
  
  # read by lines
  rmd <- readLines(file)
  
  # check title lines
  idx <- grep("^\\[title", rmd)
  idx_next <- c(idx[-1] - 1, length(rmd))
  
  # assign values
  note_list <- list()
  idx_used <- NULL
  for (i in seq_along(idx)) {
    .title <- rmd[idx[i] + 1]
    if (is.na(.title) || nchar(.title) == 0) next
    idx_used <- c(idx_used, idx[i])
    
    # create R6 object
    tmp <- note_R6$new(.title)
    i_rmd <- rmd[seq(idx[i] + 1, idx_next[i])]
    
    # fetch attributes
    for (x in c("tag", "group", "summary", "quote")) {
      
      i_x <- grep(paste0("^\\[", x), i_rmd)
      
      if (length(i_x) > 0) {
        if (x %in% c("tag", "group")) 
          {
          tmp[[x]] <- unlist(strsplit(i_rmd[i_x + 1], "; |;"))
          
        } else if (x == "summary")
          {
          tmp[[x]] <- i_rmd[i_x + 1]
          
        } else if (x == "quote") 
          {
          i_x_q <- grep("^\\[\\?", i_rmd)
          
          if (length(i_x_q) > 0) {
            q_2_q <- rowSums(matrix(sapply(i_x, function(n) i_x_q > n), nrow = length(i_x_q)))
            q_2_q <- q_2_q[q_2_q > 0]
          } else {
            q_2_q <- NA
          }
          
          for (n in seq_along(i_x)) {
            tmp[["add_quote"]](quote = i_rmd[i_x[n] + 1], 
                               question = ifelse(!n %in% q_2_q, 
                                                 NA, i_rmd[i_x_q[q_2_q == n]]))
          }
        }
        
      }
    }
    note_list <- c(note_list, tmp)
  }
  names(note_list) <- rmd[idx_used + 1]
  note_list
}

translate_ris_note <- function(file) {
  
  # read by lines
  ris <- readLines(file)
  
  # check title lines
  idx <- grep("^T1", ris)
  idx_next <- c(idx[-1] - 1, length(ris))
  
  # assign values
  note_list <- list()
  idx_used <- NULL
  for (i in seq_along(idx)) {
    .title <- gsub("T1  - ", "", ris[idx[i]])
    if (is.na(.title) || nchar(.title) == 0) next
    idx_used <- c(idx_used, idx[i])
    
    # create R6 object
    tmp <- note_R6$new(.title)
    note_list <- c(note_list, tmp)
  }
  names(note_list) <- unlist(lapply(note_list, function(x) x$title), 
                             use.names = FALSE)
  note_list
}

# translate_bib_note <- function(file) {}


# search()
# delete()
# print()



pubmed <- function(.title) {
  .title <- "H2A.Z.1 Monoubiquitylation Antagonizes BRD2 to Maintain Poised Chromatin in ESCs"
  
  res <- EUtilsSummary(.title,
                       type = "esearch",
                       db = "pubmed", 
                       datetype = "pdat")
  
  authors <- RISmed::Author(RISmed::EUtilsGet(res))
  
  
  if (length(authors) == 1) {
    authors <- authors[[1]][, c("ForeName", "LastName")]
  } else {
    authors <- NULL
  }
  
}


# read line -----------------------------------------------------------------------

current_file <- rstudioapi::getSourceEditorContext()$path
note_list <- translate_rmd_note(current_file)


# objects -------------------------------------------------------------------------

# a list of question, to increase typing consistency
questions <- na.omit(unname(unlist(lapply(note_list,  function(x) x$question))))
questions <- questions[nchar(questions) > 5]
questions <- `names<-`(as.list(questions), questions)
questions <- unique(questions)



# b <- note_R6$new("Mitosis Gives a Brief Window of Opportunity for a Change in Gene Transcription")
# b$group <- c("A", "B")
# b$tag <- c("A", "B")
# b$add_quote("Nuclear transfer to amphibian oocytes provides a special opportunity to test transcriptional reprogramming without cell division.")
# b$add_quote("We find that, as cells traverse mitosis, their genes pass through a temporary phase of unusually high responsiveness to oocyte reprogramming factors (mitotic advantage)... Our results suggest that histone H2A deubiquitination may account, at least in part, for the acquisition of mitotic advantage.")
# 
# b$print()



