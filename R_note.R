
if (!require("pacman")) install.packages("pacman")
pacman::p_load(R6, cli, RISmed, crayon)

# library(R6)
# library(cli)
# library(RISmed)
# library(crayon)

# library(DBI)
# library(RSQLite)

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
                     },
                     
                     search = function(keyword = "keyword", field = NA) {
                       
                       is_match <- FALSE
                       
                       if (!is.na(field) & all(field %in% names(self))) {
                         is_match <- is_match | any(grepl(keyword, self[[field]]))
                         self[[field]] <- gsub(keyword, crayon::bgRed(keyword), 
                                               self[[field]], ignore.case = TRUE)
                       }
                       
                       for (field in c("title", "group", "tag", "summary", "quote", "question")) {
                         is_match <- is_match | any(grepl(keyword, self[[field]]))
                         self[[field]] <- gsub(keyword, crayon::bgYellow(keyword), 
                                               self[[field]], ignore.case = TRUE)
                       }
                       
                       if (is_match) {
                         invisible(self)
                       } else {
                         invisible(NULL)
                       }
                     }
                   )
                   )

# functions -----------------------------------------------------------------------
insert_line <- function(file, at_row, new_line, is.overwrite = TRUE) {
  
  file_name <- gsub(".*\\/(.*)\\..*", "\\1", file)
  file_path <- gsub(paste0(file_name, "\\..*"), "", file)
  file_type <- gsub(paste0(".*", file_name, "\\.(.*)"), "\\1", file)
  
  if (is.overwrite) {
    new_file_name <- paste0(file_path, file_name, "_new.", file_type)
  } else {
    new_file_name <- file
  }
  
  file_lines <- readLines(file)
  write(x = file_lines[seq(1, at_row)], 
        file = new_file_name, append = FALSE)
  write(x = new_line, 
        file = new_file_name, append = TRUE)
  write(x = file_lines[seq(at_row + 1, length(file_lines))], 
        file = new_file_name, append = TRUE)
}


translate_rmd_note <- function(file, is.overwrite = FALSE) {
  
  # read by lines
  rmd_lines <- readLines(file)
  
  # check title lines
  idx <- grep("^\\[title", rmd_lines)
  idx_next <- c(idx[-1] - 1, length(rmd_lines))
  
  # assign values
  note_list <- list()
  idx_used <- NULL
  
  for (i in seq_along(idx)) {
    .title <- rmd_lines[idx[i] + 1]
    if (is.na(.title) || nchar(.title) == 0) next
    idx_used <- c(idx_used, idx[i])
    
    # create R6 object
    tmp <- note_R6$new(.title)
    i_rmd_lines <- rmd_lines[seq(idx[i] + 1, idx_next[i])]
    
    # fetch attributes
    for (x in c("tag", "group", "summary", "quote", "date")) {
      
      i_x <- grep(paste0("^\\[", x), i_rmd_lines)
      
      if (length(i_x) > 0) {
        if (x %in% c("tag", "group")) {
          
          tmp[[x]] <- unlist(strsplit(i_rmd_lines[i_x + 1], "; |;"))
          
        } else if (x == "summary") {
          
          tmp[[x]] <- i_rmd_lines[i_x + 1]
          
        } else if (x == "quote") {
          
          i_x_q <- grep("^\\[\\?", i_rmd_lines)
          
          if (length(i_x_q) > 0) {
            
            q_2_q <- rowSums(matrix(sapply(i_x, function(n) i_x_q > n), nrow = length(i_x_q)))
            q_2_q <- q_2_q[q_2_q > 0]
            
          } else {
            
            q_2_q <- NA
            
          }
          
          for (n in seq_along(i_x)) {
            tmp[["add_quote"]](quote = i_rmd_lines[i_x[n] + 1], 
                               question = ifelse(!n %in% q_2_q, 
                                                 NA, 
                                                 gsub("^\\[\\?\\]\\s*", "", i_rmd_lines[i_x_q[q_2_q == n]])))
          }
          
        } else if (x == "date") {
          
          tmp[["date"]] <- i_rmd_lines[i_x + 1]
          
        } 
        
      }
    } # end of fetch
    
    # add lines for missing info, e.g. date
    if (!any(grepl("^\\[date", i_rmd_lines))) {
      rmd_lines <- c(rmd_lines[seq(1, idx_next[i] - 1)],
                     "[date]", as.character(tmp[["date"]]), "",
                     rmd_lines[seq(idx_next[i], length(rmd_lines))])
      idx <- idx + 3
      idx_next <- idx_next + 3
    }
    
    note_list <- c(note_list, tmp)
  } # end of looping lines
  
  # write processed file
  file_name <- gsub(".*\\/(.*)\\..*", "\\1", file)
  file_path <- gsub(paste0(file_name, "\\..*"), "", file)
  file_type <- gsub(paste0(".*", file_name, "\\.(.*)"), "\\1", file)
  
  if (is.overwrite) {
    new_file_name <- paste0(file_path, file_name, "_new.", file_type)
  } else {
    new_file_name <- file
  }
  
  write(x = rmd_lines, file = new_file_name)
  
  names(note_list) <- rmd_lines[idx_used + 1]
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


search <- function(note_list, keyword, field = NA) {
  # hits with highlight will be returned in a list object
  out_list <- list()
  for (note_i in note_list) {
    clone_i <- note_i$clone()
    out_list <- c(out_list, clone_i$search(keyword = keyword, field = field))
  }
  out_list
}

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



