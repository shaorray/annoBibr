
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
                     authors = NULL,
                     year = NULL,
                     abstract = NULL,
                     
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
                     
                     print = function(is.all = FALSE) {
                       cat(rep("-", cli::console_width()), "\n", sep = "")
                       cat("Group: ", sapply(self$group, function(x) paste0(crayon::bgWhite(crayon::black(x)), " ")), "\n", sep = "")
                       cat("Tag: ", sapply(self$tag, function(x) paste0(crayon::bgWhite(crayon::black(x)), " ")), "\n\n", sep = "")
                       cat(crayon::bgGreen("[Title]"), " ", crayon::bold(self$title), "\n\n", sep = "")
                       for (i in seq_along(self$quote)) {
                         cat(crayon::bgCyan('[Quote]'), ' "', self$quote[i], '"', "\n", sep = "")
                         cat(crayon::bgBlue("[?]"), " ", self$question[i], "\n\n", sep = "")
                       }
                       if (is.all) {
                         cat(crayon::bgWhite(crayon::black("[year]")), " ", crayon::bold(self$year), "\n\n", sep = "")
                         cat(crayon::bgWhite(crayon::black("[authors]")), " ", crayon::bold(self$authors), "\n\n", sep = "")
                         cat(crayon::bgWhite(crayon::black("[abstract]")), " ", crayon::bold(self$abstract), "\n\n", sep = "")
                       }
                       cat(as.character(self$date), "\n", sep = "")
                       cat(rep("-", cli::console_width()), "\n\n", sep = "")
                     },
                     
                     # enable keyword match, otherwise returns NULL
                     search = function(keyword = "keyword", field = NA) {
                       
                       is_match = FALSE
                       
                       if (!is.na(field) & all(field %in% names(self))) {
                         is_match = is_match | any(grepl(keyword, self[[field]], ignore.case = TRUE))
                         self[[field]] = gsub(keyword, crayon::bgRed(keyword), 
                                              self[[field]], ignore.case = TRUE)
                       }
                       
                       for (field in c("title", "group", "tag", "summary", "quote", "question")) {
                         is_match = is_match | any(grepl(keyword, self[[field]], ignore.case = TRUE))
                         self[[field]] = gsub(keyword, crayon::bgYellow(keyword), 
                                              self[[field]], ignore.case = TRUE)
                       }
                       
                       if (is_match) {
                         invisible(self)
                       } else {
                         invisible(NULL)
                       }
                     },
                     
                     pubmed = function() {
                       # Gather complete information of a paper given its object
                       
                       if (!is.null(self[["title"]])) {
                         query = RISmed::EUtilsSummary(self[["title"]],
                                                       type = "esearch",
                                                       db = "pubmed", 
                                                       datetype = "pdat")
                         res = RISmed::EUtilsGet(query)
                         res_authors = RISmed::Author(res)
                         year = RISmed::YearAccepted(res)
                         abstract = RISmed::AbstractText(res)
                         
                         if (!is.na(names(res_authors[1])) ) {
                           res_authors = res_authors[1]
                           year = year[1]
                           abstract = abstract[1]
                           
                           res_authors = as.data.frame(res_authors)
                           res_authors = paste(res_authors[[3]], res_authors[[2]],
                                               sep = ", ", collapse = "\n")
                           
                           self$authors = res_authors
                           self$year = year
                           self$abstract = abstract
                           
                           invisible(self)
                         } 
                       }
                     }
                   )
                   )

# functions -----------------------------------------------------------------------

translate_rmd_note <- function(file, is.overwrite = FALSE, is.Xref = FALSE) {
  # Args:
  # is.overwrite: add date and cross-references in the original file, or a new file
  # is.Xref:      add pubmed cross-references of authors, years, and abstract
  
  if (is.Xref) 
    message("Fetching Pubmed cross-references...")
  
  # read by lines
  rmd_lines <- readLines(file)
  
  # check title lines
  idx <- grep("^\\[title", rmd_lines)
  idx_next <- c(idx[-1] - 1, length(rmd_lines))
  
  # output processed lines
  out_rmd_lines <- rmd_lines
  out_idx_next <- idx_next
  
  # assign values
  note_list <- list()
  idx_used <- NULL
  
  # set progress bar
  pb <- txtProgressBar(min = 1, max = length(idx), style = 3)
  
  for (i in seq_along(idx)) {
    setTxtProgressBar(pb, i)
    
    .title <- rmd_lines[idx[i] + 1]
    .title <- gsub("^\\s*(.*)\\s*$", "\\1", .title) # remove space
    # cat("\n", .title, "\n")
    
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
      out_rmd_lines <- c(out_rmd_lines[seq(1, out_idx_next[i] - 1)],
                     "[date]", as.character(tmp[["date"]]), "",
                     out_rmd_lines[seq(out_idx_next[i], length(out_rmd_lines))])
      out_idx_next <- out_idx_next + 3
    }
    
    if (is.Xref) {
      tmp$pubmed()
      # add year
      if (!any(grepl("^\\[year", i_rmd_lines))) {
        out_rmd_lines <- c(out_rmd_lines[seq(1, out_idx_next[i] - 1)],
                       "[year]", as.character(tmp[["year"]]), "",
                       out_rmd_lines[seq(out_idx_next[i], length(out_rmd_lines))])
        out_idx_next <- out_idx_next + 3
      }
      # add authors
      if (!any(grepl("^\\[authors", i_rmd_lines))) {
        out_rmd_lines <- c(out_rmd_lines[seq(1, out_idx_next[i] - 1)],
                       "[authors]", as.character(tmp[["authors"]]), "",
                       out_rmd_lines[seq(out_idx_next[i], length(out_rmd_lines))])
        out_idx_next <- out_idx_next + 3
      }
      # add abstract
      if (!any(grepl("^\\[abstract", i_rmd_lines))) {
        out_rmd_lines <- c(out_rmd_lines[seq(1, out_idx_next[i] - 1)],
                       "[abstract]", as.character(tmp[["abstract"]]), "",
                       out_rmd_lines[seq(out_idx_next[i], length(out_rmd_lines))])
        out_idx_next <- out_idx_next + 3
      }
    }
    
    
    note_list <- c(note_list, tmp)
  } # end of looping lines
  
  # write processed file
  file_name <- gsub(".*\\/(.*)\\..*", "\\1", file)
  file_path <- gsub(paste0(file_name, "\\..*"), "", file)
  file_type <- gsub(paste0(".*", file_name, "\\.(.*)"), "\\1", file)
  
  if (is.overwrite) {
    new_file_name <- file
  } else {
    new_file_name <- paste0(file_path, file_name, "_new.", file_type)
  }
  
  write(x = out_rmd_lines, file = new_file_name)
  
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



# read line -----------------------------------------------------------------------

current_file <- rstudioapi::getSourceEditorContext()$path

note_list <- translate_rmd_note(current_file, is.overwrite = FALSE, is.Xref = TRUE)


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



