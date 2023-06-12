custom_spell_check <- function(files, dict = "dictionary.dic") {
  
  misspellings <- spelling::spell_check_files(
    files, 
    ignore = readLines(dict),
    lang = "en-GB"
  )
  
  if (nrow(misspellings) > 0) {
    if (interactive()) {
      message("Some words not found in the dictionary. Should they be added?")
      print(misspellings)
      choice <- menu(
        c("yes", "no", "repeat check")
      )
      if (choice == 1L) {
        old <- readLines(dict)
        new <- sort(c(old, misspellings$word))
        writeLines(new, dict)
      } else if (choice == 3L) {
        custom_spell_check(files, dict)
      } else {
        stop("Correct misspellings.")
      }
    } else {
      stop("Correct misspellings: ", paste0(misspellings$word, ": ", misspellings$found))
    }
  }
  return(TRUE)
}

## install missing R packages
install_missing <- function(path = "..", 
                            return_needed = FALSE,
                            install_dependencies = FALSE) {
  
  files = list.files(
    path = path,
    pattern = ".R$|.Qmd$", 
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = TRUE
  )
  
  if (!requireNamespace("attachment", quietly = TRUE)) {
    if (!identical(Sys.getenv("AUTOINSTALL"), "true")) {
      choice <- menu(
        c("yes", "no"), 
        title = "Package attachment is needed to perform check. Should it be installed?"
      )
    } else {
      choice <- 1L
    }
    
    if (choice == 1L) {
      install.packages("attachment", dependencies = TRUE)
    }
  }
  
  files <- split(files, tools::file_ext(files))
  
  needed_packages <- c(
    attachment::att_from_qmds(files$qmd), 
    unlist(lapply(files$R, attachment::att_from_rscript)),
    "spelling",
    "knitr",
    "tinytex",
    "rticles",
    "rmarkdown",
    "littler"
  )
  
  if (return_needed) {
    return(needed_packages)
  } else {
    missing_packages <- needed_packages[!needed_packages %in% installed.packages()[, 1]]
    if (identical(Sys.getenv("AUTOINSTALL"), "true")) {
      install.packages(missing_packages)
    } else {
      if (!requireNamespace("rlang", quietly = TRUE)) {
        if (!identical(Sys.getenv("AUTOINSTALL"), "true")) {
          choice <- menu(
            c("yes", "no"), 
            title = "Package rlang is needed to perform check. Should it be installed?"
          )
        } else {
          choice <- 1L
        }
        
        if (choice == 1L) {
          install.packages("rlang", dependencies = TRUE)
        }
      }
      rlang::check_installed(needed_packages)
    }
    return(TRUE)
  }
}

## print hypotheses as bullet
hyp <- function(num) {
  if (file.exists("./misc/hypotheses.csv")) {
    hypotheses <- rio::import("./misc/hypotheses.csv")
  } else  if (file.exists("../misc/hypotheses.csv")) {
    hypotheses <- rio::import("../misc/hypotheses.csv")
  }
  hs <- hypotheses[grepl(num, hypotheses$Nr), 1:2]
  hs$Nr <- paste0("**", hs$Nr, "**")
  paste(unlist(hs), collapse = ": ")
}

# test and update bib file
update_bib <- function(qmds = c("../paper/article.qmd"),
                       master_bib = "../paper/references_old.bib",
                       clean_bib = "../paper/references.bib") {
  
  if (file.exists(master_bib)) {
    `%>%` <- magrittr::`%>%`
    lines <- unlist(lapply(qmds, stringi::stri_read_lines))
    entries <- stringi::stri_extract_all_regex(lines, "@[[:alnum:]]+")  %>%  
      unlist() %>% 
      na.omit() %>% 
      unique() %>% 
      gsub("@", "", ., fixed = TRUE) %>% 
      sort() %>% 
      setdiff("vu")
    
    if (file.exists("../data/pp_studies.csv")) {
      pp_studies <- rio::import("../data/pp_studies.csv")$`Bib-Key`
      entries <- c(entries, pp_studies) %>% 
        sort() %>% 
        unique()
    }
    
    # remove to save words
    l <- readLines(master_bib)
    tmp <- tempfile()
    l <- l[!grepl("abstract\\s*=|address\\s*=|edition\\s*=",
                  l, ignore.case = TRUE)]
    writeLines(l, tmp)
    
    bib_master <- suppressMessages(bibtex::read.bib(tmp))
    unlink(tmp)
    
    
    if (!all(entries %in% names(bib_master))) {
      stop("Unknown entry: ", entries[!entries %in% names(bib_master)])
    }
    
    bib <- bib_master[entries]
    suppressMessages(bibtex::write.bib(bib, clean_bib))
  }
  
  return(TRUE)
}
