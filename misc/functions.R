# some custom colours
jbg_pal <- function(n, sel) {
  cols <- c("lightred" = "#FF2600",
            "darkred" = "#B41821",
            "lightblue" = "#85D4E3",
            "darkblue" = "#046C9A",
            "brown" = "#79402F",
            "gold" = "#E1AF00",
            "grey" = "#899DA4",
            "black" = "#252525")
  if (!missing(n)) {
    unname(cols[seq_len(n)])
  } else if (!missing(sel)) {
    unname(cols[sel])
  }
}

scale_fill_jbg <- function(...) {
  discrete_scale(aesthetics = "fill", scale_name = "jbg", palette = jbg_pal, ...)
}
scale_colour_jbg <- function(...) {
  discrete_scale(aesthetics = "colour", scale_name = "jbg", palette = jbg_pal, ...)
}

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
    "littler",
    "lme4"
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

clear_var_df <- data.frame(pattern = c("protesterdemand2anti-war", "protesterdemand2labour protests", 
                                       "protesterdemand2police", "protesterdemand2social-issue protests", 
                                       "protesterviolence", "staterepression_peaceful", "np_ideologyright", 
                                       "np_typetabloid", "ideo_gulfconflict", "ideo_gulftossup", "days_since_start", 
                                       "year"), 
                           replacement = c("goal: anti-war", "goal: labour protests", 
                                           "goal: police", "goal: social-issue", "violent protest", "repression of peaceful p.", 
                                           "right-wing", "tabloid newspaper", "ideological divide: conflict", 
                                           "ideological divide: ambiguous", "days from start", "Year of protest (since 1992)"
                           ))

# rename variables (or anything else) for plots and tables
clear_var_names <- function(x, pattern_df = clear_var_df, factor = TRUE) {
  out <- stringi::stri_replace_all_fixed(
    x,
    pattern = pattern_df$pattern,
    replacement = pattern_df$replacement,
    vectorise_all = FALSE
  )
  if (factor) {
    out <- fct_relevel(
      out, 
      rev(pattern_df$replacement[pattern_df$replacement %in% out])
    )
  }
  out
}

# test and update bib file
update_bib <- function(qmds = c("../paper/article.qmd"),
                       master_bib = "../paper/references_main.bib",
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
      setdiff(c("vu", "fig", "sec", "tbl"))
    
    message(length(entries), " references found")
    
    if (file.exists("../data/pp_studies.csv")) {
      pp_studies <- rio::import("../data/pp_studies.csv")$`Bib-Key`
      entries <- c(entries, pp_studies) %>% 
        sort() %>% 
        unique()
    }
    
    # remove to save words
    l <- readLines(master_bib)
    tmp <- tempfile()
    # l <- l[!grepl("abstract\\s*=|address\\s*=|edition\\s*=",
    #               l, ignore.case = TRUE)]
    writeLines(l, tmp)
    
    bib_master <- suppressMessages(bibtex::read.bib(tmp))
    unlink(tmp)
    
    if (!all(entries %in% names(bib_master))) {
      stop("Unknown entry: ", entries[!entries %in% names(bib_master)])
    }
    
    bib <- bib_master[entries]
    suppressMessages(bibtex::write.bib(bib, clean_bib))
  } else {
    warning("master_bib not found")
  }
  
  return(TRUE)
}


is_latex <- function() knitr::pandoc_to("latex")

is_word <- function() knitr::pandoc_to("docx")


# printing tables for different formats
custom_df_print <- function(tbl, 
                            caption = NULL, 
                            footnote = NULL, 
                            notation = "symbol",
                            font_size = 10, 
                            escape = FALSE,
                            latex_options = "basic",
                            ...) {
  
  if (is_latex()) {
    tbl %>% 
      knitr::kable(format = "latex", 
                   booktabs = TRUE,
                   linesep = "",
                   #caption = caption,
                   escape = escape,
                   ...) %>%
      kableExtra::kable_styling(
        latex_options = latex_options,
        font_size = font_size
      ) %>%
      kableExtra::add_footnote(footnote, notation = notation)
  } else if (is_word()) {
    tbl %>%
      flextable::flextable() %>%
      flextable::align(part = "all") %>% # left align
      flextable::set_caption(caption = caption,
                             autonum = autonum) %>%
      flextable::fontsize(size = font_size, part = "body") %>%
      {
        if (isTRUE(!is.null(footnote))) {
          if (length(footnote) > 1) {
            footnote <- paste0(footnote, collapse = "\n")
          }
          (.) %>% 
            flextable::add_footer_row(values = footnote,
                                      colwidths = ncol(tbl))
        } else {
          (.)
        }
      } %>% 
      flextable::theme_booktabs() %>%
      flextable::autofit()
  } else { # all other formats, e.g., HTML
    knitr::kable(tbl)
  }
}


# custom printing for AME
marg_default <- function(factor, 
                         model = "delegitimising", 
                         unit = c("none", "percent", "points", "double"), 
                         abs = TRUE,
                         df = NULL) {
  unit <- match.arg(unit)
  if (!factor %in% df$factor) {
    stop("'factor' not in selection. Choose one of ", 
         paste0("'", unique(df$factor), "'", collapse = ", "))
  }
  if (!model %in% df$model) {
    stop("'model' not in selection. Choose one of ", 
         paste0("'", unique(df$model), "'", collapse = ", "))
  }
  out <- df %>% 
    filter(factor == !!factor & model == !!model) %>% 
    pull(AME) %>% 
    unname()
  if (abs) {
    out <- abs(out)
  }
  switch (unit,
          none = scales::comma(out, accuracy = 0.01),
          percent = scales::percent(out),
          points = scales::comma(out * 100, accuracy = 1),
          double = out
  )
}

