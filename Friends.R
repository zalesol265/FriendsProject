library(rvest)
library(xml2)
library(tidyverse)
library(stringi)
library(rstudioapi)
library(lubridate)


separate_script_and_scenes <- function(scripts) {
  
  lines_to_split <- filter(scripts, grepl('.*:.*[[]', script))
  lines_to_split <- arrange(lines_to_split, desc(row_id))
  if(nrow(lines_to_split)!=0){
    for(i in 1:nrow(lines_to_split)){
      split_row <- str_split(lines_to_split$script[i], "(?<=.)(?=\\[)")
      cur_row = lines_to_split$row_id[i]
      scripts$script[cur_row] <- split_row[[1]][1]
      scripts <- scripts %>% add_row(script = split_row[[1]][2], .before = cur_row+1)
    }
  }
  return(scripts)
}


extract_direction_from_dialogue <- function(tib_script, i){
  
  dialogue = unlist(tib_script$dialogue[i])
  if (!is.na(gregexpr('\\(', dialogue))[1] & 
      unlist(gregexpr('\\(', dialogue))[1] != -1) {
    startParen = unlist(gregexpr('\\(', dialogue)[1])
    endParen = unlist(gregexpr('\\)', dialogue)[1])
    quotesParen = ''
    # for(j in length(startParen):1) {
    #   quoteReplace = substr(dialogue, startParen[j], endParen[j])
    #   quotesParen = paste(quotesParen,
    #                       quoteReplace)
    #   dialogue = gsub('\\(\\)', '', gsub(quoteReplace, '', dialogue))
    # }
    tib_script$direction[i] = stri_trim_both(quotesParen)
    tib_script$dialogue[i] = stri_trim_both(dialogue)
  }
  return(tib_script)
}


create_season_and_episode_columns <- function(df_script, file_name){
  file_name_excl_html = substr(file_name, 1, nchar(file_name) - 5)
  file_name_length = nchar(file_name_excl_html)
  episode_number = ''
  episode_number_2 = ''
  episode_special = ''
  episode_season = substr(file_name, 1, 2)
  
 
  suppressWarnings({
    if (file_name_length == 4 & !is.na(as.numeric(file_name_excl_html))) {  # generates harmless warning for file names not in 'ssee.html' format, so suppressed
      episode_number = substr(file_name, 3, 4)
    } else if (file_name_length == 9 & grepl("-", file_name)) { # for double episodes exception
      episode_number = substr(file_name, 3, 4)
      episode_number_2 = substr(file_name, 8, 9)
    } else {
      episode_special = substr(file_name_excl_html, 3, file_name_length) # for outtakes exception
    }
  })
  
  df_script <- df_script %>% mutate(
    season = episode_season,
    episode = episode_number,
    special = episode_special,
    episode_2 = episode_number_2
  )
  return(df_script)
}

scrape_files <- function(){
  
  setwd(paste0(dirname(getActiveDocumentContext()$path), '\\ALL_Seasons')) 
  myfiles <- list.files(path = getwd(), pattern = "\\.html",  full.names = FALSE)
  
  master_script <- tibble()
  
  # Code for progress bar when running code
  n_iter <- length(myfiles)
  pb <- txtProgressBar(min = 0,
                       max = n_iter,
                       style = 3,
                       width = n_iter, # Needed to avoid multiple printings
                       char = "=") 
  cur_file <- 0
  
  for(file_name in myfiles){
  
    if (file_name == '0911.html') { # replace XXXX with the file name if you want to test a single file
      next
    }
  
    # Update progress bar
    setTxtProgressBar(pb, cur_file)
    cat(paste(" | Files processed:", cur_file,
              "| Current file:", file_name)," ")
    
    # Use rvest to read in the script
    episode_html <- rvest::read_html(x = file_name, encoding = "UTF-8")
    episode_title <- iconv(html_elements(x = episode_html, css ="head > title"), "UTF-8", "ASCII", sub="") 
    script_paragraphs <- html_elements(x = episode_html, css ="p") %>% html_text()
    script_paragraphs = iconv(script_paragraphs, "UTF-8", "ASCII", sub="")
    
    # Create a tibble from the paragraph text & drop nulls
    tib_script <- tibble(script = script_paragraphs )
    
    # Clean
    tib_script <- tib_script %>% drop_na()
    
    # Get writers
    episode_writers <- html_elements(x = episode_html, css ="body > font > p")[1]%>% html_text2()
    episode_writers <- stri_trim_both(gsub('Written by:', '', episode_writers))
    
    # find transcribers
    
    # FIX: rewrite using data from all_lines
    # full_text <- html_elements(x = friends_html, css ="body ") %>% html_text2()
    # 
    # transcribe_marks = str_locate_all(pattern ='ranscrib', full_text)
    # transcribe_marks[[2]] = list(NA)
    # for (i in 1:nrow(transcribe_marks[[1]])) {
    #   start = transcribe_marks[[1]][i, "start"] - 1 
    #   rest_of_text = substring(full_text, start, nchar(full_text))
    #   byLoc = unlist(gregexpr('by', rest_of_text))[1]
    #   end = unlist(gregexpr('\n', rest_of_text))[1]
    #   transcribe_marks[[2]][i] = stri_trim_both(gsub(':', '', substring(rest_of_text, byLoc + 2, end - 1)))
    # }
    # episode_transciber = unlist(transcribe_marks[[2]][1])
    # episode_transciber_addl = unlist(transcribe_marks[[2]][2])
    
    # Add the row number
    tib_script <- tib_script %>% mutate(row_id = row_number()) 
    tib_script <- separate_script_and_scenes(tib_script)
    tib_script <- tib_script %>% mutate(row_id = row_number()) 
    
    tib_script <- create_season_and_episode_columns(tib_script, file_name)
    
    tib_script['scene'] <- NA
    tib_script['transcriber_note'] <- NA
    tib_script['scene_number'] <- NA
    tib_script['character'] <- NA
    tib_script['dialogue'] <- NA
    tib_script['direction'] <- NA
    tib_script['prevUnseen'] <- NA
    
    scene_num = 0
    
    
    # Loop through each line in script and sort into the appropriate column 
    for (i in 1:nrow(tib_script)) {
      
      d = tib_script$script[i]
      d1 = substr(d, 1, 1)
      d6 = substr(d, 1, 6)
      
      if(!is.na(d)){
        if (str_detect(d, "\\[Scene:")) {
          tib_script[i, "scene"] = d
          scene_num = scene_num + 1
        } else if ((d == 'End' | d1 == '(' | d1 == '[' | d6 == 'Commer' | d6 == 'Closin' | d6 == 'Openin')) {
          tib_script[i, "direction"] = d
        } else if (str_detect(d, "\\{Transcrib")){
          tib_script[i, "transcriber_note"] = d  
        } else if (str_detect(d, ":")) {
          tib_script[i, "dialogue"] = d
        } else {
          tib_script[i, "prevUnseen"] = d
        } 
        tib_script <- extract_direction_from_dialogue(tib_script, i)
        tib_script[i, "scene_number"] = scene_num
      }
    }
    
    # Create the character column by splitting the dialogue column in two
    tib_script[c('character', 'dialogue')] <- str_split_fixed(tib_script$dialogue, ':', 2)
    
    # Clean up unwanted characters
    tib_script$scene <- str_replace_all(tib_script$scene, c('\\[Scene:'='', '\\]'=''))
    tib_script$direction <- str_replace_all(tib_script$direction, c('\\('='', '\\)'='', '\\['='', '\\]'=''))
    tib_script$prevUnseen <- str_replace_all(tib_script$prevUnseen, c('\\['='', '\\]'=''))
    
    master_script = rbind(master_script, tib_script)
    
    cur_file <- cur_file + 1
  
  }
  
  close(pb)
  
  return(master_script)
}


