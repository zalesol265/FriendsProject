library(rvest)
library(xml2)
library(tidyverse)
library(stringi)
library(rstudioapi)
library(lubridate)


separate_script_and_scenes <- function(scripts) {
  
  lines_to_split <- filter(scripts, grepl('.*:.*[[]Scene.*', script))
  lines_to_split <- arrange(lines_to_split, desc(row_id))
  if(nrow(lines_to_split)!=0){
    for(i in 1:nrow(lines_to_split)){
      split_row <- str_split(lines_to_split$script[i], "\\[")
      cur_row = lines_to_split$row_id[i]
      scripts$script[cur_row] <- split_row[[1]][1]
      scripts <- scripts %>% add_row(script = paste0("[", split_row[[1]][2]), .before = cur_row+1)
    }
  }
  lines_to_split <- filter(scripts, grepl('[[]Scene.*[]].*:.*', script))
  lines_to_split <- arrange(lines_to_split, desc(row_id))
  print(lines_to_split)
  if(nrow(lines_to_split)!=0){
    for(i in 1:nrow(lines_to_split)){
      split_row <- str_split(lines_to_split$script[i], "\\]")
      cur_row = lines_to_split$row_id[i]
      scripts$script[cur_row] <- paste0(split_row[[1]][1], "]")
      scripts <- scripts %>% add_row(script = split_row[[1]][2], .before = cur_row+1)
    }
  }
  
  return(scripts)
}


extract_direction_from_dialogue <- function(tib_script, i){
  

  dialogue = unlist(tib_script$dialogue[i])

  if (!is.na(gregexpr('\\(', dialogue))[1] & 
      unlist(gregexpr('\\(', dialogue))[1] != -1) {
    startParen = unlist(str_locate_all(pattern ='\\(', dialogue)[[1]][,1]) # unlist(gregexpr('\\(', dialogue)[1])
    endParen = unlist(str_locate_all(pattern ='\\)', dialogue)[[1]][,1]) # unlist(gregexpr('\\)', dialogue)[1])
    quotesParen = ''
    for(j in length(startParen):1) {
      quoteReplace = substr(dialogue, startParen[j], endParen[j])
      quotesParen = paste(quotesParen, quoteReplace)
      dialogue = paste0(substr(dialogue, 1, startParen[j] - 1), 
                        substr(dialogue, endParen[j] + 1, nchar(dialogue)))
    }
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



setwd(paste0(dirname(getActiveDocumentContext()$path), '\\ALL_Seasons')) 
myfiles <- list.files(path = getwd(), pattern = "\\.html",  full.names = FALSE)

master_script <- tibble()
master_episode_list <- tibble()

# Code for progress bar when running code
n_iter <- length(myfiles)
pb <- txtProgressBar(min = 0,
                     max = n_iter,
                     style = 3,
                     width = n_iter, # Needed to avoid multiple printings
                     char = "=") 
cur_file <- 0

for(file_name in myfiles){
  
#   if (file_name != '0209-test.html') { # replace XXXX with the file name if you want to test a single file
#     next
#   }

  # Update progress bar
  setTxtProgressBar(pb, cur_file)
  cat(paste(" | Files processed:", cur_file,
            "| Current file:", file_name)," ")
  
  # Use rvest to read in the script
  episode_html <- rvest::read_html(x = file_name, encoding = "UTF-8")
  episode_title <- iconv(html_elements(x = episode_html, css ="title"), "UTF-8", "ASCII", sub="") 
  episode_title <- str_replace_all(episode_title, c('<title>'='', '</title>'='', '\\\n'=''))
  episode_title = str_to_title(episode_title)
  theOneLoc = max(regexpr('The One', episode_title)[1],
                  regexpr('The Last One', episode_title)[1])

  script_paragraphs <- html_elements(x = episode_html, css ="p") %>% html_text()
  script_paragraphs = iconv(script_paragraphs, "UTF-8", "ASCII", sub="")
  
  if (is_empty(script_paragraphs) | length(script_paragraphs) < 20) { # patch for various episodes (e.g., 0204, 0911...)
    temp <- episode_html %>% html_text2()
    temp = paste0('<p>',str_replace_all(temp, '\\\n', '</p><p>'),'</p>')
    temp = str_replace_all(temp, 'Phoebe Thank you.', 'Phoebe: Thank you.')
    temp_html = read_html(temp)
    script_paragraphs <- html_elements(x = temp_html, css ="p") %>% html_text()
  }
  
  full_text = iconv(episode_html %>% html_text(), "UTF-8", "ASCII", sub="")
  
  if(theOneLoc != -1) {
    episode_title = substring(episode_title, theOneLoc, nchar(episode_title))
  } else {
    theOneLoc = max(regexpr('The One With', full_text)[1],
                    regexpr('TOW', full_text)[1],
                    regexpr('THE ONE WITH', full_text)[1])
    
    episode_title = str_to_title(substring(full_text, theOneLoc, theOneLoc + 50))
    episode_title = str_replace_all(episode_title, 'Tow', 'The One With')
    title_end = regexpr('\\\n', episode_title)[1]
    episode_title = substring(episode_title, 1, title_end - 1)
  }
  
  # Create a tibble from the paragraph text & drop nulls
  tib_script <- tibble(script = script_paragraphs )
  
  # Clean
  tib_script <- tib_script %>% drop_na()
  tib_script <- tib_script %>% filter(script != "")
  
  # find transcribers
  
  transcribe_marks = str_locate_all(pattern ='ranscrib', full_text)
  transcribe_marks[[2]] = list(NA)
  if(nrow(transcribe_marks[[1]]) > 0) {
    for (i in 1:nrow(transcribe_marks[[1]])) {
      transcribe_start = transcribe_marks[[1]][i, "start"] - 1
      rest_of_text = substring(full_text, transcribe_start, transcribe_start+200)
      byLoc = unlist(gregexpr('by', rest_of_text))[1]
      end = unlist(gregexpr('\n', rest_of_text))[1]
      transcribe_marks[[2]][i] = stri_trim_both(gsub(':', '', substring(rest_of_text, byLoc + 2, end - 1)))
    }
    episode_transciber = unlist(transcribe_marks[[2]][1])
    episode_transciber_addl = unlist(transcribe_marks[[2]][2])
  } else {
    episode_transciber = ''
    episode_transciber_addl = ''
  }

  # Get writers -- this still isn't perfect but maybe not that important
  writer_marks = str_locate_all(pattern = 'ritten by', full_text)
  if(nrow(writer_marks[[1]]) == 0) {
    writer_marks = str_locate_all(pattern = 'eleplay by', full_text)
  }
  
  writer_marks[[2]] = list(NA)
  if(nrow(writer_marks[[1]]) > 0) {
    for (i in 1:nrow(writer_marks[[1]])) {
      writer_start = writer_marks[[1]][i, "start"] - 1
      rest_of_text = substring(full_text, writer_start, writer_start+200)
      byLoc = unlist(gregexpr('by', rest_of_text))[1]
      end = max(unlist(gregexpr('ranscrib', rest_of_text))[1] - 1,
                unlist(gregexpr('irected by', rest_of_text))[1] - 1,
                unlist(gregexpr('tory by', rest_of_text))[1] - 1,
                unlist(gregexpr('eleplay by', rest_of_text))[1] - 1,
                unlist(gregexpr('art II', rest_of_text))[1] - 1,
                unlist(gregexpr('art 2', rest_of_text))[1] - 1)
                
      if(end == -2 | (end < byLoc)) {
        end = unlist(gregexpr('\\\n', rest_of_text))[1]
      }
                
      writer_marks[[2]][i] = stri_trim_both(gsub(':', '', substring(rest_of_text, byLoc + 2, end - 1)))
    }
    episode_writers = unlist(writer_marks[[2]][1])
  } else {
    episode_writers = ''
  }
  
  tib_episode = tibble(title = episode_title)  
  if(is_empty(episode_writers)) {
    tib_episode['writer'] = ''
  } else {
    tib_episode['writer'] = episode_writers
  }
  tib_episode['transcriber'] <- episode_transciber
  if(is_empty(episode_transciber_addl)) {
    tib_episode['transcriber_addl'] = ''
  } else {
    tib_episode['transcriber_addl'] = episode_transciber_addl
  }
  tib_episode = create_season_and_episode_columns(tib_episode, file_name)
  
  
  # Add the row number
  tib_script <- tib_script %>% mutate(row_id = row_number()) 
  tib_script <- separate_script_and_scenes(tib_script)
  
  tib_script <- create_season_and_episode_columns(tib_script, file_name)
  
  tib_script['scene'] <- NA
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
    d6 = str_to_title(substr(d, 1, 6))
    
    if (str_detect(d, "\\[Scene:")) {
      tib_script[i, "scene"] = d
      scene_num = scene_num + 1
    } else if ((d == 'End' | d == 'END' | d == 'THE END' | d1 == '(' | d1 == '[' | d6 == 'Commer' | d6 == 'Closin' | 
                d6 == 'Ending' | d6 == 'Openin')) {
      tib_script[i, "direction"] = d
    } else if (str_detect(d, ":")) {
      tib_script[i, "dialogue"] = d
    } else {
      tib_script[i, "prevUnseen"] = d
    } 
    tib_script$dialogue = str_replace_all(tib_script$dialogue, c('\\\n'=' '))
    tib_script <- extract_direction_from_dialogue(tib_script, i)
    tib_script[i, "scene_number"] = scene_num
  }
  
  # Create the character column by splitting the dialogue column in two
  tib_script[c('character', 'dialogue')] <- str_split_fixed(tib_script$dialogue, ':', 2)
  tib_script$dialogue = stri_trim_both(tib_script$dialogue)
      
  # Clean up unwanted characters
  tib_script$scene <- stri_trim_both(str_replace_all(tib_script$scene, c('\\[Scene:'='', '\\]'='')))
  tib_script$direction <- str_replace_all(tib_script$direction, c('\\('='', '\\)'='', '\\['='', '\\]'=''))
  tib_script$prevUnseen <- str_replace_all(tib_script$prevUnseen, c('\\['='', '\\]'=''))
  #tib_script <- tib_script %>% filter(scene_number > 0)
  tib_script <- tib_script %>% mutate(row_id = row_number()) 
  tib_script$script = stri_trim_both(tib_script$script)
  tib_script <- tib_script %>% filter(script != "" & script != "END")
  
  master_script = rbind(master_script, tib_script)
  master_episode_list = rbind(master_episode_list, tib_episode)
  
  cur_file <- cur_file + 1
  
}

#View(master_script[!is.na(master_script$prevUnseen), ])

close(pb)

saveRDS(master_script, file = 'friends_script.RData')

