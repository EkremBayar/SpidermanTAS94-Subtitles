# Packages ----------------------------------------------------------------
library(tidyverse)
library(rvest)

# Episode Names -----------------------------------------------------------

#https://www.opensubtitles.org/tr/ssearch/sublanguageid-eng/idmovie-18297

# Episode Names
page <- read_html("https://www.opensubtitles.org/tr/ssearch/sublanguageid-eng/idmovie-18297")
ep <- page %>% html_nodes("table#search_results .change.even a") %>% html_text()
ep <- ep[!str_detect(ep, "\\\t|\\.")]


# Unzip
lf <- paste0(getwd(), "/spiderman/files/")
lf <- paste0(lf, list.files(lf))
for(i in lf){
  print(i)
  unzip(zipfile = i, exdir = paste0(getwd(), "/spiderman/extract/"))
}

# Episodes STR Files
lf <- list.files(paste0(getwd(), "/spiderman/extract/"))
fl <- lf[str_detect(lf, "LAZY")]
fl <- fl[fl != "Spider-Man.The.Animated.Series.S01E07.DSNP.WEB-DL.AAC2.0.H.264-LAZY.synced.by.D3S3RT.en.srt"]

flnames <- str_split_i(str_split_i(fl, "Series.", -1),"\\.",1)
flpath <- paste0(getwd(), "/spiderman/extract/",fl)


# Raw DF ------------------------------------------------------------------
raw_df <- lapply(1:length(flnames), function(i){
  srt_lines <- readLines(flpath[i], encoding = "UTF-8")
  data.frame(X1 = srt_lines, X2 = flnames[i])
}) %>% bind_rows() %>%
  mutate(
    book_num = str_remove_all(str_split_i(X2, "E", 1), "S"),
    chapter_num = str_split_i(X2, "E", 2),
    book_num = as.integer(ifelse(str_sub(book_num, 1) == "0", str_remove_all(book_num, "0"), book_num)),
    chapter_num = as.integer(ifelse(str_sub(chapter_num, 1) == "0", str_remove_all(chapter_num, "0"), chapter_num))
  ) %>%
  select(-X2)


# Tidy Transcription ------------------------------------------------------
unite_sentences <- function(data){
  temp <- data %>% group_by(book_num, chapter_num) %>%
    mutate(
      check = ifelse(grepl("^[a-z]", character_words), TRUE, FALSE),
      last = case_when(
        str_sub(character_words, -1) == "." ~ TRUE,
        str_sub(character_words, -1) == "!" ~ TRUE,
        str_sub(character_words, -1) == "?" ~ TRUE,
        str_sub(character_words, -1) == "..." ~ TRUE,
        .default = FALSE
      ),
      lag_check = lead(check, 1),
      lag_text = lead(character_words, 1),
      lag_X3 = lead(X3, 1),
      character_words = str_squish(ifelse(check == FALSE & lag_check == TRUE,paste0(character_words, " ", lag_text),character_words)),
      rem = lag((ifelse(check == FALSE & lag_check == TRUE,lag_X3,NA)))
    ) %>%
    ungroup() %>%
    filter(is.na(rem)) %>%
    select(-c("last", "lag_check", "lag_text", "check", "lag_X3", "rem")) %>%
    arrange(book_num, chapter_num, X3)
  return(temp)
}

President and CEO of
1
5
7654
J3 Communications.

df <- raw_df %>%
  #filter(book_num == 1, chapter_num == 5) %>%
  mutate(
    character_words = ifelse(!str_detect(X1, "-->"), X1, "")
  ) %>%
  # Remove Unneccessary
  filter(
    !str_detect(X1, "-->|Theme Song|♪"), str_squish(X1) != "",
  ) %>%
  # Word Order
  mutate(
    X3 = (str_extract_all(character_words, "\\d+")),
    X3 = as.integer(sapply(X3, function(i){ifelse(length(unlist(i)) == 0, NA_character_, unlist(i))})),
    #character_words = str_squish(ifelse(str_sub(character_words, 1, 1) == "-", str_sub(character_words, 2), character_words)),
    X3 = ifelse(character_words != X3, NA_integer_, X3)
  ) %>%
  # Fill Word Order
  group_by(book_num, chapter_num) %>%
  fill(X3, .direction = "down") %>%
  fill(X3, .direction = "up") %>%
  mutate(X3 = ifelse(is.na(X3), 1, X3)) %>%
  filter(X3 != character_words) %>%
  ungroup() %>%
  # Paste same dialouge
  #group_by(book_num, chapter_num, X3) %>%
  #summarise(text = paste0(character_words, collapse = " ")) %>%
  #ungroup() %>%
  #arrange(book_num, chapter_num, X3) %>%
  # Check dialouges
  mutate(
    check = ifelse(grepl("^[a-z]", character_words), TRUE, FALSE),
    last = case_when(
      str_sub(character_words, -1) == "." ~ TRUE,
      str_sub(character_words, -1) == "!" ~ TRUE,
      str_sub(character_words, -1) == "?" ~ TRUE,
      str_sub(character_words, -1) == "..." ~ TRUE,
      str_sub(character_words, 1,1) == "-" ~ TRUE,
      .default = FALSE
      )
  ) %>%
  # Lagged text and create new text
  group_by(book_num, chapter_num) %>%
  mutate(
    lag_check = lead(check, 1),
    lag_text = lead(character_words, 1),
    lag_X3 = lead(X3, 1),
    character_words = str_squish(ifelse(check == FALSE & lag_check == TRUE,paste0(character_words, " ", lag_text),character_words)),
    rem = lag((ifelse(check == FALSE & lag_check == TRUE,lag_X3,NA)))
  ) %>%
  ungroup() %>%
  filter(is.na(rem)) %>%
  select(-c("last", "lag_check", "lag_text", "check", "lag_X3", "rem")) %>%
  arrange(book_num, chapter_num, X3) %>%
  #
  unite_sentences() %>%
  unite_sentences() %>%
  unite_sentences() %>%
  unite_sentences() %>%
  unite_sentences() %>%
  unite_sentences() %>%
  unite_sentences() %>%
  unite_sentences() %>%
  unite_sentences() %>%
  unite_sentences() %>%
  unite_sentences() %>%
  select(-"X1") %>%
  #
  group_by(book_num, chapter_num, X3) %>%
  summarise(character_words = paste0(character_words, collapse = " ")) %>%
  ungroup() %>%
  arrange(book_num, chapter_num, X3) %>%
  # Character Names
  mutate(
    character = case_when(
      .default = "Unknown",
      !str_detect(character_words, "\\[thinking\\]") & str_detect(character_words, " thinking\\]") ~ str_remove_all(str_split_i(character_words, " thinking\\]", 1), "\\[")
    )
  ) %>%
  mutate(
    character = case_when(
      .default = character,
      str_detect(character_words, "\\[Nick\\]") ~ "Nick",
      str_detect(character_words, "\\[Shocker\\]") ~ "Shocker",
      str_detect(character_words, "\\[Spider-Man\\]|\\[Spider-man\\]") ~ "Spider-Man",
      str_detect(character_words, "\\[Peter\\]") ~ "Peter",
      str_detect(character_words, "\\[Madame Web\\]") ~ "Madame Web",
      str_detect(character_words, "\\[Mary Jane\\]|\\[Mary-Jane\\]") ~ "Mary Jane",
      str_detect(character_words, "\\[Chameleon\\]") ~ "Chameleon",
      str_detect(character_words, "\\[man\\]|\\[Man\\]") ~ "Man",
      str_detect(character_words, "\\[Dr. Octavius\\]") ~ "Dr. Octavius",
      str_detect(character_words, "\\[Chameleon\\]") ~ "Chameleon",
      str_detect(character_words, "\\[Scorpion\\]") ~ "Scorpion",
      str_detect(character_words, "\\[Morbius\\]") ~ "Morbius",
      str_detect(character_words, "\\[Norman\\]") ~ "Norman",
      str_detect(character_words, "\\[Felicia\\]") ~ "Felicia",
      str_detect(character_words, "\\[News Anchor\\]") ~ "News Anchor",
      str_detect(character_words, "\\[Doctor Octopus\\]") ~ "Doctor Octopus",
      str_detect(character_words, "\\[Wolverine\\]") ~ "Wolverine",
      str_detect(character_words, "\\[Colonel\\]") ~ "Colonel",
      str_detect(character_words, "\\[Aunt May\\]|\\[Aunt may\\]") ~ "Aunt May",
      str_detect(character_words, "\\[Newscaster\\]") ~ "Newscaster",
      str_detect(character_words, "\\[Michael\\]") ~ "Michael",
      str_detect(character_words, "\\[Dormammu\\]") ~ "Dormammu",
      str_detect(character_words, "\\[Debra\\]") ~ "Debra",
      str_detect(character_words, "\\[male voice\\]") ~ "Male voice",
      str_detect(character_words, "\\[Lizard\\]") ~ "Lizard",
      str_detect(character_words, "\\[Vulture\\]") ~ "Vulture",
      str_detect(character_words, "\\[Ashley\\]") ~ "Ashley",
      str_detect(character_words, "\\[Gargan\\]") ~ "Gargan",
      str_detect(character_words, "\\[Smythe\\]") ~ "Smythe",
      str_detect(character_words, "\\[Alisa\\]") ~ "Alisa",
      str_detect(character_words, "\\[Taina\\]") ~ "Taina",
      str_detect(character_words, "\\[John\\]") ~ "John",
      str_detect(character_words, "\\[Charles\\]") ~ "Charles",
      str_detect(character_words, "\\[Hobgoblin\\]") ~ "Hobgoblin",
      str_detect(character_words, "\\[Miriam\\]") ~ "Miriam",
      str_detect(character_words, "\\[Explorer\\]") ~ "Explorer",
      str_detect(character_words, "\\[Rogue\\]") ~ "Rogue",
      str_detect(character_words, "\\[Anastasia Hardy\\]") ~ "Anastasia Hardy",
      str_detect(character_words, "\\[Kingpin\\]") ~ "Kingpin",
      str_detect(character_words, "\\[Jonah\\]") ~ "Jonah",
      str_detect(character_words, "\\[Connors\\]") ~ "Connors",
      str_detect(character_words, "\\[Eddie\\]") ~ "Eddie",
      str_detect(character_words, "\\[Blade\\]") ~ "Blade",
      str_detect(character_words, "\\[Daredevil\\]") ~ "Daredevil",
      str_detect(character_words, "\\[Black Cat\\]") ~ "Black Cat",
      str_detect(character_words, "\\[Harry\\]") ~ "Harry",
      str_detect(character_words, "\\[woman\\]") ~ "Woman",
      str_detect(character_words, "\\[reporter\\]") ~ "Reporter",
      str_detect(character_words, "\\[Venom\\]") ~ "Venom",
      str_detect(character_words, "\\[Rhino\\]") ~ "Rhino",
      str_detect(character_words, "\\[Robbie\\]") ~ "Robbie",
      str_detect(character_words, "\\[Randy\\]") ~ "Randy",
      str_detect(character_words, "\\[The Cat\\]") ~ "The Cat",
      str_detect(character_words, "\\[Robby's voice\\]") ~ "Robby's voice",
      str_detect(character_words, "\\[Robert\\]") ~ "Robert",
      str_detect(character_words, "\\[Kraven\\]") ~ "Kraven",
      str_detect(character_words, "\\[Narrator\\]") ~ "Narrator",
      str_detect(character_words, "\\[Mysterio\\]") ~ "Mysterio",
      str_detect(character_words, "\\[Mariah\\]") ~ "Mariah",
      str_detect(character_words, "\\[Curtis\\]") ~ "Curtis",
      str_detect(character_words, "\\[Fury\\]") ~ "Fury",
      str_detect(character_words, "\\[Gila\\]") ~ "Gila",
      str_detect(character_words, "\\[Ben\\]") ~ "Ben",
      str_detect(character_words, "\\[War Machine\\]") ~ "War Machine",
      str_detect(character_words, "\\[Iron Man\\]") ~ "Iron Man",
      str_detect(character_words, "\\[Terri\\]") ~ "Terri",
      str_detect(character_words, "\\[Announcer\\]") ~ "Announcer",
      str_detect(character_words, "\\[Alistair\\]") ~ "Alistair",
      str_detect(character_words, "\\[Prowler\\]") ~ "Prowler",
      str_detect(character_words, "\\[Frank\\]") ~ "Frank",
      str_detect(character_words, "\\[Electro\\]") ~ "Electro",
      str_detect(character_words, "\\[Rheinholdt\\]") ~ "Rheinholdt",
      str_detect(character_words, "\\[newscaster\\]") ~ "Newscaster",
      str_detect(character_words, "\\[Cletus\\]") ~ "Cletus",
      str_detect(character_words, "\\[Carnage\\]") ~ "Carnage",
      str_detect(character_words, "\\[Richard\\]") ~ "Richard",
      str_detect(character_words, "\\[Farley\\]") ~ "Farley",
      str_detect(character_words, "\\[Cyclops\\]") ~ "Cyclops",
      str_detect(character_words, "\\[Sylvia\\]") ~ "Sylvia",
      str_detect(character_words, "\\[Sergei\\]") ~ "Sergei",
      str_detect(character_words, "\\[pilot\\]") ~ "Pilot",
      str_detect(character_words, "\\[Silvermane\\]") ~ "Silvermane",
      str_detect(character_words, "\\[Flash\\]") ~ "Flash",
      str_detect(character_words, "\\[Whistler\\]") ~ "Whistler",
      str_detect(character_words, "\\[May\\]") ~ "May",
      str_detect(character_words, "\\[Beyonder\\]") ~ "Beyonder",
      str_detect(character_words, "\\[Lee\\]") ~ "Lee",
      str_detect(character_words, "\\[Nurse\\]") ~ "Nurse",
      str_detect(character_words, "\\[Larry\\]") ~ "Larry",
      str_detect(character_words, "\\[Landon\\]") ~ "Landon",
      str_detect(character_words, "\\[Jean\\]") ~ "Jean",
      str_detect(character_words, "\\[Green Goblin\\]") ~ "Green Goblin",
      str_detect(character_words, "\\[Hobie\\]") ~ "Hobie",
      str_detect(character_words, "\\[Tombstone\\]") ~ "Tombstone",
      str_detect(character_words, "\\[woman on TV\\]") ~ "Woman on TV",
      str_detect(character_words, "\\[Officer\\]|\\[officer\\]") ~ "Officer",
      str_detect(character_words, "\\[Clay\\]") ~ "Clay",
      str_detect(character_words, "\\[Keene\\]") ~ "Keene",
      str_detect(character_words, "\\[Fisk\\]") ~ "Fisk",
      str_detect(character_words, "\\[Storm\\]") ~ "Storm",
      str_detect(character_words, "\\[Peter Voiceover\\]") ~ "Peter Voiceover",
      str_detect(character_words, "\\[Hydro-Man\\]") ~ "Hydro-Man",
      str_detect(character_words, "\\[The Spot\\]") ~ "The Spot",
      str_detect(character_words, "\\[scientist\\]") ~ "Scientist",
      str_detect(character_words, "\\[Spencer\\]") ~ "Spencer",
      str_detect(character_words, "\\[boy's voice\\]") ~ "Boy's voice",
      str_detect(character_words, "\\[Anna\\]") ~ "Anna",
      str_detect(character_words, "\\[Toomes\\]") ~ "Toomes",
      str_detect(character_words, "\\[Octavius\\]") ~ "Octavius",
      str_detect(character_words, "\\[Robby\\]") ~ "Robby",
      str_detect(character_words, "\\[Brock\\]") ~ "Brock",
      str_detect(character_words, "\\[Mr. Robertson\\]") ~ "Mr. Robertson",
      str_detect(character_words, "\\[Microchip\\]") ~ "Microchip",
      str_detect(character_words, "\\[John, on TV\\]") ~ "John, on TV",
      str_detect(character_words, "\\[narrator\\]") ~ "Narrator",
      str_detect(character_words, "\\[Professor X\\]") ~ "Professor X",
      str_detect(character_words, "\\[Beast\\]") ~ "Beast",
      str_detect(character_words, "\\[Morrie\\]") ~ "Morrie",
      str_detect(character_words, "\\[Liz\\]") ~ "Liz",
      str_detect(character_words, "\\[Guard\\]") ~ "Guard",
      str_detect(character_words, "\\[Henchman\\]") ~ "Henchman",
      str_detect(character_words, "\\[Hammerhead\\]") ~ "Hammerhead",
      str_detect(character_words, "\\[Fury's voice\\]") ~ "Fury's voice",
      str_detect(character_words, "\\[Glory's voice\\]") ~ "Glory's voice",
      str_detect(character_words, "\\[John's voice\\]") ~ "John's voice",
      str_detect(character_words, "\\[NASA Technician\\]") ~ "NASA Technician",
      str_detect(character_words, "\\[Security Personnel\\]") ~ "Security Personnel",
      str_detect(character_words, "\\[Police Officer\\]") ~ "Police Officer",
      str_detect(character_words, "\\[Mercenary\\]") ~ "Mercenary",
      str_detect(character_words, "\\[policeman\\]") ~ "Policeman",
      str_detect(character_words, "\\[Pilot\\]") ~ "Pilot",
      str_detect(character_words, "\\[female reporter\\]") ~ "Female reporter",
      str_detect(character_words, "\\[male reporter\\]") ~ "Male reporter",
      str_detect(character_words, "\\[Paul\\]") ~ "Paul",
      str_detect(character_words, "\\[Anchor\\]") ~ "Anchor",
      str_detect(character_words, "\\[J.J.\\]") ~ "J.J.",
      str_detect(character_words, "\\[Woman\\]") ~ "Woman",
      str_detect(character_words, "\\[man 2\\]|\\[Man 2\\]") ~ "Man 2",
      str_detect(character_words, "\\[Lieutenant Lee\\]") ~ "Lieutenant Lee",
      str_detect(character_words, "\\[TV Announcer\\]") ~ "TV Announcer",
      str_detect(character_words, "\\[Mrs. Connor\\]") ~ "Mrs. Connor",
      str_detect(character_words, "\\[Billy\\]") ~ "Billy",
      str_detect(character_words, "\\[Mr. Robertson\\]") ~ "Mr. Robertson",
      str_detect(character_words, "\\[Stan\\]") ~ "Stan",
      str_detect(character_words, "\\[Scarlet Spider\\]") ~ "Scarlet Spider",
      str_detect(character_words, "\\[Spider-Man voiceover\\]") ~ "Spider-Man voiceover",
      str_detect(character_words, "\\[Spider-Carnage\\]") ~ "Spider-Carnage",
      str_detect(character_words, "\\[Armored Spider\\]") ~ "Armored Spider",
      str_detect(character_words, "\\[Reed\\]") ~ "Reed",
      str_detect(character_words, "\\[Dr. Doom\\]") ~ "Dr. Doom",
      str_detect(character_words, "\\[woman over PA\\]") ~ "Woman over PA",
      str_detect(character_words, "\\[Tony\\]") ~ "Tony",
      str_detect(character_words, "\\[Otto\\]") ~ "Otto",
      str_detect(character_words, "\\[Steve\\]") ~ "Steve",
      str_detect(character_words, "\\[Red Skull\\]") ~ "Red Skull",
      str_detect(character_words, "\\[Glory\\]") ~ "Glory",
      str_detect(character_words, "\\[Margaret\\]") ~ "Margaret",
      str_detect(character_words, "\\[Miranda\\]") ~ "Miranda",
      str_detect(character_words, "\\[men\\]") ~ "Men",
      str_detect(character_words, "\\[Cletus\\]") ~ "Cletus",
      str_detect(character_words, "\\[Wilson\\]") ~ "Wilson",
      str_detect(character_words, "\\[As Green Goblin\\]") ~ "As Green Goblin",
      str_detect(character_words, "\\[Stark\\]") ~ "Stark",
      str_detect(character_words, "\\[Randy\\]") ~ "Randy",
      str_detect(character_words, "\\[Silver Sable\\]") ~ "Silver Sable",
      str_detect(character_words, "\\[May\\]") ~ "May",
      str_detect(character_words, "\\[Gecko\\]") ~ "Gecko",
      str_detect(character_words, "\\[Chip\\]") ~ "Chip",
      str_detect(character_words, "\\[Jackson\\]") ~ "Jackson",
      str_detect(character_words, "\\[Carter\\]") ~ "Carter",
      str_detect(character_words, "\\[as Norman\\]") ~ "As Norman",
      str_detect(character_words, "\\[Miles\\]") ~ "Miles",
      str_detect(character_words, "\\[impostor\\]") ~ "Impostor",
      str_detect(character_words, "\\[Osborne\\]") ~ "Osborne",
      str_detect(character_words, "\\[Reporter\\]") ~ "Reporter",
      str_detect(character_words, "\\[Mousie\\]") ~ "Mousie",
      str_detect(character_words, "\\[Hardy\\]") ~ "Hardy",
      str_detect(character_words, "\\[director\\]") ~ "Director",
      str_detect(character_words, "\\[nurse\\]") ~ "Nurse",
      str_detect(character_words, "\\[Bellhop\\]") ~ "Bellhop",
      str_detect(character_words, "\\[Wong\\]") ~ "Wong",
      str_detect(character_words, "\\[Peter Parker\\]") ~ "Peter Parker",
      str_detect(character_words, "\\[The Whizzer\\]") ~ "The Whizzer",
      str_detect(character_words, "\\[kid\\]") ~ "Kid",
      str_detect(character_words, "\\[boy\\]") ~ "Boy",
      str_detect(character_words, "\\[Ohn\\]") ~ "Ohn",
      str_detect(character_words, "\\[Jameson\\]") ~ "Jameson",
      str_detect(character_words, "\\[Scarlet-Spider\\]") ~ "Scarlet-Spider",
      str_detect(character_words, "\\[police officer\\]") ~ "Police Officer",
      str_detect(character_words, "\\[Omar Mosley\\]") ~ "Omar Mosley",
      str_detect(character_words, "\\[Captain America\\]") ~ "Captain America",
      str_detect(character_words, "\\[Peter voiceover\\]") ~ "Peter Voiceover",
      str_detect(character_words, "\\[Mrs. Farrell\\]") ~ "Mrs. Farrell",
      str_detect(character_words, "\\[spider-Man\\]") ~ "Spider-Man",
      str_detect(character_words, "\\[Dr. Connors\\]") ~ "Dr. Connors",
      str_detect(character_words, "\\[Choi\\]") ~ "Choi",
      str_detect(character_words, "\\[Anastasia\\]") ~ "Anastasia",
      str_detect(character_words, "\\[Arthur\\]") ~ "Arthur",
      str_detect(character_words, "\\[announcement over PA\\]") ~ "Announcement over PA",
      str_detect(character_words, "\\[as Green Goblin\\]") ~ "As Green Goblin",
      str_detect(character_words, "\\[Miss America\\]") ~ "Miss America",
      str_detect(character_words, "\\[Minion\\]") ~ "Minion",
      str_detect(character_words, "\\[Doctor Strange\\]") ~ "Doctor Strange",
      str_detect(character_words, "\\[Dr. Strange\\]") ~ "Doctor Strange",
      str_detect(character_words, "\\[dispatcher\\]") ~ "Dispatcher",
      str_detect(character_words, "\\[man on radio\\]") ~ "Man on radio",
      str_detect(character_words, "\\[Mary Jane's father\\]") ~ "Mary Jane's Father",
      str_detect(character_words, "\\[Mary Jane voiceover\\]") ~ "Mary-Jane Voiceover",
      str_detect(character_words, "\\[Agent X\\]") ~ "Agent X",
      str_detect(character_words, "\\[Brace\\]") ~ "Brace",
      str_detect(character_words, "\\[Mordo\\]") ~ "Mordo",
      str_detect(character_words, "\\[As Norman\\]") ~ "As Norman",
      str_detect(character_words, "\\[Aunt Anna\\]") ~ "Aunt Anna",
      str_detect(character_words, "\\[woman's voice\\]") ~ "Woman's Voice",
      str_detect(character_words, "\\[Osborn\\]") ~ "Osborn",
      str_detect(character_words, "\\[Hardesky\\]") ~ "Hardesky",
      str_detect(character_words, "\\[John Hardesky\\]") ~ "John Hardesky",
      str_detect(character_words, "\\[Terri grunts\\]") ~ "Terri grunts",
      str_detect(character_words, "\\[man 2\\]") ~ "Man 2",
      str_detect(character_words, "\\[Mary Jane, over telephone\\]") ~ "Mary-Jane, over telephone",
      str_detect(character_words, "\\[Spider-Man\\'s voice\\]") ~ "Spider-Man's voice",
      str_detect(character_words, "\\[Fisk\\'s Father\\]") ~ "Fisk's Father",
      str_detect(character_words, "\\[Editor\\]") ~ "Editor",
      str_detect(character_words, "\\[cameraman\\]") ~ "Cameraman",
      str_detect(character_words, "\\[driver\\]") ~ "Driver",
      str_detect(character_words, "\\[Beck\\]") ~ "Beck",
      str_detect(character_words, "\\[man on speaker\\]") ~ "Man on speaker",
      str_detect(character_words, "\\[man in mask\\]") ~ "Man in mask",
      str_detect(character_words, "\\[man 1\\]") ~ "Man 1",
      str_detect(character_words, "\\[man 2\\]") ~ "Man 2",
      str_detect(character_words, "\\[man 3\\]") ~ "Man 3",
      str_detect(character_words, "\\[Spider-Man |\\[Spider-man |\\[Spider-man ") ~ "Spider-Man",
      str_detect(character_words, "\\[Boris\\]") ~ "Boris",
      str_detect(character_words, "\\[Mary\\]") ~ "Mary",
      str_detect(character_words, "\\[minister\\]") ~ "Minister",
      str_detect(character_words, "\\[Punisher\\]") ~ "Punisher",
      str_detect(character_words, "\\[Mr. Jameson\\]") ~ "Mr. Jameson",
      str_detect(character_words, "\\[man in blue\\]") ~ "Man in blue",
      str_detect(character_words, "\\[announcer\\]") ~ "Announcer",
      str_detect(character_words, "\\[Agent\\]") ~ "Agent",
      str_detect(character_words, "\\[Black Marvel\\]") ~ "Black Marvel",
      str_detect(character_words, "\\[Sarah\\]") ~ "Sarah",
      str_detect(character_words, "\\[Officer 2\\]") ~ "Officer 2",
      str_detect(character_words, "\\[Peter reading\\]") ~ "Peter",
      str_detect(character_words, "\\[man 1, angrily\\]") ~ "Man 1",
      str_detect(character_words, "\\[man, on radio\\]") ~ "Man, on radio",
      str_detect(character_words, "\\[deeper voice\\]") ~ "Deeper voice",
      str_detect(character_words, "\\[security guard\\]") ~ "Security Guard",
      str_detect(character_words, "\\[Hobgoblin ") ~ "Hobgoblin",
      str_detect(character_words, "\\[bartender\\]") ~ "Bartender",
      str_detect(character_words, "\\[Wolverine ") ~ "Wolverine",
      str_detect(character_words, "\\[Beast Voiceover\\]") ~ "Beast Voiceover",
      str_detect(character_words, "\\[Quentin\\]") ~ "Quentin",
      str_detect(character_words, "\\[Toomes, over telephone\\]") ~ "Toomes, over telephone",
      str_detect(character_words, "\\[Curt\\]") ~ "Curt",
      str_detect(character_words, "\\[Spider-Man, \\]") ~ "Spider-Man",
      str_detect(character_words, "\\[Mousie, Jamaican accent\\]") ~ "Mousie",
      str_detect(character_words, "\\[Deepak\\]") ~ "Deepak",
      str_detect(character_words, "\\[prosecutor\\]") ~ "Prosecutor",
      str_detect(character_words, "\\[Richard, on speaker\\]") ~ "Richard, on speaker",
      str_detect(character_words, "\\[guard\\]") ~ "Guard",
      str_detect(character_words, "\\[Jason\\]") ~ "Jason",
      str_detect(character_words, "\\[Lenny\\]") ~ "Lenny",
      str_detect(character_words, "\\[Spider-Man\\[") ~ "Spider-Man",
      str_detect(character_words, "\\[Spider-Man ") ~ "Spider-Man",
      str_detect(character_words, "\\[Martha\\]") ~ "Martha",
      str_detect(character_words, "\\[Matt\\]") ~ "Matt",
      str_detect(character_words, "\\[S.H.I.E.L.D. Agent\\]") ~ "S.H.I.E.L.D. Agent",
      str_detect(character_words, "\\[Green Goblin cackling\\]") ~ "Green Goblin",
      str_detect(character_words, "\\[Jackson, on loudspeaker\\]") ~ "Jackson, on loudspeaker",
      str_detect(character_words, "\\[Peter gasps\\]") ~ "Peter"
    )
  ) %>%
  # Unneccessary
  filter(character_words != "NA", !is.na(character_words), !str_detect(character_words, "upbeat rock music|intensifying ominous music|loud rock music|orchestra music building|playing classical music|violin music|music playing|mid tempo rock music|orchestral music|orchestra playing|ominous music playing|playing theme song|Spider-Man theme|Spider-Man theme song playing|by Joe Perry playing|Spider-man Theme")) %>%
  # Scene Description
  mutate(
    character = if_else((character == "Unknown" & str_sub(character_words, 1, 1) == "[" & str_sub(character_words, -1) == "]"), "Scene Description", character),
    full_text = character_words,
    character_words = str_squish(gsub("\\[.*?\\]", "", character_words)),
    character = ifelse(character_words == "- -", "Scene Description", character),
    character_words = ifelse(character_words == "- -", "", str_squish(character_words)),
    X3 = 1
  ) %>%
  group_by(book_num, chapter_num) %>%
  mutate(X3 = cumsum(X3)) %>%
  ungroup() %>%
  arrange(book_num, chapter_num, X3) %>%
  rename(word_order = X3)

# Chapter Season Names ----------------------------------------------------
openxlsx::write.xlsx(df %>% select(book_num, chapter_num) %>% distinct() %>%
    bind_cols(chapter = ep), "episode_name.xlsx")

df <- df %>% left_join(
  df %>% select(book_num, chapter_num) %>% distinct() %>%
    bind_cols(chapter = ep)) %>%
  mutate(book = paste0("Season ", book_num)) %>%
  select(book_num, book, chapter_num, chapter, character, character_words, word_order, full_text)

# Usethis Data ------------------------------------------------------------
transcripts_spiderman_tas <- df
usethis::use_data(transcripts_spiderman_tas)

