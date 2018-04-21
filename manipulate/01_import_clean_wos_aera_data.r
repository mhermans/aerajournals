
library(readr)
library(dplyr)
library(tidyr)
library(assertr)
library(lubridate)
library(purrr)
library(here)
library(stringr)
# library(bibliometrix)

read_tsv_wos <- function(file) {
  # WOS-files have 1 column more then header labels: 
  #   read in header + pad with an "XX"
  wos_col_names = c(names(read_tsv(
    file, 
    quote = "µ", # make sure " and ' are not recognized as quote-characters for structure
    col_types = cols(.default = col_character()),
    n_max = 0)), 'XX')
  
  wos_df = read_tsv(
    file, 
    quote = "µ", # make sure " and ' are not recognized as quote-characters for structure
    skip = 1, col_names = wos_col_names, col_types = cols(.default = col_character()))  
  
  return(wos_df)
  
}

# Read in 500 record 

setwd(Sys.getenv('DATADIR_HIVA_LOCAL'))

aerj_raw = bind_rows(
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_aerj_1-500.txt'),
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_aerj_501-1000.txt'),
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_aerj_1001-1500.txt'),
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_aerj_1501-2000.txt'),
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_aerj_2001-2202.txt')) %>%
  verify(dim(.) == c(2202, 69))

eepa_raw = bind_rows(
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_eepa_1-500.txt'),
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_eepa_501-624.txt')) %>%
  verify(dim(.) == c(624, 69))

er_raw = bind_rows(
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_er_1-500.txt'),
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_er_501-516.txt')) %>%
  verify(dim(.) == c(516, 69))

jebs_raw = bind_rows(
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_jebs_1-500.txt'),
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_jebs_501-635.txt')) %>%
  verify(dim(.) == c(635, 69))

rer_raw = bind_rows(
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_rer_1-500.txt'),
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_rer_501-1000.txt'),
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_rer_1001-1500.txt'),
  read_tsv_wos('WOS/aera_journals/20180419_wos_aera_rer_1501-1676.txt')) %>%
  verify(dim(.) == c(1676, 69))


rre_raw = read_tsv_wos('WOS/aera_journals/20180419_wos_aera_rre_1-266.txt') %>%
  verify(dim(.) == c(266, 69))

setwd(here::here())


# Join and clean AERA WOS-data
## ===========================

aera_raw = bind_rows(
  aerj_raw,
  eepa_raw,
  er_raw,
  jebs_raw,
  rer_raw,
  rre_raw) %>%
  verify(dim(.) == c(5919, 69))


# http://images.webofknowledge.com/WOKRS53B4/help/WOS/hs_wos_fieldtags.html

aera = aera_raw %>%
  rename(
    publication_type = PT,
    authors = AU,
    document_type = DT,
    publication_name = SO,
    author_keywords = DE,
    abstract = AB,
    author_address = C1,
    reprint_address = RP,
    email_address = EM,
    cited_references = CR,
    cited_reference_count = NR,
    publication_date = PD,
    publication_year = PY,
    volume = VL,
    issue = IS,
    beginning_page = BP,
    ending_page = EP,
    page_count = PG,
    source_title_abbrv = J9,
    source_title_abbrv_iso = JI,
    report_generation_date = DA) %>%
  mutate(
    publication_type = case_when(
      publication_type == 'J' ~ 'journal',
      publication_type == 'B' ~ 'book',
      publication_type == 'S' ~ 'series',
      publication_type == 'P' ~ 'patent'))

aera = aera %>%
  mutate(
    source_title_shortcode = case_when(
      source_title_abbrv == 'AM EDUC RES J' ~ 'AERJ',
      source_title_abbrv == 'EDUC EVAL POLICY AN' ~ 'EEPA',
      source_title_abbrv == 'EDUC RESEARCHER' ~ 'ER',
      source_title_abbrv == 'J EDUC BEHAV STAT' ~ 'JEBS',
      source_title_abbrv == 'REV EDUC RES' ~ 'RER',
      source_title_abbrv == 'REV RES EDUC' ~ 'RRE'),
    author_count = map_int(str_split(authors, pattern=';'), length),
    cited_reference_count = as.integer(cited_reference_count),
    page_count = as.integer(page_count),
    references_per_page = cited_reference_count / page_count,
    publication_date_full = ymd(str_c(as.character(publication_year), '-06-15')))
  
write_csv(aera, 'data/processed/20180419_wos_aera_articledata.csv')


rm(aera, aera_raw, aerj_raw, eepa_raw, er_raw, jebs_raw, rre_raw, rer_raw, read_tsv_wos)