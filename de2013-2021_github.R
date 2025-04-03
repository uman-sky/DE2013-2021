## LOADING PACKAGES & DATA, CREATING DFM ====

if (!require("pacman")) install.packages("pacman") 

pacman::p_load(FactoMineR, factoextra, readtext, readxl, writexl, tidyverse,
               quanteda, quanteda.textstats, quanteda.textmodels)

## read in a combined list of stopwords from ISO
de_stopwords <- read_lines("./data/de_stopwords.txt")

## read in precleaned party manifestos as texts
de_data <- readtext("./data/precleaned_no_party_names/*.txt",
                    docvarsfrom = "filenames",
                    docvarnames = c("party", "year"),
                    dvsep = "-",
                    encoding = "UTF-8")

de_data$group <- str_extract(de_data$doc_id,  "^[^.]+")

## detect collocations and add them to the DFM
de_toks_col <- corpus(de_data, text_field = "text") %>% 
  tokens(remove_punct = T) %>% 
  tokens_wordstem(language = "de") %>% 
  tokens_select(pattern = "^[A-Z]", 
                valuetype = "regex", 
                case_insensitive = T, 
                padding = TRUE) %>% 
  textstat_collocations(min_count = 30)

de_dfm <- corpus(de_data, text_field = "text") %>% 
  tokens(remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T,
         remove_separators = TRUE) %>% 
  tokens_remove(pattern = de_stopwords) %>% 
  tokens_wordstem(language = "de") %>% 
  tokens_compound((phrase(c("erneuerbar energi", "landlich raum", "milliard euro",
                            "sozial gerecht", "europa union", "offent hand", 
                            "viel mensch", "europa eben", "sozial marktwirtschaft", 
                            "gut arbeit", "offent dien", "landlich region", 
                            "nachhalt entwicklung", "selbstbestimmt leb", 
                            "mittl unternehm", "vereint nation", "offent daseinsvorsorg",
                            "gesetz rentenversicher", "bezahlbar wohnraum", 
                            "mittl einkomm", "organisiert kriminalitat", "sexuell identitat",
                            "million euro", "weit ausbau", "gering einkomm", 
                            "wichtig beitrag", "medizin versorg", "fair wettbewerb",
                            "global sud", "bund land", "gleich lohn", "euro pro",
                            "gut arbeitsbeding", "klar regeln", "neu technologi",
                            "qualitativ hochwert", "million mensch", "sexuell orientier",
                            "zivil krisenpravention", "gleichwert lebensverhaltnis",
                            "europa parlament", "deutsch bundestag",
                            "verantwort ubernehm", "mehr transparenz", "gesetz mindestlohn",
                            "biolog vielfalt", "gesetz rent", "offentlich-recht rundfunk",
                            "schnell internet", "gleich recht", "okolog umbau", 
                            "sozial absicher", "deutsch sprach", "offent raum",
                            "alt mensch", "digital welt", "mehr personal", 
                            "digital infrastruktur", "gross koalition", "okolog modernisier",
                            "gross herausforder", "mittelstand unternehm", "beitrag leist",
                            "demokrat kontroll", "strukturschwach region", "beruf bildung",
                            "gesellschaft teilhab", "offent infrastruktur", "offent eigentum",
                            "gleich chanc", "sozial wohnungsbau", "kulturell bildung",
                            "offent verwalt", "offent nahverkehr", "gesetz regel", 
                            "gut ausgebildet", "wirtschaft entwicklung",
                            "gut bezahlt", "sozial netzwerk", "neu arbeitsplatz",
                            "finanziell unterstutz", "hoh einkomm", "sozial zusammenhalt",
                            "off gesellschaft", "gemeinsam europa", "ganz deutschland",
                            "sozial sicherungssystem", "sozial infrastruktur", 
                            "kunstlich intelligenz", "sozial sich", "sexualisiert gewalt",
                            "gesellschaft leb", "euro mehr", "sozial recht", 
                            "wirtschaft stark", "hartz IV")))) %>% 
  dfm()

## DFM PREPROCESSING ====
de_dfm <- dfm_keep(de_dfm, min_nchar = 2)
docnames(de_dfm) <- de_data$group

topfeatures(de_dfm, 100)
de_dfm <- dfm_remove(de_dfm, pattern = c("stark", "ford", "setz", 
                                         "schaff", "uns", "zukunft", "brauch",
                                         "ziel", "moglich", "wichtig", "insbesond", 
                                         "braucht", "steh", "forder", "unabhang",
                                         "ermog", "bess", "prozent", "gilt", "bereich",
                                         "verbess", "nutz", "notwend", "deutlich",
                                         "weltweit", "lehn", "schnell", "aufgab", 
                                         "geb", "klar", "fordert", "lehnt", 
                                         "gemeinsam", "abs", "jeglich", "stimmt",
                                         "kapitel", "garantierent", "tret", 
                                         "weiterhin", "zud"))

## preparing the df
de_df <- convert(de_dfm, to = "data.frame")
rownames(de_df) <- de_df$doc_id 
de_df <- de_df %>%  #remove the first col
  select(-1) %>% 
  t()


## RUN CA 2013-2021 ====
de_res.ca <- CA(de_df, graph = FALSE)
summary(de_res.ca)

## extract percentages of explained variance and write out to Excel
percents_2013_21 <- get_eigenvalue(de_res.ca) %>% 
  as.data.frame()

percents_2013_21 <-  cbind(rownames(percents_2013_21), 
                           data.frame(percents_2013_21, row.names=NULL)) 
colnames(percents_2013_21)[1] <- "dimension"

write_xlsx(percents_2013_21, "./manif_%_2013-21_new.xlsx")

## extract parties' contribution to the dimensions & write out to Excel
ca2013_21parties <- de_res.ca[["col"]]
ca2013_21parties_contr <- as.data.frame(ca2013_21parties$contrib)
ca2013_21parties_contr <- cbind(rownames(ca2013_21parties_contr), 
                                data.frame(ca2013_21parties_contr, row.names=NULL))
colnames(ca2013_21parties_contr)[1] <- "Party"
write_xlsx(ca2013_21parties_contr, "./manif_ca2013_21_parties.xlsx")

## extract parties' coordinates 
ca2013_21parties <- de_res.ca[["col"]]
ca2013_21par_pos <- as.data.frame(ca2013_21parties$coord)
ca2013_21par_pos <- cbind(rownames(ca2013_21par_pos), 
                          data.frame(ca2013_21par_pos, row.names=NULL))
colnames(ca2013_21par_pos)[1] <- "Party"
write_xlsx(ca2013_21par_pos, "./manif_ca2013_21_party_pos.xlsx")

## extract words' contribution to the definition of the dimensions
ca2013_21 <- de_res.ca[["row"]]
ca2013_21_words <- as.data.frame(ca2013_21$contrib)
ca2013_21_words <- cbind(rownames(ca2013_21_words), 
                         data.frame(ca2013_21_words, row.names=NULL))
colnames(ca2013_21_words)[1] <- "Word"
write_xlsx(ca2013_21_words, "./manif_ca2013_21_dims.xlsx")


## CORRELATE w/CMP ====
ca2013_21_party_pos <- read_excel("./manif_ca2013_21_cmp_cor.xlsx")

# correlate w/CMP-rile, method = "spearman" 
cor(ca2013_21_party_pos[2], ca2013_21_party_pos[4], method = "spearman") # D1 X rile: 0.8513932
cor(ca2013_21_party_pos[3], ca2013_21_party_pos[4], method = "spearman") # D2 X rile: -0.1104231

# correlate w/CMP-markeco, method = spearman
cor(ca2013_21_party_pos[2], ca2013_21_party_pos[5], method = "spearman") # D1 X markeco: 0.8534572
cor(ca2013_21_party_pos[3], ca2013_21_party_pos[5], method = "spearman") # D2 X markeco: 0.0753354

# correlate w/CMP-welfare, method = spearman
cor(ca2013_21_party_pos[2], ca2013_21_party_pos[6], method = "spearman") # D1 X welfare: -0.8245614
cor(ca2013_21_party_pos[3], ca2013_21_party_pos[6], method = "spearman") # D2 X welfare: 0.120743

# correlate w/CMP-planned economy, method = spearman
cor(ca2013_21_party_pos[2], ca2013_21_party_pos[7], method = "spearman") # D1 X plsneco: -0.7234262
cor(ca2013_21_party_pos[3], ca2013_21_party_pos[7], method = "spearman") # D2 X plsneco: 0.0505676

# correlate w/CMP-internationalism, method = spearman
cor(ca2013_21_party_pos[3], ca2013_21_party_pos[8], method = "spearman") # D2 X intern-ism: -0.7894737
cor(ca2013_21_party_pos[2], ca2013_21_party_pos[8], method = "spearman") # D1 X intern-ism: 0.0505676

# correlate w/CMP-national way of life, method = spearman
cor(ca2013_21_party_pos[3], ca2013_21_party_pos[9], method = "spearman") # D2 X national: 0.1331269
cor(ca2013_21_party_pos[2], ca2013_21_party_pos[9], method = "spearman") # D1 X national: 0.8039216

# correlate w/EU, method = spearman
cor(ca2013_21_party_pos[3], ca2013_21_party_pos[10], method = "spearman") # D2 X EU: -0.6697626
cor(ca2013_21_party_pos[2], ca2013_21_party_pos[10], method = "spearman") # D1 X EU: -0.120743

# OPTIONAL: correlation b/w the CMP variables ====
library(corrplot)
cor_cols <- c("CMP-rile", "CMP-markeco", "CMP-welfare", "CMP-planeco",
              "CMP-internationalism", "CMP-national","CMP-EU")

sub_ca2013_2021_cmp <- ca2013_21_party_pos[, cor_cols]

cmp_cor <- ca2013_21_party_pos %>%
  select(all_of(cor_cols)) %>%
  cor()

# visualise correlations:
graphics.off()
plot.new()
dev.off()

corrplot(cmp_cor, method = "color", type = "upper", addCoef.col = "black", tl.cex = 0.5)
