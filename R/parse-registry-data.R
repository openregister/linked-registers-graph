# Create an interactive network graph of linked registers

# or install.packages("visNetwork")

library(here)
library(tidyverse)
library(rlang)
library(stringi)
library(jsonlite)
library(glue)
library(visNetwork)
library(govstyle) # install_github("ukgovdatascience/govstyle")


# TODO: so this must all be rewritten to pull from the API because the repo is
# out of sync.

count_records <- function(register, phase) {
  if_else(phase == "beta",
          "https://{register}.register.gov.uk/register.json",
          "https://{register}.{phase}.openregister.org/register.json") %>%
  glue() %>%
  read_json() %>%
  .$`total-records`
}
# count_records("local-authority-eng", "beta")

get_all_records <- function(register, phase, page_size = 5000) {
  record_count <- count_records(register, phase)
  page_count <- ceiling(record_count / page_size)
  base_url <-
    if_else(phase == "beta",
            "https://{register}.register.gov.uk/records.tsv?page-size=5000&page-index={page_index}",
            "https://{register}.{phase}.openregister.org/records.tsv?page-size=5000&page-index={page_index}")
  map_df(seq_len(page_count),
         function(page_index) {
           base_url %>%
             glue() %>%
             read_tsv()
         }) %>%
  mutate(register = register, phase = phase) %>%
  select(register, phase, everything())
}
# get_all_records("local-authority-eng", "beta")

registers <-
  # Get all registers in all phases
  map2_df("register", c("beta", "alpha", "discovery"), get_all_records) %>%
  mutate(phase = factor(phase, levels = c("discovery", "alpha", "beta"))) %>%
  arrange(register, desc(phase)) %>%
  group_by(register) %>%
  filter(phase == first(phase)) %>%
  ungroup() %>%
  mutate(phase = as.character(phase)) %>%
  # Separate the fields from one another
  mutate(field = stri_split_fixed(fields, ";")) %>%
  select(register, phase, field) %>%
  print(n = Inf)

fields <-
  # Get all fields in all phases
  map2_df("field", c("beta", "alpha", "discovery"), get_all_records) %>%
  mutate(phase = factor(phase, levels = c("discovery", "alpha", "beta"))) %>%
  arrange(field, desc(phase)) %>%
  group_by(field) %>%
  filter(phase == first(phase)) %>%
  ungroup()

curies <-
  fields %>%
  filter(datatype == "curie") %>%
  select(phase, field) %>%
  arrange(phase, field) %>%
  print(n = Inf)

get_curies <- function(register, phase, field) {
  field <- sym(field)
  get_all_records(register, phase)  %>%
    select(!!field) %>%
    filter(!is.na(!!field)) %>%
    mutate(curie := stri_extract_first_regex(!!field, "^[^:]+")) %>%
    distinct(curie) %>%
    pull(curie)
}

registers_with_curies <-
  registers %>%
  unnest(field) %>%
  semi_join(curies, by = "field") %>%
  mutate(curie = pmap(list(register, phase, field), get_curies))

adoptions <-
  read_tsv(file.path(here(), "data", "adoption.tsv"),
           col_types = "cc")

adoption_nodes <-
  adoptions %>%
  distinct(adopter) %>%
  rename(id = adopter) %>%
  mutate(group = "adopter", label = id)

register_nodes <-
  registers %>%
  distinct(register, phase) %>%
  rename(id = register, group = phase) %>%
  mutate(label = id)

adoption_edges <-
  adoptions %>%
  rename(from = adopter, to = register) %>%
  mutate(arrows = "", dashes = FALSE)

key_edges <-
  registers %>%
  unnest(field) %>%
  filter(field == register) %>%
  select(field) %>%
  inner_join(registers %>%
             unnest(field) %>%
             filter(field != register) %>%
             select(field, register),
           by = "field") %>%
  rename(from = register, to = field) %>%
  mutate(arrows = "to",
         dashes = FALSE) %>%
  print(n = Inf)

curie_edges <-
  registers_with_curies %>%
  select(register, curie) %>%
  unnest(curie) %>%
  distinct() %>%
  rename(from = register, to = curie) %>%
  mutate(arrows = "to",
         dashes = TRUE) %>%
  print(n = Inf)

discovery_col <- "#005EA5"
alpha_col <- unname(gov_cols["pink"])
beta_col <- unname(gov_cols["orange"])
adoption_col <- unname(gov_cols["turquoise"])

plot_registers <- function() {
  nodes <- bind_rows(register_nodes)
  edges <- bind_rows(key_edges, curie_edges)
  legend_edges <-
    data_frame(label = c("link by key field", "link by curie"),
               dashes = c(FALSE, TRUE),
               arrows = "to")
  visNetwork(nodes, edges, main = "Linked Registers",
             width = "100%", height = "90vh") %>%
  visGroups(groupname = "beta", color = beta_col) %>%
  visGroups(groupname = "alpha", color = alpha_col) %>%
  visGroups(groupname = "discovery", color = discovery_col) %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE,
             manipulation = TRUE,
             selectedBy = "group")  %>%
  visLegend(addEdges = legend_edges) %>%
  visSave(file.path(here(), "registers.html"))
}

plot_adoptions <- function() {
  register_nodes <- filter(register_nodes, group == "beta")
  nodes <-
    register_nodes %>%
    filter(group == "beta") %>%
    bind_rows(adoption_nodes)
  edges <-
    bind_rows(key_edges, curie_edges) %>%
    semi_join(adoption_edges, by = c("from" = "to")) %>%
    bind_rows(adoption_edges) %>%
    arrange(to)
  legend_edges <-
    data_frame(label = c("adopter", "linked register"),
               dashes = FALSE,
               arrows = c("to"))
  visNetwork(nodes, edges, main = "Adoption of Registers",
             width = "100%", height = "90vh") %>%
  visGroups(groupname = "beta", color = beta_col) %>%
  visGroups(groupname = "adopter", color = adoption_col) %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE,
             manipulation = TRUE,
             selectedBy = "group")  %>%
  visLegend(addEdges = legend_edges) %>%
  visSave(file.path(here(), "adoptions.html"))
}
plot_registers()
plot_adoptions()
