library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(httr2)
library(tibble)

# read xml feed
xml <- read_xml(
  "https://data.parliament.uk/membersdataplatform/open/OData.svc/Members?$filter=not%20MemberMaidenSpeeches/any()%20and%20House_Id%20eq%201%20and%20EndDate%20eq%20null%20and%20StartDate%20ge%20datetime%272024-07-04T00:00:00%27&$expand=MemberMaidenSpeeches&$select=NameDisplayAs"
)

# extract the XML namespaces so R knows how to interpret tag prefixes
ns <- xml_ns(xml)

# find all <entry> nodes (each MP is one entry)
# these live in the default Atom namespace, which xml2 labels "d1"
entries <- xml_find_all(xml, ".//d1:entry", ns)

# loop over each entry and extract ID + Name
mps <- map_df(entries, function(entry) {
  # extract the <id> element (contains the URL with the numeric Member ID)
  name <- xml_text(xml_find_first(entry, ".//d:NameDisplayAs", ns))

  # extract the <id> element (contains the URL with the numeric Member ID)
  id_url <- xml_text(xml_find_first(entry, ".//d1:id", ns))

  # pull out just the number inside Members(XXXX)
  id_num <- str_extract(id_url, "(?<=Members\\()\\d+(?=\\))")

  # return a row with ID and name
  tibble(id = id_num, name = name)
})

# extract list of IDs
ids <- maiden_speech_table$id

# function to call Members API
get_mp_data <- function(id) {
  url <- sprintf("https://members-api.parliament.uk/api/Members/%s", id)

  response <- request(url) |> 
    req_perform() |> 
    resp_body_json()
}

# apply get_mp_data for each MP ID
mp_data <- lapply(ids, get_mp_data)

# extract MPs party and member
party_const <- tibble(
  id    = map_chr(mp_data, ~ as.character(.x$value$id)),
  party = map_chr(mp_data, ~ .x$value$latestParty$name),
  constituency = map_chr(mp_data, ~ .x$value$latestHouseMembership$membershipFrom)
)

maiden_speech_table <- mps |> 
  left_join(party_const, by = c("id" = "id"))
