library(dplyr)
library(edsRApi)
library(httr)
library(glue)
library(jsonlite)



project_id = "XXXXXXXXXXXXX"
conn <- edsRApi::connect("XXXXXXXXXXX", project_id, password = "XXXXXXX")

# Lire le fichier Excel
paths <- readxl::read_excel("C:\\Users\\savaame\\Desktop\\1000 premier\\xlsx\\path_eds_label.xlsx")


read_excel_paths <- function(keyword = NULL, size = 100) {
  edsRApi::connect("savaame", project_id, password = "Amelie.33650!")
  
  # keyword = df dans le tableau excel des paths
  # Recuperation de la liste des paths pour un keyword donne  
  paths_df <- paths %>%
    dplyr::filter(df==keyword)
  
  if (nrow(paths_df) == 0) {
    warning(paste("Aucune correspondance trouvée pour le mot-clé:", keyword))
    return(NULL)
  }
  
  # Grouper les chemins par df
  grouped_paths <- paths_df$path
  
  # Initialiser le tibble pour les résultats
  result_tibble <- dplyr::tibble(
    "providerId" = character(),
    "pseudonymPatientNum" = character(),
    "pseudonymEncounterNum" = character(),
    "startDate" = as.Date(character()),
    "labelPermissible" = character(),
    "labelDataElement" = character(),
    "nvalNum" = numeric(), 
    "observationBlob" = character()
  )
  
  for (path in as.character(grouped_paths)) {  # Récupération données pour chaque chemin
    
    observationFilter <- edsRApi::observation_filter_builder() %>%
      edsRApi::add_concept_path(values = path) %>%
      edsRApi::add_modifier_cd(values = c("@")) %>%
      edsRApi::build()
    
    # Recuperation du nombre de pages en fonction de size directement à partir du projet
    response <- httr::POST(
      url = glue("{conn$namespace}/services/edsobservationmicroservice/api/observations/pseudonym-group-context?page=0&size={size}&project_id={conn$cohort$projectId}&identity_pseudonym_group={conn$cohort$identityPseudonymGroup}&force_patient_level=FALSE&identity_status="),
      add_headers(Authorization = paste0("Bearer ", conn$token$access_token)),
      body = observationFilter,
      content_type("application/json"),
      encode = "json"
    )
    json <- content(response, as = "text", encoding = "UTF-8")
    json <- fromJSON(json)
    print(json$totalPages)
    
    for (page in 0:(json$totalPages - 1)) { 
      
      if (page %% 100 == 0) {
        conn <- edsRApi::connect("XXXXXXX", project_id, password = "XXXXXXX!")
        # se reconnecter tous les 100 pages
      }
      
      observations <- edsRApi::get_observations_by_observation_filter(
        conn = conn,
        observationFilter = observationFilter,
        size = size,
        fetch_all = FALSE,
        page = page
      )
      
      if (!is.null(observations) && "pseudonymPatientNum" %in% names(observations)) {
        result_tibble <- dplyr::bind_rows(
          result_tibble,
          observations 
        )
      }
    }
  }
  
  return(result_tibble)
}
