library("tidyverse")
library("sf")
library("magick")

source("00-helpers.R", encoding = "UTF-8")
source("02-read_data.R", encoding = "UTF-8")

glimpse(df)

# add the year and format it
df <- mutate(df, year = format(date, "%Y")) %>%
  mutate(year_s = ifelse(year == "2011", year, paste0("´", substr(year, 3, 4))))

# ---- year ----

count(df, year, sort = TRUE)

draw_lollipop(df, var = "year",
              title = paste("En forte augmentation entre 2011 et 2015,",
                            "le nombre d'incidents diminue depuis"),
              subtitle = paste("Évolution du nombre d'incidents répertoriés",
                               "par le China Labour Bulletin",
                               "entre 2011 et 2017"),
              caption = "données : http://www.clb.org.hk/") +
  scale_x_discrete(limits = as.character(rev(2011:2017)))

# ---- province ----

count(df, province, sort = TRUE)

df_province <- mutate(df, province = ifelse(province == 0, NA, province))

draw_lollipop(df_province, var = "province",
              title = paste("La province de Guangdong (Canton) concentre",
                            "la grande majorité des incidents"),
              subtitle = paste("Nombre d'incidents répertoriés par province",
                               "par le China Labour Bulletin"),
              caption = "données : http://www.clb.org.hk/")

draw_multilineplot(df = filter(df_province,
                               !(is.na(province) | province == "Tibet")), 
                   var = "province",
                   by_var = "year_s",
                   title = paste("L'évolution du nombre d'incidents suit une",
                                 "trajectoire plutôt semblable dans l'ensemble",
                                 "des provinces"),
                   subtitle = paste("Évolution du nombre d'incidents", 
                                    "répertoriés par province entre 2011",
                                    "et 2017 par le China Labour Bulletin"),
                   ncol = 6,
                   caption = "données : http://www.clb.org.hk/")

# ---- industry ----

count(df, enterprise_type, sort = TRUE)

draw_lollipop(df, var = "industry",
              title = paste("Bâtiment et production sont les deux secteurs où",
                            "le nombre d'incidents est le plus important"),
              subtitle = paste("Nombre d'incidents répertoriés par type",
                               "d'industrie par le China Labour Bulletin"),
              caption = "données : http://www.clb.org.hk/")

draw_multilineplot(df,
                   var = "industry",
                   by_var = "year_s",
                   title = paste("La baisse manifeste du nombre d'incidents",
                                 "en 2017 est observable dans l'ensemble",
                                 "des secteurs"),
                   subtitle = paste("Évolution du nombre d'incidents",
                                    "répertoriés par type d'industrie",
                                    "entre 2011 et 2017",
                                    "par le China Labour Bulletin"),
                   ncol = 3,
                   caption = "données : http://www.clb.org.hk/")

# ---- participants ----

count(df, participants, sort = TRUE)

# remove the " Persons" suffix for aesthetic purposes
df_participants <- mutate(df, participants = gsub(" Persons", "", participants))

draw_lollipop(df_participants,
              var = "participants",
              title = paste("La majorité des incidents impliquent des groupes",
                            "de moins de 100 personnes"),
              subtitle = paste("Nombre d'incidents répertoriés",
                               "par nombre de participants",
                               "par le China Labour Bulletin"),
              caption = "données : http://www.clb.org.hk/")

draw_multilineplot(df_participants,
                   var = "participants",
                   by_var = "year_s",
                   ncol = 3,
                   title = paste("Aucun incident impliquant plus de 10 000",
                                 "personnes n'a été répertorié lors des trois",
                                 "dernières années"),
                   subtitle = paste("Évolution du nombre d'incidents",
                                    "répertoriés par nombre de participants",
                                    "entre 2011 et 2017",
                                    "par le China Labour Bulletin"),
                   caption = "données : http://www.clb.org.hk/")

# ---- employer ----

count(df, employer, sort = TRUE)

# group together "unknown" and NA
df <- mutate(df, employer = ifelse(employer == "unknown", NA, employer))

# keep only the first twelve employers
df_employer <- right_join(
  df,
  df %>%
    filter(!is.na(employer)) %>%
    count(employer, sort = TRUE) %>%
    select(employer) %>%
    slice(1:12),
  by = "employer"
)

draw_lollipop(df_employer,
              var = "employer",
              title = paste("Certaines entreprises ont connu plusieurs",
                            "incidents entre 2011 et 2017"),
              subtitle = paste("Nombre d'incidents répertoriés",
                               "par entreprise par le China Labour Bulletin"),
              caption = "données : http://www.clb.org.hk/")

# ---- enterprise_type ----

count(df, enterprise_type, sort = TRUE)

# group together "Unknown" and NA
df <- mutate(df, enterprise_type = ifelse(is.na(enterprise_type),
                                          "Unknown",
                                          enterprise_type))

draw_lollipop(df, var = "enterprise_type",
              title = paste("Les entreprises privées sont les plus sujettes",
                            "aux incidents"),
              subtitle = paste("Nombre d'incidents répertoriés",
                               "par type d'entreprise",
                               "par le China Labour Bulletin"),
              caption = "données : http://www.clb.org.hk/")

draw_multilineplot(df,
                   var = "enterprise_type",
                   by_var = "year_s",
                   ncol = 3,
                   title = paste("La baisse des incidents sur les",
                                 "années 2016 et 2017 est manifeste",
                                 "pour l'ensemble des types d'entreprise"),
                   subtitle = paste("Évolution du nombre d'incidents",
                                    "répertoriés par type d'entreprise",
                                    "entre 2011 et 2017",
                                    "par le China Labour Bulletin"),
                   caption = "données : http://www.clb.org.hk/")

# ---- action_taken_involve ----

count(df, action_taken_involve, sort = TRUE)

# keep only the first twelve actions
df_action <- right_join(
  df,
  df %>%
    count(action_taken_involve, sort = TRUE) %>%
    select(action_taken_involve) %>%
    slice(1:12),
  by = "action_taken_involve"
)

draw_lollipop(df_action, var = "action_taken_involve",
              title = paste("La très grande majorité des incidents",
                            "se traduisent par des manifestations, sit-in",
                            "ou grèves"),
              subtitle = paste("Nombre d'incidents répertoriés",
                               "par type d'action(s)",
                               "par le China Labour Bulletin"),
              caption = "données : http://www.clb.org.hk/")

draw_multilineplot(df_action,
                   var = "action_taken_involve",
                   by_var = "year_s",
                   ncol = 4,
                   title = paste("Peu de différences tendancielles sont",
                                 "observables entre les principaux types",
                                 "d'actions"),
                   subtitle = paste("Évolution du nombre d'incidents",
                                    "répertoriés par type d'action(s)",
                                    "entre 2011 et 2017",
                                    "par le China Labour Bulletin"),
                   caption = "données : http://www.clb.org.hk/")

# ---- employee_demands_involve ----

count(df, employee_demands_involve, sort = TRUE)

# keep only the first twelve demands
df_employee_demands <- right_join(
  df,
  df %>%
    count(employee_demands_involve, sort = TRUE) %>%
    select(employee_demands_involve) %>%
    slice(1:12),
  by = "employee_demands_involve"
)

draw_lollipop(df_employee_demands, var = "employee_demands_involve",
              title = paste("La quasi totalité des demandes concernent",
                            "des arriérés de salaires"),
              subtitle = paste("Nombre d'incidents répertoriés",
                               "par type de demande(s)",
                               "par le China Labour Bulletin"),
              caption = "données : http://www.clb.org.hk/")

draw_multilineplot(df_employee_demands,
                   var = "employee_demands_involve",
                   by_var = "year_s",
                   ncol = 4,
                   title = paste("Après cinq années de croissance, le nombre",
                                 "de demandes d'arriérés de salaires à",
                                 "significativement diminué en 2016 et 2017"),
                   subtitle = paste("Évolution du nombre d'incidents",
                                    "répertoriés par type de demande(s)",
                                    "entre 2011 et 2017",
                                    "par le China Labour Bulletin"),
                   caption = "données : http://www.clb.org.hk/")

# ---- response_to_collective_action ----

count(df, response_to_collective_action, sort = TRUE)

# keep only the first twelve responses
df_response <- right_join(
  df,
  df %>%
  count(response_to_collective_action, sort = TRUE) %>%
    select(response_to_collective_action) %>%
    slice(1:12),
  by = "response_to_collective_action"
)

draw_lollipop(df_response, var = "response_to_collective_action",
              title = paste("La réponse apportée aux demandes est",
                            "très régulièrement inconnue"),
              subtitle = paste("Nombre d'incidents répertoriés",
                               "par type de réponse(s)",
                               "par le China Labour Bulletin"),
              caption = "données : http://www.clb.org.hk/")

draw_multilineplot(df_response,
                   var = "response_to_collective_action",
                   by_var = "year_s",
                   ncol = 4,
                   title = paste("Peu de différences tendancielles sont",
                                 "observables entre les principaux types",
                                 "de réponses apportées"),
                   subtitle = paste("Évolution du nombre d'incidents",
                                    "répertoriés par type de réponse(s)",
                                    "entre 2011 et 2017",
                                    "par le China Labour Bulletin"),
                   caption = "données : http://www.clb.org.hk/")

# ---- maps ----

# load a shapefile of the administrative regions of china
china <- read_sf("data/shapefiles/CHN_adm1.shp")

# change the CRS to web mercator (EPSG:3857)
china <- st_transform(china, "+init=epsg:3857")

# get a count of the number of incidents by province for each year
count_province_year <- df %>%
  filter(province != 0) %>%
  group_by(year) %>%
  count(province) %>%
  ungroup()

# rename a few provinces to match the two datasets and join the counts and
# the shapefile
df_map_year <- data_frame(
  province = rep(unique(count_province_year[["province"]]),
                 each = length(unique(count_province_year[["year"]]))),
  year = rep(unique(count_province_year[["year"]]),
             length(unique(count_province_year[["province"]])))
) %>%
  mutate(NAME_1 = case_when(
    province == "Inner Mongolia" ~ "Nei Mongol",
    province == "Ningxia" ~ "Ningxia Hui",
    province == "Tibet" ~ "Xizang",
    province == "Xinjiang" ~ "Xinjiang Uygur",
    TRUE ~ province
  )) %>%
  left_join(count_province_year, by = c("province", "year")) %>%
  left_join(china, by = "NAME_1")

# compute the min and the max of the number of incidents
min_max <- df_map_year %>%
  filter(!is.na(n)) %>%
  arrange(n) %>%
  slice(c(1, n())) %>%
  pull(n)

# draw a map for each year 
map_china <- function(df, filter_year, limits_scale = min_max) {
  df %>%
    filter(year == filter_year) %>%
    ggplot(aes(fill = n)) +
    geom_sf(color = "grey99", size = 0.15) +
    viridis::scale_fill_viridis(name = "Nombre d'incidents",
                                limits = limits_scale,
                                na.value = "grey85") +
    annotate("text", label = filter_year, x = 14500000, y = 2500000,
             color = "grey25", size = 28) +
    labs(title = paste("Répartition du nombre d'incidents répertoriés",
                       "par province par le China Labour Bulletin"),
         x = "", y = "") +
    theme_bw_2(base_size = 20) +
    theme(axis.text = element_blank(),
          panel.grid.major = element_line(color = "#ffffff", size = 0))
}

maps <- map(unique(count_province_year[["year"]]),
            ~ map_china(df = df_map_year, filter_year = .x))
names(maps) <- unique(count_province_year[["year"]])

# map(names(maps), ~ ggsave(paste0("plots/map_", .x, ".png"), maps[[.x]],
#                           width = 16, height = 9, dpi = 120))

# create a gif
img <- image_graph(width = 1920, height = 1080, res = 96)
maps <- map(unique(count_province_year[["year"]]),
            ~ print(map_china(df = df_map_year, filter_year = .x)))
dev.off()

animation <- image_animate(img, 1)
image_write(animation, "plots/maps.gif")
