library("tidyverse")
library("sf")
library("magick")

source("00-helpers.R", encoding = "UTF-8")
source("02-read_data.R", encoding = "UTF-8")

# filter out data --------------------------------------------------------------

# the way the clb collects the data changed at the start of 2017: 
# to avoid any problem we'll only work with years from 2011 to 2016
df <- filter(df, date < as.Date("2017-01-01", "%Y-%m-%d"))

# format data ------------------------------------------------------------------

# add the year
df <- mutate(df, year = format(date, "%Y"))
years <- sort(unique(df[["year"]]))

# format it
df <- mutate(df, year_s = ifelse(year == years[1],
                                 year, paste0("´", substr(year, 3, 4))))
years_s <- c(years[1], paste0("´", substr(years[-1], 3, 4)))

# time series ------------------------------------------------------------------

# count the number of incidents by week
ts_week <- df %>%
  mutate(week = lubridate::week(date)) %>%
  mutate(week = ifelse(week %in% as.character(seq_len(9)),
                       paste0("0", week), week)) %>%
  unite(timespan, year, week, sep = "-", remove = TRUE) %>%
  group_by(timespan) %>%
  count() %>%
  ungroup() %>%
  mutate(id = "Par semaine")

# count the number of incidents by month
ts_month <- df %>%
  mutate(timespan = format(date, "%Y-%m")) %>%
  group_by(timespan) %>%
  count() %>%
  ungroup() %>%
  mutate(id = "Par mois")

ts <- bind_rows(ts_week, ts_month) %>%
  mutate(id = factor(id, levels = c("Par semaine", "Par mois")))

time_series <- ggplot(ts, aes(x = timespan, y = n)) +
  geom_line(aes(group = 1)) +
  scale_x_discrete(breaks = paste0(years[-1], "-01")) +
  facet_wrap(~ id, ncol = 3, scales = "free") +
  labs(title = paste("Nombre d'incidents répertoriés par",
                     "le China Labour Bulletin entre 2011 et 2016"),
       subtitle = paste("Note : l'échelle des ordonnées est différente",
                        "pour chaque graphique"),
       x = "", y = "",
       caption = "données : http://www.clb.org.hk/") +
  theme_bw_2()

time_series

# univariate and "by year" plots -----------------------------------------------

# ---- year ----

count(df, year, sort = TRUE)

year_lollipop <- draw_lollipop(
  df,
  var = "year",
  title = paste("Après avoir fortement augmenté entre 2011 et 2015,",
                "le nombre d'incidents a été stable en 2016"),
  subtitle = paste("Évolution du nombre d'incidents répertoriés",
                   "par le China Labour Bulletin entre 2011 et 2016"),
  caption = "données : http://www.clb.org.hk/",
  panel.grid.major.y = element_blank()
) +
  scale_x_discrete(limits = as.character(rev(unique(df[["year"]]))))

year_lollipop

# ---- province ----

count(df, province, sort = TRUE)

df_province <- mutate(df, province = ifelse(province == 0, NA, province))

province_lollipop <- draw_lollipop(
  df_province,
  var = "province",
  title = paste("La région de Guangdong (Canton) concentre",
                "la grande majorité des incidents"),
  subtitle = paste("Nombre d'incidents répertoriés par région",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/",
  panel.grid.major.y = element_blank()
)

province_lollipop

province_multilineplot <- draw_multilineplot(
  df = filter(df_province, !(is.na(province) | province == "Tibet")), 
  var = "province",
  by_var = "year_s",
  title = paste("Si le nombre d'incidents a diminué en 2016 dans la région",
                "de Guangdong, la tendance à la hausse persiste pour",
                "un certain nombre d'autres régions"),
  subtitle = paste("Évolution du nombre d'incidents répertoriés par région",
                   "entre 2011 et 2016 par le China Labour Bulletin"),
  ncol = 6,
  caption = "données : http://www.clb.org.hk/"
)

province_multilineplot

# ---- industry ----

count(df, industry, sort = TRUE)

industry_lollipop <- draw_lollipop(
  df,
  var = "industry",
  title = paste("Production et bâtiment sont les deux secteurs",
                "où le nombre d'incidents est le plus important"),
  subtitle = paste("Nombre d'incidents répertoriés par type d'industrie",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/",
  panel.grid.major.y = element_blank()
)

industry_lollipop

industry_multilineplot <- draw_multilineplot(
  df,
  var = "industry",
  by_var = "year_s",
  title = paste("Si le nombre d'incidents a fortement baissé dans le secteur",
                "de la production en 2016, il a continué à augmenter dans",
                "quasiment tous les autres secteurs"),
  subtitle = paste("Évolution du nombre d'incidents répertoriés par type",
                   "d'industrie entre 2011 et 2016",
                   "par le China Labour Bulletin"),
  ncol = 3,
  caption = "données : http://www.clb.org.hk/"
)

industry_multilineplot

# ---- participants ----

count(df, participants, sort = TRUE)

# remove the " Persons" suffix for aesthetic purposes
df_participants <- mutate(df, participants = gsub(" Persons", "", participants))

participants_lollipop <- draw_lollipop(
  df_participants,
  var = "participants",
  title = paste("La majorité des incidents impliquent des groupes",
                "de moins de 100 personnes"),
  subtitle = paste("Nombre d'incidents répertoriés par nombre de participants",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/",
  panel.grid.major.y = element_blank()
)

participants_lollipop

participants_multilineplot <- draw_multilineplot(
  df_participants,
  var = "participants",
  by_var = "year_s",
  ncol = 3,
  title = paste("La forte croissance du nombre d'incidents entre 2011 et 2016",
                "s'explique par celle des incidents impliquant 100 personnes",
                "ou moins"),
  subtitle = paste("Évolution du nombre d'incidents répertoriés par nombre",
                   "de participants entre 2011 et 2016",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/"
)

participants_multilineplot

# ---- employer ----

count(df, employer, sort = TRUE)

# group together "unknown" and NA
df_employer <- mutate(df, employer = ifelse(employer == "unknown", NA, employer)) %>%
  mutate(employer = str_to_title(employer))

# keep only the first twelve employers
df_employer <- right_join(
  df_employer,
  df_employer %>%
    filter(!is.na(employer)) %>%
    count(employer, sort = TRUE) %>%
    select(employer) %>%
    slice(1:12),
  by = "employer"
)

employer_lollipop <- draw_lollipop(
  df_employer,
  var = "employer",
  title = paste("Plusieurs entreprises ont connu des incidents",
                "à de multiples reprises entre 2011 et 2016"),
  subtitle = paste("Nombre d'incidents répertoriés par entreprise",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/",
  panel.grid.major.y = element_blank()
)

employer_lollipop

# ---- enterprise type ----

count(df, enterprise_type, sort = TRUE)

# group together "Unknown" and NA
df <- mutate(df, enterprise_type = ifelse(is.na(enterprise_type),
                                          "Unknown",
                                          enterprise_type))

enterprise_type_lollipop <- draw_lollipop(
  df,
  var = "enterprise_type",
  title = paste("Les entreprises privées sont les plus sujettes",
                "aux incidents"),
  subtitle = paste("Nombre d'incidents répertoriés par type d'entreprise",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/",
  panel.grid.major.y = element_blank()
)

enterprise_type_lollipop

enterprise_type_multilineplot <- draw_multilineplot(
  df,
  var = "enterprise_type",
  by_var = "year_s",
  ncol = 3,
  title = paste("Le nombre d'incidents a continué à légèrement augmenter",
                "en 2016 pour les entreprises d'État"),
  subtitle = paste("Évolution du nombre d'incidents répertoriés",
                   "par type d'entreprise entre 2011 et 2016",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/"
)

enterprise_type_multilineplot

# ---- action taken involve ----

count(df, action_taken_involve, sort = TRUE)

# clean and regroup the listed actions
df_action <- df %>%
  select(action_taken_involve, year_s) %>%
  mutate(action_taken_involve = tolower(action_taken_involve)) %>%
  mutate(block_road = ifelse(
    grepl("block road", action_taken_involve), 1L, 0L)
  ) %>%
  mutate(protest_or_demonstration = ifelse(
    grepl("protest or demonstration", action_taken_involve), 1L, 0L)
  ) %>%
  mutate(sit_in = ifelse(
    grepl("sit-in", action_taken_involve), 1L, 0L)
  ) %>%
  mutate(strike = ifelse(
    grepl("strike", action_taken_involve), 1L, 0L)
  ) %>%
  mutate(threaten_to_jump = ifelse(
    grepl("threaten to jump", action_taken_involve), 1L, 0L)
  ) %>%
  mutate(other = ifelse(block_road + protest_or_demonstration + sit_in +
                          strike + threaten_to_jump == 0L, 1L, 0L)) %>%
  select(-action_taken_involve) %>%
  gather(key = action_taken_involve, value = count, -year_s) %>%
  filter(count != 0)

# rename the labels
df_action_labels <- data_frame(
  action_taken_involve = c("block_road",
                           "other",
                           "protest_or_demonstration",
                           "sit_in",
                           "strike",
                           "threaten_to_jump"),
  action_taken_involve_labels = c("Block Road",
                                  "Other",
                                  "Protest or Demonstration",
                                  "Sit-in",
                                  "Strike", 
                                  "Threaten to Jump Off a Building")
)

df_action <- left_join(df_action, df_action_labels, by = "action_taken_involve")

count(df_action, action_taken_involve_labels, sort = TRUE)

action_taken_involve_lollipop <- draw_lollipop(
  df_action,
  var = "action_taken_involve_labels",
  title = paste("La grande majorité des incidents se traduit",
                "par des manifestations, sit-in ou grèves"),
  subtitle = paste("Nombre d'incidents répertoriés par type d'action(s)",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/",
  panel.grid.major.y = element_blank()
)

action_taken_involve_lollipop

action_taken_involve_multilineplot <- draw_multilineplot(
  df_action,
  var = "action_taken_involve_labels",
  by_var = "year_s",
  ncol = 3,
  title = "Le nombre de sit-in a fortement décliné en 2016",
  subtitle = paste("Évolution du nombre d'incidents répertoriés",
                   "par type d'action(s) entre 2011 et 2016",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/"
)

action_taken_involve_multilineplot

# ---- employee demands involve ----

count(df, employee_demands_involve, sort = TRUE)

# clean and regroup the listed demands
df_employee_demands <- df %>%
  select(employee_demands_involve, year_s) %>%
  mutate(employee_demands_involve = tolower(employee_demands_involve)) %>%
  mutate(bonus = ifelse(
    grepl("bonus", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(compensation = ifelse(
    grepl("compensation", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(closure = ifelse(
    grepl("closure", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(fees = ifelse(
    grepl("fees", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(illegal_cabs = ifelse(
    grepl("illegal cab", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(layoffs = ifelse(
    grepl("layoffs", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(management = ifelse(
    grepl("management", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(pay_increase = ifelse(
    grepl("pay increase", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(pension = ifelse(
    grepl("pension", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(relocation = ifelse(
    grepl("relocation", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(restructure_merger = ifelse(
    grepl("restructure merger", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(rickshaws = ifelse(
    grepl("rickshaws", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(ride_app = ifelse(
    grepl("ride app", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(social_security = ifelse(
    grepl("social security", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(unlicensed_taxi = ifelse(
    grepl("unlicensed taxi", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(wage_arrears = ifelse(
    grepl("wage arrear", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(work_conditions = ifelse(
    grepl("work condition", employee_demands_involve), 1L, 0L)
  ) %>%
  mutate(other = ifelse(
    bonus + compensation + fees + illegal_cabs + layoffs + management +
      pay_increase + pension + relocation + restructure_merger + rickshaws +
      ride_app + social_security + unlicensed_taxi + wage_arrears +
      work_conditions == 0L, 1L, 0L)) %>%
  select(-employee_demands_involve) %>%
  gather(key = employee_demands_involve, value = count, -year_s) %>%
  filter(count != 0)

# rename the labels
df_employee_demands_labels <- data_frame(
  employee_demands_involve = c("bonus",
                               "closure",
                               "compensation",
                               "fees",
                               "illegal_cabs",
                               "layoffs",
                               "management",
                               "other",
                               "pay_increase",
                               "pension",
                               "relocation",
                               "restructure_merger",
                               "rickshaws",
                               "ride_app",
                               "social_security",
                               "unlicensed_taxi",
                               "wage_arrears",
                               "work_conditions"),
  employee_demands_involve_labels = c("Bonus",
                                      "Closure",
                                      "Compensations",
                                      "Fees",
                                      "Illegal Cabs",
                                      "Layoffs",
                                      "Management",
                                      "Other",
                                      "Pay Increase",
                                      "Pension",
                                      "Relocation",
                                      "Restructure & Merger",
                                      "Rickshaws",
                                      "Ride App",
                                      "Social Security",
                                      "Unlicensed Taxi",
                                      "Wage Arrears",
                                      "Work Conditions")
)

df_employee_demands <- left_join(df_employee_demands,
                                 df_employee_demands_labels,
                                 by = "employee_demands_involve")

count(df_employee_demands, employee_demands_involve_labels, sort = TRUE)

employee_demands_involve_lollipop <- draw_lollipop(
  df_employee_demands,
  var = "employee_demands_involve_labels",
  title = "La quasi totalité des demandes concerne des arriérés de salaires",
  subtitle = paste("Nombre d'incidents répertoriés par type de demande(s)",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/",
  panel.grid.major.y = element_blank()
)

employee_demands_involve_lollipop

employee_demands_involve_multilineplot <- draw_multilineplot(
  df_employee_demands,
  var = "employee_demands_involve_labels",
  by_var = "year_s",
  ncol = 6,
  title = paste("Si toujours très majoritaire, le nombre de demandes",
                "d'arriérés de salaires a stagné en 2016 après deux années",
                "de très forte croissance"),
  subtitle = paste("Évolution du nombre d'incidents répertoriés",
                   "par type de demande(s) entre 2011 et 2016",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/"
)

employee_demands_involve_multilineplot

# ---- response to collective action ----

count(df, response_to_collective_action, sort = TRUE)

# clean and regroup the listed demands
df_response <- df %>%
  select(response_to_collective_action, year_s) %>%
  mutate(response_to_collective_action =
           tolower(response_to_collective_action)) %>%
  mutate(arrest = ifelse(
    grepl("arrest", response_to_collective_action), 1L, 0L)
  ) %>%
  mutate(beaten = ifelse(
    grepl("beaten", response_to_collective_action), 1L, 0L)
  ) %>%
  mutate(government_mediation = ifelse(
    grepl("government mediation", response_to_collective_action), 1L, 0L)
  ) %>%
  mutate(negotiation = ifelse(
    grepl("negotiation", response_to_collective_action), 1L, 0L)
  ) %>%
  mutate(police = ifelse(
    grepl("police", response_to_collective_action), 1L, 0L)
  ) %>%
  mutate(unknown = ifelse(
    is.na(response_to_collective_action), 1L, 0L)
  ) %>%
  mutate(other = ifelse(
    arrest + beaten + government_mediation + negotiation +
      police + unknown == 0L, 1L, 0L)) %>%
  select(-response_to_collective_action) %>%
  gather(key = response_to_collective_action, value = count, -year_s) %>%
  filter(count != 0)

# rename the labels
df_response_labels <- data_frame(
  response_to_collective_action = c("arrest",
                                    "beaten",
                                    "government_mediation",
                                    "negotiation",
                                    "other",
                                    "police",
                                    "unknown"),
  response_to_collective_action_labels = c("Arrest(s)",
                                           "Beaten",
                                           "Government Mediation",
                                           "Negotiation",
                                           "Other",
                                           "Police",
                                           "Unknown")
)

df_response <- left_join(df_response, df_response_labels,
                         by = "response_to_collective_action")

count(df_response, response_to_collective_action_labels, sort = TRUE)

response_to_collective_action_lollipop <- draw_lollipop(
  df_response,
  var = "response_to_collective_action_labels",
  title = paste("La réponse apportée aux demandes des salariés",
                "est généralement policière lorsque connue"),
  subtitle = paste("Nombre d'incidents répertoriés par type de réponse(s)",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/",
  panel.grid.major.y = element_blank()
)

response_to_collective_action_lollipop

response_to_collective_action_multilineplot <- draw_multilineplot(
  df_response,
  var = "response_to_collective_action_labels",
  by_var = "year_s",
  ncol = 4,
  title = paste("Peu de différences tendancielles sont observables entre",
                "les principales réponses apportées"),
  subtitle = paste("Évolution du nombre d'incidents répertoriés par",
                   "type de réponse(s) entre 2011 et 2016",
                   "par le China Labour Bulletin"),
  caption = "données : http://www.clb.org.hk/"
)

response_to_collective_action_multilineplot

# maps -------------------------------------------------------------------------

# load a shapefile of the administrative regions of china
sf_china <- read_sf("data/shapefiles/CHN_adm1.shp")

# change the CRS to web mercator (EPSG:3857)
sf_china <- st_transform(sf_china, "+init=epsg:3857")

# load a dataset with the population per administrative region in 2010
pop_2010 <- read_tsv("data/province_population_2010.csv")

# rename a few provinces to match the datasets and join
sf_china <- sf_china %>%
  mutate(province = case_when(
    NAME_1 == "Nei Mongol" ~ "Inner Mongolia",
    NAME_1 == "Ningxia Hui" ~ "Ningxia",
    NAME_1 == "Xizang" ~ "Tibet",
    NAME_1 == "Xinjiang Uygur" ~ "Xinjiang",
    TRUE ~ NAME_1
  )) %>%
  left_join(pop_2010, by = "province")

ggplot(data = sf_china, aes(fill = population)) +
  geom_sf(color = "grey99", size = 0.15) +
  viridis::scale_fill_viridis(name = "Population",
                              na.value = "grey85",
                              labels = scales::format_format(scientific = FALSE,
                                                             big.mark = " ")) +
  labs(title = paste("Population par région administrative en 2010"),
       x = "", y = "",
       caption = "données : National Bureau of Statistics of China") +
  theme_bw_2(base_size = 20) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_line(color = "#ffffff", size = 0))

# get a count of the number of incidents by province for each year
count_province_year <- df %>%
  filter(province != 0) %>%
  group_by(year) %>%
  count(province) %>%
  ungroup()

# join the counts and the shapefile and compute the number of incidents
# per 1M habitants
df_map_year <- data_frame(
  province = rep(unique(count_province_year[["province"]]),
                 each = length(unique(count_province_year[["year"]]))),
  year = rep(unique(count_province_year[["year"]]),
             length(unique(count_province_year[["province"]])))
) %>%
  left_join(count_province_year, by = c("province", "year")) %>%
  left_join(sf_china, by = "province") %>%
  mutate(n_1M = n / (population / 1000000))

# draw a map for each year
map_china <- function(df, metric, filter_year, title,
                      subtitle = "", legend, caption = "") {
  metric_sym <- rlang::sym(metric)
  
  # compute the min and the max for the given metric
  min_max <- df_map_year %>%
    filter(!is.na(!!metric_sym)) %>%
    arrange(!!metric_sym) %>%
    slice(c(1, n())) %>%
    pull(!!metric_sym)
  
  df %>%
    filter(year == filter_year) %>%
    ggplot(aes_string(fill = metric)) +
    geom_sf(color = "grey99", size = 0.15) +
    viridis::scale_fill_viridis(name = legend,
                                limits = min_max,
                                na.value = "grey85") +
    annotate("text", label = filter_year, x = 14500000, y = 2500000,
             color = "grey25", size = 28, family = "Roboto Condensed") +
    labs(title = title, subtitle = subtitle,
         x = "", y = "",
         caption = caption) +
    theme_bw_2(base_size = 20) +
    theme(axis.text = element_blank(),
          panel.grid.major = element_line(color = "#ffffff", size = 0))
}

maps <- map(unique(count_province_year[["year"]]),
            ~ map_china(
              df = df_map_year, metric = "n", filter_year = .x,
              title =
                paste("Répartition du nombre d'incidents répertoriés",
                      "par région administrative par le China Labour Bulletin"),
              legend = "Nombre d'incidents",
              caption = "données : http://www.clb.org.hk/"))
names(maps) <- unique(count_province_year[["year"]])
maps

# walk(names(maps), ~ ggsave(paste0("plots/map_", .x, ".png"), maps[[.x]],
#                            width = 16, height = 9, dpi = 120))

maps_1M <- map(unique(count_province_year[["year"]]),
               ~ map_china(
                 df = df_map_year, metric = "n_1M", filter_year = .x,
                 title = paste("Répartition du nombre d'incidents répertoriés",
                               "par région administrative",
                               "par le China Labour Bulletin"),
                 legend = "Nombre d'incidents\npar million d'habitants",
                 caption = paste("données : National Bureau of Statistics",
                                 "of China - http://www.clb.org.hk/")))
names(maps_1M) <- unique(count_province_year[["year"]])
maps_1M

# walk(names(maps_1M), ~ ggsave(paste0("plots/map_1M_", .x, ".png"),
#                               maps_1M[[.x]],
#                               width = 16, height = 9, dpi = 120))

# create a gif - warning: this is really slow!
# img <- image_graph(width = 1920, height = 1080, res = 96)
# maps <- map(unique(count_province_year[["year"]]),
#             ~ print(map_china(
#               df = df_map_year, metric = "n", filter_year = .x,
#               title =
#                 paste("Répartition du nombre d'incidents répertoriés",
#                       "par région administrative par le China Labour Bulletin"),
#               legend = "Nombre d'incidents",
#               caption = "données : http://www.clb.org.hk/")))
# dev.off()
# 
# animation <- image_animate(img, 1)
# image_write(animation, "plots/maps.gif")
