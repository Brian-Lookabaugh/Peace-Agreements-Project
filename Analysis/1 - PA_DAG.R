############################################################################
######################--------Creating the DAG--------######################
############################################################################

pacman::p_load(
  "tidyverse", # General Data Manipulation and Visualization
  "dagitty", # Creating DAGs
  "ggdag", # Plotting DAGs
  install = FALSE
)

# Creating the DAG
dag <- dagify(
  v ~ p_agg + w_dur + gov_cap + pop + dem + dev + pko,
  p_agg ~ gov_cap + w_dur + pko + eth_frac + med,
  med ~ w_dur + pko,
  w_dur ~ gov_cap,
  pko ~ gov_cap + w_dur + pop,
  dev ~ eth_frac + dem + pko + w_dur + pop,
  dem ~ eth_frac + pko + p_agg + pop,
  exposure = "p_agg",
  outcome = "v",
  coords = list(
    x = c(
      med = 1.5,
      p_agg = 2,
      v = 3,
      w_dur = 1.5,
      gov_cap = 3,
      pko = 2,
      eth_frac = 2,
      pop = 1,
      dem = 3,
      dev = 1
    ),
    y = c(
      med = 3,
      p_agg = 3,
      v = 3,
      w_dur = 1,
      gov_cap = 1,
      pko = 5,
      eth_frac = 2,
      pop = 5,
      dem = 4,
      dev = 4
    )
  ),
  labels = c(
    v = "Violence",
    p_agg = "Peace Agreement",
    w_dur = "War Duration",
    gov_cap = "Government Capacity",
    pop = "Population",
    dem = "Democracy",
    dev = "Development",
    pko = "PKO",
    eth_frac = "Ethnic Diversity",
    med = "Mediation"
  )
)

# Determine the Adjustment Set
adjustmentSets(dag)
ancestors(dag, "v")

# Converting DAG to a Tidy Object for Better Plotting
tidy_dag <- dag %>%
  tidy_dagitty() %>%
  node_status()

# Plotting the DAG
ggplot(tidy_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_dag() +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(aes(label = label, fill = status),
                       color = "white", fontface = "bold") +
  guides(color = "none", fill = "none")
