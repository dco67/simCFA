source("global.R")

ui <- page_fluid(
  theme = bs_theme(
    # Couleurs principales
    primary = "#2C3E50",    # Bleu foncé pour les éléments principaux
    secondary = "#95A5A6",  # Gris pour les éléments secondaires
    success = "#18BC9C",    # Vert teal pour les succès
    info = "#3498DB",       # Bleu clair pour les informations
    warning = "#F39C12",    # Orange pour les avertissements
    danger = "#E74C3C",     # Rouge pour les erreurs
    
    # Personnalisation supplémentaire
    base_font = "system-ui",
    heading_font = "system-ui",
    font_scale = 1.0,       # Réduire légèrement la taille de police globale
    
    # Ajuster l'apparence des cartes
    card_bg = "#FFFFFF",
    card_cap_bg = "#F7F7F7",
    
    # Arrondir les coins des éléments
    border_radius = "0.5rem",
    
    # Espacement global
    spacer = "1rem"
  ),
  useShinyjs(),
  tags$head(tags$style(HTML(".dataTables_wrapper, .dt-container {max-width:100%; width:fit-content;} table.dataTable {width:auto !important; margin-left:0 !important;} table.dataTable th, table.dataTable td {padding:6px 12px; vertical-align:top;} table.dataTable th {white-space:normal; min-width:4.5ch;} table.dataTable td {max-width:34rem; overflow-wrap:anywhere;} .dataTables_scrollBody, .dt-scroll-body {max-width:100%;} .selectize-input {height:auto !important;} .bslib-card .card-body {overflow:visible;} pre {white-space:pre-wrap; overflow-wrap:anywhere;} .cfa-status {margin:12px 0;}"))),
  title = "Analyse Factorielle Confirmatoire",
  # Ajouter un style CSS personnalisé
  tags$head(
    tags$style(HTML("
      
      /* Style pour améliorer l'apparence des entrées de formulaire */
      .form-control:focus {
        border-color: #2C3E50;
        box-shadow: 0 0 0 0.2rem rgba(44, 62, 80, 0.25);
      }
      
      /* Style pour améliorer les cartes */
      .card {
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        transition: box-shadow 0.3s ease;
      }
      
      .card:hover {
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
      }
      
      /* Amélioration des boutons */
      .btn-primary {
        text-transform: uppercase;
        font-weight: 500;
        letter-spacing: 0.5px;
      }
      
      /* Style pour la barre de progression */
      .progress {
        height: 0.8rem;
        border-radius: 0.4rem;
      }
      
      /* fileInput : séparer le bouton et la case du nom de fichier */
      .shiny-input-container .input-group {
        display: flex;
        flex-wrap: nowrap;
        gap: 8px;
      }
      .shiny-input-container .input-group .btn-file,
      .shiny-input-container .input-group-btn .btn {
        border-radius: 0.4rem !important;
      }
      .shiny-input-container .input-group > .form-control {
        border-radius: 0.4rem !important;
        border-left-width: 1px !important;
        margin-left: 0 !important;
      }
      .shiny-input-container .progress {
        margin-top: 6px;
      }
        ",
        css_standardization  # Ajouter notre standardisation CSS
    )
  )),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="cfa-layout.css")),
  div(class="border rounded p-3 mb-3",
    radioButtons("interface_mode","Affichage",choices=c("Parcours guidé"="guided","Options avancées"="advanced"),selected="guided",inline=TRUE),
    div(class="d-flex flex-wrap gap-2",
      actionButton("step_source","1. Choisir les données"),
      actionButton("step_model","2. Définir le modèle"),
      actionButton("step_estimate","3. Estimer"),
      actionButton("step_interpret","4. Interpréter")),
    uiOutput("active_constraints_notice"),
    helpText("Commencez par les données, associez les indicateurs aux facteurs, estimez le modèle puis lisez la synthèse. Les options avancées donnent accès aux contraintes, aux comparaisons et au code.")),
  # ── Écran 1 : Configuration ──
  conditionalPanel(
    condition = "!output.show_results && !output.show_interpretation",
    card(
      class = "cfa-configuration", fill = FALSE,
      card_header(
        class = "bg-primary text-white",
        h4("Configuration de l'analyse", class = "m-0")
      ),
      card_body(
        fillable = FALSE, fill = FALSE, gap = 3,
        card(
          class = "shadow-sm cfa-config-inner", fill = FALSE,
          card_header(class = "bg-light", "Paramètres de l'AFC"),
          card_body(fillable = FALSE, fill = FALSE,
            navset_card_tab(id="config_nav", wrapper=function(...) card_body(...,fillable=FALSE,fill=FALSE),
              nav_panel(
                title = "Introduction",value="introduction",
                class = "p-3",
                
                navset_card_tab(
                  
                  # Onglet 1 - Vue d'ensemble
                  nav_panel(
                    title = "Vue d'ensemble",
                    
                    # En-tête principal
                    div(
                      class = "text-center mb-4",
                      h2("Analyse Factorielle Confirmatoire", class = "text-primary"),
                      p(class = "lead", "Comprendre et réaliser une analyse factorielle confirmatoire à partir de l’interface")
                    ),
                    
                    # Section présentation de l'application
                    card(
                      card_header(
                        div(class = "d-flex align-items-center",
                            icon("rocket", class = "me-2"), 
                            h4("Présentation de l'application", class = "m-0"))
                      ),
                      card_body(
                        p("Cette application vous guide pas à pas dans la réalisation d'une analyse factorielle confirmatoire (AFC). Elle combine la puissance du package ", 
                          code("lavaan"), " avec une interface intuitive pour :"),
                        
                        div(class = "row",
                            div(class = "col-md-6",
                                tags$ul(
                                  tags$li("Importer vos données depuis différents formats"),
                                  tags$li("Spécifier et tester vos modèles de mesure"),
                                  tags$li("Choisir les méthodes d'estimation appropriées"),
                                  tags$li("Interpréter les indices d'ajustement")
                                )),
                            div(class = "col-md-6",
                                tags$ul(
                                  tags$li("Visualiser vos modèles avec des diagrammes"),
                                  tags$li("Comparer des modèles alternatifs"),
                                  tags$li("Générer du code R reproductible"),
                                  tags$li("Exporter vos résultats et graphiques")
                                ))
                        )
                      )
                    ),
                    
                    # Section navigation
                    card(
                      card_header(
                        div(class = "d-flex align-items-center",
                            icon("compass", class = "me-2"), 
                            h4("Comment utiliser cette application", class = "m-0"))
                      ),
                      card_body(
                        p("Suivez les onglets dans l'ordre pour une analyse complète :"),
                        
                        div(class = "row",
                            div(class = "col-md-4 mb-3",
                                div(class = "text-center",
                                    span(class = "badge bg-primary fs-6 mb-2", "1"),
                                    h6("Source de données"),
                                    p(class = "small text-muted", "Importez un fichier, choisissez un exemple réel ou simulez des données")
                                )),
                            div(class = "col-md-4 mb-3",
                                div(class = "text-center",
                                    span(class = "badge bg-primary fs-6 mb-2", "2"),
                                    h6("Modèle"),
                                    p(class = "small text-muted", "Définissez vos facteurs et associez les variables")
                                )),
                            div(class = "col-md-4 mb-3",
                                div(class = "text-center",
                                    span(class = "badge bg-primary fs-6 mb-2", "3"),
                                    h6("Spécification"),
                                    p(class = "small text-muted", "Choisissez l'estimateur et lancez l'analyse")
                                ))
                        ),
                        
                        div(class = "alert alert-info mt-3",
                            icon("lightbulb"), " ",
                            strong("Astuce : "), "Utilisez les données simulées pour vous familiariser avec l'application avant d'importer vos propres fichiers."
                        )
                      )
                    )
                  ),
                  
                  # Onglet 2 - Concepts de base
                  nav_panel(
                    title = "Concepts de base",
                    
                    card(
                      card_header(
                        div(class = "d-flex align-items-center",
                            icon("project-diagram", class = "me-2"), 
                            h4("Qu'est-ce que l'Analyse Factorielle Confirmatoire ?", class = "m-0"))
                      ),
                      card_body(
                        p("L'AFC est une méthode statistique qui permet de ", strong("tester"), " si un ensemble de variables observées reflète bien une structure factorielle théorique spécifique. Contrairement à l'analyse factorielle exploratoire, l'AFC part d'hypothèses a priori sur :"),
                        
                        div(class = "row mt-3",
                            div(class = "col-md-4",
                                div(class = "text-center p-3 border rounded",
                                    icon("eye", class = "fa-2x text-primary mb-2"),
                                    h6("Variables manifestes"),
                                    p(class = "small text-muted", "Variables directement observées dans vos données")
                                )),
                            div(class = "col-md-4",
                                div(class = "text-center p-3 border rounded",
                                    icon("ghost", class = "fa-2x text-success mb-2"),
                                    h6("Variables latentes"),
                                    p(class = "small text-muted", "Facteurs non observés, construits théoriquement")
                                )),
                            div(class = "col-md-4",
                                div(class = "text-center p-3 border rounded",
                                    icon("link", class = "fa-2x text-warning mb-2"),
                                    h6("Relations"),
                                    p(class = "small text-muted", "Liens entre variables manifestes et latentes")
                                ))
                        )
                      )
                    )
                  ),
                  
                  # Onglet 3 - Équations fondamentales
                  nav_panel(
                    title = "Équations",
                    
                    card(
                      card_header(
                        div(class = "d-flex align-items-center",
                            icon("calculator", class = "me-2"), 
                            h4("Équations fondamentales", class = "m-0"))
                      ),
                      card_body(
                        h5("Modèle de mesure"),
                        p("L'AFC repose sur l'équation de mesure fondamentale :"),
                        div(class = "bg-light p-3 rounded mb-3",
                            code("X = Λη + δ", class = "fs-5")),
                        
                        div(class = "row",
                            div(class = "col-md-6",
                                h6("Où :"),
                                tags$ul(
                                  tags$li(strong("X"), " = vecteur des variables observées"),
                                  tags$li(strong("Λ"), " = matrice des loadings factoriels"),
                                  tags$li(strong("η"), " = vecteur des facteurs latents"),
                                  tags$li(strong("δ"), " = vecteur des erreurs de mesure")
                                )),
                            div(class = "col-md-6",
                                h6("Hypothèses clés :"),
                                tags$ul(
                                  tags$li("E[δ] = 0 (erreurs centrées)"),
                                  tags$li("Cov(η,δ) = 0 (facteurs et erreurs indépendants)"),
                                  tags$li("δ ~ N(0,Θ) (erreurs normales)")
                                ))
                        ),
                        
                        h5("Matrice de covariance implicite"),
                        p("Le modèle prédit la matrice de covariance observée :"),
                        div(class = "bg-light p-3 rounded mb-3",
                            code("Σ = ΛΨΛ' + Θ", class = "fs-5")),
                        
                        p("Où ", strong("Ψ"), " est la matrice de covariance entre facteurs et ", 
                          strong("Θ"), " la matrice de covariance des erreurs.")
                      )
                    )
                  ),
                  
                  # Onglet 4 - Séquence d'analyse
                  nav_panel(
                    title = "Séquence d'analyse",
                    
                    card(
                      card_header(
                        div(class = "d-flex align-items-center",
                            icon("list-ol", class = "me-2"), 
                            h4("Séquence d'analyse", class = "m-0"))
                      ),
                      card_body(
                        p("L'AFC suit une séquence logique d'étapes :"),
                        
                        div(class = "row",
                            div(class = "col-md-6",
                                div(class = "card border-primary mb-3",
                                    div(class = "card-header bg-primary text-white",
                                        h6(class = "m-0", "1. Préparation des données")),
                                    div(class = "card-body",
                                        tags$ul(class = "mb-0",
                                                tags$li("Importation et nettoyage"),
                                                tags$li("Vérification des distributions"),
                                                tags$li("Détection des outliers"),
                                                tags$li("Gestion des valeurs manquantes")
                                        ))
                                ),
                                div(class = "card border-success mb-3",
                                    div(class = "card-header bg-success text-white",
                                        h6(class = "m-0", "3. Estimation du modèle")),
                                    div(class = "card-body",
                                        tags$ul(class = "mb-0",
                                                tags$li("Choix de l'estimateur (ML, DWLS, ULS)"),
                                                tags$li("Estimation des paramètres"),
                                                tags$li("Calcul des erreurs standard"),
                                                tags$li("Tests de signification")
                                        ))
                                ),
                                div(class = "card border-danger mb-3",
                                    div(class = "card-header bg-danger text-white",
                                        h6(class = "m-0", "5. Interprétation")),
                                    div(class = "card-body",
                                        tags$ul(class = "mb-0",
                                                tags$li("Analyse des loadings"),
                                                tags$li("Fiabilité des construits"),
                                                tags$li("Validité discriminante"),
                                                tags$li("Implications théoriques")
                                        ))
                                )),
                            div(class = "col-md-6",
                                div(class = "card border-info mb-3",
                                    div(class = "card-header bg-info text-white",
                                        h6(class = "m-0", "2. Spécification du modèle")),
                                    div(class = "card-body",
                                        tags$ul(class = "mb-0",
                                                tags$li("Définition des facteurs"),
                                                tags$li("Association variables-facteurs"),
                                                tags$li("Contraintes et paramètres fixes"),
                                                tags$li("Identification du modèle")
                                        ))
                                ),
                                div(class = "card border-warning mb-3",
                                    div(class = "card-header bg-warning text-white",
                                        h6(class = "m-0", "4. Évaluation de l'ajustement")),
                                    div(class = "card-body",
                                        tags$ul(class = "mb-0",
                                                tags$li("Indices d'ajustement global"),
                                                tags$li("Résidus standardisés"),
                                                tags$li("Indices de modification"),
                                                tags$li("Comparaison de modèles")
                                        ))
                                ),
                                div(class = "card border-secondary mb-3",
                                    div(class = "card-header bg-secondary text-white",
                                        h6(class = "m-0", "6. Communication")),
                                    div(class = "card-body",
                                        tags$ul(class = "mb-0",
                                                tags$li("Diagrammes de modèle"),
                                                tags$li("Tableaux de résultats"),
                                                tags$li("Code R reproductible"),
                                                tags$li("Rapport d'analyse")
                                        ))
                                ))
                        )
                      )
                    ),
                    
                    # Section navigation
                    card(
                      card_header(
                        div(class = "d-flex align-items-center",
                            icon("compass", class = "me-2"), 
                            h4("Comment utiliser cette application", class = "m-0"))
                      ),
                      card_body(
                        p("Suivez les onglets dans l'ordre pour une analyse complète :"),
                        
                        div(class = "row",
                            div(class = "col-md-4 mb-3",
                                div(class = "text-center",
                                    span(class = "badge bg-primary fs-6 mb-2", "1"),
                                    h6("Source de données"),
                                    p(class = "small text-muted", "Importez vos fichiers ou utilisez des données simulées")
                                )),
                            div(class = "col-md-4 mb-3",
                                div(class = "text-center",
                                    span(class = "badge bg-primary fs-6 mb-2", "2"),
                                    h6("Modèle"),
                                    p(class = "small text-muted", "Définissez vos facteurs et associez les variables")
                                )),
                            div(class = "col-md-4 mb-3",
                                div(class = "text-center",
                                    span(class = "badge bg-primary fs-6 mb-2", "3"),
                                    h6("Spécification"),
                                    p(class = "small text-muted", "Choisissez l'estimateur et lancez l'analyse")
                                ))
                        ),
                        
                        div(class = "alert alert-info mt-3",
                            icon("lightbulb"), " ",
                            strong("Astuce : "), "Utilisez les données simulées pour vous familiariser avec l'application avant d'importer vos propres fichiers."
                        )
                      )
                    )
                  )
                )
              ),
                        
              nav_panel(
                title = "Source de données",value="source",
                div(class="cfa-source-layout",

                  # ── Colonne gauche : méthode + formulaire ──
                  div(
                    radioButtons("import_method", "Méthode :",
                             choices = c(
                               "Charger un fichier" = "file",
                               "Données réelles" = "real",
                               "Données simulées" = "example"
                             ),
                             selected = "file",
                             inline = TRUE),
                
                conditionalPanel(
                  condition = "input.import_method == 'file'",
                  div(
                    # FileInput et ses options
                    fileInput("file", NULL, width = "100%",
                              accept = c(".csv", ".sav", ".dta", ".xlsx", ".xls"),
                              buttonLabel = "Parcourir...",
                              placeholder = "Aucun fichier sélectionné") %>%
                      tagAppendAttributes(
                        onchange = "handleFileSelect(this)",
                        `data-full-path` = ""
                      ),
                    
                    # Options de fichier (conditionnelles)
                    conditionalPanel(
                      condition = "input.file != null",
                      layout_columns(
                        col_widths = c(6, 6),
                        checkboxInput("header", "Noms des variables en 1ère ligne", TRUE),
                        radioButtons("sep", "Séparateur (CSV)",
                                     choices = c(Virgule = ",",
                                                 "Point-virgule" = ";",
                                                 Tabulation = "\\t"),
                                     selected = ",",
                                     inline = TRUE)
                      )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "input.import_method == 'real'",
                  h5("Jeux de données illustratifs"),
                  selectInput("real_dataset", "Choisir un jeu de données :",
                              width = "100%",
                              choices = cfa_example_choices(),
                    selected = "HolzingerSwineford1939"
                  )
                ),

                conditionalPanel(
                  condition = "input.import_method == 'example'",
                  h5("Configuration des données simulées"),
                  layout_columns(
                    col_widths = c(4, 4, 4),
                    numericInput("example_n_factors", 
                                 "# facteurs",
                                 value = 2,
                                 min = 1,
                                 max = 4,
                                 width = "100%"),
                    numericInput("example_n_indicators",
                                 "# indicateurs",
                                 value = 6,
                                 min = 2,
                                 width = "100%"),
                    numericInput("example_n_obs",
                                 "n",
                                 value = 200,
                                 min = 50,
                                 max = 1000,
                                 step = 50,
                                 width = "100%")
                  ),
                  layout_columns(
                    col_widths = c(6, 6),
                    radioButtons("example_data_type", "Type de données :",
                                 choices = c(
                                   "Continues" = "continuous",
                                   "Ordinales" = "ordinal"
                                 ),
                                 selected = "continuous",
                                 inline = TRUE),
                    conditionalPanel(
                      condition = "input.example_data_type == 'ordinal'",
                      numericInput("example_n_categories",
                                   "# catégories",
                                   value = 5,
                                   min = 3,
                                   max = 7,
                                   step = 1,
                                   width = "100%")
                    )
                  ),
                  layout_columns(
                    col_widths = c(6, 6),
                    div(
                      h6("Force des loadings"),
                      sliderInput("example_loading_range",
                                  NULL,
                                  min = 0.1,
                                  max = 0.9,
                                  value = c(0.5, 0.8),
                                  step = 0.05,
                                  width = "100%")
                    ),
                    div(
                      h6("Corrélation entre facteurs"),
                      sliderInput("example_factor_cor_range",
                                  NULL,
                                  min = 0,
                                  max = 0.9,
                                  value = c(0.1, 0.5),
                                  step = 0.05,
                                  width = "100%")
                    )
                  ),
                  # Message informatif
                  div(
                    class = "alert alert-info mt-3",
                    style = "font-size: 0.9em;",
                    icon("info-circle"), " ",
                    "Ces paramètres définissent la structure des données. Passez à l'onglet ",
                    strong("Modèle & Spécification"), " pour associer les variables aux facteurs et générer les données."
                  )
                )
                  ), # fin div colonne gauche

                  # ── Colonne droite : résumé/info ──
                  div(
                    conditionalPanel(
                      condition = "input.import_method == 'file'",
                      div(
                        class = "border rounded p-3 bg-light",
                        h6("📋 Résumé du fichier"),
                        uiOutput("file_summary_compact")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.import_method == 'real'",
                      uiOutput("real_dataset_info")
                    ),
                    conditionalPanel(
                      condition = "input.import_method == 'example'",
                      div(
                        class = "border rounded p-3 bg-light",
                        h6("💡 Rappel"),
                        p(style="font-size:0.85rem;",
                          "Après avoir configuré les paramètres, allez à l'onglet ",
                          strong("Modèle"), " pour assigner les variables aux facteurs et générer les données.")
                      )
                    )
                  ) # fin div colonne droite
                ) # fin layout_columns
              ),
              
              nav_panel(
                title = "Modèle & Spécification", value="model",
                class = "p-2 cfa-model-page",
                div(class="cfa-model-layout",
                  div(class="cfa-model-stack",
                    card(fill=FALSE,
                      card_header("Structure du modèle"),
                      card_body(fillable=FALSE,fill=FALSE,
                        numericInput("n_factors","Nombre de facteurs",value=1,min=1,width="150px"),
                        uiOutput("factor_inputs"),
                        tags$details(tags$summary("Syntaxe du modèle (facultatif)"),verbatimTextOutput("model_preview"))
                      )
                    ),
                    conditionalPanel(condition="input.import_method == 'example'",
                      card(fill=FALSE,
                        card_header("Génération des données simulées"),
                        card_body(fillable=FALSE,fill=FALSE,
                          div(class="cfa-inline-actions",
                            numericInput("simulation_seed","Graine (reproductibilité)",value=123,min=0,max=2147483647,step=1),
                            actionButton("generate_data","Générer les données",class="btn-success",icon=icon("magic"))),
                          helpText("Les saturations demandées sont standardisées dans la population. Les valeurs de l’échantillon fluctuent."),
                          conditionalPanel(condition="output.data_generated",
                            div(class="cfa-inline-actions",
                              radioButtons("save_format","Format",choices=c("CSV"="csv","Stata"="dta","SPSS"="sav","Excel"="xlsx"),selected="csv",inline=TRUE),
                              downloadButton("download_simulated_data","Télécharger les données")))
                        )
                      )
                    )
                  ),
                  div(class="cfa-model-stack",
                    card(fill=FALSE,
                      card_header("Estimation"),
                      card_body(fillable=FALSE,fill=FALSE,
                        radioButtons("estimator","Estimateur",
                          choices=c("ML"="ml","ML robuste"="mlr","Boot ML"="mlb","WLSMV (ordinal robuste)"="wlsmv","DWLS"="dwls","ULS"="uls","WLS"="wls"),
                          inline=TRUE,selected="ml"),
                        conditionalPanel(condition="input.estimator == 'mlb'",
                          numericInput("n_bootstrap","Réplications bootstrap",value=1000,min=100,max=10000,step=100)),
                        div(class="cfa-two-controls",
                          radioButtons("constraints","Identification",choices=c("σ²(F)=1"="var_1","λ₁=1"="first_1"),selected="first_1",inline=TRUE),
                          radioButtons("missing","Données manquantes",choices=c("Cas complets"="listwise","FIML"="fiml"),selected="listwise",inline=TRUE)),
                        div(class="cfa-inline-actions",
                          actionButton("run","Estimer le modèle",class="btn-primary",icon=icon("play")),
                          actionButton("reset","Réinitialiser",class="btn-outline-secondary",icon=icon("rotate-left"))),
                        tags$details(
                          tags$summary("Affichage des résultats et matrices"),
                          checkboxGroupInput("display_options","Résultats à afficher",choices=c("Coeff. Std."="std","R²"="r2","Ajustement"="fit","Moyennes"="means"),selected="std",inline=TRUE),
                          numericInput("mi_threshold","Indices de modification : seuil (Inf = masqués)",value=Inf,min=0),
                          div(class="cfa-two-controls",
                            checkboxGroupInput("cov_matrices","Covariances",choices=c("Observée"="observed","Implicite"="implied","Résiduelle"="residual")),
                            checkboxGroupInput("cor_matrices","Corrélations",choices=c("Observée"="observed","Implicite"="implied","Résiduelle"="residual","Résiduelle Std"="residual_std"))))
                      )
                    ),
                    conditionalPanel(condition="output.model_valid",
                      card(fill=FALSE,
                        card_header("Aperçu du modèle"),
                        card_body(fillable=FALSE,fill=FALSE,
                          div(class="cfa-model-count",uiOutput("model_info_left")),
                          tags$details(tags$summary("Diagramme conceptuel"),plotOutput("model_diagram_preview",height="350px")),
                          uiOutput("model_info_right")
                        )
                      )
                    )
                  )
                )
              ),

              nav_panel(
                title = "Contraintes",value="constraints",
                class = "p-2",
                navset_card_tab(
                  nav_panel(
                    title = "Covariances",
                    card(
                      card_header("Covariances entre erreurs"),
                      card_body(
                        layout_columns(
                          col_widths = c(9, 3),  # Divise l'espace en 9/3
                          # Colonne de gauche pour les inputs et le tableau
                          div(
                            # Les deux inputs sur une ligne
                            layout_columns(
                              col_widths = c(6, 6),
                              selectInput("error_cov_var1", "Variable 1",
                                          choices = NULL,
                                          selected = NULL,
                                          width = "100%"),
                              selectInput("error_cov_var2", "Variable 2",
                                          choices = NULL,
                                          selected = NULL,
                                          width = "100%")
                            ),
                            br(),
                            # Tableau sous les inputs
                            DTOutput("error_cov_table")
                          ),
                          # Colonne de droite pour les boutons
                          div(
                            style = "display: flex; flex-direction: column; gap: 10px;",
                            actionButton("add_error_cov", "Ajout",
                                         class = "btn-primary",
                                         width = "100%"),
                            actionButton("remove_error_cov", "Retrait",
                                         class = "btn-danger",
                                         width = "100%")
                          )
                        )
                      )
                    )
                  ),
                  nav_panel(
                    title = "Paramètres fixes",
                    card(
                      card_header("Paramètres fixes"),
                      card_body(
                        radioButtons("param_type", "Type de paramètre",
                                     choices = c(
                                       "Loading" = "loading",
                                       "Covariance" = "covariance",
                                       "Variance" = "variance"
                                     ),
                                     inline = TRUE),
                        radioButtons("param_action", "Action",
                                     choices = c(
                                       "Fixer une valeur" = "fix",
                                       "Nommer le paramètre" = "label"
                                     ),
                                     selected = "fix",
                                     inline = TRUE),
                        uiOutput("param_inputs"),
                        layout_columns(
                          col_widths = c(9, 3),  # Divise l'espace en 8/4
                          # Colonne de gauche pour le tableau
                          DTOutput("fixed_params_table"),
                          # Colonne de droite pour les boutons
                          div(
                            style = "display: flex; flex-direction: column; gap: 10px;",
                            actionButton("add_fixed_param", "Ajout",
                                         class = "btn-primary",
                                         width = "100%"),
                            actionButton("remove_fixed_param", "Retrait",
                                         class = "btn-danger",
                                         width = "100%")
                          )
                        )
                      )
                    )
                  )
                ),
                
                # Ajouter les boutons directement à la fin de l'onglet
                tags$hr(),
                div(
                  class = "d-flex justify-content-between gap-2",
                  div(
                    style = "width: 48%;",
                    actionButton("run_constraints", "Exécuter", 
                                 class = "btn-primary",
                                 icon = icon("play"),
                                 width = "100%")

                  ),
                  actionButton("reset_constraints", "Réinitialiser",
                               class = "btn-danger",
                               icon = icon("sync"),
                               width = "48%")
                )
              )
            ) # fin navset config
          ) # fin card_body inner
        ) # fin card inner
      ) # fin card_body outer
    ) # fin card outer
  ), # fin conditionalPanel écran 1

  conditionalPanel(condition="output.show_interpretation",
    uiOutput("interpretation_status"),
    div(class="d-flex flex-wrap justify-content-between align-items-center gap-2 mb-3",
      h4("Interprétation et notes sur le modèle",class="m-0 text-primary"),
      actionButton("return_results","Revenir aux résultats",class="btn-outline-primary")),
        uiOutput("takeaways"),
        card(card_header("Indices d’ajustement à lire ensemble"),card_body(DTOutput("diagnostic_fit"))),
        card(card_header("Signaux à examiner et pistes de lecture"),card_body(DTOutput("diagnostic_signals"))),
        conditionalPanel(condition="input.interface_mode == 'advanced'",
          card(card_header("Complément : HTMT2 et incertitude du HTMT"),card_body(
            helpText("HTMT2 utilise des moyennes géométriques des corrélations en valeur absolue. Il ne s’agit pas d’un diagnostic indépendant du HTMT."),
            DTOutput("htmt2_table"),
            layout_columns(col_widths=c(4,4,4),
              selectInput("htmt_boot_type","Indice",choices=c("HTMT arithmétique"="htmt","HTMT2 géométrique"="htmt2")),
              numericInput("htmt_boot_R","Réplications bootstrap",1000,min=200,max=5000,step=200),
              numericInput("htmt_boot_seed","Graine",123,min=0,max=2147483647,step=1)),
            helpText("Le bootstrap est proposé pour une structure simple et des données continues. Avec FIML et des absences, la covariance EM est réestimée à chaque réplication. Il n’est pas proposé pour les items ordinaux."),
            actionButton("run_htmt_boot","Calculer les intervalles",class="btn-primary"),
            uiOutput("htmt_boot_note"),DTOutput("htmt_boot_table")
          )))),

  # ── Écran 2 : Résultats ──
  conditionalPanel(
    condition = "output.show_results && !output.show_interpretation",
    uiOutput("fit_status"),
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      h4("Résultats", class = "m-0 text-primary"),
      actionButton("modifier_modele", "🔧 Modifier le modèle",
                   class = "btn-outline-primary")
    ),
    navset_card_tab(id="results_nav",
      nav_panel(
        title = "Données",value="data",
        DTOutput("data_table")
      ),

      nav_panel(
            title = "Statistiques Descriptives",
            navset_card_tab(

              nav_panel(
                title = "📊 Univariées",
                card(
                  card_header("Statistiques univariées"),
                  card_body(DTOutput("univariate_stats"))
                )
              ),

              nav_panel(
                title = "🔗 Corrélations",
                card(
                  card_header("Matrice de corrélations"),
                  card_body(
                    layout_columns(col_widths=c(6,6),
                      selectInput("correlation_order","Ordre des variables",choices=c("Regroupement empirique"="empirical","Facteurs définis"="factors","Ordre d’origine"="original")),
                      selectInput("correlation_basis","Corrélations",choices=c("Utilisées par l’estimation"="fit","Scores observés (Pearson)"="pearson"))),
                    uiOutput("correlation_note"),
                    plotOutput("correlation_plot",height="auto"),
                    tags$details(tags$summary("Matrice numérique complète"),DTOutput("correlation_table")))
                )
              ),

              nav_panel(
                title = "📈 Distributions",
                card(
                  card_header("Distribution des variables"),
                  card_body(
                    layout_columns(
                      col_widths = c(4, 4, 4),
                      selectInput("vars_to_plot", "Variables à afficher",
                                  choices = NULL, multiple = TRUE, selected = NULL),
                      numericInput("n_cols", "Colonnes",
                                   value = 2, min = 1, max = 4),
                      selectInput("plot_type", "Type de graphique",
                                  choices = c("Histogramme + Densité" = "both",
                                              "Histogramme" = "hist",
                                              "Densité" = "density",
                                              "Boîte à moustaches" = "boxplot"),
                                  selected = "both")
                    ),
                    plotOutput("distributions_plot", height = "auto")
                  )
                )
              ),

              nav_panel(
                title = "🔔 Normalité univariée",
                card(
                  card_header("Tests de normalité (Shapiro-Wilk, Lilliefors, Anderson-Darling)"),
                  card_body(uiOutput("univariate_normality_content"))
                )
              ),

              nav_panel(
                title = "🔔 Normalité multivariée",
                card(
                  card_header("Test de normalité multivariée"),
                  card_body(
                    uiOutput("mvn_scope_note"),
                    DTOutput("multivariate_normality")
                  )
                )
              ),

              nav_panel(
                title = "⚠️ Outliers",
                card(
                  card_header("Détection des outliers multivariés"),
                  card_body(
                    layout_columns(
                      col_widths = c(6, 6),
                      selectInput("outliers_method", "Méthode",
                                  choices = c("Distance de Mahalanobis" = "mahalanobis",
                                              "Mahalanobis robuste (MCD)" = "hadi"),
                                  selected = "mahalanobis"),
                      numericInput("outliers_cutoff", "Seuil de probabilité",
                                   value = 0.001, min = 0.0001,
                                   max = 0.05, step = 0.001)
                    ),
                    verbatimTextOutput("outliers_summary"),
                    DTOutput("outliers_table")
                  )
                )
              )

            ) # fin navset Statistiques Descriptives
          ),
      
          nav_panel(
            title = "Analyse",
            navset_card_tab(id="analysis_nav",
          
          # Onglet Résultats AFC
          nav_panel(
            title = "Analyse de base",value="base",
            div(
              class = "d-flex justify-content-end mb-2",
              downloadButton("download_report", "📥 Télécharger le rapport",
                             class = "btn-outline-primary btn-sm")
            ),
            navset_card_tab(

              # ── Sous-onglet 1 : Résultats lavaan + résidus ──
              nav_panel(
                title = "📋 Résultats",
                card(
                  card_header(
                    class = "bg-primary text-white",
                    h5("Paramètres estimés", class = "m-0")
                  ),
                  card_body(
                    verbatimTextOutput("parameter_estimates")
                  )
                ),
                uiOutput("residus_std_card")
              ),

              # ── Sous-onglet 2 : Matrices ─────────────────────
              nav_panel(
                title = "🔢 Matrices",
                uiOutput("matrices_output")
              ),

              # ── Sous-onglet 3 : Indices de modification ───────
              nav_panel(
                title = "🔧 Indices de modification",
                uiOutput("mi_card")
              )

            ) # fin navset Analyse de base
          ),
          nav_panel(
            title = "Comparaisons",value="comparisons",
            card(
              card_header("Tests de modèles imbriqués"),
              card_body(
                layout_columns(
                  col_widths = c(8, 4),
                  # Colonne de gauche pour les tests et options
                  div(
                    # Tests hiérarchiques
                    uiOutput("nested_tests_hierarchical_ui"),
                    # Tests structurels
                    uiOutput("nested_tests_structural_ui"),
                    # Bouton d'exécution
                    actionButton("run_nested", "Exécuter les tests", 
                                 class = "btn-primary mt-3")
                  ),
                  # Colonne de droite pour les options
                  div(
                    helpText("Les comparaisons réutilisent les données, l’identification, l’estimateur et la gestion des absences de l’analyse affichée. Le test robuste est choisi par lavaan quand il est disponible. Les égalités s’entendent dans l’unité non standardisée des indicateurs et l’échelle des facteurs. L’égalité des intercepts se teste séparément si les moyennes sont estimées.")
                  )
                ),
                # Zone pour afficher les résultats
                uiOutput("nested_results")
              )
            )
          ),        
          
          nav_panel(
            title = "Suppléments",
            navset_card_tab(

              # ── Scores factoriels ──────────────────────────────
              nav_panel(
                title = "📐 Scores factoriels",
                card(
                  card_header(
                    div(class = "d-flex justify-content-between align-items-center",
                        "Scores factoriels",
                        downloadButton("download_scores", "Exporter",
                                       class = "btn-sm"))
                  ),
                  card_body(
                    layout_columns(
                      col_widths = c(4, 8),
                      selectInput("scores_method", "Méthode",
                                  choices = c("Régression" = "regression",
                                              "Bartlett"   = "Bartlett"),
                                  selected = "regression"),
                      div()
                    ),
                    helpText("Les scores factoriels sont des estimations. La colonne ligne_source conserve la position de la personne dans le fichier chargé, hors ligne d’en-tête, même si d’autres lignes ont été exclues pour données manquantes. Avec FIML, une ligne sans aucune réponse peut être conservée avec des scores manquants."),
                    DTOutput("factor_scores_table")
                  )
                ),
                card(
                  card_header("Dispersion des scores factoriels"),
                  card_body(
                    uiOutput("scores_scatter_ui"),
                    plotOutput("scores_scatter_plot", height = "400px")
                  )
                )
              ),

              # ── Fiabilité ──────────────────────────────────────
              nav_panel(
                title = "🔧 Fiabilité",
                card(
                  card_header("Indices de fiabilité et de validité convergente"),
                  card_body(
                    div(class = "alert alert-info mb-3",
                        h6("Interprétation :"),
                        tags$ul(
                          tags$li(strong("Omega / CR :"), "fiabilité de la somme des items selon le modèle; repère descriptif .70"),
                          tags$li(strong("AVE :"), "variance moyenne extraite; repère descriptif .50"),
                          tags$li(strong("H :"), "fiabilité d’une combinaison à poids optimaux, selon le modèle")
                        )
                    ),
                    DTOutput("reliability_indices_table"),
                    uiOutput("reliability_conclusion")
                  )
                )
              ),

              # ── Validité discriminante ─────────────────────────
              nav_panel(
                title = "🔍 Validité discriminante",
                card(
                  card_header("Critère AVE — Fornell-Larcker"),
                  card_body(
                    div(class = "alert alert-info mb-2",
                        tags$ul(
                          tags$li(strong("Diagonale :"), "Racines carrées des AVE"),
                          tags$li(strong("Hors diagonale :"), "Corrélations entre facteurs latents"),
                          tags$li(strong("Critère :"), "√AVE > valeurs absolues des corrélations correspondantes")
                        )
                    ),
                    DTOutput("ave_validity_table"),
                    uiOutput("ave_conclusion")
                  )
                ),
                card(
                  card_header("Critère HTMT — Heterotrait-Monotrait Ratio"),
                  card_body(
                    div(class = "alert alert-info mb-2",
                        tags$ul(
                          tags$li(strong("Critère strict :"), "HTMT < 0.85"),
                          tags$li(strong("Critère libéral :"), "HTMT < 0.90")
                        )
                    ),
                    DTOutput("htmt_validity_table"),
                    uiOutput("htmt_conclusion")
                  )
                )
              )

            ) # fin navset Suppléments
          )
)
          ), # fin nav_panel Analyse
          
          nav_panel(
            title = "Diagramme",
            layout_columns(
              col_widths = c(3, 9),
              card(
                card_header("Contrôles"),
                  card_body(
                    style = "font-size: 1rem;",

                  # Layout + Style + Rotation sur 3 colonnes
                  layout_columns(
                    col_widths = c(5, 4, 3),
                    div(
                      tags$label("Layout", class="fw-bold"),
                      selectInput("layout", NULL,
                                  choices = c("Tree"="tree","Tree 2"="tree2",
                                              "Tree 3"="tree3","Spring"="spring",
                                              "Circle"="circle","Circle2"="circle2"),
                                  selected = "tree2", width="100%")
                    ),
                    div(
                      tags$label("Style", class="fw-bold"),
                      radioButtons("plot_style", NULL,
                                   choices = c("LISREL"="lisrel","RAM"="ram"),
                                   selected = "lisrel")
                    ),
                    div(
                      tags$label("Rotation", class="fw-bold"),
                      uiOutput("plot_rotation_ui")
                    )
                  ),

                  hr(style="margin:6px 0;"),

                  # Estimation sur 2 colonnes
                  layout_columns(
                    col_widths = c(6, 6),
                    selectInput("plot_what_labels", "Nœuds",
                                choices = c("Non std."="est","Std."="std","Aucune"="path"),
                                selected = "path", width="100%"),
                    selectInput("edge_weight", "Arêtes",
                                choices = c("Estimation"="est","Std."="std","Aucun"="path"),
                                selected = "std", width="100%")
                  ),

                  hr(style="margin:6px 0;"),

                  # Éléments
                  tags$label("Éléments", class="fw-bold"),
                  uiOutput("plot_options_ui"),
                  conditionalPanel(
                    condition = "(input.plot_options || []).includes('factor_cov')",
                    sliderInput("plot_curvature", "Courbure",
                                min=0, max=5, value=2, step=0.1)
                  ),
                  conditionalPanel(
                    condition = "input.plot_style == 'lisrel' && (input.plot_options || []).includes('residuals')",
                    sliderInput("resid_scale", "Pos. résidus",
                                min=5, max=25, value=15, step=1)
                  ),
                  conditionalPanel(
                    condition = "(input.display_options || []).includes('means') && (input.plot_options || []).includes('intercepts')",
                    sliderInput("intercept_scale", "Intercepts",
                                min=1, max=5, value=2, step=0.1)
                  ),

                  hr(style="margin:6px 0;"),

                  # Apparence — accordion replié par défaut
                  accordion(
                    open = FALSE,
                    accordion_panel(
                      "🎨 Apparence",
                      layout_columns(
                        col_widths = c(6, 6),
                        colourpicker::colourInput("latent_color", "Facteurs",
                                                  value = "lightblue"),
                        colourpicker::colourInput("manifest_color", "Variables",
                                                  value = "white")
                      ),
                      layout_columns(
                        col_widths = c(6, 6),
                        selectInput("shapeLat", "Forme lat.",
                                    choices = c("Cercle"="circle","Ellipse"="ellipse"),
                                    selected = "circle", width="100%"),
                        selectInput("shapeMan", "Forme obs.",
                                    choices = c("Rectangle"="rectangle","Carré"="square"),
                                    selected = "rectangle", width="100%")
                      ),
                      layout_columns(
                        col_widths = c(6, 6),
                        sliderInput("node_size", "Nœuds",
                                    value=5, min=2, max=10, step=0.5),
                        sliderInput("node_label_scale", "Noms",
                                    min=0.5, max=2, value=1, step=0.1)
                      ),
                      layout_columns(
                        col_widths = c(6, 6),
                        sliderInput("plot_nchar", "Long. noms (0 = complets)",
                                    min=0, max=24, value=0, step=1),
                        conditionalPanel(
                          condition = "input.edge_weight != 'path'",
                          sliderInput("label_size", "Coeff.",
                                      value=1, min=0.5, max=2, step=0.1)
                        )
                      )
                    )
                  )
                )
              ),
              card(
                card_header(
                  class = "bg-primary text-white",
                  uiOutput("model_plot_header")
                ),
                card_body(
                  plotOutput("model_plot", height = "auto")
                )
              )
            )
          ),

          
          nav_panel(
            title = "Code",value="code",
            navset_card_tab(
              nav_panel(
                title = "R (lavaan)",
                div(
                  class = "d-flex justify-content-end mb-2",
                  downloadButton("download_code", "📥 Télécharger le code R",
                                 class = "btn-outline-primary btn-sm")
                ),
                card(
                  card_header(
                    class = "bg-primary text-white",
                    h4("Script de l’analyse (aperçu)", class = "m-0")
                  ),
                  card_body(
                    verbatimTextOutput("lavaan_code")
                  )
                )
              ),
              nav_panel(
                title = "Stata (ébauche)",
                div(
                  class = "d-flex justify-content-end mb-2",
                  downloadButton("download_code_stata", "📥 Télécharger le code Stata",
                                 class = "btn-outline-primary btn-sm")
                ),
                card(
                  card_header(
                    class = "bg-primary text-white",
                    h4("Ébauche Stata à vérifier", class = "m-0")
                  ),
                  card_body(
                    verbatimTextOutput("stata_code")
                  )
                )
              )
            )
          ), # fin nav_panel Code
        
          nav_panel(
            title = "Aide & Références",
            
            # Section Documentation
            card(
              card_header("Documentation interactive"),
              card_body(
                # Sélecteur de section
                selectInput("help_section", "Section",
                            choices = c(
                              "Introduction" = "intro",
                              "Importation des données" = "import",
                              "Spécification du modèle" = "model",
                              "Estimation" = "estimation",
                              "Indices d'ajustement" = "fit",
                              "Variables ordinales" = "ordinal",
                              "Tests de modèles" = "tests",
                              "Interprétation" = "interpret",
                              "Options avancées" = "advanced",
                              "Diagnostics" = "diagnostic",
                              "Exemples" = "examples"
                            )),
                # Contenu d'aide
                uiOutput("help_content")
              )
            ),
            
            # Section Références
            card(
              card_header("Références par thème"),
              card_body(
                h5("AFC - Concepts fondamentaux"),
                tags$ul(
                  tags$li(
                    "Brown, T. A. (2015). ",
                    tags$i("Confirmatory Factor Analysis for Applied Research"),
                    ". New York: Guilford Press.",
                    tags$br(),
                    "Guide complet avec exemples pratiques."
                  )
                ),
                
                h5("Méthodes d'estimation"),
                tags$ul(
                  tags$li(
                    "Li, C. H. (2016). Confirmatory factor analysis with ordinal data: Comparing robust maximum likelihood and diagonally weighted least squares. ",
                    tags$i("Behavior Research Methods"),
                    ", 48(3), 936-949."
                  )
                ),
                
                h5("Indices d'ajustement"),
                tags$ul(
                  tags$li(
                    "Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. ",
                    tags$i("Structural Equation Modeling"),
                    ", 6(1), 1-55."
                  )
                ),
                
                h5("Variables ordinales"),
                tags$ul(
                  tags$li(
                    "Rhemtulla, M., Brosseau-Liard, P. É., & Savalei, V. (2012). When can categorical variables be treated as continuous? A comparison of robust continuous and categorical SEM estimation methods under suboptimal conditions. ",
                    tags$i("Psychological Methods"),
                    ", 17(3), 354-373."
                  )
                )
              )
            )
          )

    ) # fin navset résultats
  ) # fin conditionalPanel écran 2
)



server <- function(input, output, session) {
  
  show_interpretation <- reactiveVal(FALSE)
  results_available <- reactiveVal(FALSE)
  model_plan <- reactiveVal(NULL)
  htmt_boot_result <- reactiveVal(NULL)
  factor_names <- reactive({
    req(input$n_factors)
    plan<-model_plan()
    if(!is.null(plan) && length(plan$names)==input$n_factors) return(plan$names)
    vapply(seq_len(input$n_factors),function(i) {
      name<-input[[paste0("factor_name_",i)]]
      if(is.null(name) || !nzchar(trimws(name))) paste0("F",i) else trimws(name)
    },character(1))
  })
  apply_model_plan <- function(vars,names,ordered=character()) {
    model_plan(list(vars=unname(vars),names=unname(names),ordered=ordered))
    updateNumericInput(session,"n_factors",value=length(vars))
    for(i in seq_along(vars)) {
      updateSelectInput(session,paste0("factor_",i),selected=vars[[i]])
      updateTextInput(session,paste0("factor_name_",i),value=names[i])
      updateCheckboxGroupInput(session,paste0("ordinal_",i),selected=intersect(vars[[i]],ordered))
    }
  }
  observe({
    plan<-model_plan();req(!is.null(plan),input$n_factors==length(plan$names))
    matched<-all(vapply(seq_along(plan$names),function(i)
      identical(input[[paste0("factor_name_",i)]],plan$names[i]) &&
      setequal(as.character(input[[paste0("factor_",i)]]),as.character(plan$vars[[i]])) &&
      setequal(as.character(input[[paste0("ordinal_",i)]]),intersect(plan$vars[[i]],plan$ordered)),logical(1)))
    if(matched) model_plan(NULL)
  })
  observeEvent(input$step_source,{show_interpretation(FALSE);show_results(FALSE);bslib::nav_select("config_nav","source",session=session)})
  observeEvent(input$step_model,{show_interpretation(FALSE);show_results(FALSE);bslib::nav_select("config_nav","model",session=session)})
  observeEvent(input$step_interpret,{
    if(!isTRUE(results_available())) showNotification("Estimez d’abord un modèle à l’étape 3.",type="message")
    else {show_interpretation(TRUE);show_results(FALSE)}
  })
  observeEvent(input$interface_mode,{
    if(input$interface_mode=="guided") {
      if(identical(input$config_nav,"constraints")) bslib::nav_select("config_nav","model",session=session)
      if(identical(input$results_nav,"code")) bslib::nav_select("results_nav","data",session=session)
      if(identical(input$analysis_nav,"comparisons")) bslib::nav_select("analysis_nav","base",session=session)
      bslib::nav_hide("config_nav","constraints",session=session)
      bslib::nav_hide("results_nav","code",session=session)
      bslib::nav_hide("analysis_nav","comparisons",session=session)
    } else {
      bslib::nav_show("config_nav","constraints",session=session)
      bslib::nav_show("results_nav","code",session=session)
      bslib::nav_show("analysis_nav","comparisons",session=session)
    }
  })

  nested_results <- reactiveVal(NULL)
  show_results <- reactiveVal(FALSE)

  output$show_results <- reactive({ show_results() })
  outputOptions(output, "show_results", suspendWhenHidden = FALSE)
  output$show_interpretation <- reactive({show_interpretation()})
  outputOptions(output,"show_interpretation",suspendWhenHidden=FALSE)
  observeEvent(input$return_results,{show_interpretation(FALSE);show_results(TRUE)})

    observeEvent(input$modifier_modele, { show_interpretation(FALSE);show_results(FALSE) })

  analysis_request <- reactiveVal(NULL)
  request_analysis <- function() {
    n<-isolate(analysis_request()); analysis_request(if(is.null(n)) 1L else n+1L)
  }
  observeEvent(input$run, request_analysis())
  observeEvent(input$run_constraints, request_analysis())
  observeEvent(input$step_estimate, request_analysis())
  observeEvent(analysis_request(), {
    res <- model_results()
    if (!is.null(res)) { results_available(TRUE);show_interpretation(FALSE);show_results(TRUE); bslib::nav_select("results_nav","data",session=session) }
  })
  
  # Réactif pour indiquer si des données ont été générées
  data_generated <- reactiveVal(FALSE)
  
  # Output conditionnel pour afficher le bouton de téléchargement
  output$data_generated <- reactive({
    data_generated()
  })
  outputOptions(output, "data_generated", suspendWhenHidden = FALSE)
  
  simulation_info <- reactiveVal(NULL)
  observeEvent(input$generate_data, {
    tryCatch({
      sim <- cfa_simulate(input$example_n_factors,input$example_n_indicators,
          input$example_n_obs,input$example_loading_range,input$example_factor_cor_range,
          seed=input$simulation_seed,ordinal=input$example_data_type=="ordinal",
          categories=if (is.null(input$example_n_categories)) 5 else input$example_n_categories)
      fileData(sim$data); simulation_info(sim); data_generated(TRUE); data_source(list(method="example",nom="Données simulées",pkg=""))
      showNotification("Données générées. Configurez les indicateurs, puis lancez l’analyse.",type="message")
    },error=function(e) showNotification(conditionMessage(e),type="error",duration=10))
  })

  # Observer pour synchroniser automatiquement le nombre de facteurs
  observe({
    if (input$import_method == "example") {
      # Synchroniser n_factors avec example_n_factors
      if (input$n_factors != input$example_n_factors) {
        updateNumericInput(session, "n_factors", value = input$example_n_factors)
      }
    }
  })
  
  # Observer pour réinitialiser l'indicateur lors du changement de méthode
  observeEvent(input$import_method, {
    if(input$import_method == "file") {
      data_generated(FALSE)
    }
  })
  
  # Handler pour le téléchargement des données simulées
  output$download_simulated_data <- downloadHandler(
    filename = function() {
      data_type <- if(input$example_data_type == "ordinal") "ordinales" else "continues"
      extension <- switch(input$save_format,
                          "csv" = ".csv",
                          "dta" = ".dta", 
                          "sav" = ".sav",
                          "xlsx" = ".xlsx")
      
      paste0("donnees_simulees_", data_type, "_", 
             format(Sys.time(), "%Y%m%d_%H%M%S"), extension)
    },
    content = function(file) {
      req(fileData())
      
      tryCatch({
        switch(input$save_format,
               "csv" = {
                 write.csv(fileData(), file, row.names = FALSE)
               },
               "dta" = {
                 if (!requireNamespace("haven", quietly = TRUE)) {
                   showNotification("Le package 'haven' est requis pour sauvegarder en format Stata", 
                                    type = "error")
                   return()
                 }
                 haven::write_dta(fileData(), file)
               },
               "sav" = {
                 if (!requireNamespace("haven", quietly = TRUE)) {
                   showNotification("Le package 'haven' est requis pour sauvegarder en format SPSS", 
                                    type = "error")
                   return()
                 }
                 haven::write_sav(fileData(), file)
               },
               "xlsx" = {
                 if (!requireNamespace("openxlsx", quietly = TRUE)) {
                   showNotification("Le package 'openxlsx' est requis pour sauvegarder en format Excel", 
                                    type = "error")
                   return()
                 }
                 openxlsx::write.xlsx(fileData(), file, rowNames = FALSE)
               }
        )
        
        showNotification(
          paste("Données sauvegardées avec succès en format", 
                switch(input$save_format,
                       "csv" = "CSV",
                       "dta" = "Stata",
                       "sav" = "SPSS", 
                       "xlsx" = "Excel")),
          type = "message"
        )
      }, error = function(e) {
        showNotification(
          paste("Erreur lors de la sauvegarde:", e$message),
          type = "error"
        )
      })
    }
  )

  # Au début de la fonction server, après la définition des réactifs
  observeEvent(navset_card_tab(), {
    cat("Changement d'onglet détecté\n")
 #   print(navset_card_tab())
  }, ignoreInit = TRUE)
  
  # Observer pour mettre à jour le nombre d'indicateurs par défaut
  observe({
    req(input$example_n_factors)
    
    # Calculer le nombre minimum d'indicateurs (2 par facteur)
    default_indicators <- input$example_n_factors * 2
    
    # Mettre à jour le numericInput
    updateNumericInput(session, "example_n_indicators",
                       value = default_indicators,
                       min = default_indicators)  # Le minimum est 2 fois le nombre de facteurs
  })
  

  # Observer pour afficher tous les noms d'input
  observe({
    input_ids <- names(input)
    tab_ids <- input_ids[grepl("tabset", input_ids)]
 #   print(tab_ids)
    
    if (length(tab_ids) > 0) {
      for (tab_id in tab_ids) {
        print(paste("Tab id:", tab_id, "Value:", input[[tab_id]]))
      }
    }
  })
  
  
  # Réactif pour charger les données
  fileData <- reactiveVal(NULL)
  data_source <- reactiveVal(NULL)
  observeEvent(input$import_method, {
    fileData(NULL); data_generated(FALSE); simulation_info(NULL); data_source(NULL)
  },priority=100)


  # ── Métadonnées des jeux de données réels ─────────────────
  real_dataset_meta <- cfa_example_catalog()

# ── Chargement des données réelles ────────────────────────
  observe({
    req(input$import_method == "real", input$real_dataset)
    meta <- real_dataset_meta[[input$real_dataset]]
    req(!is.null(meta))

    fileData(NULL); data_source(NULL)
    df <- tryCatch(cfa_load_example(input$real_dataset),error=function(e) {
      showNotification(paste("Erreur chargement :",e$message),type="error")
      NULL
    })

    if (!is.null(df)) {
      fileData(df)
      data_source(list(method="real",nom=input$real_dataset,pkg=meta$pkg))
      showNotification(
        paste0("✓ ", input$real_dataset, " chargé (", nrow(df), " obs., ",
               ncol(df), " variables)"),
        type = "message"
      )
    }
  })

  observeEvent(input$load_real_example,{
    req(input$import_method=="real",fileData())
    meta<-real_dataset_meta[[input$real_dataset]];req(meta$factors)
    error_covs(error_covs()[0,,drop=FALSE]);fixed_params(fixed_params()[0,,drop=FALSE])
    nested_results(NULL);htmt_boot_result(NULL);results_available(FALSE)
    apply_model_plan(meta$factors,names(meta$factors),ordered=meta$ordered)
    updateRadioButtons(session,"estimator",selected=meta$estimator)
    updateRadioButtons(session,"constraints",selected="first_1")
    updateRadioButtons(session,"missing",selected="listwise")
    bslib::nav_select("config_nav","model",session=session)
    showNotification("Exemple configuré. Vérifiez les noms et les indicateurs, puis cliquez sur Estimer.",type="message")
  })

  # ── Info sur le jeu sélectionné ───────────────────────────
  output$real_dataset_info <- renderUI({
    req(input$import_method == "real", input$real_dataset)
    meta <- real_dataset_meta[[input$real_dataset]]
    req(!is.null(meta))
    df<-fileData()
    items<-unique(unlist(meta$factors))
    missing_n<-if(!is.null(df)) sum(!complete.cases(df[,items,drop=FALSE])) else NA_integer_
    div(class="border rounded p-3 bg-light mt-2",
      h5(meta$question),p(meta$desc),
      div(class="cfa-example-grid",
        div(tags$ul(lapply(meta$dictionary,tags$li))),
        div(div(class="alert alert-info",meta$caution),
          if(!is.null(df)) p(strong(paste(nrow(df),"lignes;",length(items),"indicateurs;",missing_n,"lignes incomplètes pour ces indicateurs."))),
          p("Réglages proposés : ",strong(toupper(meta$estimator)),"; cas complets; ",if(length(meta$ordered)) "items déclarés ordinaux au chargement du modèle." else "indicateurs continus."))),
      div(class="cfa-example-actions",
        actionButton("load_real_example","Charger cet exemple et son modèle",class="btn-primary"),
        downloadButton("download_real_data","Données CSV"),
        downloadButton("download_real_notice","Notice et modèle")),
      helpText("Charger le modèle remplace la structure et efface les anciennes contraintes. Le fichier sélectionné est déjà disponible pour l’aperçu. Les copies incluses fonctionnent sans connexion Internet."),
      tags$details(tags$summary("Provenance, préparation et syntaxe"),
        p(meta$preparation),p("Source : ",meta$pkg,". ",meta$reference),
        tags$a(href=meta$url,target="_blank",rel="noopener","Documentation du jeu de données"),
        tags$pre(meta$syntax)))
  })
  output$download_real_data <- downloadHandler(
    filename=function() paste0(input$real_dataset,".csv"),
    content=function(file) {
      req(input$import_method=="real",fileData())
      file.copy(real_dataset_meta[[input$real_dataset]]$file,file,overwrite=TRUE)
    },contentType="text/csv; charset=utf-8")
  output$download_real_notice <- downloadHandler(
    filename=function() paste0(input$real_dataset,"_notice.txt"),
    content=function(file) writeLines(cfa_example_notice(input$real_dataset),file,useBytes=TRUE),
    contentType="text/plain; charset=utf-8")

  # Observer pour mettre à jour fileData selon la méthode d'importation
  observe({
    if (input$import_method == "file" && !is.null(input$file)) {
      fileData(NULL); data_source(NULL)
      ext <- tools::file_ext(input$file$name)
      
      df <- tryCatch({
        if(ext == "csv") {
          read.csv(input$file$datapath, 
                   header = input$header,
                   sep = input$sep, check.names=FALSE)
        } else {
          rio::import(input$file$datapath)
        }
      }, error = function(e) {
        showNotification(
          paste("Erreur lors du chargement du fichier:", e$message),
          type = "error"
        )
        return(NULL)
      })
      
      if (!is.null(df)) {
        if(anyDuplicated(names(df)) || any(make.names(names(df)) != names(df))) {
          showNotification("Noms d’indicateurs incompatibles avec la syntaxe : utilisez des noms uniques sans espaces (p. ex. Stress_1), puis réimportez.",type="error",duration=12)
          return()
        }
        showNotification(
          paste("Fichier chargé avec succès:", input$file$name),
          type = "message"
        )
        fileData(df)
        data_source(list(method="file",nom=input$file$name,pkg=""))
      }
    }
  })
  
  base_data <- reactive({
    req(fileData())
    list(
      data = fileData(),
      num_data = fileData()[sapply(fileData(), is.numeric)],
      ord_vars = ordinal_vars(),
      selected_vars = unique(unlist(lapply(1:input$n_factors, function(i) {
        input[[paste0("factor_", i)]]
      })))
    )
  })
  

  # Réactif pour les statistiques de base
  base_stats <- reactive({
    req(fileData())
    
    stats <- lapply(fileData()$num_data, function(x) {
      x_clean <- na.omit(x)
      n <- length(x_clean)
      mean_x <- mean(x_clean)
      sd_x <- sd(x_clean)
      
      list(
        clean_data = x_clean,
        n = n,
        N = length(x),
        missing = sum(is.na(x)),
        mean = mean_x,
        sd = sd_x,
        se = sd_x/sqrt(n),
        quantiles = quantile(x_clean, c(0.25, 0.75)),
        min = min(x_clean),
        max = max(x_clean),
        median = median(x_clean)
      )
    })
    return(stats)
  }) %>% bindCache(fileData())
  
  
  test_proportional_odds <- function(x) {
    if (length(unique(x)) < 3) return(NA)
    tryCatch({
      # Créer un modèle ordinal simple
      df <- data.frame(x = factor(x, ordered = TRUE))
      fit <- MASS::polr(x ~ 1, data = df)
      # Test de Brant
      brant::brant(fit)$p.value
    }, error = function(e) NA)
  }
  
  # Mettre à jour les choix disponibles dans display_options
  observe({
    choices <- c(
      "Coeff. Std." = "std",
      "R carré" = "r2",
      "Ajustement" = "fit",
      "Struct. Moyennes" = "means"
    )
    
    selected <- input$display_options
    
    # S'assurer que means est sélectionné avec FIML
    if (input$missing == "fiml") {
      selected <- unique(c(selected, "means"))
    }
    
    updateCheckboxGroupInput(session, "display_options",
                             choices = choices,
                             selected = selected)
  })
  
  
output$model_preview <- renderPrint({
    req(input$n_factors)
    lines <- lapply(1:input$n_factors, function(i) {
      vars <- input[[paste0("factor_", i)]]
      if (length(vars) < 1) return(NULL)
      paste0(factor_names()[i], " =~ ", paste(vars, collapse = " + "))
    })
    lines <- Filter(Negate(is.null), lines)
    if (length(lines) == 0) cat("Assignez des variables aux facteurs.")
    else cat(paste(lines, collapse = "\n"))
  })

  output$model_valid <- reactive({
    req(input$n_factors)
    any(sapply(1:input$n_factors, function(i)
      length(input[[paste0("factor_", i)]]) >= 2))
  })
  
  outputOptions(output, "model_valid", suspendWhenHidden = FALSE)

  
  output$model_diagram_preview <- renderPlot({
    req(input$n_factors)
    # Construire une syntaxe preview sans dépendre de fileData()
    syntax <- tryCatch(buildLavaanModel(), error = function(e) NULL)
    if (is.null(syntax)) {
      # Construire une syntaxe minimale depuis les inputs directement
      lines <- lapply(1:input$n_factors, function(i) {
        vars <- input[[paste0("factor_", i)]]
        if (length(vars) < 2) return(NULL)
        paste0(factor_names()[i], " =~ ", paste(vars, collapse = " + "))
      })
      lines <- Filter(Negate(is.null), lines)
      if (length(lines) == 0) {
        plot.new()
        text(0.5, 0.5, "Assignez au moins 2 variables\npar facteur",
             cex = 1.2, col = "gray50", adj = 0.5)
        return()
      }
      syntax <- paste(lines, collapse = "\n")
    }
    tryCatch({
      preview <- cfa_preview_model(syntax)
      semPlot::semPaths(preview, what = "path", layout = "tree2",
                        style = "lisrel", rotation = 2,
                        nCharNodes = 0, mar = c(2,2,2,2))
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Spécifiez le modèle\npour voir le diagramme",
           cex = 1.2, col = "gray50", adj = 0.5)
    })
  })

output$model_info_left <- renderUI({
    req(input$n_factors)
    factor_vars <- lapply(1:input$n_factors, function(i)
      input[[paste0("factor_", i)]])
    n_factors <- sum(sapply(factor_vars, length) >= 2)
    n_items   <- length(unique(unlist(factor_vars)))
    req(n_items > 0)
    div(
      p(strong("Facteurs : "), n_factors),
      p(strong("Items : "), n_items)
    )
  })

  output$model_info_right <- renderUI({
    req(input$n_factors)
    div(p("Le nombre de paramètres et les degrés de liberté seront indiqués après estimation, en tenant compte de l’identification, des seuils éventuels et des contraintes."))
  })

  # UI dynamique pour les facteurs
  output$factor_inputs <- renderUI({
    req(input$n_factors)
    
    # Pour les données simulées, créer des variables fictives
    if (input$import_method == "example") {
      # Créer les noms de variables basés sur la configuration
      var_names <- paste0("x", 1:input$example_n_indicators)
    } else {
      # Pour les fichiers importés, utiliser les vraies variables
      req(fileData())
      var_names <- names(fileData())
    }
    
    # Créer deux listes pour les colonnes gauche et droite
    factor_cards <- list()
    
    plan<-model_plan()
    for(i in 1:input$n_factors) {
      planned<-!is.null(plan) && length(plan$names)==input$n_factors
      default_name<-if(planned) plan$names[i] else isolate(input[[paste0("factor_name_",i)]])
      if(is.null(default_name)) default_name<-paste0("F",i)
      selected<-if(planned) plan$vars[[i]] else intersect(isolate(input[[paste0("factor_",i)]]),var_names)
      # Le selectInput pour choisir les variables
      factor_input <- selectInput(
        inputId = paste0("factor_", i),
        label = paste("Variables du facteur", i),
        choices = var_names,
        multiple = TRUE, selected=selected,
        width = "100%"
      )
      
      # Ajouter un uiOutput pour les variables ordinales seulement pour les fichiers
      if (input$import_method %in% c("file","real")) {
        ordinal_input <- uiOutput(paste0("ordinal_vars_", i))
      } else {
        ordinal_input <- NULL
      }
      
      # Combiner les inputs dans un div
      factor_div <- div(class="cfa-factor",
        textInput(paste0("factor_name_",i),paste("Nom du facteur",i),value=default_name,width="100%"),
        factor_input,
        ordinal_input
      )
      
      factor_cards[[i]] <- factor_div
    }
    factors_layout <- div(class="cfa-factor-grid",factor_cards)

    # Message d'aide pour les données simulées
    if (input$import_method == "example") {
      help_message <- div(
        class = "alert alert-info mt-3",
        style = "font-size: 0.9em;",
        icon("info-circle"), " ",
        strong("Données simulées : "), 
        sprintf("Associez les variables x1 à x%d aux facteurs. ", input$example_n_indicators),
        "La population simulée suit la répartition automatique des indicateurs. Vous pouvez ensuite analyser une autre structure pour étudier l’effet d’une mauvaise spécification."
      )
    } else {
      help_message <- NULL
    }
    
    # Retourner le tout
    tagList(
      tags$details(tags$summary("Comment nommer les facteurs ?"),helpText("Commencez par une lettre, puis utilisez lettres non accentuées, chiffres et traits de soulignement (p. ex. Stress, Engagement). Les noms doivent être uniques et distincts de ceux des indicateurs.")),
      factors_layout,
      help_message
    )
  })
  
  
  # Observer pour pré-remplir les facteurs automatiquement pour les données simulées
  observe({
    if (input$import_method == "example" && 
        input$n_factors == input$example_n_factors &&
        input$example_n_indicators > 0) {
      
      # Calculer la répartition automatique des variables
      indicators_per_factor <- floor(input$example_n_indicators / input$example_n_factors)
      extra_indicators <- input$example_n_indicators %% input$example_n_factors
      
      var_counter <- 1
      for (f in 1:input$example_n_factors) {
        n_ind <- indicators_per_factor
        if (extra_indicators > 0) {
          n_ind <- n_ind + 1
          extra_indicators <- extra_indicators - 1
        }
        
        # Créer la liste des variables pour ce facteur
        factor_vars <- paste0("x", var_counter:(var_counter + n_ind - 1))
        var_counter <- var_counter + n_ind
        
        # Mettre à jour le selectInput pour ce facteur
        updateSelectInput(
          session, 
          paste0("factor_", f),
          selected = factor_vars
        )
      }
    }
  })
  
  # Les contrôles ordinaux conservent les choix et les exemples préconfigurés.
  ordinal_registered <- integer()
  observe({
    req(input$n_factors)
    for(i in setdiff(seq_len(input$n_factors),ordinal_registered)) local({
      i<-i
      output[[paste0("ordinal_vars_",i)]] <- renderUI({
        req(input$import_method %in% c("file","real"))
        selected_vars<-input[[paste0("factor_",i)]]
        if(!length(selected_vars)) return(NULL)
        plan<-model_plan()
        selected<-if(!is.null(plan) && length(plan$vars)==input$n_factors) intersect(selected_vars,plan$ordered)
          else intersect(selected_vars,isolate(input[[paste0("ordinal_",i)]]))
        tags$details(class="cfa-ordinal-options",
          tags$summary(textOutput(paste0("ordinal_count_",i),inline=TRUE)),
          helpText("Cochez les indicateurs dont les catégories sont ordonnées, y compris les réponses binaires."),
          actionButton(paste0("select_all_ordinal_",i),"Tout sélectionner",class="btn-sm btn-outline-secondary"),
          checkboxGroupInput(paste0("ordinal_",i),label=NULL,choices=selected_vars,selected=selected,inline=TRUE))
      })
      output[[paste0("ordinal_count_",i)]] <- renderText({
        vars<-input[[paste0("factor_",i)]]
        plan<-model_plan()
        selected<-if(!is.null(plan)) intersect(vars,plan$ordered) else intersect(vars,input[[paste0("ordinal_",i)]])
        paste0("Items ordinaux (",length(selected),"/",length(vars),")")
      })
      observeEvent(input[[paste0("select_all_ordinal_",i)]],{
        selected_vars<-input[[paste0("factor_",i)]]
        updateCheckboxGroupInput(session,paste0("ordinal_",i),choices=selected_vars,selected=selected_vars)
      })
    })
    ordinal_registered <<- union(ordinal_registered,seq_len(input$n_factors))
  })

  # Créer un réactif pour stocker les variables ordinales de tous les facteurs
  ordinal_vars <- reactive({
    req(input$n_factors)
    unique(unlist(lapply(1:input$n_factors, function(i) {
      input[[paste0("ordinal_", i)]]
    })))
  })
  
  observeEvent(list(length(ordinal_vars())>0,model_plan()), {
    plan<-model_plan()
    ordinal<-if(!is.null(plan)) length(plan$ordered)>0 else length(ordinal_vars())>0
    choices<-if(ordinal) c("WLSMV (ordinal robuste)"="wlsmv","DWLS"="dwls","ULS"="uls","WLS"="wls")
      else c("ML"="ml","ML robuste"="mlr","Boot ML"="mlb","DWLS"="dwls","ULS"="uls","WLS"="wls")
    selected<-isolate(input$estimator)
    if(is.null(selected) || !selected %in% choices) selected<-if(ordinal) "wlsmv" else "ml"
    updateRadioButtons(session,"estimator",choices=choices,selected=selected)
  },ignoreInit=FALSE)

  # Fonction buildLavaanModel
  buildLavaanModel <- reactive({
    req(input$n_factors)
    req(fileData())
    
    # Vérifier que des variables ont été sélectionnées pour chaque facteur
    factor_variables <- lapply(1:input$n_factors, function(i) {
      input[[paste0("factor_", i)]]
    })
    
    factor_labels<-factor_names()
    if(any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",factor_labels)) || anyDuplicated(factor_labels) || any(factor_labels %in% names(fileData()))) return(NULL)
    # Vérifications de base
    # Vérifier que les variables existent dans les données
    all_selected <- unlist(factor_variables)
    missing_vars <- setdiff(all_selected, names(fileData()))
    if (length(missing_vars) > 0) {
      showNotification(
        paste("Variables introuvables dans les données :",
              paste(missing_vars, collapse=", ")),
        type = "error"
      )
      return(NULL)
    }
    if (any(sapply(factor_variables, length) == 0)) {
      showNotification(
        "Veuillez sélectionner au moins une variable pour chaque facteur",
        type = "error"
      )
      return(NULL)
    }
    
    # Vérifier le nombre minimum de variables par facteur
    min_vars_per_factor <- 2
    if (any(sapply(factor_variables, length) < min_vars_per_factor)) {
      showNotification(
        sprintf("Chaque facteur doit avoir au moins %d variables", min_vars_per_factor),
        type = "error"
      )
      return(NULL)
    }
    
    # Construire le modèle en fonction des facteurs sélectionnés
    model_lines <- lapply(1:input$n_factors, function(i) {
      vars <- factor_variables[[i]]
      if (length(vars) > 0) {
        factor_id <- paste0("F",i)
        factor_name <- factor_labels[i]
        
        # Vérifier les paramètres fixes ou nommés
        named_params <- fixed_params()[fixed_params()$Type == "loading" & 
                                         fixed_params()$Action == "label", ]
        fixed_params_df <- fixed_params()[fixed_params()$Type == "loading" & 
                                            fixed_params()$Action == "fix", ]
        
        var_terms <- sapply(vars, function(var) {
          named_row <- named_params[named_params$Param == sprintf("%s=~%s", factor_id, var), ]
          fixed_row <- fixed_params_df[fixed_params_df$Param == sprintf("%s=~%s", factor_id, var), ]
          
          if (nrow(named_row) > 0) {
            sprintf("%s*%s", named_row$Valeur, var)
          } else if (nrow(fixed_row) > 0) {
            sprintf("%s*%s", fixed_row$Valeur, var)
          } else {
            var
          }
        })
        
        paste0(factor_name, " =~ ", paste(var_terms, collapse = " + "))
      }
    })
    
    # Construire le modèle complet
    model_text <- paste(unlist(model_lines), collapse = "\n")
    
    # Ajouter les covariances entre facteurs si nécessaire
    if (input$n_factors > 1) {
      cor_lines <- combn(input$n_factors, 2, function(pair) {
        f1 <- factor_labels[pair[1]]
        f2 <- factor_labels[pair[2]]
        param_str <- paste0("F",pair[1],"~~F",pair[2])
        
        # Vérifier s'il y a une contrainte pour cette covariance
        cov_fixed <- fixed_params()[fixed_params()$Type == "covariance" & 
                                      fixed_params()$Param == param_str & 
                                      fixed_params()$Action == "fix", ]
        cov_named <- fixed_params()[fixed_params()$Type == "covariance" & 
                                      fixed_params()$Param == param_str & 
                                      fixed_params()$Action == "label", ]
        
        if (nrow(cov_fixed) > 0) {
          paste0(f1, " ~~ ", cov_fixed$Valeur, "*", f2)
        } else if (nrow(cov_named) > 0) {
          paste0(f1, " ~~ ", cov_named$Valeur, "*", f2)
        } else {
          paste0(f1, " ~~ ", f2)
        }
      })
      model_text <- paste(c(model_text, cor_lines), collapse = "\n")
    }
    
    # Ajouter les covariances entre erreurs si définies
    if (nrow(error_covs()) > 0) {
      error_cov_lines <- apply(error_covs(), 1, function(row) {
        paste(row[1], "~~", row[2])
      })
      model_text <- paste(c(model_text, error_cov_lines), collapse = "\n")
    }
    
    # Ajouter les contraintes de variance
    var_constraints <- fixed_params()[fixed_params()$Type == "variance", ]
    if (nrow(var_constraints) > 0) {
      var_lines <- character(nrow(var_constraints))
      
      for (i in 1:nrow(var_constraints)) {
        row <- var_constraints[i, ]
        # Extraire directement le facteur à partir du motif connu "F1~~F1"
        # Le format est toujours "facteur~~facteur" pour les variances
        factor_id <- sub("~~.*$", "", row$Param)
        factor <- factor_labels[match(factor_id,paste0("F",seq_along(factor_labels)))]
        
        if (row$Action == "fix") {
          var_lines[i] <- paste0(factor, " ~~ ", row$Valeur, "*", factor)
        } else if (row$Action == "label") {
          var_lines[i] <- paste0(factor, " ~~ ", row$Valeur, "*", factor)
        }
      }
      
      model_text <- paste(c(model_text, var_lines), collapse = "\n")
    }
    
    # Vérifier la validité syntaxique du modèle
    tryCatch({
      lavaanify(model_text)
      return(model_text)
    }, error = function(e) {
      showNotification(
        paste("Erreur dans la syntaxe du modèle:", e$message),
        type = "error"
      )
      return(NULL)
    })
  })
  
  # Réactif pour l'exécution du modèle
  fit_configuration <- reactive({
    list(data=fileData(), syntax=tryCatch(buildLavaanModel(),error=function(e) NULL),
      factor_names=factor_names(),
      factors=lapply(seq_len(input$n_factors),function(i) input[[paste0("factor_",i)]]),
      ordered=ordinal_vars(), estimator=input$estimator, missing=input$missing,
      scaling=input$constraints, means="means" %in% input$display_options,
      bootstrap=if(input$estimator=="mlb") input$n_bootstrap else NULL)
  })
  model_results <- eventReactive(analysis_request(), {
    if(is.null(fileData())) {showNotification("Chargez un fichier, choisissez un exemple réel ou générez des données avant d’estimer.",type="warning");return(NULL)}
    nested_results(NULL);htmt_boot_result(NULL)
    tryCatch({
      config <- fit_configuration(); syntax <- config$syntax
      if(is.null(syntax)) stop("Vérifiez les indicateurs et les noms des facteurs : noms uniques commençant par une lettre, sans espace ni accent et distincts des colonnes de données.")
      vars <- unique(unlist(config$factors))
      if(!length(vars) || !all(vars %in% names(fileData()))) stop("Sélectionnez des indicateurs présents dans les données.")
      data <- fileData()[vars]
      if(!all(vapply(data,is.numeric,logical(1)))) stop("Les indicateurs doivent être numériques; codez les catégories avant l’importation.")
      if(any(vapply(data,function(x) length(unique(na.omit(x)))<2,logical(1)))) stop("Un indicateur n’a pas au moins deux valeurs observées.")
      ordered <- intersect(config$ordered,vars)
      estimator <- if(input$estimator=="mlb") "ml" else input$estimator
      if(length(ordered) && estimator %in% c("ml","mlr","mlm")) stop("Indicateurs ordinaux : choisissez WLSMV, DWLS, ULS ou WLS.")
      if(input$missing=="fiml" && (length(ordered) || !estimator %in% c("ml","mlr"))) stop("FIML requiert ici des indicateurs continus et ML ou ML robuste. Modifiez explicitement ce réglage.")
      opts <- list(estimator=estimator, missing=input$missing,
          std.lv=input$constraints=="var_1",meanstructure=input$missing=="fiml" || config$means)
      if(length(ordered)) opts$ordered <- ordered
      if(input$estimator=="mlb") { opts$se<-"bootstrap";opts$bootstrap<-input$n_bootstrap }
      estimated <- withProgress(message="Estimation de l’AFC…",value=.5,{ with_cfa_seed(123,cfa_fit(syntax,data,opts)) })
      for(w in estimated$warnings) showNotification(w,type="warning",duration=12)
      fv <- config$factors; names(fv)<-factor_names()
      result<-list(fit=estimated$fit, summary=summary(estimated$fit,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE),
           data=data,syntax=syntax,options=opts,factor_vars=fv,source=data_source(),analysis_seed=123L,
           simulation=if(identical(data_source()$method,"example")) simulation_info() else NULL,
           config=config,warnings=estimated$warnings,status=estimated$status,
           created=format(Sys.time(),"%Y-%m-%d %H:%M:%S"))
      result$diagnostics<-cfa_diagnostic_bundle(result$fit)
      result
    },error=function(e) {
      show_interpretation(FALSE);show_results(FALSE);results_available(FALSE)
      showNotification(paste("Analyse interrompue :",conditionMessage(e)),type="error",duration=NULL)
      NULL
    })
  })
  fit_status_content <- reactive({
    res<-model_results(); req(res)
    stale<-!identical(res$config,fit_configuration())
    div(class=if(stale) "alert alert-warning cfa-status" else "alert alert-info cfa-status",
      strong(if(stale) "Configuration modifiée : " else "Analyse enregistrée : "),
      if(stale) "les résultats et exports correspondent à l’estimation précédente. Relancez l’analyse pour appliquer vos changements."
      else paste(res$status$message,"N utilisé :",lavaan::lavInspect(res$fit,"nobs"),"; paramètres libres :",lavaan::lavInspect(res$fit,"npar")),
      if(length(res$warnings)) tags$details(tags$summary("Avertissements de l’estimation"),tags$ul(lapply(res$warnings,tags$li))))
  })
  output$fit_status <- renderUI({fit_status_content()})
  output$interpretation_status <- renderUI({fit_status_content()})
  activeData <- reactive({ if((isTRUE(show_results()) || isTRUE(show_interpretation())) && !is.null(model_results())) model_results()$data else fileData() })
  output$active_constraints_notice <- renderUI({
    if(nrow(error_covs())+nrow(fixed_params())>0) div(class="alert alert-warning mt-2",
      "Le modèle contient des contraintes avancées. Le mode guidé les conserve; passez aux options avancées pour les examiner ou réinitialisez le modèle.")
  })
  output$takeaways <- renderUI({
    req(model_results()); lines<-cfa_takeaways(model_results())
    div(class="alert alert-info",h4("Que retenir de cette analyse ?"),
      tags$ol(lapply(names(lines),function(n) tags$li(strong(paste0(n," : ")),lines[[n]]))))
  })
  output$diagnostic_fit <- renderDT({
    req(model_results()); d<-model_results()$diagnostics$fit
    d$atteint<-ifelse(is.na(d$atteint),ifelse(is.na(d$repere),"Sans repère","Non évalué"),ifelse(d$atteint,"Oui","Non"))
    fr<-function(x) ifelse(is.na(x),"—",formatC(x,digits=3,format="f",decimal.mark=","))
    d$valeur<-ifelse(d$indice=="df",ifelse(is.na(d$valeur),"—",format(round(d$valeur))),
      ifelse(d$indice=="pvalue" & !is.na(d$valeur) & d$valeur<.001,"< 0,001",fr(d$valeur)))
    d$repere<-ifelse(is.na(d$repere),"",fr(d$repere))
    etiquettes<-c(chisq="Chi carré",df="Degrés de liberté",pvalue="p",cfi="CFI",tli="TLI",rmsea="RMSEA",
      rmsea.ci.lower="RMSEA, borne inf. 90 %",rmsea.ci.upper="RMSEA, borne sup. 90 %",srmr="SRMR")
    d$indice<-ifelse(d$indice %in% names(etiquettes),etiquettes[d$indice],d$indice)
    names(d)<-c("Indice","Valeur","Version","Repère descriptif","Repère atteint")
    cfa_datatable(d,options=list(dom="t",paging=FALSE,ordering=FALSE,columnDefs=list(list(className="dt-right",targets=c(1,3)))),rownames=FALSE)
  })
  output$diagnostic_signals <- renderDT({
    req(model_results());d<-model_results()$diagnostics$signals
    if(!nrow(d)) d<-data.frame(signal="Aucun signal selon les critères examinés",detail="Ce contrôle ne couvre pas toutes les sources de mauvaise spécification.",action="Examiner aussi la théorie, les résidus et les incertitudes.")
    names(d)<-c("Signal","Détail","Piste de lecture")
    cfa_datatable(d,options=list(dom="t",paging=FALSE,ordering=FALSE),rownames=FALSE)
  })
  output$htmt2_table <- renderDT({
    req(model_results());h<-model_results()$diagnostics$htmt2
    if(is.null(h)) return(cfa_datatable(data.frame(Message="HTMT2 non proposé : un seul facteur ou structure à saturations croisées."),options=list(dom="t"),rownames=FALSE))
    h[upper.tri(h,diag=TRUE)]<-NA_real_
    cfa_datatable(as.data.frame(h),options=list(dom="t",paging=FALSE,ordering=FALSE)) %>% formatRound(columns=seq_len(ncol(h)),digits=3,dec.mark=",")
  })
  observeEvent(input$run_htmt_boot,{
    req(model_results());htmt_boot_result(NULL)
    tryCatch({
      r<-withProgress(message="Bootstrap du HTMT…",value=.5,{cfa_htmt_bootstrap(model_results(),R=input$htmt_boot_R,seed=input$htmt_boot_seed,type=input$htmt_boot_type)})
      htmt_boot_result(r)
    },error=function(e) showNotification(conditionMessage(e),type="warning",duration=NULL))
  })
  output$htmt_boot_note <- renderUI({req(htmt_boot_result());div(class="alert alert-info mt-2",htmt_boot_result()$note)})
  output$htmt_boot_table <- renderDT({req(htmt_boot_result());d<-htmt_boot_result()$table
    names(d)<-c("Facteur 1","Facteur 2","Indice","ET bootstrap","Borne inf. 95 %","Borne sup. 95 %","Réplications valides")
    cfa_datatable(d,options=list(dom="t",paging=FALSE,ordering=FALSE),rownames=FALSE) %>% formatRound(columns=3:6,digits=3,dec.mark=",")
  })


  output$plot_rotation_ui <- renderUI({
    req(input$layout)
    choices<-if(input$layout %in% c("circle","circle2")) c(1,3) else 1:4
    selected<-isolate(input$plot_rotation)
    if(is.null(selected) || !selected %in% choices) selected<-if(length(choices)==2) 1 else 2
    radioButtons("plot_rotation",NULL,choices=choices,selected=selected,inline=TRUE)
  })

  output$plot_options_ui <- renderUI({
    # Créer la liste de base des choix
    choices <- c(
      "Résidus" = "residuals",
      "Covariances" = "factor_cov",
      "Variances" = "variances"
    )
    
    if (length(ordinal_vars()) > 0) {
      choices <- c(choices, "Seuils" = "thresholds")
    }
    
    # Ajouter l'option Intercepts si means est activé
    if ("means" %in% input$display_options) {
      choices <- c(choices, "Intercepts" = "intercepts")
    }
    
    # Créer le checkboxGroupInput
    checkboxGroupInput("plot_options", 
                       "Éléments à afficher",
                       choices = choices,
                       selected = c("factor_cov", "residuals"),
                       inline = TRUE)
  })
  
  # Afficher le tableau de données
  output$data_table <- renderDT({
    req(activeData())
    num_cols <- which(sapply(activeData(), is.numeric))
    
    cfa_datatable(activeData(),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'tip',
                processing = TRUE,
                searchHighlight = TRUE,
                deferRender = TRUE
              )) %>%
      formatRound(columns = num_cols, digits = 3, dec.mark = ",")
  })
  
  # Distribution des variables
  output$distributions_plot <- renderPlot({
    req(activeData(), input$vars_to_plot, input$n_cols)
    
    # Créer un data frame long pour ggplot
    plot_data <- activeData() %>%
      select(all_of(input$vars_to_plot)) %>%
      tidyr::pivot_longer(everything(), 
                          names_to = "variable", 
                          values_to = "value")
    
    # Créer le graphique
    p <- ggplot(plot_data, aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), 
                     fill = "lightblue", 
                     color = "black", 
                     bins = 30) +
      geom_density(color = "red", linewidth = 1) +
      geom_rug(alpha = 0.5) +
      facet_wrap(~variable, 
                 scales = "free",
                 ncol = input$n_cols) +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 12, face = "bold"),
        strip.background = element_rect(fill = "lightgray", color = NA),
        panel.spacing = unit(1, "lines")
      ) +
      labs(x = "Valeur",
           y = "Densité")
    
    p
    
  }, height = function() {
    # Calculer la hauteur en fonction du nombre de variables et de colonnes
    n_vars <- length(input$vars_to_plot)
    n_rows <- ceiling(n_vars / input$n_cols)
    input$plot_height * n_rows / 2
  })
  
  # Paramètres estimés
  output$parameter_estimates <- renderPrint({
    req(model_results())
    
    # Afficher le résumé avec les indices de base toujours inclus
    print(summary(model_results()$fit, 
                  standardized = "std" %in% input$display_options,
                  fit.measures = TRUE,  
                  rsquare = "r2" %in% input$display_options))
    if (length(model_results()$options$ordered)>0) cat("\nIndicateurs ordinaux : examinez les indices robustes disponibles dans le résumé. Aucun seuil WRMR automatique n’est appliqué.\n")

    # Si l'option all_fit est sélectionnée, afficher tous les indices
    if ("fit" %in% input$display_options) {
      cat("\nIndices d'ajustement détaillés :\n")
      print(fitMeasures(model_results()$fit))
    }
  })
  
  # Indices de modification
  output$modification_indices <- renderPrint({
    req(model_results())
    if (is.finite(input$mi_threshold)) {  
      mi <- modificationindices(model_results()$fit)
      mi <- mi[mi$mi >= input$mi_threshold, ]
      mi[order(-mi$mi), ]
    }
  })
  
  # UI conditionnelle pour les matrices
  output$matrices_output <- renderUI({
    req(model_results())

    if (length(c(input$cov_matrices, input$cor_matrices)) == 0)
      return(div(class="alert alert-info mt-3",
                 icon("info-circle"), " ",
                 "Sélectionnez des matrices à afficher dans l'onglet ",
                 strong("Modèle & Spécification"), " → Corrélations / Covariances."))
    
    matrices_ui <- list()
    
    # Liste pour stocker toutes les matrices dans l'ordre souhaité
    all_matrices <- list()
    
    # Matrices de covariance
    if (length(input$cov_matrices) > 0) {
      all_matrices[[1]] <- card(
        card_header("Matrices de covariance"),
        card_body(
          if ("observed" %in% input$cov_matrices) {
            list(
              h5("Matrice de covariance observée"),
              verbatimTextOutput("cov_matrix_observed"),
              hr()
            )
          },
          if ("implied" %in% input$cov_matrices) {
            list(
              h5("Matrice de covariance implicite"),
              verbatimTextOutput("cov_matrix_implied"),
              hr()
            )
          },
          if ("residual" %in% input$cov_matrices) {
            list(
              h5("Matrice de covariance résiduelle"),
              verbatimTextOutput("cov_matrix_residual"),
              hr()
            )
          }
        )
      )
    }
    
    # Matrices de corrélation
    if (length(input$cor_matrices) > 0) {
      all_matrices[[2]] <- card(
        card_header("Matrices de corrélation"),
        card_body(
          if ("observed" %in% input$cor_matrices) {
            list(
              h5("Matrice de corrélation observée"),
              verbatimTextOutput("cor_matrix_observed"),
              hr()
            )
          },
          if ("implied" %in% input$cor_matrices) {
            list(
              h5("Matrice de corrélation implicite"),
              verbatimTextOutput("cor_matrix_implied"),
              hr()
            )
          },
          if ("residual" %in% input$cor_matrices) {
            list(
              h5("Matrice des résidus de corrélation"),
              verbatimTextOutput("cor_matrix_residual"),
              hr()
            )
          },
          if ("residual_std" %in% input$cor_matrices) {
            list(
              h5("Matrice des résidus standardisés de corrélation"),
              verbatimTextOutput("cor_matrix_residual_std")
            )
          }
        )
      )
    }
  })
  
  # Pour les matrices de covariance
  observe({
    req(input$cov_matrices)
    req(model_results())
    
    # Pour chaque type de matrice sélectionné
    lapply(input$cov_matrices, function(type) {
      output_id <- paste0("cov_matrix_", type)
      
      output[[output_id]] <- renderPrint({
        tryCatch({
          matrix_data <- switch(type,
                                "observed" = lavInspect(model_results()$fit, "sampstat")$cov,
                                "implied" = fitted(model_results()$fit)$cov,
                                "residual" = lavResiduals(model_results()$fit)$cov)
          
          # Vérifier si matrix_data est null ou non calculable
          if (is.null(matrix_data) || !is.matrix(matrix_data)) {
            cat("Matrice non disponible ou non calculable\n")
          } else {
            # S'assurer que c'est une matrice symétrique complète
            if (exists("make_symmetric_matrix")) {
              matrix_data <- make_symmetric_matrix(matrix_data)
            }
            print(round(matrix_data, 3))
          }
        }, error = function(e) {
          cat("Erreur lors du calcul de la matrice:", e$message, "\n")
        })
      })
    })
  })
  
  # Pour les matrices de corrélation
  observe({
    req(input$cor_matrices)
    req(model_results())
    
    # Pour chaque type de matrice sélectionné
    lapply(input$cor_matrices, function(type) {
      output_id <- paste0("cor_matrix_", type)
      
      output[[output_id]] <- renderPrint({
        tryCatch({
          # Utiliser les accesseurs corrects pour chaque type de matrice
          if (type == "observed") {
            matrix_data <- cov2cor(lavInspect(model_results()$fit, "sampstat")$cov)
          } else if (type == "implied") {
            matrix_data <- cov2cor(fitted(model_results()$fit)$cov)
          } else if (type == "residual") {
            matrix_data <- lavResiduals(model_results()$fit, type = "cor")$cov
          } else if (type == "residual_std") {
            matrix_data <- lavResiduals(model_results()$fit, type = "cor")$cov.z
          } else {
            matrix_data <- NULL
          }
          
          # Vérifier si matrix_data est null ou non calculable
          if (is.null(matrix_data) || !is.matrix(matrix_data)) {
            cat("Matrice non disponible ou non calculable\n")
          } else {
            # Ajouter un titre explicatif pour les résidus
            if (type == "residual") {
              cat("Résidus bruts\n")
            } else if (type == "residual_std") {
              cat("Résidus standardisés (valeurs z)\n")
            }
            print(round(matrix_data, 3))
          }
        }, error = function(e) {
          cat("Erreur lors du calcul de la matrice:", e$message, "\n")
        })
      })
    })
  })
  
    observeEvent(input$missing, {
      if (input$missing == "fiml") {
        showNotification(
          "La structure des moyennes est automatiquement estimée avec FIML",
          type = "message"
        )
        
        current_options <- input$display_options
        if (!"means" %in% current_options) {
          updateCheckboxGroupInput(session, "display_options",
                                   selected = c(current_options, "means"))
        }
      }
    })
  
    plot_sem <- reactive({
      req(model_results())
      req(input$layout)
      rotation_val    <- if (is.null(input$plot_rotation))    2          else as.numeric(input$plot_rotation)
      what_labels     <- if (is.null(input$edge_weight))      "std"      else input$edge_weight
      what_value      <- if (is.null(input$plot_what_labels)) "path"     else input$plot_what_labels
      node_size       <- if (is.null(input$node_size))        5          else input$node_size
      label_scale     <- if (is.null(input$node_label_scale)) 1          else input$node_label_scale
      plot_nchar      <- if (is.null(input$plot_nchar))       0          else input$plot_nchar
      label_size_val  <- if (is.null(input$label_size))       1          else input$label_size
      shape_lat       <- if (is.null(input$shapeLat))         "circle"   else input$shapeLat
      shape_man       <- if (is.null(input$shapeMan))         "rectangle" else input$shapeMan
      lat_color       <- if (is.null(input$latent_color))     "lightblue" else input$latent_color
      man_color       <- if (is.null(input$manifest_color))   "white"    else input$manifest_color
      curvature       <- if (is.null(input$plot_curvature))   2          else input$plot_curvature
      resid_scale_val <- if (is.null(input$resid_scale))      15         else input$resid_scale
      intercept_scale <- if (is.null(input$intercept_scale))  2          else input$intercept_scale
      
      fit <- model_results()$fit
      
      # Déterminer les éléments à afficher
      show_residuals <- "residuals" %in% input$plot_options
      show_factor_cov <- "factor_cov" %in% input$plot_options
      show_variances <- "variances" %in% input$plot_options
      show_intercepts <- "means" %in% input$display_options && 
        "intercepts" %in% input$plot_options
      show_thresholds <- "thresholds" %in% input$plot_options && 
        length(ordinal_vars()) > 0
    
    # Paramètres d'affichage de base
    label_size <- if(what_labels == "path") 0 else (if(is.null(input$label_size)) 1 else input$label_size)
    
    g <- semPaths(fit,
                  what       = what_value,
                  whatLabels = what_labels,
                  layout     = input$layout,
                  style      = input$plot_style,
                  rotation   = rotation_val,
                  nDigits    = 2,
                  cardinal   = TRUE,
                  filetype   = "lavaan",
                  DoNotPlot  = TRUE,
                  residuals  = show_residuals,
                  exoCov     = show_factor_cov,
                  exoVar     = show_variances,
                  thresholds    = show_thresholds,
                  thresholdColor = "darkgray",
                  thresholdSize  = 0.5,
                  ThreshAtSide   = FALSE,
                  shapeMan  = shape_man,
                  shapeLat  = shape_lat,
                  sizeMan   = if(shape_man == "square") node_size else node_size * 1.5,
                  sizeMan2  = if(shape_man == "square") node_size else node_size,
                  sizeLat   = if(shape_lat == "circle") node_size else node_size * 1.5,
                  sizeLat2  = if(shape_lat == "circle") node_size else node_size,
                  color     = list(lat=lat_color, man=man_color, int="white"),
                  border.width = 1.5,
                  border.color = "black",
                  edge.color   = "black",
                  edge.label.cex      = if(what_labels == "path") 0 else label_size_val,
                  edge.label.position = 0.5,
                  edge.label.bg       = TRUE,
                  edge.label.color    = "black",
                  label.cex    = label_scale,
                  label.scale  = FALSE,
                  nCharNodes   = plot_nchar,
                  intercepts   = show_intercepts,
                  intAtSide    = show_intercepts,
                  sizeInt      = if(show_intercepts) intercept_scale else 0,
                  residScale   = if(show_residuals && input$plot_style == "lisrel")
                                   resid_scale_val else 2,
                  curve        = curvature,
                  edge.width   = 1,
                  fade         = FALSE
    )
    
    return(g)
  })
  
  # Output pour le graphique
  output$model_plot <- renderPlot({
    req(model_results())
    req(input$layout)
    req(!is.null(input$plot_rotation))
    
    g <- plot_sem()  # Obtenir l'objet graphique
    plot(g)          # Afficher le graphique
    
  }, height = function() {
    # Calculer la hauteur dynamiquement
    n_vars <- length(names(activeData()))
    600 + max(0, (n_vars - 10) * 50)
  })

  # Mise à jour des choix de variables (observateur spécifique pour vars_to_plot)
  observe({
    req(activeData())
    # Obtenir uniquement les variables numériques
    num_vars <- names(activeData()[sapply(activeData(), is.numeric)])
    if(length(num_vars) > 0) {
      # Sélectionner par défaut jusqu'à 4 variables
      default_selected <- num_vars[1:min(4, length(num_vars))]
      updateSelectInput(session, "vars_to_plot",
                        choices = num_vars,
                        selected = default_selected)
    }
  })
  
  output$distributions_plot <- renderPlot({
    # S'assurer que toutes les données nécessaires sont disponibles
    req(activeData())
    req(input$vars_to_plot)
    req(input$n_cols)
    req(length(input$vars_to_plot) > 0)  # Vérifier qu'au moins une variable est sélectionnée
    
    # Créer un data frame long pour ggplot
    plot_data <- activeData()[input$vars_to_plot] %>%
      tidyr::pivot_longer(everything(), 
                          names_to = "variable", 
                          values_to = "value")
    
    # Créer le graphique de base
    base_plot <- ggplot(plot_data, aes(x = value))
    
    # Ajouter les couches en fonction du type de graphique sélectionné
    p <- switch(input$plot_type,
                "both" = base_plot +
                  geom_histogram(aes(y = after_stat(density)), 
                                 fill = "lightblue", 
                                 color = "black", 
                                 bins = 30) +
                  geom_density(color = "red", linewidth = 1) +
                  geom_rug(alpha = 0.5),
                "hist" = base_plot +
                  geom_histogram(aes(y = after_stat(density)),
                                 fill = "lightblue",
                                 color = "black",
                                 bins = 30),
                "density" = base_plot +
                  geom_density(fill = "lightblue",
                               color = "black",
                               alpha = 0.5),
                "boxplot" = ggplot(plot_data, aes(y = variable, x = value)) +
                  geom_boxplot(fill = "lightblue",
                               color = "black"))
    
    p <- p + 
      facet_wrap(~variable, 
                 scales = "free",
                 ncol = input$n_cols) +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 12, face = "bold"),
        strip.background = element_rect(fill = "lightgray", color = NA),
        panel.spacing = unit(1, "lines")
      ) +
      labs(x = if(input$plot_type != "boxplot") "Valeur" else "Variable",
           y = if(input$plot_type != "boxplot") "Densité" else "Valeur")
    
    p
    
  }, height = function() {
    req(input$vars_to_plot, input$n_cols)
    # Calculer la hauteur en fonction du nombre de variables et de colonnes
    n_vars <- length(input$vars_to_plot)
    n_rows <- ceiling(n_vars / input$n_cols)
    400 * n_rows  # Hauteur de base de 400 pixels par ligne
  })

  output$mi_card <- renderUI({
    req(model_results())
    if (!is.finite(input$mi_threshold))
      return(div(class="alert alert-info mt-3",
                 icon("info-circle"), " ",
                 "Définissez un seuil numérique pour les indices de modification dans l'onglet ",
                 strong("Modèle & Spécification"), " → Indices modif. (seuil)."))
    if (is.finite(input$mi_threshold)) {
      card(
        card_header("Indices de modification"),
        card_body(
          verbatimTextOutput("modification_indices")
        )
      )
    }
  })
  



  # Statistiques univariées
  output$univariate_stats <- renderDT({
    req(activeData())
    
    # Sélectionner uniquement les variables numériques
    num_data <- activeData()[sapply(activeData(), is.numeric)]
    
    # Créer le data frame des statistiques
    stats_df <- do.call(rbind, lapply(names(num_data), function(var) {
      x <- num_data[[var]]
      x_clean <- na.omit(x)
      n <- length(x_clean)
      mean_x <- mean(x_clean)
      sd_x <- sd(x_clean)
      se <- sd_x/sqrt(n)
      t_val <- qt(0.975, df = n - 1)
      
      data.frame(
        Variable = var,
        N = length(x),
        Manquantes = sum(is.na(x)),
        Moyenne = mean_x,
        ET = sd_x,
        SEM = se,
        IC_inf = mean_x - t_val * se,
        IC_sup = mean_x + t_val * se,
        Médiane = median(x_clean),
        Q1 = quantile(x_clean, 0.25),
        Q3 = quantile(x_clean, 0.75),
        IQR = IQR(x_clean),
        Min = min(x_clean),
        Max = max(x_clean),
        CV = abs(sd_x / mean_x) * 100,
        Asymétrie = safe_skewness(x_clean),
        Aplatissement = safe_kurtosis(x_clean),
        row.names = NULL
      )
    }))
    
    # S'assurer que stats_df n'est pas vide avant de formater
    if (nrow(stats_df) > 0) {
      # Créer le tableau avec DT
      cfa_datatable(stats_df,
                options = dt_options) %>%
        formatRound(columns = 3:ncol(stats_df), digits = 3, dec.mark = ",")
    } else {
      # Si pas de données numériques, retourner un tableau vide
      cfa_datatable(data.frame(Message = "Aucune variable numérique disponible"),
                options = dt_options)
    }
  })
  
  correlation_view <- reactive({
    req(model_results())
    cfa_correlation_view(activeData(),fit=model_results()$fit,factors=model_results()$factor_vars,
      order=if(is.null(input$correlation_order)) "empirical" else input$correlation_order,
      basis=if(is.null(input$correlation_basis)) "fit" else input$correlation_basis)
  })
  output$correlation_note <- renderUI({div(class="alert alert-info",correlation_view()$note,
    tags$br(),"Au-dessus de la diagonale : couleurs; au-dessous : coefficients. Les cases grises sans valeur indiquent une corrélation non calculable.")})
  output$correlation_plot <- renderPlot({cfa_plot_correlations(correlation_view())},
    height=function() max(440,28*nrow(correlation_view()$matrix)+110),res=110)
  output$correlation_table <- renderDT({
    R<-correlation_view()$matrix
    cfa_datatable(as.data.frame(R),options=list(dom="t",paging=FALSE,ordering=FALSE)) %>% formatRound(columns=seq_len(ncol(R)),digits=3, dec.mark = ",")
  })

  # La portée des tests est celle du modèle estimé, conservée dans le snapshot.
  normality_continuous_data <- reactive({
    res<-model_results();req(res)
    vars<-setdiff(names(res$data)[vapply(res$data,is.numeric,logical(1))],res$options$ordered)
    res$data[,vars,drop=FALSE]
  })
  output$univariate_normality_content <- renderUI({
    res<-model_results();req(res)
    num_data<-normality_continuous_data()
    excluded<-intersect(names(res$data),res$options$ordered)
    if(!ncol(num_data)) {
      return(div(class="alert alert-info mb-0",
        if(length(excluded)==ncol(res$data))
          "Tous les indicateurs de ce modèle sont déclarés ordinaux (y compris les réponses binaires). Les tests de Shapiro–Wilk, de Lilliefors et d’Anderson–Darling ne s’appliquent pas à ces catégories observées. Aucun test de normalité univariée n’est donc calculé."
        else "Aucun indicateur continu numérique n’est disponible pour les tests de normalité univariée."))
    }
    tagList(
      if(length(excluded)) div(class="alert alert-info",
        paste0(length(excluded),if(length(excluded)==1) " indicateur ordinal exclu. " else " indicateurs ordinaux exclus. ","Le tableau porte uniquement sur les indicateurs continus (",ncol(num_data),").")),
      helpText("Ces tests signalent des écarts à la normalité; une absence de rejet ne prouve pas la normalité. Le résumé signale au moins un p < .05, sans correction pour comparaisons multiples."),
      DTOutput("normality_tests"))
  })
  output$normality_tests <- renderDT({
    num_data<-normality_continuous_data()
    req(ncol(num_data)>0)
    
    # Tests univariés uniquement
    tests <- lapply(names(num_data), function(var) {
      x <- num_data[[var]]
      x_clean <- na.omit(x)
      n <- length(x_clean)
      
      # Initialiser avec des NA
      sw_stat <- sw_p <- ks_stat <- ks_p <- ad_stat <- ad_p <- NA
      normalite <- "Non déterminé"
      
      # Vérifier les conditions pour chaque test
      if (n >= 3 && n <= 5000) {
        sw <- try(shapiro.test(x_clean), silent = TRUE)
        if (!inherits(sw, "try-error")) {
          sw_stat <- sw$statistic
          sw_p <- sw$p.value
        }
      }
      
      # Lilliefors tient compte de la moyenne et de la variance estimées
      if (n >= 5) {
        ks <- try(nortest::lillie.test(x_clean), silent = TRUE)
        if (!inherits(ks, "try-error")) {
          ks_stat <- ks$statistic
          ks_p <- ks$p.value
        }
      }
      
      # Anderson-Darling requiert au moins huit valeurs
      if (n >= 8) {
        ad <- try(suppressWarnings(nortest::ad.test(x_clean)), silent = TRUE)
        if (!inherits(ad, "try-error")) {
          ad_stat <- ad$statistic
          ad_p <- ad$p.value
        }
      }
      
      # Déterminer la normalité basée sur les tests disponibles
      p_values <- c(sw_p, ks_p, ad_p)
      p_values <- p_values[!is.na(p_values)]
      
      if (length(p_values) > 0) {
        normalite <- ifelse(min(p_values, na.rm = TRUE) > 0.05, "Aucun écart signalé", "Écart signalé")
      }
      
      data.frame(
        SW_stat = sw_stat,
        SW_p = sw_p,
        KS_stat = ks_stat,
        KS_p = ks_p,
        AD_stat = ad_stat,
        AD_p = ad_p,
        Normalité = normalite
      )
    })
    
    # Créer le data frame des tests univariés
    tests_df <- do.call(rbind, tests)
    rownames(tests_df) <- names(num_data)
    
    # Renommer les colonnes pour plus de clarté
    colnames(tests_df) <- c(
      "Shapiro-Wilk", "p(SW)",
      "Lilliefors", "p(Lillie)",
      "Anderson-D", "p(AD)",
      "Conclusion"
    )
    
    # Créer le tableau avec DT
    cfa_datatable(tests_df,
              options = dt_options) %>%
      formatRound(columns = c(1:6), digits = 3, dec.mark = ",") %>%
      formatStyle(
        'Conclusion',
        backgroundColor = styleEqual(
          c("Aucun écart signalé", "Écart signalé", "Non déterminé"),
          c('transparent', 'rgba(255,180,0,0.2)', 'transparent')
        )
      )
  })
  
  multivariate_normality_results <- reactive({
    req(model_results())
    withProgress(message="Calcul des cinq diagnostics de normalité…",value=.5,{ cfa_mvn_all(model_results()$data,ordered=model_results()$options$ordered) })
  })
  output$mvn_scope_note <- renderUI({div(class="alert alert-info",multivariate_normality_results()$note)})
  output$multivariate_normality <- renderDT({
    d<-multivariate_normality_results()$table
    if(all(!nzchar(d$Note))) d$Note<-NULL
    cfa_datatable(d,options=list(dom="t",paging=FALSE,ordering=FALSE),rownames=FALSE) %>%
      formatRound(columns="Statistique",digits=3, dec.mark = ",")
  })

  output$lavaan_code <- renderPrint({
    req(model_results())
    cat(cfa_r_script(model_results(),embed_data=FALSE),sep="\n")
  })

  stata_draft <- reactive({
    req(model_results()); r<-model_results()
    paste("* ÉBAUCHE à vérifier dans Stata : l’équivalence avec l’analyse lavaan n’est pas garantie.",
      "* Pour reproduire exactement cette estimation, utilisez le script R exporté.",
      lavaan_to_stata(r$syntax,estimator=r$options$estimator,ordinal_vars=r$options$ordered,
         missing=r$options$missing,std_lv=r$options$std.lv,n_factors=length(r$factor_vars),source_info=r$source),sep="\n")
  })
  output$stata_code <- renderPrint({ cat(stata_draft()) })
  output$download_code_stata <- downloadHandler(filename=function() "ebauche_cfa_stata.do",
    content=function(file) writeLines(stata_draft(),file),contentType="text/plain")

  output$factor_scores_table <- renderDT({
    req(model_results());d<-cfa_score_table(model_results(),input$scores_method)
    cfa_datatable(d,options=dt_options,rownames=FALSE) %>% formatRound(columns=seq.int(2,ncol(d)),digits=3, dec.mark = ",")
  })

  output$download_code <- downloadHandler(
    filename=function() "analyse_cfa.R",
    content=function(file) { req(model_results());writeLines(cfa_r_script(model_results()),file) },
    contentType="text/plain")

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("rapport_cfa_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      req(model_results())

      res <- model_results()

      # Paramètres transmis au rapport
      params_list <- list(
        fit          = model_results()$fit,
        fit_warnings = res$warnings,
        diagnostics = res$diagnostics,
        takeaways = cfa_takeaways(res),
        htmt_boot = if(is.null(htmt_boot_result())) NULL else htmt_boot_result()[c("table","note","type")],
        df           = res$data,
        model_syntax = res$syntax,
        estimator    = res$options$estimator,
        missing      = res$options$missing,
        std_lv       = res$options$std.lv,
        n_factors    = length(res$factor_vars),
        source_nom   = res$source$nom,
        display_std  = "std" %in% input$display_options,
        display_r2   = "r2"  %in% input$display_options,
        display_fit  = "fit" %in% input$display_options,
        mi_threshold = input$mi_threshold,
        cov_matrices = input$cov_matrices,
        cor_matrices = input$cor_matrices,
        source_info  = res$source
      )

      withProgress(message="Génération du rapport...", value=0.3, {
        tryCatch({
          cfa_render_report(file,params_list)
        }, error=function(e) {
          showNotification(
            paste("Erreur rapport :", e$message),
            type="error", duration=8
          )
          stop(e)
        })
      })
    },
    contentType = "text/html"
  )
  
  # Handler pour l'export des scores
  output$download_scores <- downloadHandler(
    filename = function() {
      paste0("scores_factoriels_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      req(model_results())
      write.csv(cfa_score_table(model_results(),input$scores_method),file,row.names=FALSE)
    }
  )
  
  observeEvent(list(input$reset,input$reset_constraints), {
    # Réinitialiser les inputs principaux
    n<-if(identical(input$import_method,"example")) input$example_n_factors else 1L
    vars<-if(identical(input$import_method,"example") && !is.null(simulation_info()) && length(simulation_info()$factor_vars)==n) unname(simulation_info()$factor_vars) else rep(list(character()),n)
    apply_model_plan(vars,paste0("F",seq_len(n)))
    error_covs(error_covs()[0,,drop=FALSE]);fixed_params(fixed_params()[0,,drop=FALSE])
    nested_results(NULL);htmt_boot_result(NULL);results_available(FALSE)
    updateCheckboxGroupInput(session,"nested_tests_hierarchical",selected=character())
    updateCheckboxGroupInput(session,"nested_tests_structural",selected=character())
    show_interpretation(FALSE);show_results(FALSE)
    updateRadioButtons(session, "estimator", selected = "ml")
    updateRadioButtons(session, "constraints", selected = "first_1")
    updateRadioButtons(session, "missing", selected = "listwise")
    updateCheckboxGroupInput(session, "display_options", selected = c("std"))
    updateNumericInput(session, "mi_threshold", value = Inf)
    
    # Réinitialiser les matrices
    updateCheckboxGroupInput(session, "cov_matrices", selected = character(0))
    updateCheckboxGroupInput(session, "cor_matrices", selected = character(0))
    
    # Réinitialiser les options du plot
    updateSelectInput(session, "plot_what_labels", selected = "path")
    updateSelectInput(session, "edge_weight", selected = "std")
    updateSelectInput(session, "layout", selected = "tree2")
    updateRadioButtons(session, "plot_style", selected = "lisrel")
    updateRadioButtons(session, "plot_rotation", selected = 2)
    
    # Réinitialiser les options d'apparence
    updateSelectInput(session, "shapeLat", selected = "circle")
    updateSelectInput(session, "shapeMan", selected = "rectangle")
    
    # Réinitialiser les sliders
    updateSliderInput(session, "node_size", value = 5)
    updateSliderInput(session, "node_label_scale", value = 1)
    updateSliderInput(session, "plot_nchar", value = 0)
    updateSliderInput(session, "label_size", value = 1)
    updateSliderInput(session, "error_scale", value = 1)
    updateSliderInput(session, "plot_curvature", value = 2)
    updateSliderInput(session, "resid_scale", value = 15)
    updateSliderInput(session, "intercept_scale", value = 2)
    
    # Afficher une notification
    showNotification("Modèle réinitialisé : anciennes contraintes effacées; données conservées",
                     type = "message")
  },ignoreInit=TRUE)
  
  # Liste pour stocker les covariances entre erreurs
  error_covs <- reactiveVal(data.frame(
    Var1 = character(),
    Var2 = character(),
    stringsAsFactors = FALSE
  ))
  
  # Liste pour stocker les paramètres fixes
  fixed_params <- reactiveVal(data.frame(
    Type = character(),
    Param = character(),
    Action = character(),  # Nouvelle colonne
    Valeur = character(),  # Changé en character pour accepter les noms
    stringsAsFactors = FALSE
  ))
  
  # Mise à jour des choix de variables pour les covariances
  observe({
    req(fileData())
    variables <- names(fileData())
    
    updateSelectInput(session, "error_cov_var1",
                      choices = variables)
    updateSelectInput(session, "error_cov_var2",
                      choices = variables)
  })
  
  # Ajouter une covariance entre erreurs
  observeEvent(input$add_error_cov, {
    req(input$error_cov_var1, input$error_cov_var2)
    
    current <- error_covs()
    new_row <- data.frame(
      Var1 = input$error_cov_var1,
      Var2 = input$error_cov_var2,
      stringsAsFactors = FALSE
    )
    error_covs(rbind(current, new_row))
  })
  
  # Ajouter un paramètre fixe
  observeEvent(input$add_fixed_param, {
    req(input$param_type)
    req(input$param_action)

    # Ensuite vérifier la valeur selon l'action
    if(input$param_action == "fix") {
      req(!is.null(input$fixed_value))
    } else {
      req(input$param_label)
    }
    
    # Créer le paramètre en fonction du type
    param_str <- switch(input$param_type,
                        "loading" = {
                          req(input$fixed_factor, input$fixed_var)
                          sprintf("%s=~%s", input$fixed_factor, input$fixed_var)
                        },
                        "covariance" = {
                          req(input$fixed_factor1, input$fixed_factor2)
                          if(input$fixed_factor1 == input$fixed_factor2) {
                            showNotification(
                              "Un facteur ne peut pas avoir une covariance fixe avec lui-même",
                              type = "warning"
                            )
                            return()
                          }
                          sprintf("%s~~%s", input$fixed_factor1, input$fixed_factor2)
                        },
                        "variance" = {
                          req(input$fixed_var_factor)
                          sprintf("%s~~%s", input$fixed_var_factor, input$fixed_var_factor)
                        }
    )
    
    # Vérifier si le paramètre existe déjà
    current <- fixed_params()
    if(nrow(current) > 0 && param_str %in% current$Param) {
      showNotification("Ce paramètre est déjà défini", type = "warning")
      return()
    }
    
    # Ajouter le nouveau paramètre
    new_row <- data.frame(
      Type = input$param_type,
      Param = param_str,
      Action = input$param_action,
      Valeur = if(input$param_action == "fix") input$fixed_value else input$param_label,
      stringsAsFactors = FALSE
    )
    
    fixed_params(rbind(current, new_row))
  })

  
  # Output pour le tableau des paramètres fixes
  output$fixed_params_table <- renderDT({
    req(fixed_params())
    display<-fixed_params()
    label_factor<-function(x) {i<-match(x,paste0("F",seq_along(factor_names())));if(is.na(i)) x else factor_names()[i]}
    if(nrow(display)) for(i in seq_len(nrow(display))) {
      parts<-strsplit(display$Param[i],"=~|~~")[[1]]
      if(length(parts)==2) display$Param[i]<-paste0(label_factor(parts[1]),if(display$Type[i]=="loading") " =~ " else " ~~ ",if(display$Type[i]=="loading") parts[2] else label_factor(parts[2]))
    }
    cfa_datatable(
      display,
      selection = 'multiple',
      options = list(
        dom = 't',
        pageLength = 50,
        ordering = FALSE
      )
    )
  })
  
  # Afficher les tableaux
  output$error_cov_table <- renderDT({
    cfa_datatable(error_covs(),
              selection = 'multiple',
              options = list(dom = 't'))
  })
  
  # Supprimer les éléments sélectionnés
  observeEvent(input$remove_error_cov, {
    req(input$error_cov_table_rows_selected)
    current <- error_covs()
    error_covs(current[-input$error_cov_table_rows_selected, ])
  })
  
  observeEvent(input$remove_fixed_param, {
    req(input$fixed_params_table_rows_selected)
    current <- fixed_params()
    fixed_params(current[-input$fixed_params_table_rows_selected, ])
  })
  
  # Mise à jour des choix pour les paramètres fixes
  observe({
    req(fileData(), input$n_factors, input$param_type)
    
    # Variables pour les loadings
    if (input$param_type == "loading") {
      updateSelectInput(session, "fixed_factor",
                        choices = setNames(paste0("F", 1:input$n_factors),factor_names()))
      updateSelectInput(session, "fixed_var",
                        choices = names(fileData()))
    }
    
    # Facteurs pour les covariances
    if (input$param_type == "covariance") {
      factors <- setNames(paste0("F", 1:input$n_factors),factor_names())
      updateSelectInput(session, "fixed_factor1",
                        choices = factors)
      updateSelectInput(session, "fixed_factor2",
                        choices = factors)
    }
    
    # Facteurs pour les variances
    if (input$param_type == "variance") {
      updateSelectInput(session, "fixed_var_factor",
                        choices = setNames(paste0("F", 1:input$n_factors),factor_names()))
    }
  })
  
  # Observer pour réagir aux changements de type de paramètre
  observeEvent(input$param_type, {
    req(fileData(), input$n_factors)
    
    # Mettre à jour les choix en fonction du type sélectionné
    switch(input$param_type,
           "loading" = {
             updateSelectInput(session, "fixed_factor",
                               choices = setNames(paste0("F", 1:input$n_factors),factor_names()))
             updateSelectInput(session, "fixed_var",
                               choices = names(fileData()))
           },
           "covariance" = {
             factors <- setNames(paste0("F", 1:input$n_factors),factor_names())
             updateSelectInput(session, "fixed_factor1",
                               choices = factors)
             updateSelectInput(session, "fixed_factor2",
                               choices = factors)
           },
           "variance" = {
             updateSelectInput(session, "fixed_var_factor",
                               choices = setNames(paste0("F", 1:input$n_factors),factor_names()))
           }
    )
  })
  
  # UI dynamique pour les paramètres fixes
  observeEvent(input$param_action, {
    req(input$param_action)
    # Forcer la mise à jour de param_inputs
    output$param_inputs <- renderUI({
      req(input$param_type, input$param_action, fileData())
      
      # Créer les inputs en fonction du type de paramètre
      switch(input$param_type,
             "loading" = {
               layout_columns(
                 col_widths = c(4, 4, 4),
                 selectInput("fixed_factor", "Facteur",
                             choices = setNames(paste0("F", 1:input$n_factors),factor_names()),
                             width = "100%"),
                 selectInput("fixed_var", "Variable",
                             choices = names(fileData()),
                             width = "100%"),
                 if(input$param_action == "fix") {
                   numericInput(
                     "fixed_value",
                     "Valeur fixe",
                     value = 1,
                     width = "100%"
                   )
                 } else {
                   textInput(
                     "param_label",
                     "Nom du paramètre",
                     value = "",
                     width = "100%"
                   )
                 }
               )
             },
             "covariance" = {
               layout_columns(
                 col_widths = c(4, 4, 4),
                 selectInput("fixed_factor1", "Facteur 1",
                             choices = setNames(paste0("F", 1:input$n_factors),factor_names()),
                             width = "100%"),
                 selectInput("fixed_factor2", "Facteur 2",
                             choices = setNames(paste0("F", 1:input$n_factors),factor_names()),
                             width = "100%"),
                 if(input$param_action == "fix") {
                   numericInput(
                     "fixed_value",
                     "Valeur fixe",
                     value = 1,
                     width = "100%"
                   )
                 } else {
                   textInput(
                     "param_label",
                     "Nom du paramètre",
                     value = "",
                     width = "100%"
                   )
                 }
               )
             },
             "variance" = {
               layout_columns(
                 col_widths = c(6, 6),
                 selectInput("fixed_var_factor", "Facteur",
                             choices = setNames(paste0("F", 1:input$n_factors),factor_names()),
                             width = "100%"),
                 if(input$param_action == "fix") {
                   numericInput(
                     "fixed_value",
                     "Valeur fixe",
                     value = 1,
                     width = "100%"
                   )
                 } else {
                   textInput(
                     "param_label",
                     "Nom du paramètre",
                     value = "",
                     width = "100%"
                   )
                 }
               )
             }
      )
    })
    
    # Ajouter un observateur pour changer le type d'input quand param_action change
    observeEvent(input$param_action, {
      # Ne faire la mise à jour que si param_action est défini
      req(input$param_action)
      
      if (input$param_action == "label") {
        # Remplacer le numericInput par un textInput
        insertUI(
          selector = "#fixed_value-label",
          where = "afterEnd",
          ui = textInput(
            "param_label",
            "Nom du paramètre",
            value = "",
            width = "100%"
          )
        )
        removeUI(selector = "#fixed_value")
      } else {
        # Remplacer le textInput par un numericInput
        insertUI(
          selector = "#param_label-label",
          where = "afterEnd",
          ui = numericInput(
            "fixed_value",
            "Valeur fixe",
            value = 1,
            width = "100%"
          )
        )
        removeUI(selector = "#param_label")
      }
    })
  })

  

  # Aide & Référence
  output$help_content <- renderUI({
    req(input$help_section)
    
    help_text <- switch(input$help_section,
                        "intro" = list(
                          h4("Introduction à l'Analyse Factorielle Confirmatoire"),
                          p("L'AFC est une méthode statistique qui permet de tester si un ensemble de variables mesurées reflète bien la structure factorielle théorique attendue."),
                          h5("Concepts clés"),
                          tags$ul(
                            tags$li(strong("Variables manifestes :"), "Variables directement observées/mesurées dans les données"),
                            tags$li(strong("Variables latentes :"), "Facteurs non directement observés, construits à partir des variables manifestes"),
                            tags$li(strong("Loadings :"), "Coefficients qui quantifient la relation entre variables manifestes et latentes"),
                            tags$li(strong("Erreurs de mesure :"), "Part de variance non expliquée par les facteurs")
                          ),
                          h5("Hypothèses"),
                          tags$ul(
                            tags$li("Linéarité des relations entre variables"),
                            tags$li("Taille d’échantillon à justifier selon les saturations, la complexité, l’estimateur et la précision recherchée"),
                            tags$li("Normalité multivariée (pour l'estimation ML)"),
                            tags$li("Absence de multicolinéarité")
                          ),
                          h5("Utilisation de cette application"),
                          p("L'application vous guide à travers les étapes suivantes :"),
                          tags$ol(
                            tags$li("Importation des données depuis différents formats"),
                            tags$li("Spécification du modèle et des facteurs"),
                            tags$li("Choix des paramètres d'estimation"),
                            tags$li("Analyse des résultats"),
                            tags$li("Visualisation du modèle"),
                            tags$li("Tests de modèles alternatifs")
                          ),
                          h5("Ressources en ligne"),
                          tags$ul(
                            tags$li(tags$a(href="https://lavaan.ugent.be/tutorial/", target="_blank", "Tutoriel officiel de lavaan")),
                            tags$li(tags$a(href="https://stats.oarc.ucla.edu/r/seminars/rcfa/", target="_blank", "UCLA - Introduction à l'AFC avec R")),
                            tags$li(tags$a(href="https://psico.fcep.urv.cat/utilitats/factor/", target="_blank", "FACTOR - Logiciel gratuit dédié à l'analyse factorielle")),
                            tags$li(tags$a(href="https://www.statmodel.com/features.shtml", target="_blank", "Mplus - Logiciel commercial populaire pour l'AFC et SEM")),
                            tags$li(tags$a(href="https://www.r-bloggers.com/2019/03/confirmatory-factor-analysis-in-r/", target="_blank", "R-Bloggers - Tutorial pratique sur l'AFC en R"))
                          )
                        ),
                        
                        "import" = list(
                          h4("Importation des données"),
                          h5("Formats de fichiers acceptés"),
                          tags$ul(
                            tags$li(strong("CSV :"), "Fichiers délimités par des virgules ou points-virgules"),
                            tags$li(strong("SPSS (.sav) :"), "Fichiers SPSS avec métadonnées"),
                            tags$li(strong("Stata (.dta) :"), "Fichiers Stata versions 8-15"),
                            tags$li(strong("Excel (.xlsx, .xls) :"), "Classeurs Excel")
                          ),
                          h5("Structure requise des données"),
                          p("Les données doivent être organisées selon les principes suivants :"),
                          tags$ul(
                            tags$li("Une ligne par observation/participant"),
                            tags$li("Une colonne par variable"),
                            tags$li("Noms de variables dans la première ligne"),
                            tags$li("Variables numériques pour l'analyse")
                          ),
                          h5("Gestion des valeurs manquantes"),
                          p("L'application propose deux approches :"),
                          tags$ul(
                            tags$li(strong("Listwise deletion :"), "Supprime les observations avec données manquantes"),
                            tags$li(strong("FIML :"), "Full Information Maximum Likelihood, utilise toute l'information disponible")
                          ),
                          h5("Bonnes pratiques"),
                          tags$ul(
                            tags$li("Vérifier l'encodage des fichiers CSV"),
                            tags$li("S'assurer que les variables sont correctement typées"),
                            tags$li("Examiner les statistiques descriptives après importation"),
                            tags$li("Vérifier la présence de valeurs aberrantes")
                          ),
                          h5("Ressources supplémentaires"),
                          tags$ul(
                            tags$li(tags$a(href="https://guides.library.duke.edu/c.php?g=289704&p=1930833", target="_blank", "Duke University - Guide des formats de données")),
                            tags$li(tags$a(href="https://r4ds.hadley.nz/data-import", target="_blank", "R for Data Science - Importation de données")),
                            tags$li(tags$a(href="https://cran.r-project.org/web/packages/rio/vignettes/rio.html", target="_blank", "Package rio - Importation simplifiée en R")),
                            tags$li(tags$a(href="https://readxl.tidyverse.org/", target="_blank", "Package readxl - Importer des fichiers Excel"))
                          )
                        ),
                        
                        "model" = list(
                          h4("Spécification du modèle"),
                          h5("Composantes du modèle"),
                          tags$ul(
                            tags$li(strong("Relations de mesure :"), 
                                    "Liens entre variables manifestes et facteurs latents"),
                            tags$li(strong("Structure factorielle :"), 
                                    "Organisation et relations entre facteurs"),
                            tags$li(strong("Paramètres :"), 
                                    "Loadings, variances, covariances")
                          ),
                          h5("Identification du modèle"),
                          p("Deux méthodes principales pour l'identification :"),
                          tags$ul(
                            tags$li(strong("Variance des facteurs = 1 :"),
                                    "Standardise les facteurs latents, facilite l'interprétation"),
                            tags$li(strong("Premier loading = 1 :"),
                                    "Fixe l'échelle via un indicateur de référence")
                          ),
                          h5("Types de contraintes"),
                          tags$ul(
                            tags$li(strong("Égalité entre paramètres :"), 
                                    "Force des paramètres à être égaux"),
                            tags$li(strong("Fixation de valeurs :"), 
                                    "Assigne une valeur spécifique à un paramètre"),
                            tags$li(strong("Covariances :"), 
                                    "Spécifie les relations entre facteurs ou erreurs")
                          ),
                          h5("Modèles spéciaux"),
                          tags$ul(
                            tags$li(strong("Tau-équivalent :"), 
                                    "Loadings égaux pour un même facteur"),
                            tags$li(strong("Parallèle :"), 
                                    "Loadings et variances d'erreur égaux"),
                            tags$li(strong("Congeneric :"), 
                                    "Loadings libres, modèle le plus flexible")
                          ),
                          h5("Tutoriels et exemples"),
                          tags$ul(
                            tags$li(tags$a(href="https://lavaan.ugent.be/tutorial/syntax1.html", target="_blank", "Guide de syntaxe lavaan")),
                            tags$li(tags$a(href="https://lavaan.ugent.be/tutorial/cfa.html", target="_blank", "Exemples de modèles CFA avec lavaan")),
                            tags$li(tags$a(href="https://psyarxiv.com/hgv5a/", target="_blank", "Guide complet de la syntaxe lavaan pour l'AFC")),
                            tags$li(tags$a(href="https://m-clark.github.io/sem/confirmatory-factor-analysis.html", target="_blank", "Tutoriel d'AFC avec visualisations"))
                          )
                        ),
                        
                        "estimation" = list(
                          h4("Méthodes d'estimation"),
                          h5("Maximum de vraisemblance (ML)"),
                          tags$ul(
                            tags$li(strong("Avantages :"), 
                                    "Efficace, précis avec grands échantillons"),
                            tags$li(strong("Hypothèses :"), 
                                    "Normalité multivariée, échantillon suffisant"),
                            tags$li(strong("Variantes :"), 
                                    "ML robuste, ML bootstrappé")
                          ),
                          h5("DWLS (Diagonally Weighted Least Squares)"),
                          tags$ul(
                            tags$li(strong("Usage :"), 
                                    "Recommandé pour variables ordinales"),
                            tags$li(strong("Avantages :"), 
                                    "Robuste à la non-normalité"),
                            tags$li(strong("Limitations :"), 
                                    "Peut nécessiter de grands échantillons")
                          ),
                          h5("ULS (Unweighted Least Squares)"),
                          tags$ul(
                            tags$li(strong("Caractéristiques :"), 
                                    "Pas d'hypothèse de distribution"),
                            tags$li(strong("Avantages :"), 
                                    "Stable avec petits échantillons"),
                            tags$li(strong("Limitations :"), 
                                    "Moins efficace que ML")
                          ),
                          h5("Conseils de choix"),
                          tags$ul(
                            tags$li("ML : données continues, normales"),
                            tags$li("DWLS : variables ordinales"),
                            tags$li("ULS : petits échantillons, non-normalité")
                          ),
                          h5("Ressources techniques"),
                          tags$ul(
                            tags$li(tags$a(href="https://lavaan.ugent.be/tutorial/est.html", target="_blank", "Options d'estimation dans lavaan")),
                            tags$li(tags$a(href="https://www.tandfonline.com/doi/full/10.1080/10705511.2016.1186549", target="_blank", "Comparaison des estimateurs pour l'AFC")),
                            tags$li(tags$a(href="https://doi.org/10.1037/met0000201", target="_blank", "Estimateurs robustes pour données non-normales")),
                            tags$li(tags$a(href="https://doi.org/10.1080/10705511.2018.1449111", target="_blank", "Bootstrapping dans les modèles SEM"))
                          )
                        ),
                        "fit" = list(
                          h4("Indices d'ajustement"),
                          h5("Indices absolus"),
                          tags$ul(
                            tags$li(strong("Chi-carré :"), 
                                    "Test l'écart au modèle parfait. Non significatif souhaité."),
                            tags$li(strong("RMSEA :"), 
                                    "< 0.06 bon ajustement, < 0.08 acceptable"),
                            tags$li(strong("SRMR :"), 
                                    "< 0.08 indique un bon ajustement")
                          ),
                          h5("Indices incrémentaux"),
                          tags$ul(
                            tags$li(strong("CFI :"), 
                                    "> 0.95 bon ajustement, > 0.90 acceptable"),
                            tags$li(strong("TLI :"), 
                                    "> 0.95 bon ajustement, > 0.90 acceptable"),
                            tags$li(strong("NFI :"), 
                                    "> 0.95 indique un bon ajustement")
                          ),
                          h5("Indices de parcimonie"),
                          tags$ul(
                            tags$li(strong("AIC :"), 
                                    "Compare des modèles non-nichés"),
                            tags$li(strong("BIC :"), 
                                    "Pénalise plus fortement la complexité")
                          ),
                          h5("Interprétation"),
                          p("Points importants à considérer :"),
                          tags$ul(
                            tags$li("Ne pas se fier à un seul indice"),
                            tags$li("Tenir compte de la taille d'échantillon"),
                            tags$li("Considérer la complexité du modèle"),
                            tags$li("Examiner les résidus")
                          ),
                          h5("Guides d'interprétation"),
                          tags$ul(
                            tags$li(tags$a(href="https://stats.idre.ucla.edu/stat/r/seminars/rcfa/#3", target="_blank", "Guide UCLA pour les indices d'ajustement")),
                            tags$li(tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6124398/", target="_blank", "Revue complète des indices d'ajustement (article)"))
                          )
                        ),
                        
                        "ordinal" = list(
                          h4("Traitement des variables ordinales"),
                          h5("Particularités"),
                          tags$ul(
                            tags$li("Variables catégorielles ordonnées"),
                            tags$li("Échelles de Likert"),
                            tags$li("Scores discrets")
                          ),
                          h5("Adaptations nécessaires"),
                          tags$ul(
                            tags$li(strong("Estimateur :"), 
                                    "WLSMV proposé par défaut pour les indicateurs déclarés ordinaux; DWLS/ULS/WLS également disponibles"),
                            tags$li(strong("Matrices :"), 
                                    "Utilisation de corrélations polychoriques"),
                            tags$li(strong("Seuils :"), 
                                    "Estimation des seuils de catégories")
                          ),
                          h5("Recommandations"),
                          tags$ul(
                            tags$li("Le nombre de catégories ne suffit pas pour justifier un traitement continu : examiner aussi l’asymétrie et les fréquences"),
                            tags$li("Vérifier la distribution des réponses"),
                            tags$li("Attention aux catégories peu fréquentes")
                          ),
                          h5("Indices d'ajustement spécifiques"),
                          tags$ul(
                            tags$li(strong("WRMR :"), "aucun seuil universel retenu ici"),
                            tags$li(strong("Indices robustes :"), "Utiliser CFI et RMSEA robustes"),
                            tags$li(strong("Modification Indices :"), "Interpréter avec prudence")
                          ),
                          h5("Ressources pour variables ordinales"),
                          tags$ul(
                            tags$li(tags$a(href="https://doi.org/10.1080/10705511.2016.1186549", target="_blank", 
                                           "Li (2016) - DWLS vs Robust ML pour données ordinales")),
                            tags$li(tags$a(href="https://doi.org/10.1037/a0029315", target="_blank", 
                                           "Rhemtulla et al. (2012) - Quand les variables catégorielles peuvent être traitées comme continues?"))
                          )
                        ),
                        
                        "tests" = list(
                          h4("Tests de modèles"),
                          h5("Modèles hiérarchiques"),
                          tags$ul(
                            tags$li(strong("Modèle tau-équivalent :"), 
                                    "Contrainte d'égalité sur les loadings d'un même facteur"),
                            tags$li(strong("Modèle parallèle :"), 
                                    "Contraintes sur loadings et variances d'erreur"),
                            tags$li(strong("Modèle congeneric :"), 
                                    "Aucune contrainte d'égalité (modèle le plus flexible)")
                          ),
                          h5("Tests de modèles imbriqués"),
                          tags$ul(
                            tags$li("Comparaison entre modèles avec différents niveaux de contraintes"),
                            tags$li("Utilisation du test du rapport de vraisemblance (LRT)"),
                            tags$li("Avec MLR, lavaan calcule la différence robuste. Les comparaisons d’égalité proposées ici sont réservées aux indicateurs continus.")
                          ),
                          h5("Comparaison de modèles non-imbriqués"),
                          tags$ul(
                            tags$li("Utilisation des indices AIC et BIC"),
                            tags$li("L’application n’effectue pas de test de Vuong; l’unidimensionnalité n’est pas testée ici par une différence usuelle de chi carré")
                          ),
                          h5("Stratégies de test"),
                          tags$ul(
                            tags$li("Tester d'abord le modèle le moins restrictif"),
                            tags$li("Ajouter progressivement des contraintes"),
                            tags$li("Examiner les indices de modification après chaque étape")
                          ),
                          h5("Ressources sur les tests de modèles"),
                          tags$ul(
                            tags$li(tags$a(href="https://doi.org/10.1037/1082-989X.8.3.369", target="_blank", 
                                           "Cheung & Rensvold (2002) - Évaluer l'invariance de mesure avec CFI")),
                            tags$li(tags$a(href="https://doi.org/10.1080/10705511.2014.919210", target="_blank", 
                                           "Beaujean (2014) - Tests de modèles latents dans R"))
                          )
                        ),
                        
                        "interpret" = list(
                          h4("Interprétation des résultats"),
                          h5("Loadings standardisés"),
                          tags$ul(
                            tags$li(strong("> 0.70 :"), "Excellent"),
                            tags$li(strong("0.55 - 0.70 :"), "Bon"),
                            tags$li(strong("0.45 - 0.55 :"), "Correct"),
                            tags$li(strong("< 0.45 :"), "Faible")
                          ),
                          h5("R² (Communalités)"),
                          tags$ul(
                            tags$li(strong("> 0.50 :"), "Bonne variance expliquée"),
                            tags$li(strong("0.30 - 0.50 :"), "Modérée"),
                            tags$li(strong("< 0.30 :"), "Faible")
                          ),
                          h5("Fiabilité"),
                          tags$ul(
                            tags$li(strong("Alpha de Cronbach :"), "repère descriptif .70, selon le score et son usage"),
                            tags$li(strong("Oméga :"), "Alternative moderne à l'alpha"),
                            tags$li(strong("Composite Reliability :"), "repère descriptif .70, selon le score et son usage"),
                            tags$li(strong("Average Variance Extracted :"), "repère descriptif .50, sans preuve suffisante de validité")
                          ),
                          h5("Indices d'ajustement"),
                          tags$ul(
                            tags$li(strong("Chi-carré :"), "Non significatif (p > 0.05)"),
                            tags$li(strong("RMSEA :"), "repères .06 et .08 à contextualiser"),
                            tags$li(strong("SRMR :"), "< 0.08 acceptable"),
                            tags$li(strong("CFI/TLI :"), "repères .95 et .90 à contextualiser")
                          ),
                          h5("Ressources d'interprétation"),
                          tags$ul(
                            tags$li(tags$a(href="https://doi.org/10.1037/1082-989X.12.1.58", target="_blank", 
                                           "Marsh et al. (2004) - Interprétation des indices d'ajustement")),
                            tags$li(tags$a(href="https://doi.org/10.1080/00273171.2012.715555", target="_blank", 
                                           "McNeish et al. (2018) - Alternatives modernes à l'alpha de Cronbach"))
                          )
                        ),
                        
                        "advanced" = list(
                          h4("Options avancées"),
                          h5("Gestion des valeurs manquantes"),
                          tags$ul(
                            tags$li(strong("FIML :"), "Pour indicateurs continus avec ML/ML robuste; hypothèses MAR et modèle correctement spécifié à examiner"),
                            tags$li(strong("Multiple Imputation :"), "Alternative robuste pour certains scénarios"),
                            tags$li(strong("Listwise deletion :"), "À éviter sauf si données MCAR")
                          ),
                          h5("Modèles avec variances-covariances structurées"),
                          tags$ul(
                            tags$li("Contraintes d'égalité entre variances d'erreur"),
                            tags$li("Covariances entre erreurs de mesure"),
                            tags$li("Contraintes sur les covariances entre facteurs")
                          ),
                          h5("Contraintes avancées"),
                          tags$ul(
                            tags$li("Contraintes non-linéaires sur les paramètres"),
                            tags$li("Contraintes d'inégalité"),
                            tags$li("Modèles avec mesures répétées")
                          ),
                          h5("Ressources avancées"),
                          tags$ul(
                            tags$li(tags$a(href="https://doi.org/10.1080/10705511.2016.1269606", target="_blank", 
                                           "McNeish (2018) - Alternatives modernes aux méthodes traditionnelles")),
                            tags$li(tags$a(href="https://lavaan.ugent.be/tutorial/syntax2.html", target="_blank", 
                                           "Tutoriel avancé de syntaxe lavaan")),
                            tags$li(tags$a(href="https://doi.org/10.1080/10705511.2017.1402334", target="_blank", 
                                           "Rosseel et al. (2017) - Nouvelles fonctionnalités dans lavaan"))
                          )
                        ),
                        
                        "diagnostic" = list(
                          h4("Diagnostics du modèle"),
                          h5("Normalité multivariée"),
                          tags$ul(
                            tags$li("Test de Mardia"),
                            tags$li("Test de Henze-Zirkler"),
                            tags$li("QQ-plots multivariés")
                          ),
                          h5("Détection des outliers"),
                          tags$ul(
                            tags$li("Distance de Mahalanobis"),
                            tags$li("Indices d'influence"),
                            tags$li("Résidus standardisés")
                          ),
                          h5("Résidus et modification indices"),
                          tags$ul(
                            tags$li("Patterns de résidus élevés"),
                            tags$li("Interprétation des indices de modification"),
                            tags$li("Modification empirique vs. théorique")
                          ),
                          h5("Ressources pour diagnostics"),
                          tags$ul(
                            tags$li(tags$a(href="https://doi.org/10.1080/10705510701575396", target="_blank", 
                                           "Yuan & Bentler (2006) - Diagnostics structurels")),
                            tags$li(tags$a(href="https://doi.org/10.1037/met0000224", target="_blank", 
                                           "Jorgensen et al. (2018) - Évaluation de modèles mal spécifiés")),
                            tags$li(tags$a(href="https://cran.r-project.org/web/packages/MVN/", target="_blank", 
                                           "Package MVN - Tests de normalité multivariée dans R"))
                          )
                        ),
                        
                        "examples" = list(
                          h4("Exemples pratiques d'AFC"),
                          h5("Exemple unidimensionnel"),
                          tags$pre(
                            "# Modèle à un facteur (4 indicateurs)\nmodel <- '\nF1 =~ x1 + x2 + x3 + x4\n'\nfit <- cfa(model, data = data)\nsummary(fit, fit.measures = TRUE, standardized = TRUE)"
                          ),
                          h5("Exemple à deux facteurs corrélés"),
                          tags$pre(
                            "# Modèle à deux facteurs corrélés\nmodel <- '\nF1 =~ x1 + x2 + x3\nF2 =~ x4 + x5 + x6\nF1 ~~ F2\n'\nfit <- cfa(model, data = data)\nsummary(fit, standardized = TRUE)"
                          ),
                          h5("Exemple avec variables ordinales"),
                          tags$pre(
                            "# Modèle avec variables ordinales\nmodel <- '\nF1 =~ x1 + x2 + x3 + x4\n'\nfit <- cfa(model, data = data, ordered = c('x1', 'x2', 'x3', 'x4'), estimator = 'DWLS')\nsummary(fit, fit.measures = TRUE)"
                          ),
                          h5("Ressources et tutoriels"),
                          tags$ul(
                            tags$li(tags$a(href="https://lavaan.ugent.be/tutorial/index.html", target="_blank", 
                                           "Tutoriel officiel lavaan")),
                            tags$li(tags$a(href="https://rpubs.com/mkearney/lavaan", target="_blank", 
                                           "Exemple détaillé d'AFC avec lavaan")),
                            tags$li(tags$a(href="https://bookdown.org/bean_jerry/using_r_for_social_work_research/confirmatory-factor-analysis.html", target="_blank", 
                                           "Guide pratique d'AFC en sciences sociales")),
                            tags$li(tags$a(href="https://github.com/tdienlin/SEM-in-R", target="_blank", 
                                           "Ressources pour SEM dans R (GitHub)")),
                            tags$li(tags$a(href="https://psyteachr.github.io/sem-book/confirmatory-factor-analysis.html", target="_blank", 
                                           "Manuel en ligne d'AFC en psychologie"))
                          )
                        )
    )
    
    help_text
  })
                        

  
  # Fonction pour calculer les outliers multivariés
  detect_outliers <- reactive({
    req(activeData())
    
    # Obtenir les variables sélectionnées de tous les facteurs
    selected_vars <- names(activeData())

    # Si aucune variable n'est sélectionnée, retourner NULL
    if(length(selected_vars) == 0) return(NULL)
    
    # Sélectionner uniquement les variables choisies
    data <- activeData()[selected_vars]
    
    # Vérifier que toutes les variables sont numériques
    if(!all(sapply(data, is.numeric))) {
      showNotification("Toutes les variables doivent être numériques pour la détection d'outliers",
                       type = "warning")
      return(NULL)
    }
    
    # Supprimer les lignes avec des valeurs manquantes
    data <- na.omit(data)
    
    # S'assurer qu'il y a suffisamment d'observations
    if(nrow(data) < ncol(data) + 1) {
      showNotification("Pas assez d'observations pour la détection d'outliers multivariés",
                       type = "error")
      return(NULL)
    }
    
    # Calculer selon la méthode choisie
    if(input$outliers_method == "mahalanobis") {
      # Calculer les distances de Mahalanobis
      md <- mahalanobis(data, 
                        center = colMeans(data),
                        cov = cov(data))
      
      # Calculer les p-values
      p_values <- pchisq(md, df = ncol(data), lower.tail = FALSE)
      
      # Identifier les outliers
      outliers <- p_values < input$outliers_cutoff
      
      list(
        method = "Mahalanobis",
        distances = md,
        p_values = p_values,
        outliers = outliers,
        data = data,
        cutoff = qchisq(1 - input$outliers_cutoff, df = ncol(data))
      )
      
    } else if(input$outliers_method == "hadi") {
      tryCatch({
        robust <- with_cfa_seed(123,MASS::cov.rob(as.matrix(data),method="mcd"))
        md <- mahalanobis(data,center=robust$center,cov=robust$cov)
        pv <- pchisq(md,df=ncol(data),lower.tail=FALSE)
        list(method="Mahalanobis robuste (MCD; repère chi carré approximatif)",distances=md,
             p_values=pv,outliers=pv<input$outliers_cutoff,data=data,
             cutoff=qchisq(1-input$outliers_cutoff,df=ncol(data)))
      },error=function(e) {
        showNotification(paste("Diagnostic MCD indisponible :",conditionMessage(e)),type="warning")
        NULL
      })
    }
  })


  output$outliers_summary <- renderPrint({
    req(detect_outliers())
    result <- detect_outliers()
    
    cat("Résumé de la détection d'outliers multivariés\n")
    cat("--------------------------------------------\n")
    cat(sprintf("Méthode : %s\n", result$method))
    cat(sprintf("Nombre de variables : %d\n", ncol(result$data)))
    cat(sprintf("Nombre d'observations : %d\n", nrow(result$data)))
    cat(sprintf("Valeur seuil : %.3f\n", result$cutoff))
    cat(sprintf("Nombre d'outliers détectés : %d (%.1f%%)\n",
                sum(result$outliers),
                100 * sum(result$outliers) / length(result$outliers)))
  })
  
  # Output pour le tableau des outliers
  # Output pour le tableau des outliers
  output$outliers_table <- renderDT({
    req(detect_outliers())
    result <- detect_outliers()
    
    # S'assurer que toutes les composantes ont la même longueur
    n_obs <- length(result$distances)
    
    # Créer un data frame avec les résultats en s'assurant que tous les vecteurs ont la même longueur
    outliers_df <- data.frame(
      Observation = seq_len(n_obs),
      Distance = result$distances,
      P_value = if(result$method == "Mahalanobis") {
        result$p_values
      } else {
        rep(NA_real_, n_obs)
      },
      Outlier = if(length(result$outliers) == n_obs) {
        result$outliers
      } else {
        rep(FALSE, n_obs)
      },
      row.names = NULL
    )
    
    # Limiter le nombre d'observations à afficher
    max_rows <- 100
    
    # Trier par distance décroissante et limiter le nombre de lignes
    outliers_df <- outliers_df[order(-outliers_df$Distance), ]
    outliers_df <- head(outliers_df, max_rows)
    
    # Créer le tableau
    cfa_datatable(outliers_df,
              rownames = FALSE,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'tip',
                language = list(
                  info = paste("Affichage des", max_rows, "observations les plus extrêmes")
                )
              )) %>%
      formatRound(columns = c("Distance", "P_value"), digits = 4, dec.mark = ",") %>%
      formatStyle(
        'Outlier',
        backgroundColor = styleEqual(
          c(TRUE, FALSE),
          c('rgba(255, 0, 0, 0.2)', 'rgba(255, 255, 255, 0)')
        )
      )
  })
  
  # Observer pour les tests de modèles imbriqués
  observeEvent(input$run_nested, {
    req(model_results()); res<-model_results()
    selected<-unique(c(input$nested_tests_hierarchical,input$nested_tests_structural))
    results<-list()
    for(kind in selected) {
      desc<-"Restriction non applicable."
      test<-tryCatch({
        restriction<-cfa_restrict(res$fit,kind); desc<-restriction$description
        constrained<-cfa_fit(restriction$table,res$data,res$options)$fit
        if(lavaan::fitMeasures(constrained,"df")<=lavaan::fitMeasures(res$fit,"df")) stop("Aucun degré de liberté supplémentaire : test non effectué.")
        lavaan::lavTestLRT(res$fit,constrained)
      },error=function(e) conditionMessage(e))
      results[[kind]]<-list(test=test,base_model=res$syntax,
         constrained_model=paste("Modèle de base conservé; égalités supplémentaires :",desc,sep="\n"))
    }
    nested_results(results)
  })

  # Output pour afficher les résultats des tests
  output$nested_results <- renderUI({
    req(nested_results())
    results <- nested_results()
    
    if(length(results) == 0) {
      return(p("Aucun test sélectionné"))
    }
    
    result_cards <- lapply(names(results), function(test_name) {
      result <- results[[test_name]]
      
      test_title <- switch(test_name,
                           "tau" = "Test de tau-équivalence",
                           "homo" = "Test d'homoscédasticité",
                           "parallel" = "Test d’égalité des saturations et des erreurs",
                           "intercepts" = "Test d'égalité des intercepts",
                           "means" = "Test d'égalité des moyennes factorielles",
                           "var_equal" = "Test d'égalité des variances des facteurs",
                           "cov_equal" = "Test d'égalité des covariances",
                           "unidim" = "Test d'unidimensionnalité")
      
      # Contenu de base
      content <- list(
        h5("Modèle de base (sans contrainte) :"),
        verbatimTextOutput(paste0("base_model_", test_name)),
        hr(),
        h5("Modèle contraint :"),
        verbatimTextOutput(paste0("constrained_model_", test_name)),
        hr(),
        h5("Résultats du test :"),
        verbatimTextOutput(paste0("test_result_", test_name))
      )
      
      card(
        card_header(test_title),
        card_body(
          content
        )
      )
    })
    
    div(
      class = "mt-3",
      result_cards
    )
  })
  
  # Outputs dynamiques pour chaque test
  observe({
    req(nested_results())
    results <- nested_results()
    
    for(test_name in names(results)) {
      local({
        test_name <- test_name
        
        # Output pour le modèle de base
        output[[paste0("base_model_", test_name)]] <- renderPrint({
          cat(results[[test_name]]$base_model)
        })
        
        # Output pour le modèle contraint
        output[[paste0("constrained_model_", test_name)]] <- renderPrint({
          cat(results[[test_name]]$constrained_model)
        })
        
        # Output pour les résultats du test
        output[[paste0("test_result_", test_name)]] <- renderPrint({
          print(results[[test_name]]$test)
        })
      })
    }
  })
  
  output$nested_tests_hierarchical_ui <- renderUI({
    choices_hierarchical <- c(
      "Tau-équivalence essentielle (saturations)" = "tau",
      "Homoscédasticité" = "homo",
      "Parallélisme (saturations et erreurs)" = "parallel"
    )
    
    # Ajouter les tests de moyennes si la structure des moyennes est estimée
    if ("means" %in% input$display_options) {
      choices_hierarchical <- c(
        choices_hierarchical,
        "Égalité des intercepts (par facteur)" = "intercepts"
      )
    }
    
    checkboxGroupInput(
      "nested_tests_hierarchical",
      "Tests hiérarchiques",
      choices = choices_hierarchical
    )
  })
  # Tests structurels
  output$nested_tests_structural_ui <- renderUI({
    # Pour l'égalité des variances, il faut au moins 2 facteurs
    # Pour l'égalité des covariances, il faut au moins 3 facteurs
    choices <- list(
      "Unidimensionnalité" = "unidim"
    )
    
    if (length(model_results()$factor_vars) >= 2) {
      choices[["Égalité des variances des facteurs"]] <- "var_equal"
    }
    
    if (length(model_results()$factor_vars) >= 3) {
      choices[["Égalité des covariances entre facteurs"]] <- "cov_equal"
    }
    
    checkboxGroupInput(
      "nested_tests_structural",
      "Tests structurels",
      choices = choices
    )
  })
  # Prévisualisation de l'exemple sélectionné
  output$example_preview <- renderPrint({
    req(input$example_choice)
    
    descriptions <- list(
      example_1factor = list(
        model = "F1 =~ x1 + x2 + x3 + x4",
        description = "Modèle à un facteur avec 4 indicateurs"
      ),
      example_2factors = list(
        model = "F1 =~ x1 + x2 + x3\nF2 =~ x4 + x5 + x6\nF1 ~~ F2",
        description = "Modèle à deux facteurs corrélés"
      ),
      example_constraints = list(
        model = "F1 =~ x1 + a*x2 + a*x3\nF2 =~ y1 + b*y2 + b*y3\nF1 ~~ F2",
        description = "Modèle avec contraintes d'égalité"
      )
    )
    
    selected <- descriptions[[input$example_choice]]
    cat("Description :\n")
    cat(selected$description, "\n\n")
    cat("Syntaxe du modèle :\n")
    cat(selected$model, "\n")
  })
  
  validity_results <- reactive({ req(model_results()); tryCatch(cfa_validity(model_results()$fit),error=function(e) validate(need(FALSE,conditionMessage(e)))) })
  output$reliability_indices_table <- renderDT({
    r<-validity_results()$results
    cols<-c("Facteur","Alpha","Omega","AVE","H","N_alpha")
    cfa_datatable(r[,cols],options=list(dom="t",paging=FALSE,ordering=FALSE),rownames=FALSE) %>%
      formatRound(columns=c("Alpha","Omega","AVE","H"),digits=3, dec.mark = ",")
  })

  # Observer pour le résumé du fichier
  observe({
    output$file_summary_compact <- renderUI({
      # S'assurer que les données sont disponibles
      if (is.null(fileData())) return(NULL)
      
      # Fonction pour déterminer le type de variable
      determine_var_type <- function(x) {
        if (!is.numeric(x)) return("texte")
        
        unique_values <- unique(na.omit(x))
        
        # Critères plus stricts pour considérer comme catégorielle
        is_integer <- all(abs(unique_values - round(unique_values)) < .Machine$double.eps^0.5)
        
        # Augmenter le seuil ou ajouter d'autres critères
        if (is_integer && length(unique_values) <= 7 && all(unique_values %in% 1:10)) {
          return("catégorielle")
        } else {
          return("numérique")
        }
      }
      
      # Calculer les statistiques
      var_types <- sapply(fileData(), determine_var_type)
      n_vars <- ncol(fileData())
      n_obs <- nrow(fileData())
      n_numeric <- sum(var_types == "numérique")
      n_categorical <- sum(var_types == "catégorielle")
      n_text <- sum(var_types == "texte")
      n_missing <- sum(is.na(fileData()))
      missing_percent <- round(100 * n_missing / (n_vars * n_obs), 1)
      
      # Créer l'affichage
      div(
        h6("Caractéristiques", class = "mb-3"),
        div(
          style = "font-size: 0.9em;",
          div(
            class = "mb-2",
            strong("Variables : "), span(n_vars)
          ),
          div(
            class = "mb-2",
            strong("Observations : "), span(n_obs)
          ),
          div(
            class = "mb-2",
            strong("Variables numériques : "), span(n_numeric)
          ),
          div(
            class = "mb-2",
            strong("Variables catégorielles : "), span(n_categorical)
          ),
          if(n_text > 0) {
            div(
              class = "mb-2",
              strong("Variables texte : "), span(n_text)
            )
          },
          if(n_missing > 0) {
            div(
              class = "mb-2",
              strong("Valeurs manquantes : "), 
              span(sprintf("%d (%0.1f%%)", n_missing, missing_percent))
            )
          }
        )
      )
    })
  })
  
  output$progress_message <- renderText({
    # S'assurer qu'on a une seule valeur numérique
    progress_value <- as.numeric(input$analysis_progress)[1]
    if(is.na(progress_value)) return("En cours...")
    
    # Arrondir la valeur pour éviter les problèmes avec les nombres décimaux
    progress_value <- round(progress_value)
    
    # Utiliser une fonction case plutôt que switch
    case_when(
      progress_value == 0 ~ "Initialisation...",
      progress_value == 5 ~ "Vérification initiale...",
      progress_value == 10 ~ "Vérification du modèle...",
      progress_value == 20 ~ "Vérification des variables...",
      progress_value == 30 ~ "Vérification des variables ordinales...",
      progress_value == 40 ~ "Préparation des données...",
      progress_value == 50 ~ "Vérification des valeurs manquantes...",
      progress_value == 60 ~ "Vérification de la matrice de covariance...",
      progress_value == 70 ~ "Vérification des observations...",
      progress_value == 80 ~ "Configuration du modèle...",
      progress_value == 90 ~ "Estimation du modèle...",
      progress_value == 100 ~ "Finalisation...",
      TRUE ~ "En cours..."
    )
  })
  
  output$model_plot_header <- renderUI({
    div(
      class = "d-flex justify-content-between align-items-center w-100",
      h4("Diagramme du modèle", class = "m-0"),
      div(
        class = "d-flex align-items-center",
        actionButton("plot_options_btn", "Options", 
                     class = "btn-sm btn-light me-2"),
        downloadButton("download_plot", "Télécharger le graphique", 
                       class = "btn-light")
      )
    )
  })
  
  plot_download_width <- reactiveVal(1200)
  plot_download_height <- reactiveVal(900)
  plot_download_res <- reactiveVal(120)
  
  observeEvent(input$plot_options_btn, {
    showModal(modalDialog(
      title = "Options d'exportation du graphique",
      
      numericInput("plot_export_width", "Largeur (pixels)",
                   value = plot_download_width(),
                   min = 600, max = 4000, step = 100),
      
      numericInput("plot_export_height", "Hauteur (pixels)",
                   value = plot_download_height(),
                   min = 600, max = 4000, step = 100),
      
      numericInput("plot_export_res", "Résolution (dpi)",
                   value = plot_download_res(),
                   min = 72, max = 600, step = 10),
      
      footer = tagList(
        actionButton("save_plot_options", "Enregistrer les options", class = "btn-primary"),
        modalButton("Annuler")
      )
    ))
  })
  
  # Observer pour mettre à jour les options d'exportation
  observeEvent(input$save_plot_options, {
    plot_download_width(input$plot_export_width)
    plot_download_height(input$plot_export_height)
    plot_download_res(input$plot_export_res)
    
    removeModal()
    
    showNotification(
      "Options d'exportation mises à jour",
      type = "message"
    )
  })

  output$ave_validity_table <- renderDT({
    r<-validity_results();cfa_datatable(as.data.frame(r$FL),options=list(dom="t",paging=FALSE,ordering=FALSE)) %>%
      formatRound(columns=seq_len(ncol(r$FL)),digits=3, dec.mark = ",")
  })
  output$htmt_validity_table <- renderDT({
    r<-validity_results(); h<-r$HTMT
    if(nrow(h)<2) return(cfa_datatable(data.frame(Message="HTMT non applicable : un seul facteur."),options=list(dom="t"),rownames=FALSE))
    h[upper.tri(h,diag=TRUE)]<-NA_real_
    cfa_datatable(as.data.frame(h),options=list(dom="t",paging=FALSE,ordering=FALSE)) %>% formatRound(columns=seq_len(ncol(h)),digits=3,dec.mark=",")
  })
  output$reliability_conclusion <- renderUI({
    r<-validity_results()
    div(class="alert alert-info",strong("Interpréter le score : "),tags$ul(lapply(r$notes,tags$li)))
  })
  output$ave_conclusion <- renderUI({
    r<-validity_results()
    if(nrow(r$COR)<2) return(div(class="alert alert-info","Validité discriminante non applicable : un seul facteur."))
    issues<-r$results$Facteur[r$results$flag_disc]
    div(class="alert alert-info",if(length(issues)) paste("AVE inférieure à au moins une corrélation factorielle au carré pour :",paste(issues,collapse=", "),". Examinez la distinction conceptuelle et empirique des facteurs.")
      else "Le repère de Fornell–Larcker ne signale pas de chevauchement excessif. Il ne démontre pas à lui seul la validité discriminante.")
  })
  output$htmt_conclusion <- renderUI({
    r<-validity_results(); values<-r$HTMT[lower.tri(r$HTMT)]
    if(!length(values)) return(div(class="alert alert-info","HTMT non applicable : un seul facteur."))
    if(any(!is.finite(values))) return(div(class="alert alert-warning","Certaines paires n’ont pas de HTMT calculable : aucune conclusion globale ne peut être donnée."))
    div(class="alert alert-info",sprintf("HTMT arithmétique maximal : %.3f. Les repères .85 et .90 servent à signaler un chevauchement à examiner, sans établir seuls la validité. Des intervalles bootstrap peuvent être demandés dans la synthèse, en mode avancé, pour les données compatibles.",max(values)))
  })

  # Handler pour le téléchargement du graphique
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("diagramme_afc_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      # Obtenir le graphique actuel
      req(input$layout)
      req(!is.null(input$plot_rotation))
      
      g <- plot_sem()
      
      # Utiliser les dimensions spécifiées par l'utilisateur
      png(file, 
          width = plot_download_width(), 
          height = plot_download_height(),
          res = plot_download_res())
      plot(g)
      dev.off()
    },
    contentType = "image/png"
  )

}

shinyApp(ui = ui, server = server)



