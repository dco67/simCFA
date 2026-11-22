# cfadiag 0.2.3

- Les charges sont lues dans la table des paramètres de lavaan, et non dans la matrice lambda. Lorsqu'un indicateur est aussi régressé sur une autre variable (`x4 ~ x1`), lavaan le représente par une variable fantôme et la matrice lambda contenait des zéros, d'où de faux signaux « Charge faible = 0 ». La variance résiduelle standardisée de ces indicateurs, que lavaan place dans la matrice psi, est reconstituée à partir de la matrice impliquée, ce qui permet de calculer de nouveau le coefficient H du facteur concerné.
- Un effet direct sur un indicateur ne suspend plus que la fiabilité et l'AVE du facteur de cet indicateur, avec un statut et un signal qui le nomment ; les autres facteurs gardent leurs indices.
- Modèle multigroupe : l'ajustement affiché dans chaque groupe est indiqué comme celui du modèle entier (« tous groupes confondus »).
- Modèle juste identifié : la mention « versions corrigées : indisponible » n'est plus affichée.
- Les valeurs des signaux sont écrites avec la virgule décimale, comme le reste du rapport.
- Cinq vignettes : prise en main, comprendre les indices, données imparfaites, au-delà de l'AFC simple, simulation et enseignement.

# cfadiag 0.2.2

- `compute_reliability()` et `cfadiag()` reçoivent l'argument `variance` : `"impliquee"` (défaut) ou `"observee"`, qui reproduit `semTools::compRelSEM()`.
- Chaque fonction exportée a des exemples dans sa page d'aide.
- Tests de concordance avec semTools (exécutés si le paquet est installé) et test de l'argument `variance`.

# cfadiag 0.2.1

- H emploie la matrice résiduelle complète, avec covariances résiduelles.
- Les structures complexes ne reçoivent pas de fiabilité cible automatique sans définition du score; un statut explique les indices non calculés.
- Les noms, dimensions, matrices de corrélations et arguments de simulation/bootstrap sont vérifiés.
- Le conteneur conserve la structure des relations de mesure, y compris une saturation libre estimée nulle.
- Le point HTMT et ses réplications bootstrap emploient le même traitement des absences. Sous FIML, la covariance EM est réestimée à chaque réplication.
- Les matrices HTMT traitent explicitement un facteur, un indicateur, des blocs qui se chevauchent et les corrélations non calculables.
- Les réplications bootstrap dégénérées donnent des bornes NA et un compte valide nul, sans erreur de quantile. Une graine explicite préserve l'état aléatoire global.
- Les bornes RMSEA conservent la même version que l'estimation; les repères ne sont pas évalués pour un modèle juste identifié.
- Les matrices résiduelles inadmissibles, saturations nulles et modèles non convergés sont signalés sans résultats trompeurs.
- Les graphiques conservent les valeurs hors de [0,1] lorsqu'elles sont présentes, plutôt que de les masquer par des axes fixes.
- Documentation et tests complétés; population_model expose les paramètres exacts d'une simulation.
