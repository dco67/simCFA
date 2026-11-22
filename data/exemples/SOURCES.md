# Données réelles incluses

Ces copies locales permettent le chargement et le téléchargement sans accès à Internet. L’application fournit, pour chaque jeu, une question de départ, une description des variables, un modèle et des réglages de départ. Aucun ajustement satisfaisant n’est présumé.

| Fichier | Lignes | Indicateurs du modèle | Facteurs | Cas complets | Réglage proposé |
|---|---:|---:|---:|---:|---|
| HolzingerSwineford1939.csv | 301 | 9 | 3 | 301 | ML, continu |
| PoliticalDemocracy.csv | 75 | 11 | 3 | 75 | ML, continu |
| lsat6.csv | 1 000 | 5 | 1 | 1 000 | WLSMV, binaire |
| lsat7.csv | 1 000 | 5 | 1 | 1 000 | WLSMV, binaire |
| ability.csv | 1 525 | 16 | 4 | 1 248 | WLSMV, binaire |
| msq_affect.csv | 3 032 | 20 | 2 | 2 951 | WLSMV, ordinal |
| bfi.csv | 2 800 | 25 | 5 | 2 436 | WLSMV, ordinal |

Les cas complets concernent uniquement les indicateurs du modèle proposé. Les CSV conservent les valeurs manquantes sous forme de cellules vides. Aucune ligne n’est supprimée au chargement. Les notices `*_notice.txt` donnent les références, les codages, les transformations et la syntaxe du modèle. Le fichier Holzinger–Swineford contient aussi six colonnes descriptives d’origine, soit quinze colonnes au total.

## Préparation et provenance

Copies préparées le 20 novembre 2026 depuis lavaan 0.7-3, psych 2.4.12 et psychTools 2.6.4. Les notices des auteurs sont accessibles par les liens ci-dessous; les packages de préparation ne sont pas nécessaires pour lire les CSV.

- [Holzinger–Swineford](https://search.r-project.org/CRAN/refmans/lavaan/html/HolzingerSwineford1939.html) et [PoliticalDemocracy](https://search.r-project.org/CRAN/refmans/lavaan/html/PoliticalDemocracy.html) : fichiers complets de lavaan. Le modèle de démocratie proposé est une AFC à trois facteurs corrélés; il ne reproduit pas le modèle structurel de Bollen.
- [LSAT 6 et 7](https://search.r-project.org/CRAN/refmans/psych/html/bock.table.html) : copies de `psych::lsat6` et `psych::lsat7`, développées à partir des fréquences des profils publiés par Bock et Lieberman. Les numéros de lignes ne sont pas des identifiants de personnes et les deux fichiers ne doivent pas être appariés.
- [SAPA / ability](https://search.r-project.org/CRAN/refmans/psychTools/html/ability.html) : les seize colonnes et toutes les lignes de `psychTools::ability`, sans imputation. Le modèle comprend quatre facteurs corrélés.
- [MSQ](https://search.r-project.org/CRAN/refmans/psychTools/html/msqR.html) : sélection `time == 1` dans `psychTools::msqR`, puis des vingt items des clés PA/NA documentées. La sélection conserve une seule occasion pour les 3 032 participants; les identifiants locaux aux études et les variables de condition ne sont pas inclus. Il s’agit d’un sous-ensemble du MSQ et non d’une collecte indépendante du PANAS. Le mélange d’études et de conditions constitue une limite à discuter.
- [BFI / SAPA](https://search.r-project.org/CRAN/refmans/psych/html/bfi.html) : les vingt-cinq items de `psych::bfi`, sans les trois colonnes démographiques. Aucun recodage des items inversés : A1, C4, C5, E1, E2, O2, O5. Le nom du fichier ne désigne pas le Big Five Inventory d’Oliver John.

Le script facultatif `reconstruire.R` explicite les sélections et recrée les fichiers depuis les packages. L’application utilise directement les CSV inclus.

## Attribution et licence

Les jeux sont distribués avec les packages sources lavaan, psych et psychTools, déclarés sous GPL (>= 2); le texte GPL-2 est joint. Les auteurs des données et leurs références restent indiqués dans les notices. Les items IPIP et les ressources ICAR sont présentés par leurs auteurs comme des ressources du domaine public; cette mention ne dispense pas de citer la source de l’échantillon. Les notices françaises décrivent les copies pédagogiques; elles ne constituent pas une validation d’une traduction des instruments.
