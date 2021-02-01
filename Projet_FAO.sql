-- Question 1
-- 1. Les 10 pays ayant le plus haut ratio disponibilité alimentaire/habitant en termes de protéines (en kg) par habitant, 
-- puis en termes de kcal par habitant.

CREATE VIEW dispo_alim_habitant As
SELECT
pays,
année,
(dispo_alim_kcal_p_j * 365) / (dispo_alim_tonnes * 1000) As ratio_habitant_kcal_kg,
100 * (dispo_prot_g_p_j * 0.001 * 365) / (dispo_alim_tonnes * 1000)  As percentage_protein_habitant
From
dispo_alim
WHERE 
dispo_alim_tonnes != 0;

-- 1.a Les 10 pays ayant le plus haut ratio disponibilité alimentaire/habitant en termes de kcal par habitant
SELECT 
pays, 
SUM(ratio_habitant_kcal_kg) As ratio_habitant_kcal_kg
FROM dispo_alim_habitant
GROUP BY pays
ORDER BY ratio_habitant_kcal_kg DESC
LIMIT 10;

-- 1.b Les 10 pays ayant le plus haut ratio disponibilité alimentaire/habitant en termes de protéines (en kg) par habitant
SELECT 
pays, 
SUM(percentage_protein_habitant) As percentage_protein_habitant
FROM dispo_alim_habitant
GROUP BY pays
ORDER BY percentage_protein_habitant DESC
LIMIT 10;

-- Question 2
-- Pour chaque année disponible, les 10 pays ayant le plus faible ratio disponibilité alimentaire/habitant en termes de protéines (en kg) par habitant.
-- Le nombre de lignes de la table renvoyée sera donc égal à 10 fois le nombre d'années disponibles

SELECT * FROM 
	(SELECT 
		ROW_NUMBER() OVER(PARTITION BY année) AS POSITION,
		année,
		pays,
		percentage_protein_habitant
			FROM (SELECT année, pays, SUM(percentage_protein_habitant) As percentage_protein_habitant
					FROM dispo_alim_habitant
					GROUP BY année, pays
					ORDER BY année, percentage_protein_habitant) As temp) As result
WHERE POSITION <= 10;

-- Remarque : La fonction ROW_NUMBER utiliséé ci-dessus permet d'attribuer un numéro d'ordre (numéro de ligne) selon un tri.

-- Question 3 
-- La quantité totale (en kg) de produits perdus par pays et par année. La table renvoyée contiendra donc une ligne par couple (pays, année)

SELECT pays, année, SUM(pertes * 1000000) As quantité_totale_perdue
	FROM equilibre_prod
	GROUP BY année, pays
	ORDER BY année;

-- Question 4
-- Les 10 pays pour lesquels la proportion de personnes sous-alimentées est la plus forte

SELECT country , SUM(nb_persons) As total_perso_sous_alimentées
	FROM sous_nutrition
    WHERE nb_persons is not null AND nb_persons != "<0.1"
	GROUP BY country
	ORDER BY total_perso_sous_alimentées DESC LIMIT 10;
    
-- Question 5 
-- Les 10 produits pour lesquels le ratio Autres utilisations/Disponibilité intérieure est le plus élevé

SELECT produit,
SUM(ratio_others_vs_domestic_supply) As ratio_par_produit 
FROM (SELECT produit, 
		(autres_utilisations / dispo_int) As ratio_others_vs_domestic_supply
		FROM equilibre_prod
		WHERE dispo_int != 0
	) As temp
group by produit
order by ratio_par_produit DESC LIMIT 10;


        






