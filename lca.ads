generic
	type T_Element is private;  -- Type des éléments de la LCA

package LCA is

	type T_LCA is limited private;


	procedure Initialiser (LCA : out T_Pile) with
		Post => Est_Vide (LCA);


	function Est_Vide (LCA : in T_Pile) return Boolean;


	procedure Enregistrer (LCA : in out T_Pile; Element : in T_Element) with
		Post => Sommet (LCA) = Element;


private

	type T_Cellule;

	type T_LCA is access T_Cellule;

	type T_Cellule is
		record
			Element: T_Element;
			Suivant: T_LCA;
		end record;

end LCA;
