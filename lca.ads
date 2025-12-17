generic
	type T_Element is private;  -- Type des éléments de la LCA

package LCA is

	type T_LCA is limited private;


	procedure Initialiser (LCA : out T_LCA);


	function Est_Vide (LCA : in T_LCA) return Boolean;


	procedure Enregistrer (LCA : in out T_LCA; Element : in T_Element);
		

private

	type T_Cellule;

	type T_LCA is access T_Cellule;

	type T_Cellule is
		record
			Element: T_Element;
			Suivant: T_LCA;
		end record;

end LCA;
