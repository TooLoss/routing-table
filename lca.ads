generic
	type T_Element is private;  -- Type des éléments de la LCA

package LCA is

	type T_LCA is private;

	procedure Initialiser (LCA : out T_LCA) with
        Post => Est_Vide (LCA);

    procedure Detruire (LCA : in out T_LCA);

	function Est_Vide (LCA : in T_LCA) return Boolean;

    -- Enregistre un élément dans le LCA et l'ajoute en queue.
	procedure Enregistrer (LCA : in out T_LCA; Element : in T_Element);

    function Premier (LCA : in T_LCA) return T_LCA;

    function Element (LCA : in T_LCA) return T_Element;
		
    function Suivant (LCA : in T_LCA) return T_LCA;

    procedure Supprimer (LCA : in out T_LCA; Element : in T_Element);

    function Taille (LCA: in T_LCA) return Integer;

private

	type T_Cellule;

	type T_LCA is access T_Cellule;

	type T_Cellule is
		record
			Element: T_Element;
			Suivant: T_LCA;
		end record;

end LCA;
