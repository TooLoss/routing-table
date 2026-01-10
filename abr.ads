generic

    type T_Element is private;
    with function Est_Inferieur(Gauche : T_Element; Droite : T_Element) return Boolean;
    with function Est_Egal(Gauche : T_Element; Droite : T_Element) return Boolean;

package ABR is

    type T_ABR is private;

    -- Initialise l'ABR
    procedure Initialiser(ABR : out T_ABR);

    -- Renvoie True si ABR est vide
    function Est_Vide(ABR : in T_ABR) return Boolean;

    -- Insère un élément dans l'ABR
    procedure Enregistrer(ABR : in out T_ABR; Element : in T_Element);

    function Element(ABR : in T_ABR) return T_Element;

    function Gauche(ABR : in T_ABR) return T_ABR;

    function Droite(ABR : in T_ABR) return T_ABR;

    procedure Supprimer(ABR : in out T_ABR; Element : in T_Element);

    function Taille(ABR : in T_ABR) return Integer;

    procedure Detruire(ABR : in out T_ABR);

private

    type T_Noeud;
    
    type T_ABR is access T_Noeud;

    type T_Noeud is
        record
            Element : T_Element;
            Droite : T_ABR;
            Gauche : T_ABR;
        end record;

end ABR;
