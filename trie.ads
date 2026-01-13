with Routage;   use Routage;

generic

    type T_Element is private;

package Trie is

    type T_Trie is private;

    -- Initialise l'Trie
    procedure Initialiser(Trie : out T_Trie);

    -- Renvoie True si Trie est vide
    function Est_Vide(Trie : in T_Trie) return Boolean;

    -- Insère un élément dans l'Trie
    procedure Enregistrer(Trie : in out T_Trie; IP : in IP_Adresse; Element : in T_Element);

    function Gauche(Trie : in T_Trie) return T_Trie;

    function Droite(Trie : in T_Trie) return T_Trie;

    function Element(Trie : in T_Trie) return T_Element;

    procedure Supprimer(Trie : in out T_Trie; IP : in IP_Adresse);

    function Taille(Trie : in T_Trie) return Integer;

    procedure Detruire(Trie : in out T_Trie);

private

    type T_Noeud;
    
    type T_Trie is access T_Noeud;

    type T_Noeud is
        record
            Element : T_Element;
            Droite : T_Trie;
            Gauche : T_Trie;
        end record;

end Trie;
