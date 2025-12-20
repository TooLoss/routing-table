with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package Fichier is

    type String_List is array(1..4) of Unbounded_String;
    type Int_List is array(1..4) of Integer;

    -- TODO String_List est partagé entre lire l'IP et lire arguments
    -- ce n'est pas conseillé

    function Est_Convertible_Entier(liste_string : in String_List) return Boolean;

    function Est_Entier_Valide(texte : in Unbounded_String) return Boolean;

    -- Séparer la chaine de charactère en liste en découpant au séparateur.
    function Separer(ligne : in Unbounded_String; separateur : in Character) return String_List;

    -- Convertie la liste de String à 4 éléments en ligne d'entier à 4 éléments.
    function Convertir_StringEntier(liste_string : in String_List) return Int_List with
        Pre => (Est_Convertible_Entier(liste_string));

end Fichier;
