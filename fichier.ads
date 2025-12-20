with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package Fichier is

    type String_List is array(1..4) of Unbounded_String;
    type Int_List is array(1..4) of Integer;

    -- TODO String_List est partagé entre lire l'IP et lire arguments
    -- ce n'est pas conseillé

    function Separer(ligne : in Unbounded_String; separateur : in Character) return String_List;

    function Convertir_StringEntier(liste_string : in String_List) return Int_List;

end Fichier;
