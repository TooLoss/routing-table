package body Fichier is

    function Separer(ligne : in Unbounded_String;
        separateur : in Character) return String_List is
        mot: Integer;
        chaine: Unbounded_String;
        liste: String_List;
    begin
        -- TODO gérer les cas d'erreurs quand il y plus de 4 éléments
        mot := 1;
        chaine := To_Unbounded_String("");
        for i in 1..Length(ligne) loop
            if Element(ligne, i) = separateur then
                liste(mot) := chaine;
                mot := mot + 1;
                chaine := To_Unbounded_String("");
            else
                chaine := chaine & Element(ligne, i);
            end if;
        end loop;
        liste(mot) := chaine;
        return liste;
    end Separer;


    function Recuperer_Arguments(fichier : in File_Type) return Unbounded_String is
    begin
        return To_Unbounded_String("");
    end Recuperer_Arguments;


    function Convertir_StringEntier(liste_string : in String_List) return Int_List is
        liste_entier: Int_List;
    begin
        for i in 1..4 loop
            liste_entier(i) := Integer'Value(To_String(liste_string(i)));
        end loop;
        return liste_entier;
    end Convertir_StringEntier;

end Fichier;
