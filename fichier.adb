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
                -- cas au on trouve un séparateur. Si l'accumulation est vide,
                -- on ne fait rien car il peut y avoir plusieurs séparateurs à
                -- la suite.
                if chaine /= To_Unbounded_String("") then
                    liste(mot) := chaine;
                    mot := mot + 1;
                    chaine := To_Unbounded_String("");
                else
                    null;
                end if;
            else
                -- cas courant, on accumule les caractères.
                chaine := chaine & Element(ligne, i);
            end if;
        end loop;
        liste(mot) := chaine;
        return liste;
    end Separer;


    function Convertir_StringEntier(liste_string : in String_List) return Int_List is
        liste_entier: Int_List;
    begin
        for i in 1..4 loop
            liste_entier(i) := Integer'Value(To_String(liste_string(i)));
        end loop;
        return liste_entier;
    end Convertir_StringEntier;


    function Est_Entier_Valide(texte : in Unbounded_String) return Boolean is
        contient_alphabet : Boolean;
    begin
        contient_alphabet := False;
        for i in 1..Length(texte) loop
            case (Element(texte, i)) is
                when '0'..'9' =>
                    null;
                when others =>
                    contient_alphabet := True;
            end case;
        end loop;
        return not contient_alphabet;
    end Est_Entier_Valide;


    function Est_Convertible_Entier(liste_string : in String_List) return Boolean is
        contient_alphabet : Boolean;
    begin
        contient_alphabet := False;
        for i in 1..4 loop
            if not Est_Entier_Valide(liste_string(i)) then
                contient_alphabet := True;
            else
                null;
            end if;
        end loop;
        return not contient_alphabet;
    end Est_Convertible_Entier;

end Fichier;
