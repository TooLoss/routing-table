with Routage;                   use Routage;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Strings;               use Ada.Strings;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Routage_Exceptions;        use Routage_Exceptions;


procedure Simple_Routeur is

    -- Vérifier que le fichier existe, sinon raise une exception
    procedure Verifier_Presence_Fichier(filename : in Unbounded_String);

    -- Vérifier si le prochaine argument existe, sinon raise une exception.
    procedure Verifier_Prochain_Argument(i : in Integer; max : in Integer);

    -- Traiter une IP présent dans le paquets.txt
    procedure Traiter_Paquet(paquet : in Unbounded_String;
        table : in T_Table_Routage;
        fichier_resultats : in out File_Type);


    --
    -- Implémentations
    --

    type T_Arguments is
        record
            nom_table : Unbounded_String := To_Unbounded_String("table.txt");
            nom_paquets : Unbounded_String := To_Unbounded_String("paquets.txt");
            nom_resultats : Unbounded_String := To_Unbounded_String("resultats.txt");
        end record;

    procedure Verifier_Prochain_Argument(i : in Integer; max : in Integer) is
    begin
        if i+1 > max then
            raise Argument_Routage_Error;
        end if;
    end Verifier_Prochain_Argument;

    procedure Traiter_Paquet(
        paquet : in Unbounded_String;
        table : in T_Table_Routage;
        fichier_resultats : in out File_Type)
    is
        interface_trouvee : Unbounded_String;
    begin
        interface_trouvee := Find_Interface(String_Vers_Ip(paquet), table);
        if To_String(interface_trouvee) /= "" then
            Put(fichier_resultats, paquet & " " & interface_trouvee);
            New_Line(fichier_resultats);
        end if;
    end Traiter_Paquet;

    procedure Verifier_Presence_Fichier(filename : in Unbounded_String) is
        fichier_locale : File_Type;
    begin
        begin
            Open(fichier_locale, In_File, To_String(filename));
            if not Is_Open(fichier_locale) then
                raise Fichier_Introuvable_Error;
            end if;
            Close(fichier_locale);
        exception
            when others =>
                raise Fichier_Introuvable_Error;
        end;
    end Verifier_Presence_Fichier;

    Arguments : T_Arguments;
    fichier_table : File_Type;
    fichier_resultats : File_Type;
    fichier_paquets : File_Type;
    ligne : Unbounded_String;
    table : T_Table_Routage;
    founded_interface : Unbounded_String;

begin
    -- lire les arguments
    for i in 1..Argument_Count loop
        if Argument(i) = "-t" then
            Verifier_Prochain_Argument(i, Argument_Count);
            Arguments.nom_table := To_Unbounded_String(Argument(i+1));
        elsif Argument(i) = "-q" then
            Verifier_Prochain_Argument(i, Argument_Count);
            Arguments.nom_paquets := To_Unbounded_String(Argument(i+1));
        elsif Argument(i) = "-r" then
            Verifier_Prochain_Argument(i, Argument_Count);
            Arguments.nom_resultats := To_Unbounded_String(Argument(i+1));
        end if;
    end loop;

    -- preparer le fichier résultats
    Create(fichier_resultats, Out_File, To_String(Arguments.nom_resultats));

    -- creer la table de routage
    Verifier_Presence_Fichier(Arguments.nom_table);
    Open(fichier_table, In_File, To_String(Arguments.nom_table));
    Charger_Table_Routage(table, fichier_table);
    Afficher_Table(table);
    Close(fichier_table);

    -- Pour chaque paquet, on extrait l'adresse IP destination,
    -- on cherche l'interface correspondante dans la table de routage,
    -- et on écrit le résultat dans le fichier de sortie.
    Verifier_Presence_Fichier(Arguments.nom_paquets);
    Open(fichier_paquets, In_File, To_String(Arguments.nom_paquets));
    begin
        loop
            ligne := Get_Line(fichier_paquets);
            Traiter_Paquet(ligne, table, fichier_resultats);
            exit when End_Of_File(fichier_paquets);
        end loop;
    exception
        when End_Error =>
            Put_Line("Blancs en surplus à la fin du fichier.");
            null;
        when IP_Invalide_Erreur =>
            Put_Line("Une IP est invalide dans le fichier paquets");
            null;
    end;

    -- operations terminé, fermer les fichiers.
    Close(fichier_paquets);
    Close(fichier_resultats);

exception
    when Fichier_Introuvable_Error =>
        Put_Line("Vérifier que les fichiers en arguments existent : " &
        Arguments.nom_table & " " & Arguments.nom_paquets);
    when Argument_Routage_Error =>
        Put_Line("Arguments incorrects");
    when Duplicate_Route_Error =>
        Put_Line("La table de routage contient des routes en doublons");
end Simple_Routeur;
