with Routage;                   use Routage;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Fichier;                   use Fichier;
with Ada.Strings;               use Ada.Strings;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;
with Routage_Exceptions;        use Routage_Exceptions;


procedure Simple_Routeur is

    type T_Arguments is
        record
            nom_table : Unbounded_String;
            nom_paquets : Unbounded_String;
            nom_resultats : Unbounded_String;
        end record;

    Arguments : T_Arguments;
    fichier_table : File_Type;
    fichier_resultats : File_Type;
    fichier_paquets : File_Type;
    ligne : Unbounded_String;
    table : T_Table_Routage;
    founded_interface : Unbounded_String;

    procedure Check_Next_Argument(i : in Integer; max : in Integer) is
    begin
        if i+1 > max then
            raise Argument_Routage_Error;
        end if;
    end Check_Next_Argument;

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

begin
    -- valeurs par défauts
    Arguments := (
        nom_resultats => To_Unbounded_String("resultats.txt"),
        nom_table => To_Unbounded_String("table.txt"),
        nom_paquets => To_Unbounded_String("paquets.txt"));

    -- lire les arguments
    for i in 1..Argument_Count loop
        if Argument(i) = "-t" then
            Check_Next_Argument(i, Argument_Count);
            Arguments.nom_table := To_Unbounded_String(Argument(i+1));
        elsif Argument(i) = "-q" then
            Check_Next_Argument(i, Argument_Count);
            Arguments.nom_paquets := To_Unbounded_String(Argument(i+1));
        elsif Argument(i) = "-r" then
            Check_Next_Argument(i, Argument_Count);
            Arguments.nom_resultats := To_Unbounded_String(Argument(i+1));
        end if;
    end loop;

    -- preparer le fichier résultats
    Create(fichier_resultats, Out_File, To_String(Arguments.nom_resultats));

    -- creer la table de routage
    begin
        Open(fichier_table, In_File, To_String(Arguments.nom_table));
    exception
        when others =>
            raise Fichier_Introuvable_Error;
    end;
    Charger_Table_Routage(table, fichier_table);
    Close(fichier_table);

    -- Pour chaque paquet, on extrait l'adresse IP destination,
    -- on cherche l'interface correspondante dans la table de routage,
    -- et on écrit le résultat dans le fichier de sortie.
    begin
        Open(fichier_paquets, In_File, To_String(Arguments.nom_paquets));
    exception
        when others =>
            raise Fichier_Introuvable_Error;
    end;
    begin
        loop
            ligne := Get_Line(fichier_paquets);
            Traiter_Paquet(ligne, table, fichier_resultats);
            exit when End_Of_File(fichier_paquets);
        end loop;
    end;
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
