with Routage;                   use Routage;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Fichier;                   use Fichier;
with Ada.Strings;               use Ada.Strings;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;


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

begin
    -- valeurs par défauts
    Arguments := (
        nom_resultats => To_Unbounded_String("resultats.txt"),
        nom_table => To_Unbounded_String("table.txt"),
        nom_paquets => To_Unbounded_String("paquets.txt"));

    -- lire les arguments
    for i in 1..Argument_Count loop
        if Argument(i) = "-t" and then i+1 <= Argument_Count then
            Arguments.nom_table := To_Unbounded_String(Argument(i+1));
        elsif Argument(i) = "-q" and then i+1 <= Argument_Count then
            Arguments.nom_paquets := To_Unbounded_String(Argument(i+1));
        elsif Argument(i) = "-r" and then i+1 <= Argument_Count then
            Arguments.nom_resultats := To_Unbounded_String(Argument(i+1));
        end if;
    end loop;

    -- preparer le fichier résultats
    Create(fichier_resultats, Out_File, To_String(Arguments.nom_resultats));

    -- creer la table de routage
    Open(fichier_table, In_File, To_String(Arguments.nom_table));
    Charger_Table_Routage(table, fichier_table);
    Close(fichier_table);

    -- essayer tous les paquets
    Open(fichier_paquets, In_File, To_String(Arguments.nom_paquets));
    begin
        loop
            ligne := Get_Line(fichier_paquets);
            founded_interface := Get_Interface(String_Vers_Ip(ligne), table); 
            if To_String(founded_interface) /= "" then
                Put(fichier_resultats, ligne & " " & founded_interface);
                New_Line(fichier_resultats);
            else
                null;
            end if;
            exit when End_Of_File(fichier_paquets);
        end loop;
    end;
    Close(fichier_paquets);
    Close(fichier_resultats);
end Simple_Routeur;
