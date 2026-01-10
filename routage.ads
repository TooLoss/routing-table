with LCA;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;  
with Ada.Strings;               use Ada.Strings;
with Ada.Text_IO;               use Ada.Text_IO;

package Routage is 

    type IP_Adresse is mod 2**32;
    type T_Route is limited private;
    type T_Table_Routage is limited private;

    -- Vérifie la validité du masque, des 1 puis 0 (optionel)
    function Masque_Valide(masque : IP_Adresse) return Boolean;

    -- Creer une route avec l'ip, le masque et l'interface.
    procedure Creer_Route(
        route : out T_Route;
        ip : in IP_Adresse;
        masque : in IP_Adresse;
        interface_route : in Unbounded_String
    ) with
        Pre => Masque_Valide(masque);

    -- Obtenir si une ip est associé à cette route;
    function Est_Valide(ip : in IP_Adresse; route : in T_Route) return Boolean;

    -- Obtenir la route d'une ip correspondante dans la table de routage.
    procedure Find_Interface(route : out T_Route; ip : in IP_Adresse; table : in T_Table_Routage);

    -- Lire un fichier de la table de routage puis crée une liste chainé de Route.
    procedure Charger_Table_Routage(table : out T_Table_Routage; file : in File_Type) with
        Pre => Is_Open(file) and Mode(file) = In_File;

    -- Convertir une chaine IP x.x.x.x en type adresse IP
    function String_Vers_Ip(ip_string : in Unbounded_String) return IP_Adresse;

    function Ip_Vers_String(ip : in IP_Adresse) return Unbounded_String;

    -- Est ce que la table est vide ?
    function Table_Vide(table : T_Table_Routage) return Boolean;

    -- Enregistrer une route dans la table
    procedure Enregistrer_Route(table : in out T_Table_Routage; route : in T_Route) with
        Post => not Table_Vide(table);

    -- Initaliser une table vide
    procedure Initialiser_Table(table : out T_Table_Routage) with
        Post => Table_Vide(table);

    -- Detruire la table de routage
    procedure Detruire_Table(table : in out T_Table_Routage);

    procedure Afficher_Table(table : in T_Table_Routage);
    procedure Afficher_Route(route : in T_Route);

    function Get_Ip(route: T_Route) return IP_Adresse;
    function Get_Masque(route: T_Route) return IP_Adresse;
    function Get_Interface(route: T_Route) return Unbounded_String;

private

    -- Valeurs par défauts temporaires pour vérifier l'ivariant de type.
    type T_Route is
        record
            Ip : Ip_Adresse := IP_Adresse(0);
            Masque : Ip_Adresse := IP_Adresse(0);
            Interface_Route : Unbounded_String;
        end record with
        Type_Invariant => Masque_Valide(Masque);

    package LCA_Routage is
        new LCA (T_Element => T_Route);
    use LCA_Routage;
    type T_Table_Routage is new LCA_Routage.T_LCA;

end Routage;
