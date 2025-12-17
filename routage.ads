with LCA;
with Fichier;                   use Fichier; 
with Ada.Strings.Unbounded ;    use Ada.Strings.Unbounded ;  
with Ada.Strings;               use Ada.Strings;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;

package Routage is 

    type IP_Adresse is mod 2**32;
    type Route is limited private;
    type Table_Routage is limited private;

    -- Creer une route avec l'ip, le masque et l'interface.
    procedure Creer_Route(la_route : out Route; ip : in IP_Adresse;
        masque : in IP_Adresse; interface_route : in Unbounded_String) with
        Pre => Length(interface_route) > 0,
        Post => la_route.Ip = ip and
                la_route.Masque = masque and
                la_route.Interface_Route = interface_route;

    -- Obtenir si une ip est associé à cette route;
    function Est_Valide(ip : in IP_Adresse; la_route : in Route) return Boolean ;  	

    -- Obtenir l'interface d'une ip correspondante dans la table de routage.
    function Get_Interface(ip : in Unbounded_String; table : in Table_Routage) return Unbounded_String;
        Pre  => Length(ip) > 0, -- L'IP ne doit pas être une chaîne vide

    -- Lire un fichier de la table de routage puis crée une liste chainé de Route.
    function Charger_Table_Routage(file : in File_Type) return Table_Routage;
        Pre => Is_Open(file) and Mode(file) = In_File, -- Utilisation de IsOpen 

    -- Convertir une chaine IP x.x.x.x en type adresse IP
    function String_Vers_Ip(ip_string : in Unbounded_String) return IP_Adresse;


private

    type Route is
        record
            Ip : Ip_Adresse;
            Masque : Ip_Adresse;
            Interface_Route : Unbounded_String;
        end record;

    package LCA_Routage is
        new LCA (T_Element => Route);
    use LCA_Routage;
    type Table_Routage is new LCA_Routage.T_LCA;

end Routage;
