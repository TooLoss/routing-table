with LCA;
with Fichier_Utils;             use Fichier_Utils; 
with Ada.Strings.Unbounded ;    use Ada.Strings.Unbounded ;  

package Routage is 

    type Route is limited private;

    package LCA_Routage is
        new LCA (T_Element => Route);
    use LCA_Routage;

    type Table_Routage is LCA_Routage.LCA;

    function Est_Valide(IP_Adresse ip, Route la_route) return Boolean ;  	

    function Get_Interface(Unbounded_String ip, Table_Routage table) return Unbounded_String;

    function Charger_Table_Routage(File_Type file) return Table_Routage;

    function String_Vers_Ip(Unbounded_String ip_string) return IP_Adresse;


private:

    type IP_Adresse is mod 2**32;
    type Route is
        record
            Ip : Ip_Adresse;
            Masque : Ip_Adresse;
            Interface_Route : Unbounded_String;
        end record;

end Routage;
