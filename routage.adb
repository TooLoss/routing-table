package body Routage is

    procedure Creer_Route(route : out T_Route; ip : in IP_Adresse;
        masque : in IP_Adresse; interface_route : in Unbounded_String) is
    begin
        route.Ip := ip;
        route.Masque := masque;
        route.Interface_Route := interface_route; 
    end Creer_Route;


    function Est_Valide(ip : in IP_Adresse; route : in T_Route) return Boolean is
        route_ip: IP_Adresse;
        route_masque: IP_Adresse;
    begin
        route_ip := route.Ip;
        route_masque := route.Masque;
        return (ip and route_masque) = route_ip;
    end Est_Valide;


    function String_Vers_Ip(ip_string : in Unbounded_String) return IP_Adresse is
        ip_list: Int_List;
        ip: IP_Adresse;
    begin
        ip_list := Convertir_StringEntier(Separer(ip_string, '.'));
        ip := 0;
        for i in 1..4 loop
            ip := ip * 256 + IP_Adresse(ip_list(i));
        end loop;
        return ip;
    end String_Vers_Ip;


    function Get_Interface(ip : in Unbounded_String;
        table : in T_Table_Routage) return Unbounded_String is
        curseur_table: T_LCA;
        route_actuel: T_Route;
        fit: Integer;
        return_interface: Unbounded_String;
    begin
        curseur_table := Premier(T_LCA(table)); 
        fit := 0;
        while not Est_Vide(curseur_table) loop
            route_actuel := Element(curseur_table); 
            if Est_Valide(String_Vers_Ip(ip), route_actuel) and then
                    IP_Adresse(fit) < route_actuel.Masque then
                fit := Integer(route_actuel.Masque);
                return_interface := route_actuel.Interface_Route;
            else
                null;
            end if;
            curseur_table := Suivant(curseur_table);
        end loop;
        return return_interface;
    end Get_Interface;


    -- TODO Rendre generic car la place des éléments peuvent être différents
    procedure Enregistrer_Route(ligne : in Unbounded_String; table : in out T_Table_Routage) is
        route: T_Route;
        liste: String_List;
    begin
        liste := Separer(ligne, ' ');
        route.Ip := String_Vers_Ip(liste(1));
        route.Masque := String_vers_Ip(liste(2));
        route.Interface_Route := liste(3);
        Enregistrer(table, route);
    end Enregistrer_Route;


    function Charger_Table_Routage(file : in File_Type) return T_Table_Routage is
        table: T_Table_Routage;
        numero_ligne: Integer;
        valeur: Unbounded_String;
    begin
        Initialiser(table);
        begin
            loop
                numero_ligne := Integer(Line(file));
                valeur := Get_Line(file); 
                Trim(valeur, Both);
                Enregistrer_Route(valeur, table);
                exit when End_Of_File (file);
            end loop;
        exception
            when End_Error =>
                Put ("Blanc en surplus");
                null;
        end;
        return table;
    end Charger_Table_Routage;

end Routage;
