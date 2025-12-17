function Est_Valide(ip : in IP_Adresse, la_route : in Route) return Boolean is
    route_ip: IP_Adresse;
    route_masque: IP_Adresse;
begin
    route_ip := la_route.Ip;
    route_masque := la_route.Masque;
    return (ip and route_masque) = route_ip;
end Est_Valide;


function String_Vers_Ip(ip_string : in Unbounded_String) return IP_Adresse is
    ip_list: Int_List;
    ip: IP_Adresse;
begin
    ip_list := Convertir_StringEntier(Separer(ip_string, '.'));
    ip := 0;
    for i in 1..4 loop
        ip := ip * 256 + ip_list(i);
    end loop;
    return ip;
end String_Vers_Ip;


function Get_Interface(ip : in Unbounded_String,
        table : in Table_Routage) return Unbounded_String is
    curseur_table: Table_Routage;
    route_actuel: Route;
    fit: Integer;
    return_interface: Unbounded_String;
begin
    curseur_table := table;
    fit := 0
    while route_actuel /= null loop
        route_actuel := curseur_table.Element; 
        if Est_Valide(ip, route_actuel) and fit < route_actuel.Masque then
            fit := route_actuel.Masque;
            return_interface := route_actuel.Interface_Route;
        else
            null;
        end if;
        curseur_table := curseur_table.Suivant;
        return return_interface;
    end loop;
end Get_Interface;


-- TODO Rendre generic  ?
procedure Enregistrer_Route(ligne : in Unbounded_String, table : in out Table_Routage) is
    route: Route;
begin
    route := new Route;
    route.Ip := String_Vers_Ip(ligne(1));
    route.Masque := String_vers_Ip(ligne(2));
    route.Interface_Route := ligne(3);
    Enregistrer(table, route);
end Enregistrer_Route;


function Charger_Table_Routage(file : in File_Type) return Table_Routage is
    table: Table_Routage;
    numero_ligne: Integer;
    valeur: Unbounded_String;
    valeur_table: String_List;
begin
    Initialiser(table);
    begin
        loop
            numero_ligne := Integer(Line(file));
            valeur := Get_Line(file); 
            Trim(valeur, Both);
            valeur_table := Separer(valeur, " ");
            Enregistrer_Route(valeur_table, table);
            exit when End_Of_File (Entree);
        end loop;
    exception
        when End_Error =>
            Put ("Blanc en surplus");
            null;
    end;
    return table;
end 
