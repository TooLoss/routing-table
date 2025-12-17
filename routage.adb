function Est_Valide(IP_Adresse ip, Route la_route) return Boolean is
    route_ip: IP_Adresse;
    route_masque: IP_Adresse;
begin
    route_ip := la_route.Ip;
    route_masque := la_route.Masque;
    return (ip and route_masque) = route_ip;
end Est_Valide;


function String_Vers_Ip(Unbounded_String ip_string) return IP_Adresse is
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


function Get_Interface(Unbounded_String ip,
    Table_Routage table) return Unbounded_String is
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


function Charger_Table_Routage(File_Type file) return Table_Routage is
    table: Table_Routage;
begin
    Initialiser(table);

end 
