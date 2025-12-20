package body Routage is

    procedure Creer_Route(route : out T_Route; ip : in IP_Adresse;
        masque : in IP_Adresse; interface_route : in Unbounded_String) is
    begin
        route := (
            Ip => ip,
            Masque => masque,
            Interface_Route => interface_route
        );
    end Creer_Route;


    function Est_Valide(ip : in IP_Adresse; route : in T_Route) return Boolean is
        route_ip: IP_Adresse;
        route_masque: IP_Adresse;
    begin
        route_ip := route.Ip;
        route_masque := route.Masque;
        return (ip and route_masque) = (route_ip and route_masque);
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


    function Find_Interface(ip : in IP_Adresse; table : in T_Table_Routage)
        return Unbounded_String is
        curseur_table: T_LCA;
        route_actuel: T_Route;
        fit: IP_Adresse;
        return_interface : Unbounded_String;
    begin
        curseur_table := Premier(T_LCA(table)); 
        fit := 0;
        while not Est_Vide(curseur_table) loop
            route_actuel := Element(curseur_table); 
            if Est_Valide(ip, route_actuel) and then
                    fit < route_actuel.Masque then
                fit := route_actuel.Masque;
                return_interface := route_actuel.Interface_Route;
            else
                null;
            end if;
            curseur_table := Suivant(curseur_table);
        end loop;
        return return_interface;
    end Find_Interface;


    procedure Initialiser_Table(table : out T_Table_Routage) is
    begin
        Initialiser(table);
    end Initialiser_Table;


    procedure Enregistrer_Route(table : in out T_Table_Routage; route : in T_Route) is
    begin
        Enregistrer(table, route);
    end Enregistrer_Route;


    -- TODO Rendre generic car la place des éléments peuvent être différents
    -- Enregistre la ligne x.x.x.x x.x.x.x ethx string en route dans la table
    -- de routage.
    procedure Enregistrer_Ligne(ligne : in Unbounded_String; table : in out T_Table_Routage) is
        route: T_Route;
        liste: String_List;
    begin
        liste := Separer(ligne, ' ');
        route.Ip := String_Vers_Ip(liste(1));
        route.Masque := String_vers_Ip(liste(2));
        route.Interface_Route := liste(3);
        Enregistrer(table, route);
    end Enregistrer_Ligne;


    procedure Charger_Table_Routage(table : out T_Table_Routage; file : in File_Type) is
        numero_ligne: Integer;
        valeur: Unbounded_String;
    begin
        Initialiser(table);
        begin
            loop
                numero_ligne := Integer(Line(file));
                valeur := Get_Line(file); 
                Trim(valeur, Both);
                -- Fonction locale pour séparer les éléments de la ligne
                Enregistrer_Ligne(valeur, table);
                exit when End_Of_File (file);
            end loop;
        exception
            when End_Error =>
                Put ("Blanc en surplus");
                null;
        end;
    end Charger_Table_Routage;


    function Table_Vide(table : T_Table_Routage) return Boolean is
    begin
        return Est_Vide(table);
    end Table_Vide;


    function Masque_Valide(masque : IP_Adresse) return Boolean is
        POIDS_FORT : constant IP_Adresse := 2 ** 31;
        switch : Boolean := False;
        bit : Boolean;
    begin
        for i in 1..31 loop
            bit := ((masque * (2 ** (i-1))) and POIDS_FORT) /= 0;
            if not bit then
                switch := True;
            elsif bit and switch then
                return False;
            end if;
        end loop;
        return True;
    end Masque_Valide;


    function Get_Ip(route: T_Route) return IP_Adresse is
    begin
        return route.Ip;
    end Get_Ip;


    function Get_Masque(route: T_Route) return IP_Adresse is
    begin
        return route.Masque;
    end Get_Masque;


    function Get_Interface(route: T_Route) return Unbounded_String is
    begin
        return route.Interface_Route;
    end Get_Interface;

end Routage;
