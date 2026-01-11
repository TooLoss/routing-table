with Routage_Exceptions;    use Routage_Exceptions;

package body Cache_LL is

    procedure Initialiser_Cache(cache : out T_Cache) is
    begin
        Initialiser(cache);
    end Initialiser_Cache;

    procedure Supprimer_Cache(cache : in out T_Cache; politique : in T_Cache_Politique) is
        mini_nombre : Integer;
        mini_cellule : T_Cache_Cellule;
        curseur : T_Cache;
    begin
        case politique is
            when FIFO =>
                Supprimer(cache, Element(cache));
            when LRU => 
                Supprimer(cache, Element(cache));
            when LFU =>
                curseur := cache;
                mini_cellule := Element(curseur);
                mini_nombre := mini_cellule.nombre_utilisations;
                while not Est_Vide(curseur) loop
                    if Element(curseur).nombre_utilisations < mini_nombre then
                        mini_cellule := Element(curseur);
                        mini_nombre := mini_cellule.nombre_utilisations;
                    end if;
                    curseur := Suivant(curseur);
                end loop;
                Supprimer(cache, mini_cellule);
                null;
        end case;
    end Supprimer_Cache;

    function String_Vers_Politique(S : in Unbounded_String) return T_Cache_Politique is
    begin
        if S = "FIFO" then
            return FIFO;
        elsif S = "LRU" then
            return LRU;
        elsif S = "LFU" then
            return LFU;
        else
            raise Argument_Routage_Error; 
        end if;
    end String_Vers_Politique;

    procedure Chercher_Cache(route : out T_Route; cache : in T_Cache; ip : in IP_Adresse) is
        cursor : T_Cache;
        interface_return : Unbounded_String;
        masque_max : IP_Adresse := 0;
        route_trouvee : Boolean := False;
        ip_cache : IP_Adresse;
        masque_cache : IP_Adresse;
        interface_cache : Unbounded_String;
    begin
        cursor := cache;
        while not Est_Vide(cursor) loop
            ip_cache := Element(cursor).ip;
            masque_cache := Element(cursor).masque;
            interface_cache := Element(cursor).interface_route;

            if (ip and masque_cache) = (ip_cache) then
                if masque_cache > masque_max then
                    masque_max := masque_cache;
                    interface_return := interface_cache;
                    route_trouvee := True;
                end if;
            end if;
            cursor := Suivant(cursor);
        end loop;

        if not route_trouvee then
            raise Route_Non_Presente;
        end if;

        Creer_Route(route, ip, masque_max, interface_return);
    end Chercher_Cache;

    procedure Enregistrer_Cache(cache : in out T_Cache;
        ip : in IP_Adresse;
        masque : in IP_Adresse;
        interface_route : in Unbounded_String;
        politique : in T_Cache_Politique) is
        curseur : T_Cache;
        ip_masque : IP_Adresse;
        ip_trouvee : Boolean := False;
        cellule : T_Cache_Cellule;
        nouvelle_cellule : T_Cache_Cellule;
    begin
        curseur := cache;
        ip_masque := ip and masque;

        -- Recherche de l'IP
        while not Est_Vide(curseur) and not ip_trouvee loop
            if Element(curseur).ip = ip_masque then
                cellule := Element(curseur);
                if politique = LRU then
                    Supprimer(cache, cellule);
                    cellule.nombre_utilisations := cellule.nombre_utilisations + 1;
                    Enregistrer(cache, cellule);
                else
                    nouvelle_cellule := cellule;
                    nouvelle_cellule.nombre_utilisations := nouvelle_cellule.nombre_utilisations + 1;
                    Reaffecter(cache, cellule, nouvelle_cellule);
                end if;
                ip_trouvee := True;
            end if;
            curseur := Suivant(curseur);
        end loop;

        -- Si l'IP n'est pas dans le cache, on l'ajoute
        if not ip_trouvee then
            cellule.ip := ip_masque;
            cellule.masque := masque;
            cellule.nombre_utilisations := 1;
            cellule.interface_route := interface_route;
            Enregistrer(cache, cellule);
        end if;
    end Enregistrer_Cache;

    function Taille_Cache(cache : in T_Cache) return Integer is
    begin
        return Taille(cache);
    end Taille_Cache;

    procedure Afficher_Cache(cache : in T_Cache) is
        route : T_Route;
        curseur : T_Cache;
    begin
        curseur := cache;
        while not Est_Vide(curseur) loop
            Creer_Route(route, Element(curseur).ip, Element(curseur).masque, Element(curseur).interface_route);
            Afficher_Route(route);
            curseur := Suivant(curseur);
        end loop;
    end;

    procedure Detruire_Cache(cache : in out T_Cache) is
    begin
        Detruire(cache);
    end Detruire_Cache;

end Cache_LL; 
