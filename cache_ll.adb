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

    function Chercher_Cache(cache : in T_Cache; ip : in IP_Adresse) return Unbounded_String is
        cursor : T_Cache;
        interface_return : Unbounded_String;
    begin
        cursor := cache;
        while not Est_Vide(cursor) loop
            if Element(cursor).ip = ip then
                interface_return := Element(cursor).interface_route;
            else
                null;
            end if;
            cursor := Suivant(cursor);
        end loop;
        return interface_return;
    end Chercher_Cache;

    procedure Mise_A_Jour_Cache(cache : in out T_Cache; politique : in T_Cache_Politique; ip : in IP_Adresse) is
        curseur : T_Cache;
        cellule : T_Cache_Cellule;
    begin
        curseur := cache;
        while not Est_Vide(curseur) loop
            if Element(curseur).ip = ip then
                -- Incrémentation du nombre d'utilisation,
                -- Remet en avant la route si le mode LRU
                cellule := Element(curseur);
                if politique = LRU then
                    Supprimer(cache, cellule);
                    cellule.nombre_utilisations := cellule.nombre_utilisations + 1;
                    Enregistrer(cache, cellule);
                else 
                    cellule.nombre_utilisations := cellule.nombre_utilisations + 1;
                end if;
            else
                null;
            end if;
            curseur := Suivant(curseur);
        end loop;
    end Mise_A_Jour_Cache;

    procedure Enregistrer_Cache(cache : in out T_Cache;
        ip : in IP_Adresse;
        masque : in IP_Adresse;
        interface_route : in Unbounded_String) is
        curseur : T_Cache;
        ip_masque : IP_Adresse;
        ip_trouvee : Boolean := False;
        cellule : T_Cache_Cellule;
    begin
        curseur := cache;
        ip_masque := ip and masque;

        -- Recherche de l'IP
        while not Est_Vide(curseur) and not ip_trouvee loop
            if Element(curseur).ip = ip_masque then
                cellule := Element(curseur);
                cellule.nombre_utilisations := cellule.nombre_utilisations + 1;

                -- Pour la politique LRU 
                Supprimer(cache, cellule);
                Enregistrer(cache, cellule);

                ip_trouvee := True;
            end if;
            curseur := Suivant(curseur);
        end loop;

        -- Si l'IP n'est pas dans le cache, on l'ajoute
        if not ip_trouvee then
            cellule.ip := ip_masque;
            cellule.nombre_utilisations := 1;
            cellule.interface_route := interface_route;
            Enregistrer(cache, cellule);
        end if;
    end Enregistrer_Cache;

    function Taille(cache : in T_Cache) return Integer is
    begin
        return Taille(cache);
    end Taille;

end Cache_LL; 
