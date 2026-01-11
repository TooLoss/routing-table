with Routage_Exceptions;    use Routage_Exceptions;

package body Cache_LA is

    procedure Initialiser_Cache(cache : out T_Cache) is
    begin
        Initialiser(cache);
    end Initialiser_Cache;

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


    -- Fonction qui parcours l'abre pour trouver la route qui correspond à l'IP récursivement
    procedure Trouver_Route(cache : in T_Cache; ip : in IP_Adresse; masque_max : in out IP_Adresse; interface_return : in out Unbounded_String; trouve : in out Boolean) is
        cellule : T_Cache_Cellule;
    begin
        if not Est_Vide(cache) then
            cellule := Element(cache);
            if (ip and cellule.masque) = cellule.ip then
                if cellule.masque >= masque_max then
                    masque_max := cellule.masque;
                    interface_return := cellule.interface_route;
                    trouve := True;
                end if;
            end if;

            if cellule.ip > ip then
                Trouver_Route(Gauche(cache), ip, masque_max, interface_return, trouve);
            else
                Trouver_Route(Droite(cache), ip, masque_max, interface_return, trouve);
            end if;
        else
            null;
        end if;
    end Trouver_Route;


    procedure Chercher_Cache(route : out T_Route; cache : in T_Cache; ip : in IP_Adresse) is
        masque_max : IP_Adresse := 0;
        interface_return : Unbounded_String;
        route_trouvee : Boolean := False;
    begin
        Trouver_Route(cache, ip, masque_max, interface_return, route_trouvee);

        if not Route_Trouvee then
            raise Route_Non_Presente;
        end if;

        Creer_Route(route, ip, Masque_Max, Interface_Return);
    end Chercher_Cache;


    function Route_Existe(cache : in T_Cache; ip : in IP_Adresse) return Boolean is
    begin
        if Est_Vide(cache) then
            return False;
        elsif Element(cache).ip = ip then
            return True;
        elsif ip < Element(cache).ip then
            return Route_Existe(Gauche(cache), ip);
        else
            return Route_Existe(Droite(cache), ip);
        end if;
    end Route_Existe;


    procedure Mettre_A_Jour_Cellule(
        cache : in out T_Cache;
        ip: in IP_Adresse;
        politique : in T_Cache_Politique)
    is
        cellule : T_Cache_Cellule;
        courant : T_Cache;
        termine : Boolean := False;
    begin
        courant := Cache;
        while not termine and then not Est_Vide(courant) loop
            cellule := Element(courant);
            if cellule.ip = ip then
                cellule.nombre_utilisations := cellule.nombre_utilisations + 1;
                if Politique = LRU then
                    cellule.index := Taille(cache) + 1;
                end if;
                Enregistrer(cache, cellule);
                termine := True;
            elsif ip < cellule.ip then
                courant := Gauche(courant);
            else
                courant := Droite(courant);
            end if;
        end loop;
    end Mettre_A_Jour_Cellule;


    procedure Enregistrer_Cache(cache : in out T_Cache;
        ip : in IP_Adresse;
        masque : in IP_Adresse;
        interface_route : in Unbounded_String;
        politique : in T_Cache_Politique)
    is
        ip_masquee : IP_Adresse;
        nouvelle_cellule : T_Cache_Cellule;
    begin
        ip_masquee := ip and masque;

        if Route_Existe(cache, ip_masquee) then
            Mettre_A_Jour_Cellule(cache, ip_masquee, politique);
        else
            nouvelle_cellule.ip := ip_masquee;
            nouvelle_cellule.masque := masque;
            nouvelle_cellule.interface_route := interface_route;
            nouvelle_cellule.nombre_utilisations := 1;

            nouvelle_cellule.index := taille(cache) + 1;

            Enregistrer(cache, nouvelle_cellule);
        end if;
    end Enregistrer_Cache;


    function Taille_Cache(cache : in T_Cache) return Integer is
    begin
        return Taille(cache);
    end Taille_Cache;


    procedure Afficher_Recursif(ABR : in T_Cache) is
        route : T_Route;
    begin
        if not Est_Vide(ABR) then
            Afficher_Recursif(Gauche(ABR));

            Creer_Route(route, Element(ABR).ip, Element(ABR).masque, Element(ABR).interface_route);
            Afficher_Route(route);

            Afficher_Recursif(Droite(ABR));
        end if;
    end Afficher_Recursif;


    procedure Afficher_Cache(cache : in T_Cache) is
    begin
        Afficher_Recursif(cache);
    end Afficher_Cache;


    procedure Trouver_Candidat_Suppression(
        cache : in T_Cache;
        politique : in T_Cache_Politique;
        candidat : in out T_Cache_Cellule;
        trouve : in out Boolean)
    is
        cellule : T_Cache_Cellule;
    begin
        if not Est_Vide(cache) then
            cellule := Element(cache);

            if not trouve then
                candidat := cellule;
                trouve := True;
            else
                case politique is
                    when FIFO | LRU =>
                        if cellule.index < candidat.index then
                            candidat := cellule;
                        else
                            null;
                        end if;
                    when LFU =>
                        if cellule.nombre_utilisations < candidat.nombre_utilisations then
                            candidat := cellule;
                        else
                            null;
                        end if;
                end case;
            end if;

            Trouver_Candidat_Suppression(Gauche(cache), politique, candidat, trouve);
            Trouver_Candidat_Suppression(Droite(cache), politique, candidat, trouve);
        end if;
    end Trouver_Candidat_Suppression;


    procedure Supprimer_Cache(cache : in out T_Cache; politique : in T_Cache_Politique) is
        Candidat_A_Supprimer : T_Cache_Cellule;
        trouve : Boolean := False;
    begin
        Trouver_Candidat_Suppression(cache, politique, Candidat_A_Supprimer, trouve);

        if trouve then
            Supprimer(cache, Candidat_A_Supprimer);
        end if;
    end Supprimer_Cache;


    function Est_Inferieur_Cellule(gauche : in T_Cache_Cellule; droite : in T_Cache_Cellule) return Boolean is
    begin
        return gauche.ip < droite.ip;
    end Est_Inferieur_Cellule;

    function Est_Egal_Cellule(gauche : in T_Cache_Cellule; droite : in T_Cache_Cellule) return Boolean is
    begin
        return gauche.ip = droite.ip;
    end Est_Egal_Cellule;


    procedure Detruire_Cache(cache : in out T_Cache) is
    begin
        Detruire(cache);
    end Detruire_Cache;

end Cache_LA;
