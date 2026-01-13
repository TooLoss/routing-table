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


    procedure Chercher_Cache(route : out T_Route; cache : in T_Cache; ip : in IP_Adresse) is
        Courant : T_Cache;
        Dernier_Candidat : T_Cache_Cellule;
        Trouve : Boolean := False;
        Bit : Integer;
    begin
        Courant := cache;
        for i in reverse 0 .. 31 loop
            if not Est_Vide(Courant) then
                if Element(Courant).interface_route /= Null_Unbounded_String then
                    -- Cela signifie qu'on à trouvé une IP masquée en cours
                    Dernier_Candidat := Element(Courant);
                    Trouve := True;
                end if;
                Bit := Obtenir_Bit(ip, i);
                if Bit = 0 then
                    Courant := Gauche(Courant);
                else
                    Courant := Droite(Courant);
                end if;
            end if;
        end loop;
        if Trouve then
            Creer_Route(route, Dernier_Candidat.ip, Dernier_Candidat.masque, Dernier_Candidat.interface_route);
        else
            raise Route_Non_Presente;
        end if;
    end Chercher_Cache;


    function Route_Existe(cache : in T_Cache; ip : in IP_Adresse) return Boolean is
        Courant : T_Cache;
    begin
        Courant := cache;
        for i in reverse 0 .. 31 loop
            if not Est_Vide(Courant) then
                if Obtenir_Bit(ip, i) = 0 or Est_Vide(Droite(Courant)) then
                    Courant := Gauche(Courant);
                else
                    Courant := Droite(Courant);
                end if;
            else
                null;
            end if;
        end loop;
        return not Est_Vide(Courant);
    end Route_Existe;


    procedure Mettre_A_Jour_Cellule(
        cache : in out T_Cache;
        ip: in IP_Adresse;
        politique : in T_Cache_Politique)
    is
        courant : T_Cache;
        cellule : T_Cache_Cellule;
        Bit : Integer;
    begin
        courant := cache;
        for i in reverse 0 .. 31 loop
            if not Est_Vide(courant) then
                Bit := Obtenir_Bit(ip, i);
                if Bit = 0 or Est_Vide(Droite(courant)) then
                    courant := Gauche(courant);
                else
                    courant := Droite(courant);
                end if;
            else
                null;
            end if;
        end loop;

        if Est_Vide(courant) then
            raise Route_Non_Presente;
        end if;

        cellule := Element(courant);
        cellule.nombre_utilisations := cellule.nombre_utilisations + 1;

        if politique = LRU then
            Compteur_Ticket := Compteur_Ticket + 1;
            cellule.index := Compteur_Ticket;
        end if;

        Enregistrer(cache, cellule.ip, cellule);
    end Mettre_A_Jour_Cellule;


    procedure Enregistrer_Cache(cache : in out T_Cache;
        ip : in IP_Adresse;
        masque : in IP_Adresse;
        interface_route : in Unbounded_String;
        politique : in T_Cache_Politique)
    is
        Nouvelle_Cellule : T_Cache_Cellule;
        IP_Masquee : constant IP_Adresse := ip and masque;
    begin
        if not Route_Existe(cache, IP_Masquee) then
            Nouvelle_Cellule.ip := IP_Masquee;
            Nouvelle_Cellule.masque := masque;
            Nouvelle_Cellule.interface_route := interface_route;
            Nouvelle_Cellule.nombre_utilisations := 1;
            Compteur_Ticket := Compteur_Ticket + 1;
            Nouvelle_Cellule.index := Compteur_Ticket;
            Enregistrer(cache, IP_Masquee, Nouvelle_Cellule);
        else
            Mettre_A_Jour_Cellule(cache, IP_Masquee, politique);
        end if;
    end Enregistrer_Cache;


    function Compter_Routes(Arbre : in T_Cache) return Integer is
        Compte : Integer;
    begin
        if Est_Vide(Arbre) then
            return 0;
        end if;
        if Element(Arbre).interface_route /= Null_Unbounded_String then
            Compte := 1;
        else
            Compte := 0;
        end if;
        return Compte + Compter_Routes(Gauche(Arbre)) + Compter_Routes(Droite(Arbre));
    end Compter_Routes;

    function Taille_Cache(cache : in T_Cache) return Integer is
    begin
        return Compter_Routes(cache);
    end Taille_Cache;


    procedure Afficher_Recursif(ABR : in T_Cache) is
        route : T_Route;
    begin
        if not Est_Vide(ABR) then
            Afficher_Recursif(Gauche(ABR));

            if Element(ABR).interface_route /= Null_Unbounded_String then
                Creer_Route(route, Element(ABR).ip, Element(ABR).masque, Element(ABR).interface_route);
                Afficher_Route(route);
            end if;

            Afficher_Recursif(Droite(ABR));
        end if;
    end Afficher_Recursif;


    procedure Afficher_Cache(cache : in T_Cache) is
    begin
        Afficher_Recursif(cache);
    end Afficher_Cache;


    procedure Trouver_Candidat(
        Arbre      : in T_Cache;
        Profondeur : in Integer;
        Politique  : in T_Cache_Politique;
        Candidat   : in out T_Cache_Cellule;
        Trouve     : in out Boolean)
    is
    begin
        if not Est_Vide(Arbre) then
            if Profondeur = 32 then
                if not Trouve then
                    Candidat := Element(Arbre);
                    Trouve := True;
                else
                    case Politique is
                        when FIFO | LRU =>
                            if Element(Arbre).index > 0 and then Element(Arbre).index < Candidat.index then
                                Candidat := Element(Arbre);
                            end if;
                        when LFU =>
                            if Element(Arbre).nombre_utilisations < Candidat.nombre_utilisations then
                                Candidat := Element(Arbre);
                            end if;
                    end case;
                end if;
            end if;
            if not Est_Vide(Gauche(Arbre)) then
                Trouver_Candidat(Gauche(Arbre), Profondeur + 1, Politique, Candidat, Trouve);
            end if;

            if not Est_Vide(Droite(Arbre)) then
                Trouver_Candidat(Droite(Arbre), Profondeur + 1, Politique, Candidat, Trouve);
            end if;
        end if;
    end Trouver_Candidat;


    procedure Supprimer_Cache(cache : in out T_Cache; politique : in T_Cache_Politique) is
        Candidat : T_Cache_Cellule;
        Trouve   : Boolean := False;
    begin
        Trouver_Candidat(cache, 0, politique, Candidat, Trouve);
        if Trouve then
            Supprimer(cache, Candidat.ip);
        end if;
    end Supprimer_Cache;


    procedure Detruire_Cache(cache : in out T_Cache) is
    begin
        Detruire(cache);
    end Detruire_Cache;

end Cache_LA;
