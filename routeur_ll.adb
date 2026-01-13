with Routage;                   use Routage;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Strings;               use Ada.Strings;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Routage_Exceptions;        use Routage_Exceptions;
with Cache_LL;                  use Cache_LL;


procedure Routeur_LL is

    type T_Arguments is
        record
            nom_table : Unbounded_String := To_Unbounded_String("table.txt");
            nom_paquets : Unbounded_String := To_Unbounded_String("paquets.txt");
            nom_resultats : Unbounded_String := To_Unbounded_String("resultats.txt");
            cache_taille : Integer := 10;
            cache_politique : T_Cache_Politique := FIFO;
            est_statistique_active : Boolean := True;
            est_cache_active : Boolean := True;
        end record;

    type T_Stat is
        record
            cache_erreur : Integer := 0;
            route_total : Integer := 0;
        end record;


    --
    --  Fonctions pour Routage
    --


    -- Vérifier que le fichier existe, sinon une exception est levée
    procedure Verifier_Presence_Fichier(filename : in Unbounded_String);

    -- Vérifier si le prochaine argument existe, sinon raise une exception.
    procedure Verifier_Prochain_Argument(i : in Integer; max : in Integer);

    -- Traiter une IP présent dans le paquets.txt
    procedure Traiter_Paquet(paquet : in Unbounded_String;
        table : in T_Table_Routage;
        cache : in out T_Cache;
        fichier_resultats : in out File_Type;
        Arguments : in T_Arguments;
        Statistiques : in out T_Stat);

    procedure Effectuer_Action(ligne : in Unbounded_String;
        table : in T_Table_Routage;
        cache : in T_Cache;
        stat : in T_Stat;
        num : in Integer);

    procedure Afficher_Statistiques(stat : in T_Stat);
    
    procedure Mettre_A_Jour_Cache(cache : in out T_Cache; route_t : in T_Route; Arguments : in T_Arguments);

    --
    -- Implémentations
    --

    procedure Verifier_Prochain_Argument(i : in Integer; max : in Integer) is
    begin
        if i+1 > max then
            raise Argument_Routage_Error;
        end if;
    end Verifier_Prochain_Argument;

    procedure Traiter_Paquet(
        paquet : in Unbounded_String;
        table : in T_Table_Routage;
        cache : in out T_Cache;
        fichier_resultats : in out File_Type;
        Arguments : in T_Arguments;
        Statistiques : in out T_Stat)
    is
        route_t : T_Route;
        ip : IP_Adresse;
        route_dans_cache : Boolean := False;
        ip_valide : Boolean := True;
        trouve : Boolean := False;
    begin

        -- Conversion en ip, sensible et nécéssite de respecter
        -- nombreuses pré-conditions
        begin
            ip := String_Vers_Ip(paquet);
        exception
            when others => ip_valide := False;
        end;

        if ip_valide then
            Statistiques.route_total := Statistiques.route_total + 1;
            -- Recherche dans le cache
            if Arguments.est_cache_active then
                begin
                    Chercher_Cache(route_t, cache, ip);
                    Put(fichier_resultats, paquet & " " & Get_Interface(route_t));
                    New_Line(fichier_resultats);
                    route_dans_cache := True;
                    trouve := True;
                exception
                    when Route_Non_Presente => route_dans_cache := False;
                end;
            else
                null;
            end if;

            -- Recherche dans la table
            if not route_dans_cache then
                Statistiques.cache_erreur := Statistiques.cache_erreur + 1;
                begin
                    Find_Interface(route_t, ip, table);
                    Put(fichier_resultats, paquet & " " & Get_Interface(route_t));
                    New_Line(fichier_resultats);
                    trouve := True;
                exception
                    when Route_Non_Presente => null; 
                end;
            else
                null;
            end if;

            -- Mise à jour du cache
            if Arguments.est_cache_active and trouve then
                Mettre_A_Jour_Cache(cache, route_t, Arguments);
            else
                null;
            end if;
        end if; 
    end Traiter_Paquet;

    procedure Verifier_Presence_Fichier(filename : in Unbounded_String) is
        fichier_locale : File_Type;
    begin
        begin
            Open(fichier_locale, In_File, To_String(filename));
            if not Is_Open(fichier_locale) then
                raise Fichier_Introuvable_Error;
            end if;
            Close(fichier_locale);
        exception
            when others =>
                raise Fichier_Introuvable_Error;
        end;
    end Verifier_Presence_Fichier;

    procedure Effectuer_Action(ligne : in Unbounded_String;
        table : in T_Table_Routage;
        cache : in T_Cache;
        stat : in T_Stat;
        num : in Integer) is
    begin
        if To_String(ligne) = "table" then
            Put_Line("table (ligne " & Integer'Image(num) & ")");
            Afficher_Table(table);
            new_line;
        elsif To_String(ligne) = "cache" then
            Put_Line("cache (ligne " & Integer'Image(num) & ")");
            Afficher_Cache(cache);
            new_line;
        elsif To_String(ligne) = "fin" then
            Put_Line("fin (ligne " & Integer'Image(num) & ")");
            raise Shutdown;
        elsif To_String(ligne) = "stat" then
            Put_Line("stat (ligne " & Integer'Image(num) & ")");
            Afficher_Statistiques(stat);
            new_line;
        end if;
    end Effectuer_Action;

    procedure Afficher_Statistiques(stat : in T_Stat) is
        taux_defaut : Float;
    begin
        Put_Line("Nombre de défaut de cache : " & Integer'Image(stat.cache_erreur));
        Put_Line("Nombre total de route : " & Integer'Image(stat.route_total));
        taux_defaut := Float(stat.cache_erreur)/Float(stat.route_total);
        Put_Line("Taux de défaut de cache : " & Float'Image(taux_defaut));
    end Afficher_Statistiques;

    procedure Mettre_A_Jour_Cache(cache : in out T_Cache; route_t : in T_Route; Arguments : in T_Arguments) is
    begin
        if Taille_Cache(cache) >= Arguments.cache_taille then
            Supprimer_Cache(cache, Arguments.cache_politique);
        else
            null;
        end if;
        Enregistrer_Cache(cache, Get_Ip(route_t), Get_Masque(route_t), Get_Interface(route_t), Arguments.cache_politique);
    end Mettre_A_Jour_Cache;

    Arguments : T_Arguments;
    Statistiques : T_Stat;
    fichier_table : File_Type;
    fichier_resultats : File_Type;
    fichier_paquets : File_Type;
    ligne : Unbounded_String;
    table : T_Table_Routage;
    cache : T_Cache;
    ligne_numero : Integer := 1;

begin

    --
    --  Routeur_LL
    --

    -- lire les arguments
    for i in 1..Argument_Count loop
        if Argument(i) = "-t" then
            Verifier_Prochain_Argument(i, Argument_Count);
            Arguments.nom_table := To_Unbounded_String(Argument(i+1));
        elsif Argument(i) = "-q" then
            Verifier_Prochain_Argument(i, Argument_Count);
            Arguments.nom_paquets := To_Unbounded_String(Argument(i+1));
        elsif Argument(i) = "-r" then
            Verifier_Prochain_Argument(i, Argument_Count);
            Arguments.nom_resultats := To_Unbounded_String(Argument(i+1));
        elsif Argument(i) = "-c" then
            Verifier_Prochain_Argument(i, Argument_Count);
            if Integer'Value(Argument(i+1)) < 0 then
                raise Argument_Routage_Error;
            elsif Integer'Value(Argument(i+1)) = 0 then
                Arguments.est_cache_active := False;
            else
                Arguments.cache_taille := Integer'Value(Argument(i+1));
            end if;
        elsif Argument(i) = "-p" then
            Verifier_Prochain_Argument(i, Argument_Count);
            Arguments.cache_politique := String_Vers_Politique(To_Unbounded_String(Argument(i+1)));
        elsif Argument(i) = "-s" then
            Arguments.est_statistique_active := True;
        elsif Argument(i) = "-S" then
            Arguments.est_statistique_active := False;
        end if;
    end loop;


    -- preparer le fichier résultats
    Create(fichier_resultats, Out_File, To_String(Arguments.nom_resultats));

    -- creer le cache
    Initialiser_Cache(cache);

    -- creer la table de routage
    Verifier_Presence_Fichier(Arguments.nom_table);
    Open(fichier_table, In_File, To_String(Arguments.nom_table));
    Charger_Table_Routage(table, fichier_table);
    --Afficher_Table(table);
    Close(fichier_table);

    -- Pour chaque paquet, on extrait l'adresse IP destination,
    -- on cherche l'interface correspondante dans la table de routage,
    -- et on écrit le résultat dans le fichier de sortie.
    Verifier_Presence_Fichier(Arguments.nom_paquets);
    Open(fichier_paquets, In_File, To_String(Arguments.nom_paquets));
    begin
        loop
            ligne := Get_Line(fichier_paquets);
            Effectuer_Action(ligne, table, cache, Statistiques, ligne_numero);
            Traiter_Paquet(ligne, table, cache, fichier_resultats, Arguments, Statistiques);
            ligne_numero := ligne_numero + 1;
            exit when End_Of_File(fichier_paquets);
        end loop;
    exception
        when End_Error =>
            Put_Line("Blancs en surplus à la fin du fichier.");
            null;
        when IP_Invalide_Erreur =>
            Put_Line("Une IP est invalide dans le fichier paquets");
            null;
    end;

    -- operations terminé, fermer les fichiers.
    Close(fichier_paquets);
    Close(fichier_resultats);

    -- détruire la table de routage
    Detruire_Table(table);
    Detruire_Cache(cache);

    if Arguments.est_statistique_active then
        Afficher_Statistiques(Statistiques);
    else
        null;
    end if;

exception
    when Fichier_Introuvable_Error =>
        Put_Line("Vérifier que les fichiers en arguments existent : " &
        Arguments.nom_table & " " & Arguments.nom_paquets);
    when Argument_Routage_Error =>
        Put_Line("Arguments incorrects");
    when Duplicate_Route_Error =>
        Put_Line("La table de routage contient des routes en doublons");
    when Shutdown =>
        if Arguments.est_statistique_active then
            Afficher_Statistiques(Statistiques);
        else
            null;
        end if;
end Routeur_LL;
