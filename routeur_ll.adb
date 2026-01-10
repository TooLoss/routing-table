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
            est_statistique_active : Boolean := False;
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
        Arguments : in T_Arguments);

    procedure Effectuer_Action(ligne : in Unbounded_String;
        table : in T_Table_Routage;
        cache : in T_Cache);

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
        Arguments : in T_Arguments)
    is
        route_t : T_Route;
        ip : IP_Adresse;
        route_dans_cache : Boolean := False;
        ip_valide : Boolean := True;
    begin
        -- Vérifie d'abord que la ligne est une action 
        Effectuer_Action(paquet, table, cache);

        -- Conversion en ip, sensible et nécéssite de respecter
        -- nombreuses pré-conditions
        begin
            ip := String_Vers_Ip(paquet);
        exception
            when others => ip_valide := False;
        end;

        if ip_valide then
            -- Recherche dans le cache
            begin
                Chercher_Cache(route_t, cache, ip);
                Put(fichier_resultats, paquet & " " & Get_Interface(route_t));
                New_Line(fichier_resultats);
                route_dans_cache := True;
            exception
                when Route_Non_Presente => route_dans_cache := False;
            end;

            -- Recherche dans la table
            if not route_dans_cache then
                begin
                    Find_Interface(route_t, ip, table);
                    Put(fichier_resultats, paquet & " " & Get_Interface(route_t));
                    New_Line(fichier_resultats);
                    if Taille_Cache(cache) > Arguments.cache_taille then
                        Enregistrer_Cache(cache, Get_Ip(route_t), Get_Masque(route_t), Get_Interface(route_t));
                    else
                        Enregistrer_Cache(cache, Get_Ip(route_t), Get_Masque(route_t), Get_Interface(route_t));
                    end if;
                exception
                    when Route_Non_Presente => null; 
                end;
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
        cache : in T_Cache) is
    begin
        if To_String(ligne) = "table" then
            Afficher_Table(table);
        elsif To_String(ligne) = "cache" then
            Afficher_Cache(cache);
        end if;
    end Effectuer_Action;

    Arguments : T_Arguments;
    fichier_table : File_Type;
    fichier_resultats : File_Type;
    fichier_paquets : File_Type;
    ligne : Unbounded_String;
    table : T_Table_Routage;
    cache : T_Cache;

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
            Arguments.cache_taille := Integer'Value(Argument(i+1));
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
            Traiter_Paquet(ligne, table, cache, fichier_resultats, Arguments);
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

exception
    when Fichier_Introuvable_Error =>
        Put_Line("Vérifier que les fichiers en arguments existent : " &
        Arguments.nom_table & " " & Arguments.nom_paquets);
    when Argument_Routage_Error =>
        Put_Line("Arguments incorrects");
    when Duplicate_Route_Error =>
        Put_Line("La table de routage contient des routes en doublons");
end Routeur_LL;
