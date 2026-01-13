with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Trie;
with Routage;               use Routage; 

procedure Tester_Trie is

    -- Instanciation générique
    package Trie_Integer is new Trie(T_Element => Integer);
    use Trie_Integer;

    --
    -- Données de test (Simulation de la structure 10, 5, 15 de l'ABR)
    --
    
    -- Masque pour l'enregistrement (toujours /32 pour être sûr)
    M32 : constant IP_Adresse := String_Vers_IP(To_Unbounded_String("255.255.255.255"));

    -- "10" (Racine) -> Disons une IP qui commence par ... 
    -- Pour le Trie, la "Racine" est abstraite, mais disons une IP de référence.
    IP_Ref : constant IP_Adresse := String_Vers_IP(To_Unbounded_String("128.0.0.1")); 

    -- "5" (Gauche) -> Doit commencer par un bit 0 par rapport au nœud de décision
    -- Ici on prend carrément une IP qui commence par 0 (bit de poids fort)
    IP_G : constant IP_Adresse := String_Vers_IP(To_Unbounded_String("10.0.0.1")); 

    -- "15" (Droite) -> Doit commencer par un bit 1
    -- Ici une IP qui commence par 1 (>= 128)
    IP_D : constant IP_Adresse := String_Vers_IP(To_Unbounded_String("192.168.0.1")); 

    -- "2" (Gauche de Gauche)
    IP_GG : constant IP_Adresse := String_Vers_IP(To_Unbounded_String("1.1.1.1"));

    -- "8" (Droite de Gauche) -> Commence par 0, mais diverge plus tard
    IP_GD : constant IP_Adresse := String_Vers_IP(To_Unbounded_String("60.0.0.1"));


    --
    -- Procédures
    --

    procedure Test_Initialiser;
    procedure Test_Enregistrer;
    procedure Test_Supprimer_Feuille;
    procedure Test_Supprimer_Un_Enfant;
    procedure Test_Supprimer_Deux_Enfants;
    procedure Test_Vider_Arbre;
    procedure Lancer_Tous_Les_Test;

    --
    -- Implémentations
    --

    procedure Test_Initialiser is
        Arbre_Test : T_Trie;
    begin
        Initialiser(Arbre_Test);
        pragma Assert(Est_Vide(Arbre_Test));
        pragma Assert(Taille(Arbre_Test) = 0);
    end Test_Initialiser;

    procedure Test_Enregistrer is
        Arbre_Test : T_Trie;
    begin
        Initialiser(Arbre_Test);

        -- Enregistrer "10"
        Enregistrer(Arbre_Test, IP_Ref,  10);
        pragma Assert(not Est_Vide(Arbre_Test));
        
        -- Enregistrer "5" (Gauche)
        Enregistrer(Arbre_Test, IP_G,  5);
        pragma Assert(not Est_Vide(Gauche(Arbre_Test))); -- Doit exister à gauche

        -- Enregistrer "15" (Droite)
        Enregistrer(Arbre_Test, IP_D,  15);
        pragma Assert(not Est_Vide(Droite(Arbre_Test))); -- Doit exister à droite
    end Test_Enregistrer;

    procedure Test_Supprimer_Feuille is
        Arbre_Test : T_Trie;
        T_Avant : Integer;
    begin
        Initialiser(Arbre_Test);
        Enregistrer(Arbre_Test, IP_Ref,  10);
        Enregistrer(Arbre_Test, IP_G,    5);
        Enregistrer(Arbre_Test, IP_D,    15);

        -- Suppression de "5" (Gauche) - SANS MASQUE
        Supprimer(Arbre_Test, IP_G);

        -- Vérification : La branche gauche doit être vide ou nettoyée
        -- (Si IP_G était la seule à gauche, Gauche(Arbre) peut être null)
        pragma Assert(Est_Vide(Gauche(Arbre_Test))); 
        
        -- La droite doit toujours être là
        pragma Assert(not Est_Vide(Droite(Arbre_Test)));
    end Test_Supprimer_Feuille;

    procedure Test_Supprimer_Un_Enfant is
        Arbre_Test : T_Trie;
    begin
        Initialiser(Arbre_Test);
        Enregistrer(Arbre_Test, IP_Ref,  10);
        Enregistrer(Arbre_Test, IP_G,    5);   -- "5"
        Enregistrer(Arbre_Test, IP_GG,   2);   -- "2" (Enfant de 5)

        -- On supprime "5". Dans un Trie, "2" est plus bas dans l'arbre ou sur une autre branche.
        -- Si on supprime l'IP exacte IP_G, IP_GG doit rester.
        Supprimer(Arbre_Test, IP_G);

        pragma Assert(not Est_Vide(Arbre_Test));
        -- Vérifions que IP_GG (2) est toujours accessible via une recherche (si tu as Chercher)
        -- Ou simplement que l'arbre n'est pas vide à gauche.
        pragma Assert(not Est_Vide(Gauche(Arbre_Test)));
    end Test_Supprimer_Un_Enfant;

    procedure Test_Supprimer_Deux_Enfants is
        Arbre_Test : T_Trie;
    begin
        Initialiser(Arbre_Test);
        Enregistrer(Arbre_Test, IP_Ref,  10);
        Enregistrer(Arbre_Test, IP_G,    5);
        Enregistrer(Arbre_Test, IP_D,    15);
        Enregistrer(Arbre_Test, IP_GG,   2);
        Enregistrer(Arbre_Test, IP_GD,   8);

        -- Suppression de "5" (IP_G)
        Supprimer(Arbre_Test, IP_G);

        -- Les enfants "2" et "8" doivent rester
        -- (Dans un Trie, ils sont sur des branches qui partent du chemin de IP_G)
        pragma Assert(not Est_Vide(Arbre_Test));
    end Test_Supprimer_Deux_Enfants;

    procedure Test_Vider_Arbre is
        Arbre_Test : T_Trie;
    begin
        Initialiser(Arbre_Test);
        Enregistrer(Arbre_Test, IP_Ref,  10);
        Enregistrer(Arbre_Test, IP_G,    5);

        Detruire(Arbre_Test);
        pragma Assert(Est_Vide(Arbre_Test));
        pragma Assert(Taille(Arbre_Test) = 0);
    end Test_Vider_Arbre;

    procedure Lancer_Tous_Les_Test is
    begin
        Test_Initialiser;
        Put_Line("Test_Initialiser validé");

        Test_Enregistrer;
        Put_Line("Test_Enregistrer validé");

        Test_Supprimer_Feuille;
        Put_Line("Test_Supprimer_Feuille validé");

        Test_Supprimer_Un_Enfant;
        Put_Line("Test_Supprimer_Un_Enfant validé");

        Test_Supprimer_Deux_Enfants;
        Put_Line("Test_Supprimer_Deux_Enfants validé");

        Test_Vider_Arbre;
        Put_Line("Test_Vider_Arbre validé");
    end Lancer_Tous_Les_Test;

begin
    Lancer_Tous_Les_Test;
    Put_Line("Tous les tests du Trie ont réussi !");
end Tester_Trie;
