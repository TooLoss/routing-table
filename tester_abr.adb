with Ada.Text_IO;           use Ada.Text_IO;
with ABR;

procedure Tester_ABR is

    function Est_Inferieur_Entier(G, D : Integer) return Boolean is
    begin
        return G < D;
    end Est_Inferieur_Entier;

    function Est_Egal_Entier(G, D : Integer) return Boolean is
    begin
        return G = D;
    end Est_Egal_Entier;

    package ABR_Integer is new ABR(
    T_Element => Integer,
    Est_Inferieur => Est_Inferieur_Entier,
    Est_Egal => Est_Egal_Entier);
    use ABR_Integer;

    --
    -- Declarations
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
        Arbre_Test : T_ABR;
    begin
        Initialiser(Arbre_Test);
        pragma Assert(Est_Vide(Arbre_Test));
        pragma Assert(Taille(Arbre_Test) = 0);
    end Test_Initialiser;

    procedure Test_Enregistrer is
        Arbre_Test : T_ABR;
    begin
        Initialiser(Arbre_Test);

        -- Insertion Racine
        Enregistrer(Arbre_Test, 10);
        pragma Assert(not Est_Vide(Arbre_Test));
        pragma Assert(Element(Arbre_Test) = 10);
        pragma Assert(Taille(Arbre_Test) = 1);

        -- Insertion Gauche
        Enregistrer(Arbre_Test, 5);
        pragma Assert(Element(Gauche(Arbre_Test)) = 5);
        pragma Assert(Taille(Arbre_Test) = 2);

        -- Insertion Droite
        Enregistrer(Arbre_Test, 15);
        pragma Assert(Element(Droite(Arbre_Test)) = 15);
        pragma Assert(Taille(Arbre_Test) = 3);

        -- Mise à jour 
        Enregistrer(Arbre_Test, 10); 
        pragma Assert(Taille(Arbre_Test) = 3); 
    end Test_Enregistrer;

    procedure Test_Supprimer_Feuille is
        Arbre_Test : T_ABR;
    begin
        Initialiser(Arbre_Test);
        Enregistrer(Arbre_Test, 10);
        Enregistrer(Arbre_Test, 5);
        Enregistrer(Arbre_Test, 15);

        Supprimer(Arbre_Test, 5);

        pragma Assert(Taille(Arbre_Test) = 2);
        pragma Assert(Est_Vide(Gauche(Arbre_Test)));
    end Test_Supprimer_Feuille;

    procedure Test_Supprimer_Un_Enfant is
        Arbre_Test : T_ABR;
    begin
        Initialiser(Arbre_Test);
        Enregistrer(Arbre_Test, 10);
        Enregistrer(Arbre_Test, 5);
        Enregistrer(Arbre_Test, 2);

        Supprimer(Arbre_Test, 5);

        pragma Assert(Taille(Arbre_Test) = 2);
        pragma Assert(Element(Arbre_Test) = 10);
        pragma Assert(Element(Gauche(Arbre_Test)) = 2);
    end Test_Supprimer_Un_Enfant;

    procedure Test_Supprimer_Deux_Enfants is
        Arbre_Test : T_ABR;
    begin
        Initialiser(Arbre_Test);
        Enregistrer(Arbre_Test, 10);
        Enregistrer(Arbre_Test, 5);
        Enregistrer(Arbre_Test, 15);
        Enregistrer(Arbre_Test, 2);
        Enregistrer(Arbre_Test, 8);

        Supprimer(Arbre_Test, 5);

        pragma Assert(Taille(Arbre_Test) = 4);
        pragma Assert(Element(Droite(Gauche(Arbre_Test))) = 8);
    end Test_Supprimer_Deux_Enfants;

    procedure Test_Vider_Arbre is
        Arbre_Test : T_ABR;
    begin
        Initialiser(Arbre_Test);
        Enregistrer(Arbre_Test, 10);
        Enregistrer(Arbre_Test, 5);
        Enregistrer(Arbre_Test, 15);

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
    Put_Line("Tous les tests de l'ABR ont réussi !");
end Tester_ABR;
