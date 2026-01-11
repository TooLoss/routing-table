with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Cache_LA;                  use Cache_LA;
with Routage;                   use Routage;
with Routage_Exceptions;        use Routage_Exceptions;

procedure Tester_Cache_LA is

    procedure Test_String_Vers_Politique;
    procedure Test_Initialiser_Cache;
    procedure Test_Enregistrer_Cache;
    procedure Test_Chercher_Cache;
    procedure Test_Supprimer_Cache_FIFO;
    procedure Test_Supprimer_Cache_LRU;
    procedure Test_Supprimer_Cache_LFU;
    procedure Lancer_Tous_Les_Test;

    procedure Test_Chercher_Cache is
        Cache_Test : T_Cache;
        Route1 : T_Route;
        Ip1 : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("192.168.1.1"));
        Masque : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("255.255.255.0"));
        interface_route : constant Unbounded_String := To_Unbounded_String("eth0");
    begin
        Initialiser_Cache(Cache_Test);
        Enregistrer_Cache(Cache_Test, Ip1, Masque, interface_route, FIFO);
        Chercher_Cache(Route1, Cache_Test, Ip1);
        pragma Assert(Get_Ip(Route1) = Ip1);
    end Test_Chercher_Cache;

    procedure Test_Supprimer_Cache_FIFO is
        Cache_Test : T_Cache;
        Ip1 : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("192.168.12.1"));
        Ip2 : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("192.168.1.2"));
        Masque : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("255.255.255.0"));
        interface_route : constant Unbounded_String := To_Unbounded_String("eth0");
        Route1, Route2 : T_Route;
    begin
        Initialiser_Cache(Cache_Test);
        Enregistrer_Cache(Cache_Test, Ip1, Masque, interface_route, FIFO);
        Enregistrer_Cache(Cache_Test, Ip2, Masque, interface_route, FIFO);
        Enregistrer_Cache(Cache_Test, Ip1, Masque, interface_route, FIFO);
        Supprimer_Cache(Cache_Test, FIFO);
        pragma Assert(Taille_Cache(Cache_Test) = 1);
        Chercher_Cache(Route1, Cache_Test, Ip2);
        begin
            Chercher_Cache(Route2, Cache_Test, Ip1);
            pragma Assert(false);
        exception
            when Route_Non_Presente =>
                null;
        end;
    end Test_Supprimer_Cache_FIFO;

    procedure Test_Supprimer_Cache_LRU is
        Cache_Test : T_Cache;
        Ip1 : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("192.168.12.1"));
        Ip2 : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("192.168.1.2"));
        Masque : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("255.255.255.0"));
        interface_route : constant Unbounded_String := To_Unbounded_String("eth0");
        Route1, Route2 : T_Route;
    begin
        Initialiser_Cache(Cache_Test);
        Enregistrer_Cache(Cache_Test, Ip1, Masque, interface_route, LRU);
        Enregistrer_Cache(Cache_Test, Ip2, Masque, interface_route, LRU);
        Enregistrer_Cache(Cache_Test, Ip1, Masque, interface_route, LRU);
        Supprimer_Cache(Cache_Test, LRU);
        pragma Assert(Taille_Cache(Cache_Test) = 1);
        Chercher_Cache(Route1, Cache_Test, Ip1);
        begin
            Chercher_Cache(Route2, Cache_Test, Ip2);
            pragma Assert(false);
        exception
            when Route_Non_Presente =>
                null;
        end;
    end Test_Supprimer_Cache_LRU;

    procedure Test_Supprimer_Cache_LFU is
        Cache_Test : T_Cache;
        Ip1 : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("192.168.12.1"));
        Ip2 : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("192.168.1.2"));
        Ip3 : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("192.168.3.2"));
        Masque : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("255.255.255.0"));
        interface_route : constant Unbounded_String := To_Unbounded_String("eth0");
        Route1, Route2, Route3 : T_Route;
    begin
        Initialiser_Cache(Cache_Test);
        Enregistrer_Cache(Cache_Test, Ip1, Masque, interface_route, LFU);
        Enregistrer_Cache(Cache_Test, Ip1, Masque, interface_route, LFU);
        Enregistrer_Cache(Cache_Test, Ip1, Masque, interface_route, LFU);
        Enregistrer_Cache(Cache_Test, Ip1, Masque, interface_route, LFU);
        Enregistrer_Cache(Cache_Test, Ip2, Masque, interface_route, LFU);
        Enregistrer_Cache(Cache_Test, Ip2, Masque, interface_route, LFU);
        Enregistrer_Cache(Cache_Test, Ip2, Masque, interface_route, LFU);
        Enregistrer_Cache(Cache_Test, Ip3, Masque, interface_route, LFU);
        Enregistrer_Cache(Cache_Test, Ip3, Masque, interface_route, LFU);
        Enregistrer_Cache(Cache_Test, Ip3, Masque, interface_route, LFU);
        Enregistrer_Cache(Cache_Test, Ip3, Masque, interface_route, LFU);
        Enregistrer_Cache(Cache_Test, Ip3, Masque, interface_route, LFU);

        Supprimer_Cache(Cache_Test, LFU);
        pragma Assert(Taille_Cache(Cache_Test) = 2);
        Chercher_Cache(Route1, Cache_Test, Ip3);
        Chercher_Cache(Route2, Cache_Test, Ip1);
        begin
            Chercher_Cache(Route3, Cache_Test, Ip2);
            pragma Assert(false);
        exception
            when Route_Non_Presente =>
                null;
        end;
        Supprimer_Cache(Cache_Test, LFU);
        pragma Assert(Taille_Cache(Cache_Test) = 1);
        Chercher_Cache(Route1, Cache_Test, Ip3);
        begin
            Chercher_Cache(Route2, Cache_Test, Ip1);
            pragma Assert(false);
        exception
            when Route_Non_Presente =>
                null;
        end;
    end Test_Supprimer_Cache_LFU;

    -- Les autres procédures restent inchangées
    procedure Test_String_Vers_Politique is
        Politique1, Politique2, Politique3 : T_Cache_Politique;
    begin
        Politique1 := String_Vers_Politique(To_Unbounded_String("FIFO"));
        pragma Assert(Politique1 = FIFO);

        Politique2 := String_Vers_Politique(To_Unbounded_String("LRU"));
        pragma Assert(Politique2 = LRU);

        Politique3 := String_Vers_Politique(To_Unbounded_String("LFU"));
        pragma Assert(Politique3 = LFU);

        begin
            Politique1 := String_Vers_Politique(To_Unbounded_String("INVALIDE"));
            pragma Assert(False);
        exception
            when Argument_Routage_Error =>
                null;
        end;
    end Test_String_Vers_Politique;

    procedure Test_Initialiser_Cache is
        Cache_Test : T_Cache;
    begin
        Initialiser_Cache(Cache_Test);
        pragma Assert(Taille_Cache(Cache_Test) = 0);
    end Test_Initialiser_Cache;

    procedure Test_Enregistrer_Cache is
        Cache_Test : T_Cache;
        Ip1 : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("192.168.1.1"));
        Masque : constant IP_Adresse := String_Vers_Ip(To_Unbounded_String("255.255.255.0"));
        interface_route : constant Unbounded_String := To_Unbounded_String("eth0");
    begin
        Initialiser_Cache(Cache_Test);
        Enregistrer_Cache(Cache_Test, Ip1, Masque, interface_route, FIFO);
        pragma Assert(Taille_Cache(Cache_Test) = 1);
    end Test_Enregistrer_Cache;

    procedure Lancer_Tous_Les_Test is
    begin
        Test_String_Vers_Politique;
        Put_Line("Test_String_Vers_Politique validé");

        Test_Initialiser_Cache;
        Put_Line("Test_Initialiser_Cache validé");

        Test_Enregistrer_Cache;
        Put_Line("Test_Enregistrer_Cache validé");

        Test_Chercher_Cache;
        Put_Line("Test_Chercher_Cache validé");

        Test_Supprimer_Cache_FIFO;
        Put_Line("Test_Supprimer_Cache_FIFO validé");

        Test_Supprimer_Cache_LFU;
        Put_Line("Test_Supprimer_Cache_LFU validé");

        Test_Supprimer_Cache_LRU;
        Put_Line("Test_Supprimer_Cache_LRU validé");
    end Lancer_Tous_Les_Test;

begin
    Lancer_Tous_Les_Test;
    Put_Line("Tous les tests ont réussi !");
end Tester_Cache_LA;
