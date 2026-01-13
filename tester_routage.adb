with Routage;               use Routage;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Routage_Exceptions;    use Routage_Exceptions;

procedure Tester_Routage is

    procedure Test_String_Vers_Ip;

    procedure Test_Masque_Valide;

    procedure Test_Creer_Route;

    procedure Test_Est_Valide;

    procedure Test_Find_Interface;

    procedure Test_Table_Routage;

    procedure Test_Exceptions;

    procedure Lancer_Tous_Les_Test;

    procedure Test_String_Vers_Ip is
        Ip1, Ip2, Ip3, Ip4, Ip5 : IP_Adresse;
    begin
        Ip1 := String_Vers_Ip(To_Unbounded_String("0.0.0.1"));
        pragma Assert(Ip1 = 1);

        Ip2 := String_Vers_Ip(To_Unbounded_String("0.0.255.0"));
        pragma Assert(Ip2 = 255 * 256);

        Ip3 := String_Vers_Ip(To_Unbounded_String("0.255.0.0"));
        pragma Assert(Ip3 = 255 * 256 * 256);

        Ip4 := String_Vers_Ip(To_Unbounded_String("255.0.0.0"));
        pragma Assert(Ip4 = 255 * 256 * 256 * 256);

        Ip5 := String_Vers_Ip(To_Unbounded_String("0.0.0.0"));
        pragma Assert(Ip5 = 0);
    end Test_String_Vers_Ip;

    procedure Test_Masque_Valide is
    begin
        pragma Assert(not Masque_Valide(String_Vers_Ip(To_Unbounded_String("0.0.255.255"))));
        pragma Assert(not Masque_Valide(String_Vers_Ip(To_Unbounded_String("255.0.0.255"))));
        pragma Assert(Masque_Valide(String_Vers_Ip(To_Unbounded_String("255.255.0.0"))));
        pragma Assert(Masque_Valide(String_Vers_Ip(To_Unbounded_String("255.255.255.0"))));
        pragma Assert(Masque_Valide(String_Vers_Ip(To_Unbounded_String("255.255.255.255"))));
        pragma Assert(Masque_Valide(String_Vers_Ip(To_Unbounded_String("0.0.0.0"))));
        pragma Assert(not Masque_Valide(String_Vers_Ip(To_Unbounded_String("0.0.0.255"))));
        pragma Assert(not Masque_Valide(String_Vers_Ip(To_Unbounded_String("0.255.255.255"))));
        pragma Assert(Masque_Valide(String_Vers_Ip(To_Unbounded_String("255.0.0.0"))));
        pragma Assert(not Masque_Valide(String_Vers_Ip(To_Unbounded_String("255.0.255.0"))));
        pragma Assert(not Masque_Valide(String_Vers_Ip(To_Unbounded_String("0.255.255.255"))));
        pragma Assert(not Masque_Valide(String_Vers_Ip(To_Unbounded_String("0.255.255.0"))));
    end Test_Masque_Valide;

    procedure Test_Creer_Route is
        MaRoute : T_Route;
    begin
        Creer_Route(
            route => MaRoute,
            ip => String_Vers_Ip(To_Unbounded_String("192.168.1.0")),
            masque => String_Vers_Ip(To_Unbounded_String("255.255.255.0")),
            interface_route => To_Unbounded_String("eth0")
            );
        pragma Assert(Get_Ip(MaRoute) = String_Vers_Ip(To_Unbounded_String("192.168.1.0")));
        pragma Assert(Get_Masque(MaRoute) = String_Vers_Ip(To_Unbounded_String("255.255.255.0")));
        pragma Assert(Get_Interface(MaRoute) = To_Unbounded_String("eth0"));
    end Test_Creer_Route;

    procedure Test_Est_Valide is
        RouteTest1, RouteTest2 : T_Route;
        Ip1, Ip2 : IP_Adresse;
    begin
        Ip1 := String_Vers_Ip(To_Unbounded_String("192.168.1.1"));
        Ip2 := String_Vers_Ip(To_Unbounded_String("10.0.0.1"));

        Creer_Route(
            route => RouteTest1,
            ip => String_Vers_Ip(To_Unbounded_String("192.168.1.0")),
            masque => String_Vers_Ip(To_Unbounded_String("255.255.255.0")),
            interface_route => To_Unbounded_String("eth0")
            );
        Creer_Route(
            route => RouteTest2,
            ip => String_Vers_Ip(To_Unbounded_String("192.168.0.0")),
            masque => String_Vers_Ip(To_Unbounded_String("255.255.0.0")),
            interface_route => To_Unbounded_String("eth1")
            );

        pragma Assert(Est_Valide(Ip1, RouteTest1));
        pragma Assert(Est_Valide(Ip1, RouteTest2));
        pragma Assert(not Est_Valide(Ip2, RouteTest1));
        pragma Assert(not Est_Valide(Ip2, RouteTest2));
    end Test_Est_Valide;

    procedure Test_Find_Interface is
        Table : T_Table_Routage;
        Route0, Route1, Route2 : T_Route;
        Ip1 : IP_Adresse;
    begin
        Ip1 := String_Vers_Ip(To_Unbounded_String("192.168.1.1"));
        Creer_Route(
            route => Route0,
            ip => String_Vers_Ip(To_Unbounded_String("192.168.1.0")),
            masque => String_Vers_Ip(To_Unbounded_String("255.255.255.0")),
            interface_route => To_Unbounded_String("eth0")
            );
        Creer_Route(
            route => Route1,
            ip => String_Vers_Ip(To_Unbounded_String("192.168.0.0")),
            masque => String_Vers_Ip(To_Unbounded_String("255.255.0.0")),
            interface_route => To_Unbounded_String("eth1")
            );
        Initialiser_Table(Table);
        Enregistrer_Route(Table, Route0);
        Enregistrer_Route(Table, Route1);
        Find_Interface(Route2, Ip1, Table);
        Put(To_String(Get_Interface(Route2)));
        pragma Assert(Get_Interface(Route2) = To_Unbounded_String("eth0"));
    end Test_Find_Interface;

    procedure Test_Table_Routage is
        Table : T_Table_Routage;
        Route : T_Route;
    begin
        Initialiser_Table(Table);
        pragma Assert(Table_Vide(Table));
        Creer_Route(
            route => Route,
            ip => String_Vers_Ip(To_Unbounded_String("192.168.1.0")),
            masque => String_Vers_Ip(To_Unbounded_String("255.255.255.0")),
            interface_route => To_Unbounded_String("eth0")
            );
        Enregistrer_Route(Table, Route);
        pragma Assert(not Table_Vide(Table));
        begin
            Enregistrer_Route(Table, Route);
        exception
            when Duplicate_Route_Error =>
                null;
            when others =>
                pragma assert(false);
        end;
    end Test_Table_Routage;

    procedure Test_Exceptions is
        RouteImpossible : T_Route;
    begin
        begin
            Creer_Route(
                route => RouteImpossible,
                ip => String_Vers_Ip(To_Unbounded_String("192.168.270.0")),
                masque => String_Vers_Ip(To_Unbounded_String("255.255.255.0")),
                interface_route => To_Unbounded_String("eth0")
                );
        exception
            when IP_Invalide_Erreur =>
                null;
            when others =>
                pragma assert(false);
        end;
    end;

    procedure Lancer_Tous_Les_Test is
    begin
        Test_String_Vers_Ip;
        Put_Line("Test_String_Vers_Ip validé");
        Test_Masque_Valide;
        Put_Line("Test_Masque_Valide validé");
        Test_Creer_Route;
        Put_Line("Test_Cree_Route validé");
        Test_Est_Valide;
        Put_Line("Test_Est_Valide validé");
        Test_Find_Interface;
        Put_Line("Test_Find_Interface validé");
        Test_Table_Routage;
        Put_Line("Test_Table_Routage validé");
        Test_Exceptions;
        Put_Line("Test_Exceptions validé");
    end Lancer_Tous_Les_Test;

begin
    Lancer_Tous_Les_Test;
    Put_Line("Tous les tests ont réussi !");
end Tester_Routage;

