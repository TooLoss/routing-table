with Routage;          use Routage;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Fichier;           use Fichier;

procedure Scenario_Routage is

    Ip1 : IP_Adresse;
    Route1, Route2, Route3 : T_Route;

begin
    Ip1 := String_Vers_Ip(To_Unbounded_String("192.168.1.1"));

    Creer_Route(
        route => Route1,
        ip => String_Vers_Ip(To_Unbounded_String("192.168.1.0")),
        masque => String_Vers_Ip(To_Unbounded_String("255.255.255.0")),
        interface_route => To_Unbounded_String("eth0"));

    Creer_Route(
        route => Route2,
        ip => String_Vers_Ip(To_Unbounded_String("192.168.0.0")),
        masque => String_Vers_Ip(To_Unbounded_String("255.255.0.0")),
        interface_route => To_Unbounded_String("eth1"));

    Creer_Route(
        route => Route3,
        ip => String_Vers_Ip(To_Unbounded_String("172.16.0.1")),
        masque => String_Vers_Ip(To_Unbounded_String("255.255.0.0")),
        interface_route => To_Unbounded_String("eth2"));

    if Est_Valide(Ip1, Route1) then
        Put_Line("Route 2 est valide. C'est attendu");
    else
        Put_Line("Route 1 est invalide. Erreur");
    end if;

    if Est_Valide(Ip1, Route2) then
        Put_Line("Route 2 est valide. C'est attendu.");
    else
        Put_Line("Route 2 est invalide. Erreur");
    end if;

    if Est_Valide(Ip1, Route3) then
        Put_Line("Route 3 est valide. Erreur");
    else
        Put_Line("Route 3 est invalide. C'est attendu");
    end if;

end Scenario_Routage;
