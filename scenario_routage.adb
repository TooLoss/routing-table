with Routage;           use Routage;
with Fichier;           use Fichier;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Scenario_Routage is

    Ip1 : IP_Adresse;
    Route0, Route1, Route2, Route3 : T_Route;
    Table : T_Table_Routage;
    Founded_Interface : Unbounded_String;

begin

    Put_Line("Convertir String 0.0.0.255 en IP");
    Ip1 := String_Vers_Ip(To_Unbounded_String("0.0.0.255"));
    Put_Line(Integer'Image(Integer(Ip1))); 

    Put_Line("Vérifier que le masque 0.0.255.255 est pas correct");
    if Masque_Valide(String_Vers_Ip(To_Unbounded_String("0.0.255.255"))) then
        Put_Line("Masque Valide");
    else
        Put_Line("Masque invalide");
    end if;
    Put_Line("Vérifier que le masque 255.0.0.255 est pas correct");
    if Masque_Valide(String_Vers_Ip(To_Unbounded_String("255.0.0.255"))) then
        Put_Line("Masque Valide");
    else
        Put_Line("Masque invalide");
    end if;
    Put_Line("Vérifier que le masque 255.255.0.0 est correct");
    if Masque_Valide(String_Vers_Ip(To_Unbounded_String("255.255.0.0"))) then
        Put_Line("Masque Valide");
    else
        Put_Line("Masque invalide");
    end if;

    Put_Line("Conversion IP 192.168.1.1 en type IP_Adresse. Ip de destination.");
    Ip1 := String_Vers_Ip(To_Unbounded_String("192.168.1.1"));

    Put_Line("Cration d'une route dans la table de routage : 192.168.1.0    255.255.0.0   wifi");
    Creer_Route(
        route => Route0,
        ip => String_Vers_Ip(To_Unbounded_String("192.168.1.0")),
        masque => String_Vers_Ip(To_Unbounded_String("255.255.0.0")),
        interface_route => To_Unbounded_String("wifi"));

    Put_Line("Cration d'une route dans la table de routage : 192.168.1.0    255.255.255.0   eth0");
    Creer_Route(
        route => Route1,
        ip => String_Vers_Ip(To_Unbounded_String("192.168.1.0")),
        masque => String_Vers_Ip(To_Unbounded_String("255.255.255.0")),
        interface_route => To_Unbounded_String("eth0"));

    Put_Line("Cration d'une route dans la table de routage : 192.168.0.0    255.255.0.0   eth1");
    Creer_Route(
        route => Route2,
        ip => String_Vers_Ip(To_Unbounded_String("192.168.0.0")),
        masque => String_Vers_Ip(To_Unbounded_String("255.255.0.0")),
        interface_route => To_Unbounded_String("eth1"));

    Put_Line("Cration d'une route dans la table de routage : 172.16.0.1    255.255.0.0   eth2");
    Creer_Route(
        route => Route3,
        ip => String_Vers_Ip(To_Unbounded_String("172.16.0.1")),
        masque => String_Vers_Ip(To_Unbounded_String("255.255.0.0")),
        interface_route => To_Unbounded_String("eth2"));

    Put_Line("Vérifier que l'IP destination correspond à la Route 1 crée");
    if Est_Valide(Ip1, Route1) then
        Put_Line("Route 1 est valide. C'est attendu");
    else
        Put_Line("Route 1 est invalide. Erreur");
    end if;

    Put_Line("Vérifier que l'IP destination correspond à la Route 2 crée");
    if Est_Valide(Ip1, Route2) then
        Put_Line("Route 2 est valide. C'est attendu.");
    else
        Put_Line("Route 2 est invalide. Erreur");
    end if;

    Put_Line("Vérifier que l'IP ne correspond pas à la Route 3 crée");
    if Est_Valide(Ip1, Route3) then
        Put_Line("Route 3 est valide. Erreur");
    else
        Put_Line("Route 3 est invalide. C'est attendu");
    end if;

    Put_Line("Crée la table de routage");
    Initialiser_Table(Table);
    Put_Line("Enregister Route0");
    Enregistrer_Route(Table, Route0);
    Put_Line("Enregister Route1");
    Enregistrer_Route(Table, Route1);
    Put_Line("Enregister Route2");
    Enregistrer_Route(Table, Route2);
    Put_Line("Enregister Route3");
    Enregistrer_Route(Table, Route3);

    Put_Line("Récupérer l'interface automatiquement");
    Put("Interface retourné doit être eth0: ");
    Founded_Interface := Get_Interface(Ip1, Table);
    Put_Line(To_String(Get_Interface(Ip1, Table)));


end Scenario_Routage;
