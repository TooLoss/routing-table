with Ada.Unchecked_Deallocation;

package body ABR is

    procedure Free is new Ada.Unchecked_Deallocation(T_Noeud, T_ABR);

    procedure Initialiser(ABR : out T_ABR) is
    begin
        ABR := null;
    end;

    function Est_Vide(ABR : in T_ABR) return Boolean is
    begin
        return ABR = null;
    end Est_Vide;

    procedure Enregistrer(ABR : in out T_ABR; Element : in T_Element) is
        nouveau : T_ABR;
    begin
        if Est_Vide(ABR) then
            nouveau := new T_Noeud;
            nouveau.Element := Element;
            nouveau.Gauche := null;
            nouveau.Droite := null;
            ABR := nouveau;
        elsif Est_Egal(Element, ABR.Element) then
            ABR.Element := Element;
        elsif Est_Inferieur(Element, ABR.Element) then
            Enregistrer(ABR.Gauche, Element);
        else
            Enregistrer(ABR.Droite, Element);
        end if;
    end Enregistrer;

    function Element(ABR : in T_ABR) return T_Element is
    begin
        return ABR.Element;
    end Element;

    function Gauche(ABR : in T_ABR) return T_ABR is
    begin
        return ABR.Gauche;
    end Gauche;

    function Droite(ABR : in T_ABR) return T_ABR is
    begin
        return ABR.Droite;
    end Droite;

    -- Trouver le sous-abre le plus à droite
    procedure Remonter(ABR : in out T_ABR; Max : out T_Element) is
        to_delete : T_ABR;
    begin
        if Est_Vide(ABR.Droite) then
            Max := ABR.Element;
            to_delete := ABR;
            ABR := ABR.Gauche;
            Free(to_delete);
        else
            Remonter(ABR.Droite, Max);
        end if;
    end Remonter;

    -- Effectue tout les cas possible pour supprimer correctement
    procedure Supprimer_Cas(ABR : in out T_ABR) is
        to_delete : T_ABR;
        remplacement : T_Element;
    begin
        if Est_Vide(ABR.Gauche) and Est_Vide(ABR.Droite) then
            Free(ABR);
        elsif Est_Vide(ABR.Gauche) then
            to_delete := ABR;
            ABR := ABR.Droite;
            Free(to_delete);
        elsif Est_Vide(ABR.Droite) then
            to_delete := ABR;
            ABR := ABR.Gauche;
            Free(to_delete);
        else
            Remonter(ABR.Gauche, remplacement);
            ABR.Element := remplacement;
        end if;
    end Supprimer_Cas;

    procedure Supprimer(ABR : in out T_ABR; Element : in T_Element) is
    begin
        if not Est_Vide(ABR) then
            if Est_Egal(Element, ABR.Element) then
                Supprimer_Cas(ABR);
            elsif Est_Inferieur(Element, ABR.Element) then
                Supprimer(ABR.Gauche, Element);
            else
                Supprimer(ABR.Droite, Element);
            end if;
        else
            null;
        end if;
    end Supprimer;

    function Taille(ABR : in T_ABR) return Integer is
    begin
        if Est_Vide(ABR) then
            return 0;
        else
            return 1 + Taille(ABR.Gauche) + Taille(ABR.Droite);
        end if;
    end Taille;

    procedure Detruire(ABR : in out T_ABR) is
    begin
        if not Est_Vide(ABR) then
            Detruire(ABR.Gauche);
            Detruire(ABR.Droite);
            Free(ABR);
        end if;
    end Detruire;

end ABR;
