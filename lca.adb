with Routage_Exceptions;    use Routage_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA is 
    
    procedure Free is
        new Ada.Unchecked_Deallocation (T_Cellule, T_LCA);

    procedure Initialiser(LCA : out T_LCA) is
    begin
        LCA := null;
    end Initialiser;


    procedure Detruire(LCA : in out T_LCA) is
    begin
        if LCA = null or else LCA.Suivant = null then
            Free(LCA);
        else
            Detruire(LCA.Suivant);
            Free(LCA);
        end if;
    end Detruire;


    function Est_Vide (LCA : T_LCA) return Boolean is
    begin
        return LCA = null;
    end Est_vide;


    procedure Enregistrer (LCA : in out T_LCA; Element : in T_Element) is 
        nouvelle_LCA : T_LCA;
    begin 
        if Est_Vide(LCA) then 
            nouvelle_LCA := new T_Cellule;
            nouvelle_LCA.Element := Element;
            nouvelle_LCA.Suivant := null;
            LCA := nouvelle_LCA;
        elsif LCA.Element = Element then 
            raise Duplicate_Route_Error;
        else 
            Enregistrer(LCA.Suivant, Element);
        end if;
    end Enregistrer;


    function Premier(LCA : in T_LCA) return T_LCA is
    begin
        return LCA;
    end Premier;


    function Element (LCA : in T_LCA) return T_Element is
    begin
        return LCA.Element;
    end Element;


    function Suivant (LCA : in T_LCA) return T_LCA is
    begin
        return LCA.Suivant;
    end Suivant;


    procedure Supprimer (LCA : in out T_LCA; Element : in T_Element) is
        to_delete : T_LCA;
    begin
        if LCA = null then
            raise Element_Abscent;
        else
            if LCA.ELement = Element then
                to_delete := LCA;
                LCA := LCA.Suivant;
                Free(to_delete);
            else
                Supprimer(LCA.Suivant, Element);
            end if;
        end if;
    end Supprimer;


    function Taille (LCA : in T_LCA) return Integer is
    begin
        if LCA /= null then
            return 1 + Taille (LCA.Suivant);
        else
            return 0;
        end if;
    end Taille;


    procedure Reaffecter(
        LCA : in out T_LCA;
        Ancien_Element : in T_Element;
        Nouveau_Element : in T_Element) is
        Courant : T_LCA;
        trouve : Boolean := False;
    begin
        Courant := LCA;
        while Courant /= null loop
            if Courant.Element = Ancien_Element then
                Courant.Element := Nouveau_Element;
                trouve := True;
            end if;
            Courant := Courant.Suivant;
        end loop;
        if not trouve then
            raise Element_Abscent;
        end if;
    end Reaffecter;


end LCA;
