with Routage_Exceptions;    use Routage_Exceptions;

package body LCA is 

    procedure Initialiser(LCA : out T_LCA) is
    begin
        LCA := null;
    end Initialiser;


    function Est_Vide (LCA : T_LCA) return Boolean is
    begin
        return LCA = null;
    end Est_vide;


    procedure Enregistrer_Recursif (LCA : in out T_LCA; Element : in T_Element) is 
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
            Enregistrer_Recursif(LCA.Suivant, Element);
        end if;
    end Enregistrer_Recursif;


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

end LCA;
