with Ada.Text_IO;            use Ada.Text_IO;

package body LCA is 

     procedure Initailiser(LCA :out T_Pile )is
     begin
	     LCA:=null;
     end Initialiser;


     function Est_Vide (LCA : T_Pile) return Boolean is
     begin
	     return LCA = null ;
     end Est_vide ;
    

     procedure Enregistrer_Recursif (LCA : in out T_Pile;  Element : in T_Element) is 
     begin 
          if LCA = null then 
        	LCA : = new T_Pile'(
	        Element => Element,
	        Suivant => null);
       	elsif LCA.Element=Element then 
	   LCA.Element:= Element ;
        else 
           Enregistrer_Recursif(LCA.Suivant , Element ) ;
        end if ;
    end Enregistrer_Recursif ;


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

 end LCA ;

	     

