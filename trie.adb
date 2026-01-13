with Ada.Unchecked_Deallocation;

package body Trie is

    procedure Free is new Ada.Unchecked_Deallocation(T_Noeud, T_Trie);

    procedure Initialiser(Trie : out T_Trie) is
    begin
        Trie := null;
    end Initialiser;

    function Est_Vide(Trie : in T_Trie) return Boolean is
    begin
        return Trie = null;
    end Est_Vide;

    procedure Enregistrer(Trie : in out T_Trie; IP : in IP_Adresse; Element : in T_Element) is
        Courant : T_Trie;
        Bit : Integer;
    begin
        if Trie = null then
            Trie := new T_Noeud;
        else
            null;
        end if;
        Courant := Trie;
        for i in reverse 0 .. 31 loop
            Bit := Obtenir_Bit(IP, i);
            if Bit = 0 then
                if Courant.Gauche = null then
                    Courant.Gauche := new T_Noeud;
                else
                    null;
                end if;
                Courant := Courant.Gauche;
            else
                if Courant.Droite = null then
                    Courant.Droite := new T_Noeud;
                else
                    null;
                end if;
                Courant := Courant.Droite;
            end if;
        end loop;
        Courant.Element := Element;
    end Enregistrer;

    function Element(Trie : in T_Trie) return T_Element is
    begin
        return Trie.Element;
    end Element;

    function Gauche(Trie : in T_Trie) return T_Trie is
    begin
        return Trie.Gauche;
    end Gauche;

    function Droite(Trie : in T_Trie) return T_Trie is
    begin
        return Trie.Droite;
    end Droite;

    procedure Supprimer_Rec(Noeud : in out T_Trie; IP : in IP_Adresse; Profondeur : in Integer) is
        Bit : Integer;
    begin
        if Noeud /= null then
            if Profondeur < 32 then
                Bit := Obtenir_Bit(IP, 31 - Profondeur);
                if Bit = 0 then
                    Supprimer_Rec(Noeud.Gauche, IP, Profondeur + 1);
                else
                    Supprimer_Rec(Noeud.Droite, IP, Profondeur + 1);
                end if;
            end if;

            if Noeud.Gauche = null and Noeud.Droite = null then
                Free(Noeud);
                Noeud := null;
            end if;
        end if;
    end Supprimer_Rec;

    procedure Supprimer(Trie : in out T_Trie; IP : in IP_Adresse) is
    begin
        Supprimer_Rec(Trie, IP, 0);
    end Supprimer;

    function Taille(Trie : in T_Trie) return Integer is
    begin
        if Est_Vide(Trie) then
            return 0;
        else
            return 1 + Taille(Trie.Gauche) + Taille(Trie.Droite);
        end if;
    end Taille;

    procedure Detruire(Trie : in out T_Trie) is
    begin
        if not Est_Vide(Trie) then
            Detruire(Trie.Gauche);
            Detruire(Trie.Droite);
            Free(Trie);
            Trie := null;
        end if;
    end Detruire;

end Trie;
