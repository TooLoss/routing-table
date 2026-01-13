with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Routage;                   use Routage;
with Trie;

package Cache_LA is

    type T_Cache_Politique is (FIFO, LRU, LFU);

    type T_Cache_Cellule is limited private;

    type T_Cache is limited private;

    Compteur_Ticket : Integer := 0;

    -- Creer le cache
    procedure Initialiser_Cache(cache : out T_Cache) with
        Post => Taille_Cache(cache) = 0;

    -- Supprime du cache en fonction de la politique quand la taille est dépassée
    procedure Supprimer_Cache(cache : in out T_Cache;
        politique : in T_Cache_Politique);

    -- Convertit la chaine de caractère en cache politique
    function String_Vers_Politique(S : in Unbounded_String)
        return T_Cache_Politique;

    -- Recherche ip dans le cache (sera en O(log n) grâce à l'arbre)
    procedure Chercher_Cache(route : out T_Route; cache : in T_Cache; ip : in IP_Adresse);

    -- Enregistre la route dans le cache.
    procedure Enregistrer_Cache(cache : in out T_Cache;
        ip : in IP_Adresse;
        masque : in IP_Adresse;
        interface_route : in Unbounded_String;
        politique : in T_Cache_Politique);

    -- Récupère la taille du cache 
    function Taille_Cache(cache : in T_Cache) return Integer;

    -- Afficher toutes les routes du cache
    procedure Afficher_Cache(cache : in T_Cache);

    procedure Detruire_Cache(cache : in out T_Cache);

private

    type T_Cache_Cellule is 
        record
            ip : IP_Adresse;
            masque : IP_Adresse;
            interface_route : Unbounded_String;
            index : Integer := 0;
            nombre_utilisations : Integer := 0;
        end record;

    package Trie_Cache is
        new Trie (T_Element => T_Cache_Cellule);

    type T_Cache is new Trie_Cache.T_Trie;

end Cache_LA;
