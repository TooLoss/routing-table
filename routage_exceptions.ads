package Routage_Exceptions is

    -- si on essaie d'ajouter une route qui a la même ip 
    Duplicate_Route_Error : Exception;

    -- si les arguments sont incorrects
    Argument_Routage_Error : Exception;

    -- si le fichier n'existe pas
    Fichier_Introuvable_Error : Exception;

    -- si l'IP est invalide
    IP_Invalide_Erreur : Exception;

    -- si l'element est abscent dans la LCA
    Element_Abscent : Exception;

    -- si la route n'est pas dans la table de routage. C'est mieux
    -- que de renvoyer une valeur nul par défaut 
    Route_Non_Presente : Exception;

end Routage_Exceptions;
