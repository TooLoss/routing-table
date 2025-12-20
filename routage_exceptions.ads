package Routage_Exceptions is

    -- si on essaie d'ajouter une route qui a la même ip 
    Duplicate_Route_Error : Exception;

    -- si les arguments sont incorrects
    Argument_Routage_Error : Exception;

    -- si le fichier n'existe pas
    Fichier_Introuvable_Error : Exception;

    -- si l'IP est invalide
    IP_Invalide_Erreur : Exception;

end Routage_Exceptions;
