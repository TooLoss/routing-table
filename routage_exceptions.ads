package Routage_Exceptions is

    -- si on essaie d'ajouter une route qui a la même ip 
    Duplicate_Route_Error : Exception;

    -- si les arguments sont incorrects
    Argument_Routage_Error : Exception;

end Routage_Exceptions;
