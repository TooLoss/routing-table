with Ada.Strings;               use Ada.Strings;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Fichier_Utils is

    type String_List is array(1..4) of Unbounded_String;
    type Int_List is array(1..4) of Integer;

    function Separer(line : in Unbounded_String, separateur : in Character) return String_List;

    function Recuperer_Arguments(fichier : in File_Type) return Unbounded_String;

    function Lire_Fichier(nom : in Unbounded_String) return File_Type;

    function Convertir_StringEntier(list : in Int_List) return Int_List;

end Fichier_Utils;
