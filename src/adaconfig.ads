-- This is the main AdaConfig package.
-- Here you'll find the types you should use in your application and all 
-- visible procedures and functions.
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-25
-- lastUpdate



with Ada.Containers.Maps;

package AdaConfig is

	type Config_File is private;
	-- represents the configuration file

	type Node_Record is private;
	type Node is access Node_Record;
	-- represents the node. 

	type Node_Cursor is private;
	type Key_Cursor is private;

	package Nodes_Map is new Ada.Containers.Maps(	Key_Type => String;
							Element_Type => Node );

	package Keys_Map is new Ada.Container.Maps(	Key_Type => String;
							Element_Type => String );

private


	type Node_Cursor renames Nodes_Map.Cursor;
	type Key_Cursor renames Keys_Map.Cursor;

	type Config_File is private record
		File_Name: String;
		Root_Node: Nodes_Map;
	end record;

	type Node_Record is private record
		Name: String;
		Child_Nodes: Nodes_Map;
		Child_Keys: Keys_Map;
	end record;

end AdaConfig;
