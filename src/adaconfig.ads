-- This is the main AdaConfig package.
-- Here you'll find the types you should use in your application and all 
-- visible procedures and functions.
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-25
-- lastUpdate 2007-02-01



with Ada.Containers.Ordered_Maps;

with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with ALOS.UString_Vectors;

package AdaConfig is


	-----------------------
	-- Types Declaration --
	-----------------------

	type Config_File is private;
	-- represents the configuration file

	function "<" ( L, R: Node ) return boolean;
	-- function required by Ordered_Maps
	-- it orders the Node by it's name

	package Nodes_Map is new Ada.Containers.Ordered_Maps(
		Key_Type => Unbounded_String,
		Element_Type => Node );

	package Keys_Map is new Ada.Containers.Ordered_Maps(
		Key_Type => Unbounded_String,
		Element_Type => Unbounded_String );

	-- cursors:
	type Node_Cursor is new Nodes_Map.Cursor;
	type Key_Cursor is new Keys_Map.Cursor;

	-- iterators:
	type Key_Iterator  is access procedure( Key, Value: in String;
						Parent_Node: in Node );

	type Node_Iterator is access procedure( Key: in String;
						Value: in Node;
						Parent_Node: in Node);
	------------------------------------
	-- Methods for Project Management --
	------------------------------------

	procedure Set_Project_Name( Str: in String );
	-- Set the project name so AdaConfig can find for 
	-- config files search path
	-- This will reset the config path

	procedure Set_Project_Name( Str: in Unbounded_String );
	-- Set the project name so AdaConfig can find for 
	-- config files search path
	-- This will reset the config path

	procedure Add_Config_Path( Str: in String );
	-- add Str to config path.

	procedure Add_Config_Path( Str: in Unbounded_String );
	-- add Str to config path.

	function Get_Config_Path return ALOS.UString_Vectors.Vector;
	-- return the current config path


	----------------------------------
	-- Methods for Config Iteration --
	----------------------------------

private


	type Config_File is record
		File_Name: Unbounded_String;
		Contents: ALOS.UString_Maps.Map;
	end record;

end AdaConfig;
