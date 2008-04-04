-- This is the main AwConfig package.
-- Here you'll find the types you should use in your application and all 
-- visible procedures and functions.
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- 
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$


-- Ada Packages
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Ada.Strings.Wide_Unbounded;	use Ada.Strings.Wide_Unbounded;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

with Aw_Lib.Wide_String_Util;
with Aw_Lib.String_Util;

with Aw_Lib.UString_Vectors;
with Aw_Lib.UString_Ordered_Maps;
with Aw_Lib.UWide_String_Vectors;
with Aw_Lib.UWide_String_Ordered_Maps;


package Aw_Config is

	-----------------------
	-- Types Declaration --
	-----------------------

	type Config_File is private;
	-- represents the configuration file

	type Parser_Interface is abstract tagged limited null record;
	-- every parser got to derive from this type

	type Parser_Access is Access all Parser_Interface'Class;
	-- this type is used internally by AdaWorks but is visible just in case
	-- the developer needs it.

	------------------------------
	-- Sub Packages Declaration --
	------------------------------
	package Parser_Vectors is new Ada.Containers.Vectors(
			Index_Type   => Natural,
			Element_Type => Parser_Access );


	----------------
	-- Exceptions --
	----------------

	SYNTAX_ERROR: Exception;
	-- when there is an error in the config file

	FILE_NOT_FOUND: Exception;
	-- when there is no config file found in the config path

	NO_CONFIG_PATH: Exception;
	-- when there is no config path set

	NO_PARSER: Exception;
	-- when there is no parser set and it's trying to access config files

	------------------------
	-- Exception Handling --
	------------------------

	procedure Raise_Syntax_Error(	File_Name: in String;
					Line_Number: in Natural := 0;
					Column_Number: in Natural := 0;
					Message: in String );
	-- Raise SYNTAX_ERROR exception with message composed by:
	-- "["& File_Name &":"&Line_Number & "] " & Message


	------------------------------------
	-- Methods for Project Management --
	------------------------------------

	procedure Set_Project_Name( Str: in String );
	pragma Inline( Set_Project_Name );
	-- Set the project name so AwConfig can find for 
	-- config files search path
	-- This will reset the config path

	procedure Set_Project_Name( Str: in Unbounded_String );
	pragma Inline( Set_Project_Name );
	-- Set the project name so AwConfig can find for 
	-- config files search path
	-- This will reset the config path

	function Get_Project_Name return String;
	pragma Inline( Get_Project_Name );
	-- return the current project name

	function Get_Project_Name return Unbounded_String;
	pragma Inline( Get_Project_Name );
	-- return the current project name

	procedure Reset_Config_Path;
	-- reset the config path using the environment variable:
	-- [PROJECT_NAME]_CONFIG_PATH

	procedure Add_Config_Path( Str: in String );
	-- add Str to config path.

	procedure Add_Config_Path( Str: in Unbounded_String );
	-- add Str to config path.

	function Get_Config_Path return Aw_Lib.UString_Vectors.Vector;
	pragma Inline( Get_Config_Path );
	-- return the current config path

	-------------------
	-- File handling --
	-------------------

	function New_Config_File( N: in String; P: in Parser_Access ) return Config_File;
	-- opens a new config file that will be handled by the parser P
	-- read it's contents and return an object representing it.
	-- the file is closed right after it've been read

	procedure Reload_Config( F: in out Config_File );
	-- (re)load the configuration from the file


	----------------------------------
	-- Methods for Config Iteration --
	----------------------------------


	procedure Set_Section( F: in out Config_File; S: in Wide_String );
	pragma Inline( Set_Section );
	-- set the current section of the config file.

	procedure Set_Section( F: in out Config_File; S: in Unbounded_Wide_String );
	pragma Inline( Set_Section );
	-- set the current section of the config file.
	

	function Get_Section( F: in Config_File ) return Wide_String;
	pragma Inline( Get_Section );
	-- return the current section or "" if there is no section active
	
	function Get_Section( F: in Config_File ) return Unbounded_Wide_String;
	pragma Inline( Get_Section );
	-- return the current section or "" if there is no section active
	

	function Element( F: Config_File; Key: Unbounded_Wide_String ) return Unbounded_Wide_String;
	-- return the value of element inside the current section with
	-- key Key
	-- if no current section active, return propertie relative
	-- to root section; ie expects Key to be of the form "sectionName.key"


	function Element( F:Config_File; Key: Wide_String ) return Unbounded_Wide_String;
	-- return the value of element inside the current section with
	-- key Key
	-- if no current section active, return propertie relative
	-- to root section; ie expects Key to be of the form "sectionName.key"


	function Get_Contents_Map( F: in Config_File ) return Aw_Lib.UWide_String_Ordered_Maps.Map;
	Pragma Inline( Get_Contents_Map );
	-- return an ordered map of Unbounded_Wide_String => Unbounded_Wide_String
	-- with all keys respecting the pattern "section.subSection.key"


	-------------------------------------
	-- Methods of the Parser_Interface --
	-------------------------------------

	procedure Prepare(	P: in out Parser_Interface;
				File_Name: in String ) is abstract;
	-- prepare the parser to parse the file with the
	-- absolute path File_Name.
	-- read the 1st field

	procedure Finish( P: in out Parser_Interface ) is abstract;
	-- close the file and do whatever it's needed to finish it.

	procedure Next( P: in out Parser_Interface ) is abstract;
	-- move the parser to the next field, if it exists
	-- if not prepare the parser to return CONSTRAINT_ERROR
	-- everytime Key and Value are called

	function Key( P: in Parser_Interface ) return Unbounded_Wide_String is abstract;
	-- return the key of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Element( P: in Parser_Interface ) return Unbounded_Wide_String is abstract;
	-- return the value of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Get_File_Name( P: in Parser_Interface; Original: in String ) return String is abstract;
	-- returns the filename Original with expected extension
	-- ie, Original & ".cfg" in case of Text Parser


private

	Config_Path: Aw_Lib.UString_Vectors.Vector;

	Project_Name: Unbounded_String;

	type Config_File is record
		File_Name: Unbounded_String;
		Current_Section: Unbounded_Wide_String;
		Contents: Aw_Lib.UWide_String_Ordered_Maps.Map;
		My_Parser: Parser_Access;
	end record;

end Aw_Config;
