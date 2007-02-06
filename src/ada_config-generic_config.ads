-- This is the main AdaConfig package.
-- Here you'll find the types you should use in your application and all 
-- visible procedures and functions.
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-25
-- lastUpdate 2007-02-01



with Ada.Containers.Ordered_Maps;

with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Alos.UString_Vectors;
with Alos.UString_Ordered_Maps;


with Parsers_Interface;

generic
	type Parser is private;
	with procedure Prepare(	P: in out Parser'Class;
				File_Name: in String ) is <>;
	-- prepare the parser to parse the file with the
	-- absolute path File_Name.
	-- read the 1st field
	
	with procedure Finish( P: in out Parser'Class ) is <>;
	-- close the file and do whatever it's needed to finish it.
	
	with procedure Next( P: in out Parser'Class ) is <>;
	-- move the parser to the next field, if it exists
	-- if not prepare the parser to return CONSTRAINT_ERROR
	-- everytime Key and Value are called
	
	
	with function Key( P: in Parser'Class ) return String is <>;
	-- return the key of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	with function Element( P: in Parser'Class ) return String is <>;
	-- return the value of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	with function Get_File_Name( Config_Name: in String ) return String is <>;
	-- returns the Config_Name with expected extension 
	-- i.e., for Plain Text files: Config_Name & ".cfg"
	
package Ada_Config.Generic_Config is
	Pragma Preelaborate;


	-----------------------
	-- Types Declaration --
	-----------------------

	type Config_File is private;
	-- represents the configuration file


	SYNTAX_ERROR: Exception;
	FILE_NOT_FOUND: Exception;

	------------------------
	-- Exception Handling --
	------------------------

	procedure Raise_Syntax_Error(	File_Name: in String;
					Line_Number: in Natural;
					Message: in String );
	-- Raise SYNTAX_ERROR exception with message composed by:
	-- "["& File_Name &":"&Line_Number & "] " & Message


	------------------------------------
	-- Methods for Project Management --
	------------------------------------

	procedure Set_Project_Name( Str: in String );
	pragma Inline( Set_Project_Name );
	-- Set the project name so AdaConfig can find for 
	-- config files search path
	-- This will reset the config path

	procedure Set_Project_Name( Str: in Unbounded_String );
	pragma Inline( Set_Project_Name );
	-- Set the project name so AdaConfig can find for 
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

	function Get_Config_Path return Alos.UString_Vectors.Vector;
	pragma Inline( Get_Config_Path );
	-- return the current config path

	-------------------
	-- File handling --
	-------------------

	function New_Config_File( N: in String ) return Config_File;
	-- opens a new config file with name N.
	-- read it's contents and return an object representing it.
	-- the file is closed right after it've been read


	----------------------------------
	-- Methods for Config Iteration --
	----------------------------------

	procedure Set_Section( F: in out Config_File; S: in String );
	pragma Inline( Set_Section );
	-- set the current section of the config file.

	procedure Set_Section( F: in out Config_File; S: in Unbounded_String );
	pragma Inline( Set_Section );
	-- set the current section of the config file.

	function Get_Section( F: in Config_File ) return String;
	pragma Inline( Get_Section );
	-- return the current section or "" if there is no section active

	function Get_Section( F: in Config_File ) return Unbounded_String;
	pragma Inline( Get_Section );
	-- return the current section or "" if there is no section active

	function Element( Key: String; F: Config_File ) return String;
	-- return the value of element inside the current section with
	-- key Key
	-- if no current section active, return propertie relative
	-- to root section; ie expects Key to be of the form "sectionName.key"

	function Element( Key: Unbounded_String; F: Config_File ) return String;
	-- return the value of element inside the current section with
	-- key Key
	-- if no current section active, return propertie relative
	-- to root section; ie expects Key to be of the form "sectionName.key"

	function Element( Key: String; F: Config_File ) return Unbounded_String;
	-- return the value of element inside the current section with
	-- key Key
	-- if no current section active, return propertie relative
	-- to root section; ie expects Key to be of the form "sectionName.key"

	function Element( Key: Unbounded_String; F: Config_File ) return Unbounded_String;
	-- return the value of element inside the current section with
	-- key Key
	-- if no current section active, return propertie relative
	-- to root section; ie expects Key to be of the form "sectionName.key"

	function Get_Contents_Map( F: in Config_File ) return Alos.UString_Ordered_Maps.Map;
	Pragma Inline( Get_Contents_Map );
	-- return an ordered map of Unbounded_String => Unbounded_String
	-- with all keys respecting the pattern "section.subSection.key"

private

	Config_Path: Alos.UString_Vectors.Vector;
	Project_Name: Unbounded_String;
	

	type Config_File is record
		File_Name: Unbounded_String;
		Current_Section: Unbounded_String;
		Contents: Alos.UString_Ordered_Maps.Map;
		My_Parser: Parser;
	end record;

end Ada_Config.Generic_Config;
