------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Ydea Desenv. de Softwares Ltda          --
--                                                                          --
--                                                                          --
-- Aw_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. Aw_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with Aw_Lib; see file COPYING. If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- This is the Aw_Config package                                            --
--                                                                          --
-- This is the main AwConfig package.                                       --
-- Here you'll find the types you should use in your application and all    --
-- visible procedures and functions.                                        --
--                                                                          --
--                                                                          --
-- Notice that, even though it has been taken in consideration, there is no --
-- big concern in efficiency (which can be noted in the Elements_Array meth. --
--                                                                          --
------------------------------------------------------------------------------


-- Ada Packages
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

-- Aw_Lib Packages
with Aw_Lib.UString_Vectors;
with Aw_Lib.UString_Ordered_Maps;


package Aw_Config is


	-----------------------
	-- Types Declaration --
	-----------------------

	type Config_File is private;
	-- represents the configuration file
	
	type Config_File_Array is Array( Positive range<> ) of Config_File;
	-- one array of config files

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

	INVALID_PARAMETER : Exception;
	-- when the paramater passed to the function is not valid 

	SYNTAX_ERROR: Exception;
	-- when there is an error in the config file

	FILE_NOT_FOUND: Exception;
	-- when there is no config file found in the config path

	NO_CONFIG_PATH: Exception;
	-- when there is no config path set

	NO_PARSER: Exception;
	-- when there is no parser set and it's trying to access config files

	NOT_MY_FILE: Exception;
	-- when the file passed to the parser is not his responsability


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


	function Scan_Relative_Path( Relative_Path : in String; P: in Parser_Access) return AW_Lib.UString_Ordered_Maps.Map;
	-- Scan a given relative path within the Config_Path for the project.
	-- Return all the config files found without the extension, indexed by
	-- their relative name (inside the relative path) without the extension.


	generic
		with procedure Path_Iterator( Name: in String; Config: in out Config_File );
	procedure Generic_Iterate(	Map	: in Aw_Lib.UString_Ordered_Maps.Map;
					P	: in Parser_Access ); 
	-- Iterate over the elements returned by Scan_Relative_Path.
	-- The parameters are the initialized config file and the config name within the relative_path parameter


	function New_Config_File( N: in String; P: in Parser_Access; Is_Complete_Path: Boolean := False ) return Config_File;
	-- opens a new config file that will be handled by the parser P
	-- read it's contents and return an object representing it.
	-- the file is closed right after it've been read

	procedure Reload_Config( F: in out Config_File );
	-- (re)load the configuration from the file

	function Get_File_Name( F: in Config_File ) return String;
	-- return the file name used for this config file.

	procedure Dump_Contents( Config: in Aw_Config.Config_File );
	-- dumps the contents map into the std out

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


	function Get_Optional_Boolean(	F	: Config_File;
					Key	: String;
					Default	: Boolean := FALSE ) return Boolean;
	-- returns the element value like the Element function, but converts it to
	-- Boolean value and, if occurs a constraint error, returns the default value

	function Get_Optional_Float(	F	: Config_File;
					Key	: String;
					Default	: Float := 0.0 ) return Float;
	-- returns the element value like the Element function, but converts it to
	-- Float value and, if occurs a constraint error, returns the default value

	function Get_Optional_Integer(	F	: Config_File;
					Key	: String;
					Default	: Integer := 0 ) return Integer;
	-- returns the element value like the Element function, but converts it to
	-- Integer value and, if occurs a constraint error, returns the default value

	
	function Get_Optional_String(	F	: Config_File;
					Key	: String;
					Default	: String := "" ) return String;
	-- returns the element value like the Element function, but converts is to
	-- String and, if occurs a constraint error, returns the default value

	function Get_Optional_UString(	F	: Config_File;
					Key	: String;
					Default	: String := "" ) return Unbounded_String;
	
	-- returns the element value like the Element function
	-- if occurs a constraint error, returns the default value in Unbounded_String

	function Get_Compulsory_Boolean( F	: Config_File;
					 Key	: String ) return Boolean;
	-- returns the element value like the Element function, but converts it to
	-- Boolean value

	function Get_Compulsory_Float(	F	: Config_File;
					Key	: String ) return Float;
	-- returns the element value like the Element function, but converts it to
	-- Float value

	function Get_Compulsory_Integer( F	: Config_File;
					 Key	: String ) return Integer;
	-- returns the element value like the Element function, but converts it to
	-- Integer value
	
	
	function Get_Compulsory_String(	F	: Config_File;
					Key	: String ) return String;
	-- returns the element value like the Element function, but converts is to
	-- String 

	function Get_Compulsory_UString(	F	: Config_File;
						Key	: String ) return Unbounded_String;
	-- returns the element value like the Element function 


	function Element( F: Config_File; Key: Unbounded_String ) return Unbounded_String;
	-- return the value of element inside the current section with
	-- key Key
	-- if no current section active, return propertie relative
	-- to root section; ie expects Key to be of the form "sectionName.key"


	function Element( F: Config_File; Key: String ) return Unbounded_String;
	-- return the value of element inside the current section with
	-- key Key
	-- if no current section active, return propertie relative
	-- to root section; ie expects Key to be of the form "sectionName.key"

	function Extract( F: Config_File; Prefix: Unbounded_String ) return Config_File;
	-- return a new config file with the data prefixed by the give prefix

	function Extract( F: Config_File; Prefix: String ) return Config_File;
	-- return a new config file with the data prefixed by the give prefix

	function Elements_Array( F: Config_File; Key: Unbounded_String ) return Config_File_Array;
	-- return an array with elements withing the category named by:
	-- (THE_CURRENT_CATEGORY).Key.INDEX
	-- where INDEX starts with 1.

	function Elements_Array( F: Config_File; Key: String ) return Config_File_Array;
	-- return an array with elements withing the category named by:
	-- (THE_CURRENT_CATEGORY).Key.INDEX
	-- where INDEX starts with 1.


	function Get_Contents_Map( F: in Config_File ) return Aw_Lib.UString_Ordered_Maps.Map;
	Pragma Inline( Get_Contents_Map );
	-- return an ordered map of Unbounded_String => Unbounded_String
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

	function Key( P: in Parser_Interface ) return Unbounded_String is abstract;
	-- return the key of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Element( P: in Parser_Interface ) return Unbounded_String is abstract;
	-- return the value of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Get_File_Name( P: in Parser_Interface; Original: in String ) return String is abstract;
	-- returns the filename Original with expected extension
	-- ie, Original & ".cfg" in case of Text Parser

	function File_To_Config_Name( P: in Parser_Interface; File_Name: in String ) return String is abstract;
	-- Convert the file name to a config name.
	-- Raises NOT_MY_FILE if it's not a supported config file

private

	Config_Path: Aw_Lib.UString_Vectors.Vector;

	Project_Name: Unbounded_String;

	type Config_File is record
		File_Name: Unbounded_String;
		Current_Section: Unbounded_String;
		Contents: Aw_Lib.UString_Ordered_Maps.Map;
		My_Parser: Parser_Access;
	end record;

end Aw_Config;
