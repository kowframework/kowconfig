-- This is the main AdaConfig package.
-- Here you'll find the types you should use in your application and all 
-- visible procedures and functions.
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-25
-- lastUpdate 2007-02-01



with Ada.Containers.Ordered_Maps;
with Ada.Exceptions;

with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Alos.Env_Vars;
with Alos.File_System;
with Alos.String_Util;
with Alos.UString_Vectors;
with Alos.UString_Ordered_Maps;

-- Parsers
with Parsers_Interface;
with Text_Parsers;
with Xml_Parsers;

package body Ada_Config is


	------------------------
	-- Exception Handling --
	------------------------

	procedure Raise_Syntax_Error(	File_Name: in String;
					Line_Number: in Natural;
					Message: in String ) is
		-- Raise SYNTAX_ERROR exception with message composed by:
		-- "["& File_Name &":"&Line_Number & "] " & Message
	begin
		Ada.Exceptions.Raise_Exception( SYNTAX_ERROR'Identity, "[" & File_Name & ":" & Integer'Image(Line_Number) & "] " & Message );
	end Raise_Syntax_Error;


	------------------------------------
	-- Methods for Project Management --
	------------------------------------

	procedure Set_Project_Name( Str: in String ) is
		-- Set the project name so AdaConfig can find for 
		-- config files search path
		-- This will reset the config path
	begin
		Project_Name := To_Unbounded_String( Str );
		Reset_Config_Path;
	end Set_Project_Name;

	procedure Set_Project_Name( Str: in Unbounded_String ) is
		-- Set the project name so AdaConfig can find for 
		-- config files search path
		-- This will reset the config path
	begin
		Project_Name := Str;
		Reset_Config_Path;
	end Set_Project_Name;

	function Get_Project_Name return String is
		-- return the current project name
	begin
		return To_String( Project_Name );
	end Get_Project_Name;

	function Get_Project_Name return Unbounded_String is
		-- return the current project name
	begin
		return Project_Name;
	end Get_Project_Name;

	procedure Reset_Config_Path is
		-- reset the config path using the environment variable:
		-- [PROJECT_NAME]_CONFIG_PATH
		use Alos.String_Util;
	begin
		Config_Path := Alos.File_System.To_Vector (
			Alos.Env_Vars.Value(
				Str_Replace( ' ', '_', To_String( Project_Name ) ) &
				"_CONFIG_PATH" 	)
				);
	exception
		when  CONSTRAINT_ERROR =>
			Config_Path := Alos.UString_Vectors.Empty_Vector;
	end Reset_Config_Path;


	procedure Add_Config_Path( Str: in String ) is
		-- add Str to config path.
	begin
		Alos.UString_Vectors.Append( Config_Path, To_Unbounded_String( Str ) );
	end Add_Config_Path;

	procedure Add_Config_Path( Str: in Unbounded_String ) is
		-- add Str to config path.
	begin
		Alos.UString_Vectors.Append( Config_Path, Str );
	end Add_Config_Path;

	function Get_Config_Path return Alos.UString_Vectors.Vector is
		-- return the current config path
	begin
		return Config_Path;
	end Get_Config_Path;


	----------------------
	-- Parsers Handling --
	----------------------

	procedure Set_Parsers( Pasers_Vector: in Parsers_Vector.Vector ) is
		-- set the parsers to use from a vector of Parsers
	begin
		Parsers := Parsers_Vector;
	end Set_Parsers;

	procedure Add_Parser( Parser: in Parsers_Interface.Parser_Access ) is
		-- add a parser to the parsers to use
	begin
		Parsers_Vector.Append( Parsers, Parser );
	end Add_Parser;

	procedure Remove_Parser( N: Natural ) is
		-- remove the parser at index N
	begin
		Parsers_Vectors.Delete( Parsers, N );
	end Remove_Parser;


	function Get_Parsers return Parsers_Vector.Vector is
		-- return a vector with all parsers
	begin
		return Parsers;
	end Get_Parsers;



	-------------------
	-- File handling --
	-------------------

	function New_Config_File( N: in String ) return Config_File is
		-- opens a new config file with name N.
		-- read it's contents and return an object representing it.
		-- the file is closed right after it've been read
		use Alos.File_System;
		F: Config_File;

	begin
		for j in 1 .. Alos.UString_Vectors.Length( Config_Path ) loop
			declare
				i: Integer := j;
				tmp: String := To_String( 
					Alos.UString_Vectors.Element( Config_Path, i )
					);
			begin
				if Is_File( tmp & '/' & N & ".cfg.xml" ) then
					F.My_Parser := new XML_Parsers.Parser;
					F.File_Name := To_Unbounded_String (
						tmp & '/' & N & ".cfg.xml"
					);
					return F;
				elsif Is_File( tmp & '/' & N & ".cfg" ) then
					F.My_Parser := new Text_Parsers.Parser;
					F.File_Name := To_Unbounded_String (
						tmp & '/' & N & ".cfg"
					);
				end if;
			end;
		end loop;

		Ada.Exceptions.Raise_Exceptions( FINE_NOT_FOUND'Identity, N );

	end New_Config_File;

	procedure Reload_Config( F: in out Config_File ) is
		-- reloads the configuration from the flie. :D
		use Text_Parser;
		use Xml_Parser;
		use Alos.UString_Ordered_Maps;
	begin
		F.Contents := Empty_Map;

		Prepare( F.My_Parser.All, F.File_Name );

		loop
			Include(	F.Contents,
					Key( F.My_Parser.All ),
					Element( F.My_Parser.All )
				);
			Next( F.My_Parser.All );
		end loop;

	exception
		when CONSTRAINT_ERROR =>
			-- the file has reached the end
			Finish( F.My_Parser );
	end Reload_Config;


	----------------------------------
	-- Methods for Config Iteration --
	----------------------------------

	procedure Set_Section( F: in out Config_File; S: in String ) is
		-- set the current section of the config file.
	begin
		F.Current_Section := To_Unbounded_String( S );
	end Set_Section;

	procedure Set_Section( F: in out Config_File; S: in Unbounded_String ) is
		-- set the current section of the config file.
	begin
		F.Current_Section := S;
	end Set_Section;

	function Get_Section( F: in Config_File ) return String is
		-- return the current section or "" if there is no section active
	begin
		return To_Unbounded_String( F.Current_Section );
	end Get_Section;

	function Get_Section( F: in Config_File ) return Unbounded_String is
		-- return the current section or "" if there is no section active
	begin
		return F.Current_Section;
	end Get_Section;

	function Element( F: Config_File; Key: String ) return String is
		-- return the value of element inside the current section with
		-- key Key
		-- if no current section active, return propertie relative
		-- to root section; ie expects Key to be of the form "sectionName.key"
	begin
		return To_String( Element( F, To_Unbounded_String( Key ) ) );
	end Element;

	function Element( F: Config_File; Key: Unbounded_String  ) return String is
		-- return the value of element inside the current section with
		-- key Key
		-- if no current section active, return propertie relative
		-- to root section; ie expects Key to be of the form "sectionName.key"
	begin
		return To_String( Element( F, Key ) );
	end Element;

	function Element(  F: Config_File; Key: String ) return Unbounded_String is
		-- return the value of element inside the current section with
		-- key Key
		-- if no current section active, return propertie relative
		-- to root section; ie expects Key to be of the form "sectionName.key"
	begin
		return Element( F, To_Unbounded_String( Key ) );
	end Element;

	function Element( F: Config_File; Key: Unbounded_String ) return Unbounded_String is
		-- return the value of element inside the current section with
		-- key Key
		-- if no current section active, return propertie relative
		-- to root section; ie expects Key to be of the form "sectionName.key"
		use Alos.UString_Ordered_Maps;
	begin
		if F.Current_Section = "" then
			return Element( F.Contents, Key );
		end if;
		return Element(	F.Contents,
				F.Current_Section & To_Unbounded_String (".") & Key );
	end Element;

	function Get_Contents_Map( F: in Config_File ) return Alos.UString_Ordered_Maps.Map is
	-- return an ordered map of Unbounded_String => Unbounded_String
	-- with all keys respecting the pattern "section.subSection.key"
	begin
		return F.Contents;
	end Get_Contents_Map;
end Ada_Config;

