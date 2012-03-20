--------------
-- Ada 2005 --
--------------
with Ada.Text_IO;			use Ada.Text_IO;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_Lib.UString_Vectors;		use KOW_Lib.UString_Vectors;
with KOW_Config;			use KOW_Config;
with KOW_Config.Parsers;
with KOW_Config.Util;

package body Tests is


	procedure config_iterator( Name: in String; Cfg: in out Config_File_Type ) is
	begin
		Put_Line( "===== " & Name & " =========" );
		KOW_Config.Dump_Contents( Cfg );
	end config_iterator;

	procedure My_Iterate is new Generic_Iterate( Path_Iterator => Config_Iterator );

	procedure Run_Tests is
		-- run a test

		Config: Config_File_Type;

		My_Vect : Vector;


		Output_File : File_Type;

	begin

		Add_Config_Path( "./data" );
		-- you could use Set_Project_Name instead. But I'm using config path.
		-- Set_Project_Name( "SAMPLE" );

		Put_Line( "Loading config file... " );
		Config := New_Config_File( "config" );

		New_Line;

		Put_Line( "Dumping all the data:" );

		New_Line;


		KOW_Config.Dump_Contents( Config );

		New_Line;


		Put_Line( "Saving report" );
		New_Line;

		Create( Output_File, Out_File, "out" ); 
		
		KOW_Config.Parsers.Save( Config, Output_File );
		New_Line;


		Put_Line( "Fetching information" );

		New_Line;

		Put_Line( "A json object: " & KOW_lib.Json.To_Json( KOW_Config.Util.Json_Objects.Default_Value( Config, "json_object" ) ) );
		Put_Line( "A json array: " & KOW_lib.Json.To_Json( KOW_Config.Util.Json_Arrays.Default_Value( Config, "json_array" ) ) );

		New_Line;

		Put_Line( "Section => ""Main Section""" );
		Set_Section( Config, "Main Section" );
		Put_Line( "     company name     => " & Element( Config, "company name" ) );
		Put_Line( "     company size     => " & Element( Config, "company size" ) );

		New_Line;

		Put_Line( "Section => ""Main Section.Contact Information""" );
		Set_Section( Config, "Main Section.Contact Information" );
		Put_Line( "     address          => " & Element( Config, "address" ) );

		New_Line;

		Database_Information( Config );

		New_Line;


		Put_Line( "Scanning all known config files in the relative config path ""data"":" );

		My_Vect := Scan_Relative_Path( "data" );
		My_Iterate( My_Vect );

		New_Line;


		Set_Section( Config, "" );

		Put_Line( "Extracting a part of the config file..." );

		Dump_Contents( Extract( Config, "Database." ) );


		New_Line;

		declare
			My_Array : Config_File_Array := Elements_Array( Config, "data" );
		begin

			Put_Line( "Extracting an array of values..." );

			for i in My_Array'Range loop
				Put_Line( "Element @ " & Integer'Image(i) );
				KOW_Config.Dump_Contents( My_Array( i ) );

			end loop;
		end;

		New_Line;

		Put_Line( "If you didn't see any error message untill now it should be running fine" );


		
	end Run_Tests;

	procedure Database_Information( Config: in out Config_File_Type ) is
		-- this procedure has no idea of where the config file is
		-- and what parser was used to treat it
	begin
		Put_Line( "Section => ""Database"" " );
		Set_Section( Config, "Database" );
		Put_Line( "     host     => " & Default_Value( Element( Config, "host" ) ) );
		Put_Line( "     user     => " & Default_Value( Element( Config, "user" ) ) );
		Put_Line( "     password => " & Default_Value( Element( Config, "password" ) ) );
		Put_Line( "     database => " & Default_Value( Element( Config, "database" ) ) );
		Put_Line( "     engine   => " & Value( Element( Config, "engine" ), KOW_Lib.Locales.From_String( "pt_BR" ) ) );
	end Database_Information;
end Tests;
