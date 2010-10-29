--------------
-- Ada 2005 --
--------------
with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.UString_Hashed_Maps;	use KOW_Lib.UString_Hashed_Maps;
with KOW_Config;			use KOW_Config;
with KOW_Config.Parsers;		use KOW_Config.Parsers;

package body Tests is


	procedure iterator( C: in Cursor ) is
	begin
		Put_Line( To_String( Key( C ) ) & " ==> " & To_String( Element( C ) ) );
	end iterator;

	procedure config_iterator( Name: in String; Cfg: in out Config_File ) is
	begin
		Put_Line( "===== " & Name & " =========" );
		Iterate( Get_Contents_Map( Cfg ), Iterator'Access );
	end config_iterator;

	procedure My_Iterate is new Generic_Iterate( Path_Iterator => Config_Iterator );

	procedure Run_Tests is
		-- run a test

		Config: Config_File;

		My_Map: Map;

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

		Iterate( Get_Contents_Map( Config ), Iterator'Access );

		New_Line;


		Put_Line( "Saving report" );
		New_Line;

		Create( Output_File, Out_File, "out" ); 
		
		Save( Config, Output_File );
		New_Line;


		Put_Line( "Fetching information" );

		New_Line;

		Put_Line( "Section => ""Main Section""" );
		Set_Section( Config, "Main Section" );
		Put_Line( "     company name     => " & To_String( Element( Config, "company name" ) ) );
		Put_Line( "     company size     => " & To_String( Element( Config, "company size" ) ) );

		New_Line;

		Put_Line( "Section => ""Main Section.Contact Information""" );
		Set_Section( Config, "Main Section.Contact Information" );
		Put_Line( "     address          => " & To_String( Element( Config, "address" ) ) );

		New_Line;

		Database_Information( Config );

		New_Line;


		Put_Line( "Scanning all known config files in the relative config path ""data"":" );

		My_Map := Scan_Relative_Path( "data" );
		Iterate( My_Map, Iterator'Access );

		My_Iterate( My_Map );

		New_Line;


		Set_Section( Config, "" );

		Put_Line( "Extracting a part of the config file..." );

		Iterate( Get_Contents_Map( Extract( Config, "Database" ) ), Iterator'Access );


		New_Line;

		declare
			My_Array : Config_File_Array := Elements_Array( Config, "data" );
		begin

			Put_Line( "Extracting an array of values..." );

			for i in My_Array'Range loop
				Put_Line( "Element @ " &Integer'Image(i) );
				Iterate( Get_Contents_Map( My_Array( i ) ), Iterator'Access );
			end loop;
		end;

		New_Line;

		Put_Line( "If you didn't see any error message untill now it should be running fine" );


		
	end Run_Tests;

	procedure Database_Information( Config: in out Config_File ) is
		-- this procedure has no idea of where the config file is
		-- and what parser was used to treat it
	begin
		Put_Line( "Section => ""Database"" " );
		Set_Section( Config, "Database" );
		Put_Line( "     host     => " & To_String( Element( Config, "host" ) ) );
		Put_Line( "     user     => " & To_String( Element( Config, "user" ) ) );
		Put_Line( "     password => " & To_String( Element( Config, "password" ) ) );
		Put_Line( "     database => " & To_String( Element( Config, "database" ) ) );
		Put_Line( "     engine   => " & To_String( Element( Config, "engine" ) ) );


	end Database_Information;
end Tests;
