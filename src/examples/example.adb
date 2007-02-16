


with Aw_Config;			use Aw_Config;
with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;


with Aw_Lib.UString_Ordered_Maps;	use Aw_Lib.UString_Ordered_Maps;

package body Example is

	procedure Run_Example( P: in Parser_Access ) is
		-- run a test

		Config: Config_File;



		procedure iterator( C: in Cursor ) is
		begin
			Put_Line( To_String( Key( C ) ) & " ==> " & To_String( Element( C ) ) );
		end iterator;

	begin

		Add_Config_Path( "./data" );

		Put_Line( "Loading config file... " );
		Config := New_Config_File( "config", P );

		New_Line;

		Put_Line( "Dumping all the data:" );

		New_Line;

		Iterate( Get_Contents_Map( Config ), Iterator'Access );

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
		Put_Line( "If you didn't see any error message untill now it should be running fine" );

		
	end Run_Example;

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
end Example;
