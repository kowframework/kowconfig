------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
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
-- This is the Example package                                              --
--                                                                          --
-- Generic example implementation on how to use Aw_Config                   --
------------------------------------------------------------------------------


with Aw_Config;				use Aw_Config;
with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

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

		-- Add_Config_Path( "./data" );
		-- you could use Add_Config_Path too. But I'm using project config path:
		Set_Project_Name( "TEST" );

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
