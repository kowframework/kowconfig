
------------------------------------------------------------------------------
-- This is the KOW Config Test Package                                      --
--                                                                          --
-- Example on how to use KOW Config                                         --
------------------------------------------------------------------------------


with KOW_Config;	use KOW_Config;

package Tests is

	procedure Run_Tests;

private
	procedure Database_Information( Config: in out Config_File_Type );
	-- print database information

end Tests;
