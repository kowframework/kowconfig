


with Aw_Config;	use Aw_Config;




package Example is

	procedure Run_Example( P: in Parser_Access );
	-- run a test

private
	procedure Database_Information( Config: in out Config_File );
	-- print database information
	-- this procedure has no idea of what config file is used and
	-- what parser can handle it

end Example;
