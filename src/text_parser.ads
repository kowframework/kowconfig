-- parser for plain text/properties file
--
-- @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com>
-- @createdAt 2007-02-02
-- @lastUpdate


package Ada_Config.Text_Parsers is

	type Parser private;


	procedure Prepare(	P: in out Parser'Class;
				File_Name: in String );
	-- prepare the parser to parse the file with the
	-- absolute path File_Name.
	-- read the 1st field

	procedure Finish( P: in out Parser'Class ) is abstract;
	-- close the file and do whatever it's needed to finish it.

	procedure Next( P: in out Parser'Class );
	-- move the parser to the next field, if it exists
	-- if not prepare the parser to return CONSTRAINT_ERROR
	-- everytime Key and Value are called


	function Key( P: in Parser'Class ) return String;
	-- return the key of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Element( P: in Parser'Class ) return String;
	-- return the value of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read


	function Get_File_Name( Config_Name: in String ) return String;
	-- returns the Config_Name with expected extension 
	-- i.e., for Plain Text files: Config_Name & ".cfg"

	private

	type Parser is new Parser with record
		-- TODO: definir as propriedades do parser
		null;
	end record;

end Ada_Config.Text_Parsers;
	
