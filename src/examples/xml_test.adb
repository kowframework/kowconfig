-- Example using the generic example implementation on how to use Aw_Config
--
-- author Marcelo Coraça de Freitas <marcelo.batera@gmail.com> 
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$




with Aw_Config.Xml_Parsers;	use Aw_Config.Xml_Parsers;
with Example;			use Example;

procedure Xml_Test is
begin
	Run_Example( new Parser );
end Xml_Test;
