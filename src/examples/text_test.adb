-- Example using the generic example implementation on how to use Aw_Config
--
-- author Marcelo Coraça de Freitas <marcelo.batera@gmail.com> 
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$




with Aw_Config.Text_Parsers;	use Aw_Config.Text_Parsers;
with Example;			use Example;

procedure Text_Test is
begin
	Run_Example( new Parser );
end Text_Test;
