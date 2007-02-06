-- packa for handling XML configuration
--
-- @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com>


with Ada_Config.Generic_Config;
with Ada_Config.Xml_Parsers;

package Ada_Config.Xml_Config is new Ada_Config.Generic_Config( Parser => Ada_Config.Xml_Parsers.Parser );
