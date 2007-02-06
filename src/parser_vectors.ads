-- A vector of Parsers. ;)
-- 
-- author Marcelo Coraça de Freitas <marcelo.batera@gmail.com>



-- Ada packages
with Ada.Containers.Vectors;

-- AdaConfig packages
with Parsers_Interface;	

package Parser_Vectors is new Ada.Containers.Vectors(	Index_Type   => Natural;
							Element_Type => Parser_Access );
