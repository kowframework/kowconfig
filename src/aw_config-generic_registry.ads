

--
-- Ada2005
--

with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;

--
-- AdaWorks
--
with Aw_Config;
with Aw_Lib.String_Util;
with Aw_Lib.UString_Vectors;	use Aw_Lib.UString_Vectors;


-- Esse pacote é para ser usado para gerar um registro de elementos que nunca serão desalocados.
generic
	type Element_Type is private;
	Relative_Path: String;
	Parser : Aw_Config.Parser_Access;

package Aw_Config.Generic_Registry is

	DUPLICATED_FACTORY_TYPE : Exception;
	INVALID_FACTORY_TYPE : Exception;
	DUPLICATED_ELEMENT : Exception;


	type Element_Factory is access function( Id: in String; Config: in Aw_Config.Config_File ) return Element_Type;
	-- Função usada para criar os elementos.
	-- Para cada tipo ( a ser registrado no Factory_Registry ) o usuário ( desenvolvedor ) deve criar um factory
	-- e registrar o access no Factory_Registry a seguir



	package Factory_Maps is new Ada.Containers.Ordered_Maps(
			Key_Type	=> Unbounded_String,
			Element_Type	=> Element_Factory );
	-- Mapa onde factories são armazenadas ( usado em Factory_Registry )

	protected Factory_Registry is
		-- Registro de Factories
		-- Nota: ao criar o registro de elementos (ver Regitry a seguir) esse registro é usado
		-- A factory a ser chamada depende do parâmetro de configuração "type".
		-- Assim que a factory é localizada, 
		procedure Register( Factory_Type: in String; Factory: in Element_Factory );
		-- Registra um novo factory

		function Get( Factory_Type: in String ) return Element_Factory;
		-- Pega o factory informado.
		
		function Get( Factory_Type: in Unbounded_String ) return Element_Factory;
		-- Pega o factory informado.
		
		function Get_Ids return Aw_Lib.String_Util.UString_Array;
		-- list all the elements registered in here
		
		function Get_Ids return Aw_Lib.UString_Vectors.Vector;
		-- list all the elements registered in here

	private
		My_Map: Factory_Maps.Map;
	end Factory_Registry;


	package Element_Maps is new Ada.Containers.Ordered_Maps(
			Key_Type	=> Unbounded_String,
			Element_Type	=> Element_Type );

	package Element_Index_Maps is new Ada.Containers.Ordered_Maps(
			Key_Type	=> Unbounded_String,
			Element_Type	=> Aw_Lib.UString_Vectors.Vector );
	-- NOTE: I know that another structure would be a LOT faster and easier to use.
	-- But by the time I realized that I had already implemented almost everything.


	procedure Reload_Registry;
	-- escaneia o diretório informado e recria o registro
	-- isso precisa ficar do lado de fora do registro para evitar deadlock

	protected Registry is

		procedure Iterator( Id: in String; Config: in out Aw_Config.Config_File );
		-- procedure is used internally and shouldn't be used anywhere else!
		-- Reload_Registry utilize this one to iterate over the configuration and call the factories


		procedure Register_And_Save( Element_Id: in String; Config: in out Aw_Config.Config_File );
		-- register a new element from it's config file.
		-- also, write this new element to disk;

		procedure Delete( Element_Id: in String );

		procedure Register( Element_Id: in String; Element: in Element_Type );
		-- register a hand-made element into this registry

		procedure Register( Element_Id: in Unbounded_String; Element: in Element_type );
		-- register a hand-made element into this registry



		function Get( Id: in String ) return Element_Type;
		-- pega o elemento informado
		
		function Get( Id: in Unbounded_String ) return Element_Type;
		-- pega o elemento informado


		function Get_Ids return Aw_Lib.String_Util.UString_Array;
		-- list all the elements registered in here
		
		function Get_Ids return Aw_Lib.UString_Vectors.Vector;
		-- list all the elements registered in here



		function Get_Ids_by_Type( Factory_Type : in String ) return Aw_Lib.UString_Vectors.Vector;
		-- get the Id for all elements fabricated using the Factory_Type type;

		function Get_Ids_by_Type( Factory_Type : in Unbounded_String ) return Aw_Lib.UString_Vectors.Vector;
		-- get the Id for all elements fabricated using the Factory_Type type

		procedure Create_Factory_Type_Index( Factory_Type : in Unbounded_String; Element_Id : in Unbounded_String );
		-- create an entry in the element type index for this element.

		procedure Remove_Factory_Type_Index( Factory_Type : in Unbounded_String; Element_Id : in Unbounded_String );
		-- remove the index, knowing he factory type
		
		procedure Remove_Factory_Type_Index( Element_Id : in Unbounded_String );
		-- remove the index, not knowing the factory type (quite slow though) 

	private

		My_Map		: Element_Maps.Map;
		My_Indexes	: Element_Index_Maps.Map;
	end Registry;

end Aw_Config.Generic_Registry;
