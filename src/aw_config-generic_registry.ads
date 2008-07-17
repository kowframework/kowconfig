

--
-- Ada2005
--

with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;

--
-- AdaWorks
--

with Aw_Config;


-- Esse pacote é para ser usado para gerar um registro de elementos que nunca serão desalocados.
generic
	type Element_Type is private;
	Relative_Path: String;
	Parser : Aw_Config.Parser_Access;

package Aw_Config.Generic_Registry is

	DUPLICATED_FACTORY : Exception;
	DUPLICATED_ELEMENT : Exception;


	type Element_Factory is access function( Name: in String; Config: in Aw_Config.Config_File ) return Element_Type;
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

	private
		My_Map: Factory_Maps.Map;
	end Factory_Registry;


	package Element_Maps is new Ada.Containers.Ordered_Maps(
			Key_Type	=> Unbounded_String,
			Element_Type	=> Element_Type );



	procedure Reload_Registry;
	-- escaneia o diretório informado e recria o registro
	-- isso precisa ficar do lado de fora do registro para evitar deadlock

	protected Registry is

		procedure Iterator( Name: in String; Config: in out Aw_Config.Config_File );
		-- procedure is used internally and shouldn't be used anywhere else!
		-- Reload_Registry utilize this one to iterate over the configuration and call the factories


		function Get( Name: in String ) return Element_Type;
		-- pega o elemento informado
		
		function Get( Name: in Unbounded_String ) return Element_Type;
		-- pega o elemento informado
	private

		My_Map: Element_Maps.Map;
	end Registry;

end Aw_Config.Generic_Registry;
