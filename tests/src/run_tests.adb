

with Tests;


with KOW_Lib.Log;	use KOW_Lib.Log;



procedure Run_Tests is
begin

	Default_Level := Level_Debug;

	Tests.Run_Tests;
end;
