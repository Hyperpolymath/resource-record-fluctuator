--  Text User Interface Module Specification
--  Provides interactive menu-driven interface

with Secure_Auth;

package TUI is

   --  Main menu entry point
   --  Returns when user chooses to exit
   procedure Run_Main_Menu;

   --  Login screen
   --  Returns valid session ID or Invalid_Session
   function Login_Screen return Secure_Auth.Session_ID;

   --  Display functions
   procedure Show_Banner;
   procedure Show_Main_Menu (SID : Secure_Auth.Session_ID);
   procedure Show_Admin_Menu (SID : Secure_Auth.Session_ID);

   --  Interactive operations
   procedure Load_Data_Pools;
   procedure Generate_Random_Records (SID : Secure_Auth.Session_ID);
   procedure Generate_Quantum_Server (SID : Secure_Auth.Session_ID);
   procedure View_Pool_Status;
   procedure Change_Password (SID : Secure_Auth.Session_ID);

   --  Utility functions
   procedure Pause (Message : String := "Press Enter to continue...");
   procedure Clear_Screen;
   function Get_User_Choice return Character;
   function Get_User_Input (Prompt : String) return String;

end TUI;
