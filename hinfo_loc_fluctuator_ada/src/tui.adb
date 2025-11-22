--  Text User Interface Module Body
--  Implementation of interactive menu system

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with DNS_Records;
with Randomizer;

package body TUI is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   --  ANSI color codes (optional, can be disabled for plain terminals)
   RESET  : constant String := ASCII.ESC & "[0m";
   BOLD   : constant String := ASCII.ESC & "[1m";
   RED    : constant String := ASCII.ESC & "[31m";
   GREEN  : constant String := ASCII.ESC & "[32m";
   YELLOW : constant String := ASCII.ESC & "[33m";
   BLUE   : constant String := ASCII.ESC & "[34m";
   CYAN   : constant String := ASCII.ESC & "[36m";

   --  Clear screen (ANSI escape sequence)
   procedure Clear_Screen is
   begin
      Put (ASCII.ESC & "[2J" & ASCII.ESC & "[H");
   end Clear_Screen;

   --  Display banner
   procedure Show_Banner is
   begin
      Put_Line (CYAN & BOLD);
      Put_Line ("╔═══════════════════════════════════════════════════════════╗");
      Put_Line ("║                                                           ║");
      Put_Line ("║        HINFO-LOC Fluctuator - Quantum Server™            ║");
      Put_Line ("║                                                           ║");
      Put_Line ("║    DNS Record Randomization for Security & Amusement     ║");
      Put_Line ("║                                                           ║");
      Put_Line ("╚═══════════════════════════════════════════════════════════╝");
      Put_Line (RESET);
      Put_Line ("");
      Put_Line (YELLOW & "WARNING: HINFO/LOC records are deprecated (RFC 8482)" & RESET);
      Put_Line ("This tool is for honeypot obfuscation, security research,");
      Put_Line ("and experimental purposes only.");
      Put_Line ("");
   end Show_Banner;

   --  Pause for user input
   procedure Pause (Message : String := "Press Enter to continue...") is
      Dummy : String (1 .. 1);
      Last  : Natural;
   begin
      Put (BOLD & Message & RESET);
      if not End_Of_File then
         Get_Line (Dummy, Last);
      end if;
   exception
      when others =>
         null;  --  Handle Ctrl+C gracefully
   end Pause;

   --  Get single character choice
   function Get_User_Choice return Character is
      Line : String (1 .. 100);
      Last : Natural;
   begin
      Get_Line (Line, Last);
      if Last > 0 then
         return Line (1);
      else
         return ' ';
      end if;
   exception
      when others =>
         return ' ';
   end Get_User_Choice;

   --  Get user input string
   function Get_User_Input (Prompt : String) return String is
      Input : Unbounded_String;
   begin
      Put (Prompt);
      Input := To_Unbounded_String (Get_Line);
      return To_String (Input);
   exception
      when End_Error =>
         return "";
   end Get_User_Input;

   --  Login screen
   function Login_Screen return Secure_Auth.Session_ID is
      Username : String (1 .. 64);
      Password : String (1 .. 128);
      User_Last, Pass_Last : Natural;
      SID : Secure_Auth.Session_ID;
   begin
      Clear_Screen;
      Show_Banner;

      Put_Line (BOLD & "=== LOGIN ===" & RESET);
      Put_Line ("");
      Put_Line ("Demo Credentials:");
      Put_Line ("  admin / <any password>  (Full access)");
      Put_Line ("  user / <any password>   (Read-only)");
      Put_Line ("  operator / <any password> (Local modify)");
      Put_Line ("");

      Put ("Username: ");
      Get_Line (Username, User_Last);

      Put ("Password: ");
      --  In production, this should hide input (use terminal control)
      Get_Line (Password, Pass_Last);

      SID := Secure_Auth.Authenticate (
         Username (1 .. User_Last),
         Password (1 .. Pass_Last)
      );

      if SID = Secure_Auth.Invalid_Session then
         Put_Line ("");
         Put_Line (RED & "Authentication failed!" & RESET);
         Pause;
         return Secure_Auth.Invalid_Session;
      end if;

      Put_Line ("");
      Put_Line (GREEN & "Login successful!" & RESET);
      Put_Line ("Session: " & Secure_Auth.Session_Info (SID));
      Pause;

      return SID;
   end Login_Screen;

   --  Display main menu
   procedure Show_Main_Menu (SID : Secure_Auth.Session_ID) is
      Perm : constant Secure_Auth.Permission_Level :=
         Secure_Auth.Get_Permission_Level (SID);
   begin
      Clear_Screen;
      Put_Line (CYAN & "=== MAIN MENU ===" & RESET);
      Put_Line ("");
      Put_Line ("User: " & BOLD & Secure_Auth.Get_Username (SID) & RESET);
      Put_Line ("Permission: " & Secure_Auth.Permission_Level'Image (Perm));
      Put_Line ("Session: " & Secure_Auth.Session_Info (SID));
      Put_Line ("");
      Put_Line ("1. Load Data Pools");
      Put_Line ("2. View Pool Status");
      Put_Line ("3. Generate Random HINFO/LOC");
      Put_Line ("4. Generate Quantum Server (HINFO + LOC)");

      if Perm >= Secure_Auth.Modify_Local then
         Put_Line ("5. Generate Zone File (Local Mode)");
      end if;

      if Perm >= Secure_Auth.Modify_Remote then
         Put_Line ("6. Update Remote DNS Server");
      end if;

      if Perm >= Secure_Auth.Admin then
         Put_Line ("9. Admin Menu");
      end if;

      Put_Line ("0. Logout");
      Put_Line ("");
      Put (BOLD & "Choice: " & RESET);
   end Show_Main_Menu;

   --  Load data pools interactively
   procedure Load_Data_Pools is
      CPU_File : constant String := "data/machines.txt";
      OS_File  : constant String := "data/operating_systems.txt";
      Loc_File : constant String := "data/locations.csv";
   begin
      Clear_Screen;
      Put_Line (CYAN & "=== LOAD DATA POOLS ===" & RESET);
      Put_Line ("");

      Put ("Loading CPU pool from " & CPU_File & "... ");
      begin
         Randomizer.Load_CPU_Pool (CPU_File);
         Put_Line (GREEN & "OK (" & Natural'Image (Randomizer.CPU_Pool_Size) & " entries)" & RESET);
      exception
         when E : others =>
            Put_Line (RED & "FAILED" & RESET);
            Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
      end;

      Put ("Loading OS pool from " & OS_File & "... ");
      begin
         Randomizer.Load_OS_Pool (OS_File);
         Put_Line (GREEN & "OK (" & Natural'Image (Randomizer.OS_Pool_Size) & " entries)" & RESET);
      exception
         when E : others =>
            Put_Line (RED & "FAILED" & RESET);
            Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
      end;

      Put ("Loading location pool from " & Loc_File & "... ");
      begin
         Randomizer.Load_Location_Pool (Loc_File);
         Put_Line (GREEN & "OK (" & Natural'Image (Randomizer.Location_Pool_Size) & " entries)" & RESET);
      exception
         when E : others =>
            Put_Line (RED & "FAILED" & RESET);
            Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
      end;

      Put_Line ("");
      Pause;
   end Load_Data_Pools;

   --  View pool status
   procedure View_Pool_Status is
   begin
      Clear_Screen;
      Put_Line (CYAN & "=== POOL STATUS ===" & RESET);
      Put_Line ("");
      Put_Line (Randomizer.Pool_Status);
      Put_Line ("");
      Pause;
   end View_Pool_Status;

   --  Generate random records
   procedure Generate_Random_Records (SID : Secure_Auth.Session_ID) is
      Domain : constant String := Get_User_Input ("Enter domain name: ");
   begin
      if Domain'Length = 0 then
         Put_Line (RED & "Invalid domain name" & RESET);
         Pause;
         return;
      end if;

      Clear_Screen;
      Put_Line (CYAN & "=== RANDOM RECORDS ===" & RESET);
      Put_Line ("");

      begin
         declare
            HINFO_RR : constant DNS_Records.Resource_Record :=
               Randomizer.Random_HINFO (Domain);
         begin
            Put_Line (GREEN & "HINFO Record:" & RESET);
            Put_Line (DNS_Records.To_String (HINFO_RR));
            Put_Line ("");
         end;

         declare
            LOC_RR : constant DNS_Records.Resource_Record :=
               Randomizer.Random_LOC (Domain);
         begin
            Put_Line (GREEN & "LOC Record:" & RESET);
            Put_Line (DNS_Records.To_String (LOC_RR));
            Put_Line ("");
         end;

      exception
         when E : others =>
            Put_Line (RED & "Error: " & Ada.Exceptions.Exception_Message (E) & RESET);
      end;

      Pause;
   end Generate_Random_Records;

   --  Generate quantum server (both HINFO and LOC)
   procedure Generate_Quantum_Server (SID : Secure_Auth.Session_ID) is
      Domain : constant String := Get_User_Input ("Enter domain name: ");
   begin
      if Domain'Length = 0 then
         Put_Line (RED & "Invalid domain name" & RESET);
         Pause;
         return;
      end if;

      Clear_Screen;
      Put_Line (CYAN & "=== QUANTUM SERVER™ ===" & RESET);
      Put_Line ("");
      Put_Line ("Generating quantum superposition of server states...");
      Put_Line ("");

      begin
         declare
            HINFO_RR : DNS_Records.Resource_Record (DNS_Records.HINFO_Type);
            LOC_RR   : DNS_Records.Resource_Record (DNS_Records.LOC_Type);
         begin
            Randomizer.Generate_Quantum_Server (Domain, HINFO_RR, LOC_RR);

            Put_Line (GREEN & "Server exists simultaneously as:" & RESET);
            Put_Line ("");
            Put_Line (DNS_Records.To_String (HINFO_RR));
            Put_Line (DNS_Records.To_String (LOC_RR));
            Put_Line ("");
            Put_Line (YELLOW & "Schrödinger would be proud! 🐱" & RESET);
         end;

      exception
         when E : others =>
            Put_Line (RED & "Quantum collapse! Error: " & Ada.Exceptions.Exception_Message (E) & RESET);
      end;

      Put_Line ("");
      Pause;
   end Generate_Quantum_Server;

   --  Admin menu (placeholder)
   procedure Show_Admin_Menu (SID : Secure_Auth.Session_ID) is
   begin
      Clear_Screen;
      Put_Line (CYAN & "=== ADMIN MENU ===" & RESET);
      Put_Line ("");
      Put_Line ("Admin functions not yet implemented.");
      Put_Line ("");
      Pause;
   end Show_Admin_Menu;

   --  Change password (placeholder)
   procedure Change_Password (SID : Secure_Auth.Session_ID) is
   begin
      Clear_Screen;
      Put_Line (CYAN & "=== CHANGE PASSWORD ===" & RESET);
      Put_Line ("");
      Put_Line ("Password change not yet implemented.");
      Put_Line ("");
      Pause;
   end Change_Password;

   --  Main menu loop
   procedure Run_Main_Menu is
      SID    : Secure_Auth.Session_ID;
      Choice : Character;
      Done   : Boolean := False;
   begin
      --  Login
      SID := Login_Screen;
      if SID = Secure_Auth.Invalid_Session then
         return;
      end if;

      --  Main loop
      while not Done loop
         exit when not Secure_Auth.Is_Session_Valid (SID);

         Show_Main_Menu (SID);
         Choice := Get_User_Choice;

         case Choice is
            when '1' =>
               Load_Data_Pools;

            when '2' =>
               View_Pool_Status;

            when '3' =>
               if Secure_Auth.Has_Permission (SID, Secure_Auth.Read_Only) then
                  Generate_Random_Records (SID);
               else
                  Put_Line (RED & "Permission denied" & RESET);
                  Pause;
               end if;

            when '4' =>
               if Secure_Auth.Has_Permission (SID, Secure_Auth.Read_Only) then
                  Generate_Quantum_Server (SID);
               else
                  Put_Line (RED & "Permission denied" & RESET);
                  Pause;
               end if;

            when '5' =>
               if Secure_Auth.Has_Permission (SID, Secure_Auth.Modify_Local) then
                  Put_Line (YELLOW & "Zone file generation not yet implemented" & RESET);
                  Pause;
               else
                  Put_Line (RED & "Permission denied" & RESET);
                  Pause;
               end if;

            when '6' =>
               if Secure_Auth.Has_Permission (SID, Secure_Auth.Modify_Remote) then
                  Put_Line (YELLOW & "Remote DNS update not yet implemented" & RESET);
                  Pause;
               else
                  Put_Line (RED & "Permission denied" & RESET);
                  Pause;
               end if;

            when '9' =>
               if Secure_Auth.Has_Permission (SID, Secure_Auth.Admin) then
                  Show_Admin_Menu (SID);
               else
                  Put_Line (RED & "Permission denied" & RESET);
                  Pause;
               end if;

            when '0' =>
               Secure_Auth.Invalidate_Session (SID);
               Put_Line ("");
               Put_Line (GREEN & "Logged out successfully" & RESET);
               Done := True;

            when others =>
               Put_Line (RED & "Invalid choice" & RESET);
               Pause;
         end case;
      end loop;

      if not Secure_Auth.Is_Session_Valid (SID) then
         Clear_Screen;
         Put_Line (RED & "Session expired" & RESET);
         Pause;
      end if;

   exception
      when End_Error =>
         Put_Line ("");
         Put_Line (YELLOW & "EOF detected - exiting" & RESET);
   end Run_Main_Menu;

end TUI;
