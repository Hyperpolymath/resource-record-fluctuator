--  HINFO-LOC Fluctuator - Main Entry Point
--  DNS record randomization for security and amusement
--
--  Author: Claude & Human Collaboration
--  License: MIT (or your preferred license)
--
--  SECURITY WARNINGS:
--  1. Demo credentials included - MUST be changed for production
--  2. Simplified crypto used - replace with bcrypt/Argon2
--  3. DNS UPDATE not yet implemented - no remote server modification
--
--  Use Cases:
--  - Honeypot obfuscation (make honeypots appear to move)
--  - Attack response and deception
--  - Privacy enhancement for public DNS records
--  - "Quantum server" demonstration
--  - Security research and education

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with TUI;

procedure Main is

   use Ada.Text_IO;
   use Ada.Command_Line;

   procedure Show_Help is
   begin
      Put_Line ("HINFO-LOC Fluctuator - Quantum Server™");
      Put_Line ("");
      Put_Line ("Usage: hinfo_loc_fluctuator [OPTIONS]");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --help, -h       Show this help message");
      Put_Line ("  --version, -v    Show version information");
      Put_Line ("  --interactive    Run interactive TUI (default)");
      Put_Line ("");
      Put_Line ("Data Files Required:");
      Put_Line ("  data/machines.txt         - CPU types (60+ included)");
      Put_Line ("  data/operating_systems.txt - OS names (70+ included)");
      Put_Line ("  data/locations.csv        - Geographic locations (50+ included)");
      Put_Line ("");
      Put_Line ("Demo Credentials:");
      Put_Line ("  admin / <any password>    - Full access");
      Put_Line ("  user / <any password>     - Read-only");
      Put_Line ("  operator / <any password> - Local modifications");
      Put_Line ("");
      Put_Line ("⚠️  WARNING: Demo credentials and simplified crypto!");
      Put_Line ("   MUST be replaced before production use!");
   end Show_Help;

   procedure Show_Version is
   begin
      Put_Line ("HINFO-LOC Fluctuator v0.1.0-alpha");
      Put_Line ("DNS Record Randomization Tool");
      Put_Line ("");
      Put_Line ("Supports:");
      Put_Line ("  - HINFO records (RFC 1035) - Deprecated per RFC 8482");
      Put_Line ("  - LOC records (RFC 1876) - Deprecated per RFC 8482");
      Put_Line ("");
      Put_Line ("Built with Ada 2012 for maximum security");
      Put_Line ("Compile-time type safety • Memory safety • No buffer overflows");
   end Show_Version;

begin
   --  Parse command-line arguments
   if Argument_Count > 0 then
      declare
         Arg : constant String := Argument (1);
      begin
         if Arg = "--help" or Arg = "-h" then
            Show_Help;
            return;
         elsif Arg = "--version" or Arg = "-v" then
            Show_Version;
            return;
         elsif Arg = "--interactive" then
            null;  --  Fall through to interactive mode
         else
            Put_Line ("Unknown option: " & Arg);
            Put_Line ("Use --help for usage information");
            Set_Exit_Status (Failure);
            return;
         end if;
      end;
   end if;

   --  Run interactive TUI
   TUI.Run_Main_Menu;

exception
   when E : others =>
      Put_Line ("");
      Put_Line ("Fatal error: " & Ada.Exceptions.Exception_Message (E));
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Name (E));
      Set_Exit_Status (Failure);
end Main;
