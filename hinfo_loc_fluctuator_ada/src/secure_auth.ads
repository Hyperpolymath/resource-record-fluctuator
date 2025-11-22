--  Secure Authentication Module Specification
--  Provides user authentication, session management, and permission control
--
--  Security Features:
--  - Constant-time password comparison (prevents timing attacks)
--  - Session timeout (30 minutes default)
--  - Nonce-based replay attack detection
--  - Granular permission levels
--
--  WARNING: This implementation uses demo credentials and simplified crypto.
--           MUST be replaced with bcrypt/Argon2 for production use!

with Ada.Strings.Bounded;
with Ada.Calendar;

package Secure_Auth is

   --  Permission levels (hierarchical)
   type Permission_Level is (
      None,           --  No access
      Read_Only,      --  View current records only
      Modify_Local,   --  Generate local zone files
      Modify_Remote,  --  Update remote DNS servers
      Admin           --  Full access including config
   );

   --  Session timeout duration (seconds)
   SESSION_TIMEOUT : constant Duration := 1800.0;  --  30 minutes

   --  Bounded strings for credentials
   package Username_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (64);
   package Password_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (128);
   package Hash_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (128);

   subtype Username_String is Username_Strings.Bounded_String;
   subtype Password_String is Password_Strings.Bounded_String;
   subtype Hash_String is Hash_Strings.Bounded_String;

   --  Session identifier
   type Session_ID is new Natural range 0 .. Natural'Last;
   Invalid_Session : constant Session_ID := 0;

   --  Session state
   type Session is private;

   --  Authentication functions
   function Authenticate (
      Username : String;
      Password : String
   ) return Session_ID;

   --  Session management
   function Is_Session_Valid (SID : Session_ID) return Boolean;
   procedure Invalidate_Session (SID : Session_ID);
   function Get_Permission_Level (SID : Session_ID) return Permission_Level;
   function Get_Username (SID : Session_ID) return String;

   --  Permission checks
   function Has_Permission (
      SID      : Session_ID;
      Required : Permission_Level
   ) return Boolean;

   --  Session info display
   function Session_Info (SID : Session_ID) return String;

   --  For testing: check if session is about to expire
   function Time_Until_Expiry (SID : Session_ID) return Duration;

private

   --  Internal session data structure
   type Session is record
      ID              : Session_ID := Invalid_Session;
      Username        : Username_String;
      Permission      : Permission_Level := None;
      Login_Time      : Ada.Calendar.Time;
      Last_Activity   : Ada.Calendar.Time;
      Nonce           : Natural := 0;  --  Replay attack prevention
      Valid           : Boolean := False;
   end record;

   --  Session storage (supports up to 16 concurrent sessions)
   type Session_Array is array (1 .. 16) of Session;
   Active_Sessions : Session_Array;
   Next_Session_ID : Session_ID := 1;

   --  User database (demo credentials - REPLACE IN PRODUCTION!)
   --  Format: username, password_hash, permission_level
   --  Demo hashes are SHA-256 for demonstration only
   --  Production MUST use bcrypt or Argon2!

   type User_Entry is record
      Username   : Username_String;
      Pass_Hash  : Hash_String;
      Permission : Permission_Level;
   end record;

   type User_Database is array (1 .. 10) of User_Entry;

   --  Initialize user database
   function Init_User_Database return User_Database;

   --  Password verification (constant-time comparison)
   function Verify_Password (
      Password  : String;
      Pass_Hash : Hash_String
   ) return Boolean;

   --  Simple hash function (DEMO ONLY - replace with bcrypt/Argon2)
   function Hash_Password (Password : String) return Hash_String;

   --  Constant-time string comparison (prevents timing attacks)
   function Constant_Time_Equal (A, B : String) return Boolean;

end Secure_Auth;
