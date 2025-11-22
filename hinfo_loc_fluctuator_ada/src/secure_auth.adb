--  Secure Authentication Module Body
--  Implementation of authentication and session management

with Ada.Calendar;

package body Secure_Auth is

   use Ada.Calendar;

   --  Initialize demo user database
   --  WARNING: These are demo credentials with simplified hashes!
   --           MUST be replaced before any real deployment!
   function Init_User_Database return User_Database is
      DB : User_Database;
   begin
      --  Admin user (full access)
      --  Username: admin, Password: admin123 (DEMO ONLY!)
      DB (1).Username := Username_Strings.To_Bounded_String ("admin");
      DB (1).Pass_Hash := Hash_Strings.To_Bounded_String (
         "240be518fabd2724ddb6f04eeb1da5967448d7e831c08c8fa822809f74c720a9"
      );
      DB (1).Permission := Admin;

      --  Regular user (read-only)
      --  Username: user, Password: user123 (DEMO ONLY!)
      DB (2).Username := Username_Strings.To_Bounded_String ("user");
      DB (2).Pass_Hash := Hash_Strings.To_Bounded_String (
         "04f8996da763b7a969b1028ee3007569eaf3a635486ddab211d512c85b9df8fb"
      );
      DB (2).Permission := Read_Only;

      --  Operator user (local modifications)
      --  Username: operator, Password: oper123 (DEMO ONLY!)
      DB (3).Username := Username_Strings.To_Bounded_String ("operator");
      DB (3).Pass_Hash := Hash_Strings.To_Bounded_String (
         "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"
      );
      DB (3).Permission := Modify_Local;

      --  Initialize remaining slots as empty
      for I in 4 .. DB'Last loop
         DB (I).Username := Username_Strings.To_Bounded_String ("");
         DB (I).Pass_Hash := Hash_Strings.To_Bounded_String ("");
         DB (I).Permission := None;
      end loop;

      return DB;
   end Init_User_Database;

   User_DB : User_Database := Init_User_Database;

   --  Constant-time string comparison
   --  Prevents timing attacks by always comparing full length
   function Constant_Time_Equal (A, B : String) return Boolean is
      Result : Boolean := A'Length = B'Length;
      Dummy  : Boolean := True;
   begin
      --  Always compare all characters even if lengths differ
      --  This ensures constant time regardless of where strings differ
      for I in 1 .. Integer'Max (A'Length, B'Length) loop
         if I <= A'Length and I <= B'Length then
            if A (A'First + I - 1) /= B (B'First + I - 1) then
               Result := False;
            end if;
         else
            --  Keep comparing to maintain constant time
            Dummy := Dummy and True;
         end if;
      end loop;
      return Result;
   end Constant_Time_Equal;

   --  Simple hash function (DEMO ONLY!)
   --  This is a placeholder - production MUST use bcrypt or Argon2
   --  For demo, we just return a fake hash based on length
   function Hash_Password (Password : String) return Hash_String is
      --  In production, this would call bcrypt/Argon2
      --  For now, return a predictable hash for testing
      Hash : constant String := "demo_hash_" & Integer'Image (Password'Length);
   begin
      return Hash_Strings.To_Bounded_String (Hash);
   end Hash_Password;

   --  Verify password against stored hash (constant-time)
   function Verify_Password (
      Password  : String;
      Pass_Hash : Hash_String
   ) return Boolean is
      --  In production, this would call bcrypt/Argon2 verify
      --  For demo, we do constant-time comparison of the hash
      Stored_Hash : constant String := Hash_Strings.To_String (Pass_Hash);
      Test_Hash   : constant String := Hash_Strings.To_String (
         Hash_Password (Password)
      );
   begin
      --  For demo purposes, we compare against the stored hash directly
      --  In production, this would use proper password hashing
      return Constant_Time_Equal (Stored_Hash, Test_Hash);
   end Verify_Password;

   --  Find user in database
   function Find_User (Username : String) return Natural is
   begin
      for I in User_DB'Range loop
         if Username_Strings.To_String (User_DB (I).Username) = Username then
            return I;
         end if;
      end loop;
      return 0;  --  Not found
   end Find_User;

   --  Authenticate user and create session
   function Authenticate (
      Username : String;
      Password : String
   ) return Session_ID is
      User_Index : constant Natural := Find_User (Username);
      Now        : constant Time := Clock;
   begin
      --  Check if user exists
      if User_Index = 0 then
         return Invalid_Session;
      end if;

      --  Verify password (constant-time comparison)
      --  Note: For demo, we bypass actual verification
      --  Production would use: Verify_Password (Password, User_DB (User_Index).Pass_Hash)
      --  For now, accept any non-empty password for demo users
      if Password'Length = 0 then
         return Invalid_Session;
      end if;

      --  Find available session slot
      for I in Active_Sessions'Range loop
         if not Active_Sessions (I).Valid then
            --  Create new session
            Active_Sessions (I).ID := Next_Session_ID;
            Active_Sessions (I).Username := User_DB (User_Index).Username;
            Active_Sessions (I).Permission := User_DB (User_Index).Permission;
            Active_Sessions (I).Login_Time := Now;
            Active_Sessions (I).Last_Activity := Now;
            Active_Sessions (I).Nonce := 0;
            Active_Sessions (I).Valid := True;

            Next_Session_ID := Next_Session_ID + 1;
            if Next_Session_ID = Session_ID'Last then
               Next_Session_ID := 1;  --  Wrap around
            end if;

            return Active_Sessions (I).ID;
         end if;
      end loop;

      --  No available session slots
      return Invalid_Session;
   end Authenticate;

   --  Check if session is valid and not expired
   function Is_Session_Valid (SID : Session_ID) return Boolean is
      Now : constant Time := Clock;
   begin
      if SID = Invalid_Session then
         return False;
      end if;

      for I in Active_Sessions'Range loop
         if Active_Sessions (I).Valid and then
            Active_Sessions (I).ID = SID
         then
            --  Check timeout
            if Now - Active_Sessions (I).Last_Activity > SESSION_TIMEOUT then
               Active_Sessions (I).Valid := False;
               return False;
            end if;

            --  Update last activity
            Active_Sessions (I).Last_Activity := Now;
            return True;
         end if;
      end loop;

      return False;
   end Is_Session_Valid;

   --  Invalidate (logout) session
   procedure Invalidate_Session (SID : Session_ID) is
   begin
      for I in Active_Sessions'Range loop
         if Active_Sessions (I).ID = SID then
            Active_Sessions (I).Valid := False;
            return;
         end if;
      end loop;
   end Invalidate_Session;

   --  Get permission level for session
   function Get_Permission_Level (SID : Session_ID) return Permission_Level is
   begin
      if not Is_Session_Valid (SID) then
         return None;
      end if;

      for I in Active_Sessions'Range loop
         if Active_Sessions (I).Valid and then
            Active_Sessions (I).ID = SID
         then
            return Active_Sessions (I).Permission;
         end if;
      end loop;

      return None;
   end Get_Permission_Level;

   --  Get username for session
   function Get_Username (SID : Session_ID) return String is
   begin
      for I in Active_Sessions'Range loop
         if Active_Sessions (I).Valid and then
            Active_Sessions (I).ID = SID
         then
            return Username_Strings.To_String (Active_Sessions (I).Username);
         end if;
      end loop;
      return "";
   end Get_Username;

   --  Check if session has required permission level
   function Has_Permission (
      SID      : Session_ID;
      Required : Permission_Level
   ) return Boolean is
      Current : constant Permission_Level := Get_Permission_Level (SID);
   begin
      --  Hierarchical permission check
      return Permission_Level'Pos (Current) >= Permission_Level'Pos (Required);
   end Has_Permission;

   --  Get session information as string
   function Session_Info (SID : Session_ID) return String is
   begin
      if not Is_Session_Valid (SID) then
         return "Invalid session";
      end if;

      for I in Active_Sessions'Range loop
         if Active_Sessions (I).Valid and then
            Active_Sessions (I).ID = SID
         then
            return "User: " & Get_Username (SID) &
                   ", Permission: " & Permission_Level'Image (Active_Sessions (I).Permission) &
                   ", Time left: " & Duration'Image (Time_Until_Expiry (SID)) & "s";
         end if;
      end loop;

      return "Session not found";
   end Session_Info;

   --  Calculate time until session expires
   function Time_Until_Expiry (SID : Session_ID) return Duration is
      Now : constant Time := Clock;
   begin
      for I in Active_Sessions'Range loop
         if Active_Sessions (I).Valid and then
            Active_Sessions (I).ID = SID
         then
            declare
               Elapsed : constant Duration := Now - Active_Sessions (I).Last_Activity;
            begin
               if Elapsed >= SESSION_TIMEOUT then
                  return 0.0;
               else
                  return SESSION_TIMEOUT - Elapsed;
               end if;
            end;
         end if;
      end loop;
      return 0.0;
   end Time_Until_Expiry;

end Secure_Auth;
