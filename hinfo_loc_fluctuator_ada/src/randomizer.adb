--  Randomizer Module Body
--  Implementation of data loading and random generation

with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Randomizer is

   use Ada.Text_IO;
   use Ada.Strings;
   use Ada.Strings.Fixed;

   --  Random number generator for CPU pool indices
   subtype CPU_Random_Range is Positive range 1 .. 10_000;
   package CPU_Random is new Ada.Numerics.Discrete_Random (CPU_Random_Range);
   CPU_Gen : CPU_Random.Generator;

   --  Random number generator for OS pool indices
   subtype OS_Random_Range is Positive range 1 .. 10_000;
   package OS_Random is new Ada.Numerics.Discrete_Random (OS_Random_Range);
   OS_Gen : OS_Random.Generator;

   --  Random number generator for location pool indices
   subtype Loc_Random_Range is Positive range 1 .. 10_000;
   package Loc_Random is new Ada.Numerics.Discrete_Random (Loc_Random_Range);
   Loc_Gen : Loc_Random.Generator;

   --  Initialize random generators
   procedure Init_Random is
   begin
      CPU_Random.Reset (CPU_Gen);
      OS_Random.Reset (OS_Gen);
      Loc_Random.Reset (Loc_Gen);
   end Init_Random;

   --  Trim whitespace and check for comment/empty line
   function Is_Valid_Line (Line : String) return Boolean is
      Trimmed : constant String := Trim (Line, Both);
   begin
      return Trimmed'Length > 0 and then
             Trimmed (Trimmed'First) /= '#';
   end Is_Valid_Line;

   --  Load CPU pool from text file
   procedure Load_CPU_Pool (Filename : String) is
      File : File_Type;
      Count : Natural := 0;
   begin
      CPU_Pool.Clear;

      Open (File, In_File, Filename);

      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            if Is_Valid_Line (Line) then
               CPU_Pool.Append (Entry_Strings.To_Bounded_String (Trim (Line, Both)));
               Count := Count + 1;
            end if;
         end;
      end loop;

      Close (File);

      if Count = 0 then
         raise Data_Load_Error with "No valid CPU entries found in " & Filename;
      end if;

   exception
      when Name_Error =>
         raise Data_Load_Error with "Cannot open file: " & Filename;
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Load_CPU_Pool;

   --  Load OS pool from text file
   procedure Load_OS_Pool (Filename : String) is
      File : File_Type;
      Count : Natural := 0;
   begin
      OS_Pool.Clear;

      Open (File, In_File, Filename);

      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            if Is_Valid_Line (Line) then
               OS_Pool.Append (Entry_Strings.To_Bounded_String (Trim (Line, Both)));
               Count := Count + 1;
            end if;
         end;
      end loop;

      Close (File);

      if Count = 0 then
         raise Data_Load_Error with "No valid OS entries found in " & Filename;
      end if;

   exception
      when Name_Error =>
         raise Data_Load_Error with "Cannot open file: " & Filename;
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Load_OS_Pool;

   --  Parse CSV line (format: lat,lon,alt_m,description)
   --  Simple CSV parser - doesn't handle quoted commas
   procedure Parse_CSV_Line (
      Line : String;
      Loc  : out Location_Entry;
      Valid : out Boolean
   ) is
      Comma_Set : constant Maps.Character_Set := Maps.To_Set (',');
      Start_Pos : Positive := Line'First;
      End_Pos   : Natural;
      Field_Num : Positive := 1;
   begin
      Valid := False;

      while Start_Pos <= Line'Last and Field_Num <= 4 loop
         --  Find next comma or end of line
         End_Pos := Index (Line (Start_Pos .. Line'Last), Comma_Set);
         if End_Pos = 0 then
            End_Pos := Line'Last;
         else
            End_Pos := End_Pos - 1;
         end if;

         declare
            Field : constant String := Trim (Line (Start_Pos .. End_Pos), Both);
         begin
            case Field_Num is
               when 1 =>  --  Latitude
                  Loc.Latitude := DNS_Records.Latitude_Degrees'Value (Field);
               when 2 =>  --  Longitude
                  Loc.Longitude := DNS_Records.Longitude_Degrees'Value (Field);
               when 3 =>  --  Altitude
                  Loc.Altitude := DNS_Records.Altitude_Meters'Value (Field);
               when 4 =>  --  Description
                  Loc.Description := Entry_Strings.To_Bounded_String (Field);
                  Valid := True;  --  Successfully parsed all fields
               when others =>
                  null;
            end case;
         end;

         Field_Num := Field_Num + 1;
         Start_Pos := End_Pos + 2;  --  Skip comma
      end loop;

   exception
      when others =>
         Valid := False;
   end Parse_CSV_Line;

   --  Load location pool from CSV file
   procedure Load_Location_Pool (Filename : String) is
      File : File_Type;
      Count : Natural := 0;
   begin
      Location_Pool.Clear;

      Open (File, In_File, Filename);

      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
            Loc  : Location_Entry;
            Valid : Boolean;
         begin
            if Is_Valid_Line (Line) then
               Parse_CSV_Line (Line, Loc, Valid);
               if Valid then
                  Location_Pool.Append (Loc);
                  Count := Count + 1;
               end if;
            end if;
         end;
      end loop;

      Close (File);

      if Count = 0 then
         raise Data_Load_Error with "No valid location entries found in " & Filename;
      end if;

   exception
      when Name_Error =>
         raise Data_Load_Error with "Cannot open file: " & Filename;
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Load_Location_Pool;

   --  Query pool sizes
   function CPU_Pool_Size return Natural is
   begin
      return Natural (CPU_Pool.Length);
   end CPU_Pool_Size;

   function OS_Pool_Size return Natural is
   begin
      return Natural (OS_Pool.Length);
   end OS_Pool_Size;

   function Location_Pool_Size return Natural is
   begin
      return Natural (Location_Pool.Length);
   end Location_Pool_Size;

   --  Generate random HINFO record
   function Random_HINFO (
      Domain : String;
      Class  : DNS_Records.DNS_Class := DNS_Records.IN_Class;
      TTL    : DNS_Records.TTL_Seconds := 300
   ) return DNS_Records.Resource_Record is
   begin
      if CPU_Pool.Is_Empty or OS_Pool.Is_Empty then
         raise Data_Load_Error with "CPU or OS pool is empty";
      end if;

      --  Get random indices
      declare
         CPU_Idx : constant Positive :=
            1 + (CPU_Random.Random (CPU_Gen) mod Natural (CPU_Pool.Length));
         OS_Idx  : constant Positive :=
            1 + (OS_Random.Random (OS_Gen) mod Natural (OS_Pool.Length));

         CPU_Str : constant String := Entry_Strings.To_String (CPU_Pool.Element (CPU_Idx));
         OS_Str  : constant String := Entry_Strings.To_String (OS_Pool.Element (OS_Idx));
      begin
         return DNS_Records.Create_HINFO (
            Domain => Domain,
            CPU    => CPU_Str,
            OS     => OS_Str,
            Class  => Class,
            TTL    => TTL
         );
      end;
   end Random_HINFO;

   --  Generate random LOC record
   function Random_LOC (
      Domain : String;
      Class  : DNS_Records.DNS_Class := DNS_Records.IN_Class;
      TTL    : DNS_Records.TTL_Seconds := 300
   ) return DNS_Records.Resource_Record is
   begin
      if Location_Pool.Is_Empty then
         raise Data_Load_Error with "Location pool is empty";
      end if;

      --  Get random location
      declare
         Loc_Idx : constant Positive :=
            1 + (Loc_Random.Random (Loc_Gen) mod Natural (Location_Pool.Length));
         Loc     : constant Location_Entry := Location_Pool.Element (Loc_Idx);
      begin
         return DNS_Records.Create_LOC (
            Domain    => Domain,
            Latitude  => Loc.Latitude,
            Longitude => Loc.Longitude,
            Altitude  => Loc.Altitude,
            Class     => Class,
            TTL       => TTL
         );
      end;
   end Random_LOC;

   --  Generate both HINFO and LOC (quantum server effect!)
   procedure Generate_Quantum_Server (
      Domain      : String;
      HINFO_Out   : out DNS_Records.Resource_Record;
      LOC_Out     : out DNS_Records.Resource_Record
   ) is
   begin
      HINFO_Out := Random_HINFO (Domain);
      LOC_Out := Random_LOC (Domain);
   end Generate_Quantum_Server;

   --  Display pool status
   function Pool_Status return String is
   begin
      return "Pools loaded: " &
             "CPUs=" & Natural'Image (CPU_Pool_Size) &
             ", OSes=" & Natural'Image (OS_Pool_Size) &
             ", Locations=" & Natural'Image (Location_Pool_Size);
   end Pool_Status;

   --  Clear all pools
   procedure Clear_Pools is
   begin
      CPU_Pool.Clear;
      OS_Pool.Clear;
      Location_Pool.Clear;
   end Clear_Pools;

begin
   --  Initialize random number generators on package load
   Init_Random;
end Randomizer;
