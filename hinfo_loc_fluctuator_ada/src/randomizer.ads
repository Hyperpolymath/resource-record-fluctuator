--  Randomizer Module Specification
--  Loads data pools and generates random HINFO/LOC records
--
--  Data Sources:
--  - machines.txt: CPU types (one per line, # for comments)
--  - operating_systems.txt: OS names (one per line, # for comments)
--  - locations.csv: Geographic locations (lat,lon,alt_m,description)

with DNS_Records;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;

package Randomizer is

   --  Exception for file/parsing errors
   Data_Load_Error : exception;

   --  Bounded strings for pool entries
   package Entry_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (255);
   subtype Entry_String is Entry_Strings.Bounded_String;

   --  Location data from CSV
   type Location_Entry is record
      Latitude    : DNS_Records.Latitude_Degrees;
      Longitude   : DNS_Records.Longitude_Degrees;
      Altitude    : DNS_Records.Altitude_Meters;
      Description : Entry_String;
   end record;

   --  Vectors for storing pools
   package CPU_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Entry_String,
      "="          => Entry_Strings."="
   );

   package OS_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Entry_String,
      "="          => Entry_Strings."="
   );

   package Location_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Location_Entry
   );

   --  Data pools (global state)
   CPU_Pool      : CPU_Vectors.Vector;
   OS_Pool       : OS_Vectors.Vector;
   Location_Pool : Location_Vectors.Vector;

   --  Pool management procedures
   procedure Load_CPU_Pool (Filename : String);
   procedure Load_OS_Pool (Filename : String);
   procedure Load_Location_Pool (Filename : String);

   --  Query pool sizes
   function CPU_Pool_Size return Natural;
   function OS_Pool_Size return Natural;
   function Location_Pool_Size return Natural;

   --  Random record generation
   function Random_HINFO (
      Domain : String;
      Class  : DNS_Records.DNS_Class := DNS_Records.IN_Class;
      TTL    : DNS_Records.TTL_Seconds := 300
   ) return DNS_Records.Resource_Record;

   function Random_LOC (
      Domain : String;
      Class  : DNS_Records.DNS_Class := DNS_Records.IN_Class;
      TTL    : DNS_Records.TTL_Seconds := 300
   ) return DNS_Records.Resource_Record;

   --  Generate both HINFO and LOC for a domain (quantum server!)
   procedure Generate_Quantum_Server (
      Domain      : String;
      HINFO_Out   : out DNS_Records.Resource_Record;
      LOC_Out     : out DNS_Records.Resource_Record
   );

   --  Pool status display
   function Pool_Status return String;

   --  Clear all pools
   procedure Clear_Pools;

end Randomizer;
