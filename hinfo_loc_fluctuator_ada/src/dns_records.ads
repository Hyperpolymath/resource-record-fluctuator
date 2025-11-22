--  DNS Records Package Specification
--  Type-safe representations of HINFO and LOC DNS resource records
--
--  HINFO: Host Information (RFC 1035) - Deprecated per RFC 8482
--  LOC:   Location (RFC 1876) - Deprecated per RFC 8482
--
--  This deprecation is intentional - these records are ideal for
--  experimental security and obfuscation without affecting production.

with Ada.Strings.Bounded;

package DNS_Records is

   --  DNS Class types (RFC 1035 Section 3.2.4)
   type DNS_Class is (
      IN_Class,   --  Internet (most common)
      CH_Class,   --  Chaos (rarely supported)
      HS_Class    --  Hesiod (rarely supported)
   );

   --  TTL (Time To Live) in seconds
   --  Range: 1 second to 7 days (604800 seconds)
   --  Compile-time guarantee: TTL cannot be zero or exceed 7 days
   type TTL_Seconds is range 1 .. 604_800
      with Default_Value => 300;  --  5 minutes default

   --  Bounded string types for HINFO fields
   --  CPU and OS fields limited to 255 characters (DNS TXT field limit)
   package CPU_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (255);
   package OS_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (255);

   use CPU_Strings;
   use OS_Strings;

   --  HINFO Record: Host Information (RFC 1035)
   --  Contains CPU type and Operating System
   type HINFO_Record is record
      Class : DNS_Class;
      TTL   : TTL_Seconds;
      CPU   : CPU_Strings.Bounded_String;
      OS    : OS_Strings.Bounded_String;
   end record;

   --  Geographic coordinate types with compile-time range checking
   --
   --  Latitude: -90.0 (South Pole) to +90.0 (North Pole) degrees
   --  Precision: 6 decimal places (~0.11 meters)
   type Latitude_Degrees is delta 0.000_001 range -90.0 .. 90.0
      with Default_Value => 0.0;

   --  Longitude: -180.0 (West) to +180.0 (East) degrees
   --  Precision: 6 decimal places (~0.11 meters at equator)
   type Longitude_Degrees is delta 0.000_001 range -180.0 .. 180.0
      with Default_Value => 0.0;

   --  Altitude in meters above WGS84 ellipsoid
   --  Range: Dead Sea (-100km safety margin) to space (Kármán line + margin)
   type Altitude_Meters is delta 0.01 range -100_000.0 .. 42_849_672.95
      with Default_Value => 0.0;

   --  Size/Precision values for LOC record (RFC 1876 Section 2)
   --  These specify the uncertainty in the location
   --  Range: 0 to 90000000 centimeters (0 to 900 km)
   type Size_Centimeters is range 0 .. 90_000_000
      with Default_Value => 100;  --  1 meter default uncertainty

   --  LOC Record: Geographic Location (RFC 1876)
   --  Contains latitude, longitude, altitude, and precision values
   type LOC_Record is record
      Class           : DNS_Class;
      TTL             : TTL_Seconds;
      Latitude        : Latitude_Degrees;
      Longitude       : Longitude_Degrees;
      Altitude        : Altitude_Meters;
      Size            : Size_Centimeters;   --  Sphere diameter
      Horizontal_Prec : Size_Centimeters;   --  Horizontal precision
      Vertical_Prec   : Size_Centimeters;   --  Vertical precision
   end record;

   --  Domain name (bounded string for safety)
   package Domain_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (253);
   subtype Domain_Name is Domain_Strings.Bounded_String;

   --  Complete DNS resource record entry
   type Resource_Record_Type is (HINFO_Type, LOC_Type);

   --  Variant record to hold either HINFO or LOC
   type Resource_Record (RR_Type : Resource_Record_Type := HINFO_Type) is record
      Domain : Domain_Name;
      case RR_Type is
         when HINFO_Type =>
            HINFO : HINFO_Record;
         when LOC_Type =>
            LOC   : LOC_Record;
      end case;
   end record;

   --  Conversion functions for display
   function To_String (Class : DNS_Class) return String;
   function To_String (RR : HINFO_Record) return String;
   function To_String (RR : LOC_Record) return String;
   function To_String (RR : Resource_Record) return String;

   --  Factory functions for creating records
   function Create_HINFO (
      Domain : String;
      CPU    : String;
      OS     : String;
      Class  : DNS_Class := IN_Class;
      TTL    : TTL_Seconds := 300
   ) return Resource_Record;

   function Create_LOC (
      Domain          : String;
      Latitude        : Latitude_Degrees;
      Longitude       : Longitude_Degrees;
      Altitude        : Altitude_Meters := 0.0;
      Class           : DNS_Class := IN_Class;
      TTL             : TTL_Seconds := 300;
      Size            : Size_Centimeters := 100;
      Horizontal_Prec : Size_Centimeters := 10_000;
      Vertical_Prec   : Size_Centimeters := 10
   ) return Resource_Record;

   --  Validation functions
   function Is_Valid (RR : HINFO_Record) return Boolean;
   function Is_Valid (RR : LOC_Record) return Boolean;

end DNS_Records;
