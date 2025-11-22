--  DNS Records Package Body
--  Implementation of type conversions and factory functions

with Ada.Strings.Fixed;

package body DNS_Records is

   use Ada.Strings.Fixed;

   --  Convert DNS Class to string representation
   function To_String (Class : DNS_Class) return String is
   begin
      case Class is
         when IN_Class => return "IN";
         when CH_Class => return "CH";
         when HS_Class => return "HS";
      end case;
   end To_String;

   --  Convert HINFO record to zone file format string
   function To_String (RR : HINFO_Record) return String is
   begin
      return "HINFO " &
             To_String (RR.Class) & " " &
             TTL_Seconds'Image (RR.TTL) & " " &
             """" & CPU_Strings.To_String (RR.CPU) & """ " &
             """" & OS_Strings.To_String (RR.OS) & """";
   end To_String;

   --  Convert LOC record to zone file format string (RFC 1876)
   function To_String (RR : LOC_Record) return String is
      Lat_Str : constant String := Latitude_Degrees'Image (RR.Latitude);
      Lon_Str : constant String := Longitude_Degrees'Image (RR.Longitude);
      Alt_Str : constant String := Altitude_Meters'Image (RR.Altitude);
   begin
      return "LOC " &
             To_String (RR.Class) & " " &
             TTL_Seconds'Image (RR.TTL) & " " &
             Lat_Str & " " &
             Lon_Str & " " &
             Alt_Str & "m " &
             Size_Centimeters'Image (RR.Size) & "cm " &
             Size_Centimeters'Image (RR.Horizontal_Prec) & "cm " &
             Size_Centimeters'Image (RR.Vertical_Prec) & "cm";
   end To_String;

   --  Convert complete resource record to string
   function To_String (RR : Resource_Record) return String is
      Domain_Str : constant String := Domain_Strings.To_String (RR.Domain);
   begin
      case RR.RR_Type is
         when HINFO_Type =>
            return Domain_Str & " " & To_String (RR.HINFO);
         when LOC_Type =>
            return Domain_Str & " " & To_String (RR.LOC);
      end case;
   end To_String;

   --  Create HINFO resource record
   function Create_HINFO (
      Domain : String;
      CPU    : String;
      OS     : String;
      Class  : DNS_Class := IN_Class;
      TTL    : TTL_Seconds := 300
   ) return Resource_Record is
      RR : Resource_Record (HINFO_Type);
   begin
      RR.Domain := Domain_Strings.To_Bounded_String (Domain);
      RR.HINFO.Class := Class;
      RR.HINFO.TTL := TTL;
      RR.HINFO.CPU := CPU_Strings.To_Bounded_String (CPU);
      RR.HINFO.OS := OS_Strings.To_Bounded_String (OS);
      return RR;
   end Create_HINFO;

   --  Create LOC resource record
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
   ) return Resource_Record is
      RR : Resource_Record (LOC_Type);
   begin
      RR.Domain := Domain_Strings.To_Bounded_String (Domain);
      RR.LOC.Class := Class;
      RR.LOC.TTL := TTL;
      RR.LOC.Latitude := Latitude;
      RR.LOC.Longitude := Longitude;
      RR.LOC.Altitude := Altitude;
      RR.LOC.Size := Size;
      RR.LOC.Horizontal_Prec := Horizontal_Prec;
      RR.LOC.Vertical_Prec := Vertical_Prec;
      return RR;
   end Create_LOC;

   --  Validate HINFO record (check for non-empty fields)
   function Is_Valid (RR : HINFO_Record) return Boolean is
   begin
      return CPU_Strings.Length (RR.CPU) > 0 and then
             OS_Strings.Length (RR.OS) > 0;
   end Is_Valid;

   --  Validate LOC record (ranges checked by type system)
   function Is_Valid (RR : LOC_Record) return Boolean is
   begin
      --  Type system already enforces valid ranges
      --  This function exists for future extensibility
      return True;
   end Is_Valid;

end DNS_Records;
