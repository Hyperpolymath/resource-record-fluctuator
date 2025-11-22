--  Master Configuration Package Body
--  Comprehensive configuration management for all enterprise features

with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Master_Config is

   use Ada.Text_IO;
   use DNS_Records_Extended;

   --  Current active configuration
   Active_Config : Master_Configuration;
   Config_Loaded : Boolean := False;

   --  ========================================================================
   --  CONFIGURATION PROFILES
   --  ========================================================================

   function Get_Default_Config (Mode : Deployment_Mode) return Master_Configuration is
   begin
      case Mode is
         when Development =>
            return Development_Profile;
         when Staging =>
            return Staging_Profile;
         when Production =>
            return Production_Profile;
         when Honeypot =>
            return Honeypot_Profile;
         when Research =>
            return Research_Profile;
      end case;
   end Get_Default_Config;

   function Development_Profile return Master_Configuration is
      Config : Master_Configuration;
   begin
      Config.Mode := Development;
      Config.Environment := Local_Development;
      Config.Instance_Name := To_Bounded_String ("dev-hinfo-fluctuator-01");
      Config.Config_Version := To_Bounded_String ("2.0.0-dev");

      --  Enable all features for testing
      Config.HINFO_Fluctuation_Enabled := True;
      Config.LOC_Fluctuation_Enabled := True;
      Config.Quantum_Server_Mode := True;

      --  Extended records
      Config.A_Records_Enabled := True;
      Config.AAAA_Records_Enabled := True;
      Config.MX_Records_Enabled := True;
      Config.TXT_Records_Enabled := True;
      Config.SPF_Enabled := True;
      Config.DKIM_Enabled := True;
      Config.DMARC_Enabled := True;
      Config.CAA_Enabled := True;
      Config.TLSA_Enabled := True;
      Config.SSHFP_Enabled := True;
      Config.APL_Enabled := True;

      --  Relaxed security for development
      Config.Header_Obfuscation := False;  --  Show real stack
      Config.Diagnostic_Mode_Enabled := True;
      Config.SDP_Enabled := False;
      Config.SNMP_Allowed := True;  --  Allow for testing
      Config.NETCONF_Preferred := True;

      --  Logging
      Config.Audit_Logging := True;
      Config.Security_Logging := True;
      Config.Metrics_Export := True;

      return Config;
   end Development_Profile;

   function Staging_Profile return Master_Configuration is
      Config : Master_Configuration;
   begin
      Config.Mode := Staging;
      Config.Environment := Internal_Network;
      Config.Instance_Name := To_Bounded_String ("staging-hinfo-fluctuator-01");
      Config.Config_Version := To_Bounded_String ("2.0.0-staging");

      --  Production-like settings
      Config.HINFO_Fluctuation_Enabled := True;
      Config.LOC_Fluctuation_Enabled := True;
      Config.Quantum_Server_Mode := True;

      --  Extended records enabled
      Config.A_Records_Enabled := True;
      Config.AAAA_Records_Enabled := True;
      Config.MX_Records_Enabled := True;
      Config.TXT_Records_Enabled := True;
      Config.SPF_Enabled := True;
      Config.DKIM_Enabled := True;
      Config.DMARC_Enabled := True;
      Config.CAA_Enabled := True;
      Config.TLSA_Enabled := True;
      Config.SSHFP_Enabled := True;
      Config.APL_Enabled := True;

      --  Tighter security than development
      Config.Header_Obfuscation := True;
      Config.Diagnostic_Mode_Enabled := True;
      Config.SDP_Enabled := False;  --  Optional for staging
      Config.SNMP_Allowed := False;  --  Prefer NETCONF
      Config.NETCONF_Preferred := True;

      --  Full logging
      Config.Audit_Logging := True;
      Config.Security_Logging := True;
      Config.Metrics_Export := True;

      return Config;
   end Staging_Profile;

   function Production_Profile return Master_Configuration is
      Config : Master_Configuration;
   begin
      Config.Mode := Production;
      Config.Environment := Public_Internet;
      Config.Instance_Name := To_Bounded_String ("prod-hinfo-fluctuator-01");
      Config.Config_Version := To_Bounded_String ("2.0.0-production");

      --  Core fluctuation
      Config.HINFO_Fluctuation_Enabled := True;
      Config.LOC_Fluctuation_Enabled := True;
      Config.Quantum_Server_Mode := True;

      --  Extended records for production services
      Config.A_Records_Enabled := True;
      Config.AAAA_Records_Enabled := True;
      Config.MX_Records_Enabled := True;
      Config.TXT_Records_Enabled := True;
      Config.SPF_Enabled := True;
      Config.DKIM_Enabled := True;
      Config.DMARC_Enabled := True;
      Config.CAA_Enabled := True;
      Config.TLSA_Enabled := True;  --  DANE for TLS
      Config.SSHFP_Enabled := True;
      Config.APL_Enabled := True;  --  CIDR access control

      --  Maximum security
      Config.Header_Obfuscation := True;
      Config.Diagnostic_Mode_Enabled := True;  --  For authorized maintainers only
      Config.SDP_Enabled := True;  --  Zero-trust
      Config.SNMP_Allowed := False;  --  BANNED in production
      Config.NETCONF_Preferred := True;
      Config.Port_Rotation_Enabled := True;
      Config.SSH_Port_Rotation := True;

      --  Comprehensive logging
      Config.Audit_Logging := True;
      Config.Security_Logging := True;
      Config.Metrics_Export := True;
      Config.Backup_Enabled := True;

      return Config;
   end Production_Profile;

   function Honeypot_Profile return Master_Configuration is
      Config : Master_Configuration;
   begin
      Config.Mode := Honeypot;
      Config.Environment := DMZ;
      Config.Instance_Name := To_Bounded_String ("honeypot-hinfo-fluctuator-01");
      Config.Config_Version := To_Bounded_String ("2.0.0-honeypot");

      --  Maximum fluctuation for deception
      Config.HINFO_Fluctuation_Enabled := True;
      Config.LOC_Fluctuation_Enabled := True;
      Config.Quantum_Server_Mode := True;

      --  Deceptive DNS records
      Config.A_Records_Enabled := True;
      Config.AAAA_Records_Enabled := True;
      Config.MX_Records_Enabled := True;
      Config.TXT_Records_Enabled := True;
      Config.SPF_Enabled := True;

      --  Maximum obfuscation
      Config.Header_Obfuscation := True;
      Config.Diagnostic_Mode_Enabled := False;  --  Hide real stack completely
      Config.SDP_Enabled := False;  --  Honeypot should be accessible
      Config.SNMP_Allowed := True;  --  Intentionally expose for deception

      --  Extensive logging for attack analysis
      Config.Audit_Logging := True;
      Config.Security_Logging := True;
      Config.Metrics_Export := True;

      return Config;
   end Honeypot_Profile;

   function Research_Profile return Master_Configuration is
      Config : Master_Configuration;
   begin
      Config.Mode := Research;
      Config.Environment := Internal_Network;
      Config.Instance_Name := To_Bounded_String ("research-hinfo-fluctuator-01");
      Config.Config_Version := To_Bounded_String ("2.0.0-research");

      --  All features enabled
      Config.HINFO_Fluctuation_Enabled := True;
      Config.LOC_Fluctuation_Enabled := True;
      Config.Quantum_Server_Mode := True;

      --  All DNS record types
      Config.A_Records_Enabled := True;
      Config.AAAA_Records_Enabled := True;
      Config.MX_Records_Enabled := True;
      Config.TXT_Records_Enabled := True;
      Config.SPF_Enabled := True;
      Config.DKIM_Enabled := True;
      Config.DMARC_Enabled := True;
      Config.CAA_Enabled := True;
      Config.TLSA_Enabled := True;
      Config.SSHFP_Enabled := True;
      Config.APL_Enabled := True;
      Config.DNSSEC_Enabled := True;  --  Experimental

      --  All security features for testing
      Config.Header_Obfuscation := True;
      Config.Diagnostic_Mode_Enabled := True;
      Config.SDP_Enabled := True;
      Config.SNMP_Allowed := True;  --  Allow for comparison testing
      Config.NETCONF_Preferred := True;
      Config.Port_Rotation_Enabled := True;

      --  Verbose logging
      Config.Audit_Logging := True;
      Config.Security_Logging := True;
      Config.Metrics_Export := True;

      return Config;
   end Research_Profile;

   --  ========================================================================
   --  CONFIGURATION VALIDATION
   --  ========================================================================

   function Validate_Configuration (
      Config : Master_Configuration
   ) return Validation_Result is
      Result : Validation_Result;
      use Ada.Strings.Unbounded;
      Messages : Unbounded_String;
   begin
      Result.Valid := True;
      Result.Warnings_Count := 0;
      Result.Errors_Count := 0;

      --  Check production security
      if Config.Mode = Production then
         if Config.SNMP_Allowed then
            Append (Messages, "ERROR: SNMP not allowed in production" & ASCII.LF);
            Result.Errors_Count := Result.Errors_Count + 1;
            Result.Valid := False;
         end if;

         if not Config.Header_Obfuscation then
            Append (Messages, "WARNING: Header obfuscation disabled in production" & ASCII.LF);
            Result.Warnings_Count := Result.Warnings_Count + 1;
         end if;

         if not Config.SDP_Enabled then
            Append (Messages, "WARNING: SDP disabled in production (consider enabling)" & ASCII.LF);
            Result.Warnings_Count := Result.Warnings_Count + 1;
         end if;

         if not Config.Audit_Logging then
            Append (Messages, "ERROR: Audit logging must be enabled in production" & ASCII.LF);
            Result.Errors_Count := Result.Errors_Count + 1;
            Result.Valid := False;
         end if;
      end if;

      --  Check honeypot configuration
      if Config.Mode = Honeypot then
         if Config.SDP_Enabled then
            Append (Messages, "WARNING: SDP enabled for honeypot (may reduce effectiveness)" & ASCII.LF);
            Result.Warnings_Count := Result.Warnings_Count + 1;
         end if;
      end if;

      --  General security checks
      if not Config.NETCONF_Preferred and Config.SNMP_Allowed then
         Append (Messages, "WARNING: SNMP allowed without preferring NETCONF" & ASCII.LF);
         Result.Warnings_Count := Result.Warnings_Count + 1;
      end if;

      Result.Messages := To_Bounded_String (To_String (Messages));
      return Result;
   end Validate_Configuration;

   function Check_Security_Posture (
      Config : Master_Configuration
   ) return String is
      use Ada.Strings.Unbounded;
      Report : Unbounded_String;
   begin
      Append (Report, "Security Posture Report" & ASCII.LF);
      Append (Report, "=======================" & ASCII.LF);
      Append (Report, "Deployment Mode: " & Config.Mode'Image & ASCII.LF);
      Append (Report, "Environment: " & Config.Environment'Image & ASCII.LF);
      Append (Report, ASCII.LF);

      --  Security features
      Append (Report, "Security Features:" & ASCII.LF);
      if Config.SDP_Enabled then
         Append (Report, "  ✅ Zero-Trust SDP Enabled" & ASCII.LF);
      else
         Append (Report, "  ⚠️  SDP Disabled" & ASCII.LF);
      end if;

      if Config.Port_Rotation_Enabled then
         Append (Report, "  ✅ Port Rotation Enabled" & ASCII.LF);
      end if;

      if Config.Header_Obfuscation then
         Append (Report, "  ✅ Header Obfuscation Enabled" & ASCII.LF);
      end if;

      if Config.Diagnostic_Mode_Enabled then
         Append (Report, "  ✅ Diagnostic Mode (Authorized Access Only)" & ASCII.LF);
      end if;

      Append (Report, ASCII.LF);

      --  Protocol security
      Append (Report, "Protocol Security:" & ASCII.LF);
      if Config.SNMP_Allowed then
         Append (Report, "  ⚠️  SNMP Allowed (insecure)" & ASCII.LF);
      else
         Append (Report, "  ✅ SNMP Disabled" & ASCII.LF);
      end if;

      if Config.NETCONF_Preferred then
         Append (Report, "  ✅ NETCONF Preferred" & ASCII.LF);
      end if;

      Append (Report, ASCII.LF);

      --  Logging and monitoring
      Append (Report, "Logging:" & ASCII.LF);
      if Config.Audit_Logging then
         Append (Report, "  ✅ Audit Logging Enabled" & ASCII.LF);
      end if;
      if Config.Security_Logging then
         Append (Report, "  ✅ Security Logging Enabled" & ASCII.LF);
      end if;
      if Config.Metrics_Export then
         Append (Report, "  ✅ Metrics Export Enabled" & ASCII.LF);
      end if;

      return To_String (Report);
   end Check_Security_Posture;

   --  ========================================================================
   --  CONFIGURATION FILE OPERATIONS
   --  ========================================================================

   procedure Load_Master_Config (
      Filename : String;
      Config   : out Master_Configuration
   ) is
   begin
      Put_Line ("[Config] Loading master configuration from: " & Filename);
      --  In real implementation: parse INI file
      Config := Production_Profile;  --  Fallback to safe default
      Put_Line ("[Config] Configuration loaded (stub)");
   exception
      when others =>
         Put_Line ("[Config] ERROR: Failed to load configuration");
         raise Config_Load_Error with "Failed to load: " & Filename;
   end Load_Master_Config;

   procedure Save_Master_Config (
      Filename : String;
      Config   : Master_Configuration
   ) is
   begin
      Put_Line ("[Config] Saving master configuration to: " & Filename);
      --  In real implementation: write INI file
      Put_Line ("[Config] Configuration saved (stub)");
   exception
      when others =>
         raise Config_Load_Error with "Failed to save: " & Filename;
   end Save_Master_Config;

   procedure Import_From_YAML (
      Filename : String;
      Config   : out Master_Configuration
   ) is
   begin
      Put_Line ("[Config] Importing from YAML: " & Filename);
      --  In real implementation: parse YAML
      Config := Production_Profile;
      Put_Line ("[Config] YAML import complete (stub)");
   end Import_From_YAML;

   function Export_To_YAML (
      Config : Master_Configuration
   ) return String is
   begin
      --  In real implementation: generate YAML
      return "# Master Configuration YAML" & ASCII.LF &
             "mode: " & Config.Mode'Image & ASCII.LF &
             "environment: " & Config.Environment'Image & ASCII.LF;
   end Export_To_YAML;

   function Export_To_JSON (
      Config : Master_Configuration
   ) return String is
   begin
      --  In real implementation: generate JSON
      return "{""mode"": """ & Config.Mode'Image & """," & ASCII.LF &
             " ""environment"": """ & Config.Environment'Image & """}";
   end Export_To_JSON;

   --  ========================================================================
   --  CONFIGURATION TEMPLATES
   --  ========================================================================

   function Generate_Example_Config return String is
   begin
      return "# HINFO-LOC Fluctuator Master Configuration" & ASCII.LF &
             "# See production_config.yaml for complete example" & ASCII.LF &
             ASCII.LF &
             "[deployment]" & ASCII.LF &
             "mode = production" & ASCII.LF &
             "environment = public_internet" & ASCII.LF &
             ASCII.LF &
             "[security]" & ASCII.LF &
             "sdp_enabled = true" & ASCII.LF &
             "port_rotation = true" & ASCII.LF &
             "header_obfuscation = true" & ASCII.LF;
   end Generate_Example_Config;

   function Generate_Scenario_Config (
      Scenario : String
   ) return Master_Configuration is
   begin
      if Scenario = "public-web" then
         return Production_Profile;
      elsif Scenario = "internal-app" then
         return Staging_Profile;
      elsif Scenario = "honeypot" then
         return Honeypot_Profile;
      else
         return Development_Profile;
      end if;
   end Generate_Scenario_Config;

   --  ========================================================================
   --  RUNTIME CONFIGURATION UPDATES
   --  ========================================================================

   procedure Apply_Configuration_Hot (
      Config : Master_Configuration
   ) is
   begin
      Put_Line ("[Config] Applying configuration changes (hot reload)");

      --  Validate first
      declare
         Validation : constant Validation_Result := Validate_Configuration (Config);
      begin
         if not Validation.Valid then
            Put_Line ("[Config] ERROR: Configuration validation failed");
            Put_Line (To_String (Validation.Messages));
            raise Config_Validation_Error with "Invalid configuration";
         end if;

         if Validation.Warnings_Count > 0 then
            Put_Line ("[Config] Warnings:");
            Put_Line (To_String (Validation.Messages));
         end if;
      end;

      --  Apply configuration
      Active_Config := Config;
      Config_Loaded := True;

      Put_Line ("[Config] Configuration applied successfully");
   end Apply_Configuration_Hot;

   procedure Reload_Configuration (
      Filename : String
   ) is
      New_Config : Master_Configuration;
   begin
      Put_Line ("[Config] Reloading configuration from: " & Filename);
      Load_Master_Config (Filename, New_Config);
      Apply_Configuration_Hot (New_Config);
   end Reload_Configuration;

   function Get_Active_Configuration return Master_Configuration is
   begin
      if not Config_Loaded then
         Put_Line ("[Config] WARNING: No configuration loaded, using production defaults");
         return Production_Profile;
      end if;
      return Active_Config;
   end Get_Active_Configuration;

   --  ========================================================================
   --  MIGRATION AND COMPATIBILITY
   --  ========================================================================

   function Migrate_From_Simple_Config (
      Old_Config : Config.Application_Config
   ) return Master_Configuration is
      New_Config : Master_Configuration := Production_Profile;
   begin
      Put_Line ("[Config Migration] Migrating from simple config");

      --  Transfer compatible settings
      New_Config.Legacy_Config := Old_Config;

      Put_Line ("[Config Migration] Migration complete");
      return New_Config;
   end Migrate_From_Simple_Config;

   function Is_Compatible (
      Config_Version : String
   ) return Boolean is
   begin
      --  Check version compatibility
      --  Format: "major.minor.patch-suffix"
      --  Compatible if major version matches
      return Config_Version (1) = '2';  --  Version 2.x.x
   end Is_Compatible;

   --  ========================================================================
   --  CONFIGURATION DIFF AND COMPARISON
   --  ========================================================================

   function Compare_Configurations (
      Config_A : Master_Configuration;
      Config_B : Master_Configuration
   ) return String is
      use Ada.Strings.Unbounded;
      Diff : Unbounded_String;
   begin
      Append (Diff, "Configuration Comparison" & ASCII.LF);
      Append (Diff, "=======================" & ASCII.LF);

      if Config_A.Mode /= Config_B.Mode then
         Append (Diff, "Mode: " & Config_A.Mode'Image & " -> " & Config_B.Mode'Image & ASCII.LF);
      end if;

      if Config_A.SDP_Enabled /= Config_B.SDP_Enabled then
         Append (Diff, "SDP: " & Config_A.SDP_Enabled'Image & " -> " &
                 Config_B.SDP_Enabled'Image & ASCII.LF);
      end if;

      return To_String (Diff);
   end Compare_Configurations;

   function Generate_Migration_Plan (
      From_Config : Master_Configuration;
      To_Config   : Master_Configuration
   ) return String is
   begin
      return "Migration Plan:" & ASCII.LF &
             "1. Backup current configuration" & ASCII.LF &
             "2. Validate new configuration" & ASCII.LF &
             "3. Apply changes incrementally" & ASCII.LF &
             "4. Monitor for issues" & ASCII.LF;
   end Generate_Migration_Plan;

   --  ========================================================================
   --  EMERGENCY CONFIGURATIONS
   --  ========================================================================

   function Emergency_Lockdown_Config return Master_Configuration is
      Config : Master_Configuration := Production_Profile;
   begin
      Put_Line ("[EMERGENCY] Generating lockdown configuration");

      --  Maximum security
      Config.SDP_Enabled := True;
      Config.SNMP_Allowed := False;
      Config.Header_Obfuscation := True;
      Config.Diagnostic_Mode_Enabled := False;  --  Hide everything

      return Config;
   end Emergency_Lockdown_Config;

   function Emergency_Recovery_Config return Master_Configuration is
      Config : Master_Configuration := Development_Profile;
   begin
      Put_Line ("[EMERGENCY] Generating recovery configuration");

      --  Relaxed for recovery
      Config.SDP_Enabled := False;
      Config.Diagnostic_Mode_Enabled := True;

      return Config;
   end Emergency_Recovery_Config;

   function Minimal_Safe_Config return Master_Configuration is
      Config : Master_Configuration;
   begin
      --  Bare minimum configuration
      Config.Mode := Production;
      Config.HINFO_Fluctuation_Enabled := True;
      Config.LOC_Fluctuation_Enabled := True;
      Config.Audit_Logging := True;

      return Config;
   end Minimal_Safe_Config;

end Master_Config;
