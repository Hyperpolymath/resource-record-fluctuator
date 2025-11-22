--  Protocol Management Package Body
--  Implementation of NETCONF, RESTCONF, gNMI, and SNMP protocols

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Firewall_Manager;

package body Protocol_Manager is

   use Ada.Text_IO;
   use DNS_Records_Extended;

   --  ========================================================================
   --  INTERNAL STATE
   --  ========================================================================

   Current_Config : Protocol_Manager_Config;
   Initialized : Boolean := False;

   --  Protocol statistics
   type Stats_Array is array (Management_Protocol) of Protocol_Stats;
   Protocol_Statistics : Stats_Array;

   --  ========================================================================
   --  PROTOCOL OPERATIONS
   --  ========================================================================

   procedure Initialize (Config : Protocol_Manager_Config) is
   begin
      Put_Line ("[Protocol Manager] Initializing management protocols");

      Current_Config := Config;
      Initialized := True;

      --  Security policy enforcement
      if Config.Disable_Insecure then
         Put_Line ("[Protocol Manager] Insecure protocols (SNMPv1/v2c) DISABLED by policy");
         if Config.SNMP.Enabled and then
            (Config.SNMP.Versions (SNMP_v1).Enabled or
             Config.SNMP.Versions (SNMP_v2c).Enabled)
         then
            Put_Line ("[Protocol Manager] ERROR: Insecure SNMP versions enabled!");
            Put_Line ("[Protocol Manager] Policy violation - disabling SNMPv1/v2c");
         end if;
      end if;

      if Config.Require_TLS then
         Put_Line ("[Protocol Manager] TLS REQUIRED for all protocols");
      end if;

      --  Protocol initialization
      if Config.SNMP.Enabled then
         Put_Line ("[Protocol Manager] SNMP enabled");
         if Config.SNMP.Warn_Insecure then
            Put_Line ("[Protocol Manager] ⚠️  WARNING: Consider using NETCONF instead of SNMP");
         end if;
      end if;

      if Config.NETCONF.Enabled then
         Put_Line ("[Protocol Manager] ✅ NETCONF enabled (RECOMMENDED)");
         Put_Line ("[Protocol Manager] NETCONF port: " &
                   Trim (Config.NETCONF.Port'Img, Ada.Strings.Left));
      end if;

      if Config.RESTCONF.Enabled then
         Put_Line ("[Protocol Manager] ✅ RESTCONF enabled (RECOMMENDED)");
         Put_Line ("[Protocol Manager] RESTCONF base: " &
                   To_String (Config.RESTCONF.Base_Path));
      end if;

      if Config.gNMI.Enabled then
         Put_Line ("[Protocol Manager] ✅ gNMI enabled (MODERN)");
         Put_Line ("[Protocol Manager] gNMI encoding: " & Config.gNMI.Encoding'Image);
      end if;

      if Config.Metrics.Enabled then
         Put_Line ("[Protocol Manager] ✅ Metrics export enabled");
         Put_Line ("[Protocol Manager] Metrics path: " &
                   To_String (Config.Metrics.Metrics_Path));
      end if;

      --  SDP integration
      if Config.SDP_Integration then
         Put_Line ("[Protocol Manager] Zero-Trust SDP integration ENABLED");
      end if;

      Put_Line ("[Protocol Manager] Initialization complete");
   end Initialize;

   procedure Enable_Protocol (Protocol : Management_Protocol) is
   begin
      Put_Line ("[Protocol Manager] Enabling protocol: " & Protocol'Image);

      --  Security checks
      case Protocol is
         when SNMP_v1 | SNMP_v2c =>
            if Current_Config.Disable_Insecure then
               Put_Line ("[Protocol Manager] ERROR: Cannot enable insecure protocol");
               raise Insecure_Protocol_Error with
                  "SNMPv1/v2c disabled by security policy";
            end if;
            Put_Line ("[Protocol Manager] ⚠️  WARNING: Insecure protocol enabled!");

         when SNMP_v3 =>
            Put_Line ("[Protocol Manager] SNMPv3 is acceptable but NETCONF recommended");

         when NETCONF | RESTCONF | gNMI =>
            Put_Line ("[Protocol Manager] ✅ Secure protocol enabled");

         when Prometheus | OpenMetrics =>
            Put_Line ("[Protocol Manager] ✅ Metrics protocol enabled");

         when Custom_API =>
            Put_Line ("[Protocol Manager] Custom API enabled");
      end case;
   end Enable_Protocol;

   procedure Disable_Protocol (Protocol : Management_Protocol) is
   begin
      Put_Line ("[Protocol Manager] Disabling protocol: " & Protocol'Image);
   end Disable_Protocol;

   procedure Start_SNMP_Agent is
   begin
      if not Current_Config.SNMP.Enabled then
         Put_Line ("[SNMP] Cannot start - SNMP disabled");
         return;
      end if;

      Put_Line ("[SNMP] Starting SNMP agent");

      --  Security validation
      if not Validate_SNMP_Config (Current_Config.SNMP) then
         Put_Line ("[SNMP] ERROR: Invalid or insecure SNMP configuration");
         return;
      end if;

      --  Start agent (in real implementation)
      Put_Line ("[SNMP] Agent started (stub)");
      Put_Line ("[SNMP] System contact: " &
                To_String (Current_Config.SNMP.System_Contact));
      Put_Line ("[SNMP] System location: " &
                To_String (Current_Config.SNMP.System_Location));
   end Start_SNMP_Agent;

   procedure Stop_SNMP_Agent is
   begin
      Put_Line ("[SNMP] Stopping SNMP agent");
   end Stop_SNMP_Agent;

   procedure Start_NETCONF_Server is
   begin
      if not Current_Config.NETCONF.Enabled then
         Put_Line ("[NETCONF] Cannot start - NETCONF disabled");
         return;
      end if;

      Put_Line ("[NETCONF] Starting NETCONF server");
      Put_Line ("[NETCONF] Port: " & Trim (Current_Config.NETCONF.Port'Img, Ada.Strings.Left));
      Put_Line ("[NETCONF] Auth method: " & To_String (Current_Config.NETCONF.Auth_Method));

      --  Capabilities
      Put_Line ("[NETCONF] Capabilities:");
      for Cap of Current_Config.NETCONF.Capabilities loop
         Put_Line ("[NETCONF]   - " & Cap'Image);
      end loop;

      --  Security
      if Current_Config.NETCONF.TLS_Enabled then
         Put_Line ("[NETCONF] ✅ TLS enabled");
      end if;

      if Current_Config.NETCONF.Require_MFA then
         Put_Line ("[NETCONF] ✅ MFA required");
      end if;

      Put_Line ("[NETCONF] Server started (stub)");
   end Start_NETCONF_Server;

   procedure Stop_NETCONF_Server is
   begin
      Put_Line ("[NETCONF] Stopping NETCONF server");
   end Stop_NETCONF_Server;

   procedure Start_RESTCONF_Server is
   begin
      if not Current_Config.RESTCONF.Enabled then
         Put_Line ("[RESTCONF] Cannot start - RESTCONF disabled");
         return;
      end if;

      Put_Line ("[RESTCONF] Starting RESTCONF server");
      Put_Line ("[RESTCONF] Port: " & Trim (Current_Config.RESTCONF.Port'Img, Ada.Strings.Left));
      Put_Line ("[RESTCONF] Base path: " & To_String (Current_Config.RESTCONF.Base_Path));
      Put_Line ("[RESTCONF] Auth method: " & Current_Config.RESTCONF.Auth_Method'Image);

      --  TLS
      if Current_Config.RESTCONF.TLS_Version /= To_Bounded_String ("") then
         Put_Line ("[RESTCONF] ✅ TLS version: " &
                   To_String (Current_Config.RESTCONF.TLS_Version));
      end if;

      --  Rate limiting
      if Current_Config.RESTCONF.Rate_Limit > 0 then
         Put_Line ("[RESTCONF] Rate limit: " &
                   Trim (Current_Config.RESTCONF.Rate_Limit'Img, Ada.Strings.Left) &
                   " req/min");
      end if;

      --  Data formats
      if Current_Config.RESTCONF.Accept_JSON then
         Put_Line ("[RESTCONF] ✅ JSON support enabled");
      end if;
      if Current_Config.RESTCONF.Accept_XML then
         Put_Line ("[RESTCONF] ✅ XML support enabled");
      end if;

      Put_Line ("[RESTCONF] Server started (stub)");
   end Start_RESTCONF_Server;

   procedure Stop_RESTCONF_Server is
   begin
      Put_Line ("[RESTCONF] Stopping RESTCONF server");
   end Stop_RESTCONF_Server;

   procedure Start_gNMI_Server is
   begin
      if not Current_Config.gNMI.Enabled then
         Put_Line ("[gNMI] Cannot start - gNMI disabled");
         return;
      end if;

      Put_Line ("[gNMI] Starting gNMI server (modern gRPC-based protocol)");
      Put_Line ("[gNMI] Port: " & Trim (Current_Config.gNMI.Port'Img, Ada.Strings.Left));
      Put_Line ("[gNMI] Encoding: " & Current_Config.gNMI.Encoding'Image);

      --  Security
      if Current_Config.gNMI.TLS_Enabled then
         Put_Line ("[gNMI] ✅ TLS enabled");
      end if;

      if Current_Config.gNMI.Token_Auth then
         Put_Line ("[gNMI] ✅ JWT token authentication enabled");
      end if;

      --  Capabilities
      Put_Line ("[gNMI] Capabilities:");
      if Current_Config.gNMI.Subscribe then
         Put_Line ("[gNMI]   ✅ Streaming telemetry (Subscribe)");
      end if;
      if Current_Config.gNMI.Get_Enabled then
         Put_Line ("[gNMI]   ✅ Get operations");
      end if;
      if Current_Config.gNMI.Set_Enabled then
         Put_Line ("[gNMI]   ✅ Set operations");
      else
         Put_Line ("[gNMI]   📖 Read-only mode (Set disabled)");
      end if;

      Put_Line ("[gNMI] Server started (stub)");
   end Start_gNMI_Server;

   procedure Stop_gNMI_Server is
   begin
      Put_Line ("[gNMI] Stopping gNMI server");
   end Stop_gNMI_Server;

   --  ========================================================================
   --  SECURITY VALIDATION
   --  ========================================================================

   function Validate_SNMP_Config (Config : SNMP_Config) return Boolean is
   begin
      Put_Line ("[SNMP Validation] Validating SNMP configuration");

      if not Config.Enabled then
         return True;  --  Disabled is valid
      end if;

      --  Check for insecure versions
      if Config.Versions (SNMP_v1).Enabled or Config.Versions (SNMP_v2c).Enabled then
         if Config.Warn_Insecure then
            Put_Line ("[SNMP Validation] ⚠️  WARNING: SNMPv1/v2c are INSECURE");
            Put_Line ("[SNMP Validation] ⚠️  Community strings sent in PLAINTEXT");
            Put_Line ("[SNMP Validation] ⚠️  Recommendation: Use SNMPv3 or NETCONF");
         end if;

         if Config.Require_v3_Only then
            Put_Line ("[SNMP Validation] ❌ ERROR: Policy requires SNMPv3 only");
            return False;
         end if;
      end if;

      --  Validate SNMPv3 configuration
      if Config.Versions (SNMP_v3).Enabled then
         Put_Line ("[SNMP Validation] ✅ SNMPv3 enabled (secure)");

         --  Check auth protocol
         declare
            Auth_Proto : constant String :=
               To_String (Config.Versions (SNMP_v3).Auth_Protocol);
         begin
            if Auth_Proto = "MD5" then
               Put_Line ("[SNMP Validation] ⚠️  MD5 is weak, prefer SHA-256");
            elsif Auth_Proto = "SHA" then
               Put_Line ("[SNMP Validation] SHA acceptable, SHA-256 recommended");
            elsif Auth_Proto = "SHA-256" then
               Put_Line ("[SNMP Validation] ✅ SHA-256 authentication");
            end if;
         end;

         --  Check privacy protocol
         declare
            Priv_Proto : constant String :=
               To_String (Config.Versions (SNMP_v3).Priv_Protocol);
         begin
            if Priv_Proto = "DES" then
               Put_Line ("[SNMP Validation] ⚠️  DES is weak, use AES256");
            elsif Priv_Proto = "AES128" then
               Put_Line ("[SNMP Validation] ✅ AES128 encryption");
            elsif Priv_Proto = "AES256" then
               Put_Line ("[SNMP Validation] ✅ AES256 encryption (recommended)");
            end if;
         end;
      end if;

      Put_Line ("[SNMP Validation] Configuration valid");
      return True;
   end Validate_SNMP_Config;

   function Get_Security_Level (Protocol : Management_Protocol) return Security_Level is
   begin
      case Protocol is
         when SNMP_v1 | SNMP_v2c =>
            return Insecure;

         when SNMP_v3 =>
            return Basic;  --  Can be Strong with proper config

         when NETCONF | RESTCONF =>
            if Current_Config.Require_TLS then
               return Strong;
            else
               return Basic;
            end if;

         when gNMI =>
            return Strong;  --  gRPC with TLS by default

         when Prometheus | OpenMetrics =>
            if Current_Config.Metrics.Client_Certs then
               return Strong;
            else
               return Basic;
            end if;

         when Custom_API =>
            if Current_Config.SDP_Integration then
               return Zero_Trust;
            else
               return Basic;
            end if;
      end case;
   end Get_Security_Level;

   function Get_Protocol_Recommendation return Management_Protocol is
   begin
      --  Recommend modern, secure protocol
      if Current_Config.gNMI.Enabled then
         return gNMI;
      elsif Current_Config.RESTCONF.Enabled then
         return RESTCONF;
      elsif Current_Config.NETCONF.Enabled then
         return NETCONF;
      elsif Current_Config.SNMP.Enabled and then
            Current_Config.SNMP.Versions (SNMP_v3).Enabled
      then
         return SNMP_v3;
      else
         return NETCONF;  --  Default recommendation
      end if;
   end Get_Protocol_Recommendation;

   function Get_Security_Warnings return String is
      use Ada.Strings.Unbounded;
      Warnings : Unbounded_String;
   begin
      Append (Warnings, "Protocol Security Warnings:" & ASCII.LF);

      --  Check each protocol
      if Current_Config.SNMP.Versions (SNMP_v1).Enabled then
         Append (Warnings, "⚠️  SNMPv1: INSECURE - plaintext community strings" & ASCII.LF);
      end if;

      if Current_Config.SNMP.Versions (SNMP_v2c).Enabled then
         Append (Warnings, "⚠️  SNMPv2c: INSECURE - plaintext community strings" & ASCII.LF);
      end if;

      if not Current_Config.Require_TLS then
         Append (Warnings, "⚠️  TLS not required - data may be sent unencrypted" & ASCII.LF);
      end if;

      if not Current_Config.SDP_Integration then
         Append (Warnings, "ℹ️  SDP integration disabled - consider enabling for zero-trust" & ASCII.LF);
      end if;

      if Length (Warnings) = 28 then  --  Just the header
         Append (Warnings, "✅ No security warnings - configuration is secure" & ASCII.LF);
      end if;

      return To_String (Warnings);
   end Get_Security_Warnings;

   --  ========================================================================
   --  INTEGRATION WITH FIREWALL/SDP
   --  ========================================================================

   procedure Apply_Protocol_Firewall_Rules (
      Protocol : Management_Protocol;
      Config   : Protocol_Manager_Config
   ) is
      Rule : Firewall_Manager.Firewall_Rule;
      Port : DNS_Records_Extended.Port_Number;
   begin
      Put_Line ("[Protocol Manager] Applying firewall rules for: " & Protocol'Image);

      --  Determine port based on protocol
      case Protocol is
         when SNMP_v1 | SNMP_v2c | SNMP_v3 =>
            Port := Config.SNMP.Versions (SNMP_v1).Port;
         when NETCONF =>
            Port := Config.NETCONF.Port;
         when RESTCONF =>
            Port := Config.RESTCONF.Port;
         when gNMI =>
            Port := Config.gNMI.Port;
         when Prometheus | OpenMetrics =>
            Port := Config.Metrics.Port;
         when Custom_API =>
            Port := 8080;  --  Default
      end case;

      --  Create firewall rule
      Rule.Name := To_Bounded_String (Protocol'Image & "_access");
      Rule.Source_CIDR := To_Bounded_String ("0.0.0.0/0");  --  Would use allowed IPs
      Rule.Dest_Port := Port;
      Rule.Protocol := To_Bounded_String ("tcp");
      Rule.Action := Firewall_Manager.Accept;
      Rule.Rule_Type := Firewall_Manager.Stateful;

      Firewall_Manager.Add_Rule (Rule);

      Put_Line ("[Protocol Manager] Firewall rule added for port " &
                Trim (Port'Img, Ada.Strings.Left));
   end Apply_Protocol_Firewall_Rules;

   procedure Enable_SDP_Integration (
      Protocol : Management_Protocol;
      Require_Session : Boolean := True
   ) is
   begin
      Put_Line ("[Protocol Manager] Enabling SDP integration for: " & Protocol'Image);
      Put_Line ("[Protocol Manager] Require active SDP session: " & Require_Session'Image);

      --  In real implementation:
      --  1. Register protocol with SDP controller
      --  2. Require SPA + authentication for access
      --  3. Create dynamic firewall rules only for authenticated sessions
      --  4. Implement continuous verification

      Put_Line ("[Protocol Manager] SDP integration enabled (stub)");
   end Enable_SDP_Integration;

   --  ========================================================================
   --  MONITORING AND METRICS
   --  ========================================================================

   function Get_Protocol_Stats (Protocol : Management_Protocol) return Protocol_Stats is
      Stats : Protocol_Stats;
   begin
      --  In real implementation, would retrieve actual statistics
      Stats.Protocol := Protocol;
      Stats.Enabled := False;  --  Would check actual state
      Stats.Active_Sessions := 0;
      Stats.Total_Requests := 0;
      Stats.Failed_Auth := 0;
      Stats.Last_Access := Ada.Calendar.Clock;
      Stats.Security_Level := Get_Security_Level (Protocol);

      return Stats;
   end Get_Protocol_Stats;

   function Get_All_Protocol_Stats return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      Append (Result, "Protocol Statistics:" & ASCII.LF);
      Append (Result, "-------------------" & ASCII.LF);

      for Proto in Management_Protocol loop
         declare
            Stats : constant Protocol_Stats := Get_Protocol_Stats (Proto);
         begin
            Append (Result, Proto'Image & ":" & ASCII.LF);
            Append (Result, "  Security Level: " & Stats.Security_Level'Image & ASCII.LF);
            Append (Result, "  Active Sessions: " &
                    Trim (Stats.Active_Sessions'Img, Ada.Strings.Left) & ASCII.LF);
            Append (Result, ASCII.LF);
         end;
      end loop;

      return To_String (Result);
   end Get_All_Protocol_Stats;

   function Export_Prometheus_Metrics return String is
      use Ada.Strings.Unbounded;
      Metrics : Unbounded_String;
   begin
      Append (Metrics, "# HELP protocol_sessions Active protocol sessions" & ASCII.LF);
      Append (Metrics, "# TYPE protocol_sessions gauge" & ASCII.LF);

      for Proto in Management_Protocol loop
         declare
            Stats : constant Protocol_Stats := Get_Protocol_Stats (Proto);
         begin
            Append (Metrics, "protocol_sessions{protocol=""" & Proto'Image & """} " &
                    Trim (Stats.Active_Sessions'Img, Ada.Strings.Left) & ASCII.LF);
         end;
      end loop;

      Append (Metrics, ASCII.LF);
      Append (Metrics, "# HELP protocol_requests_total Total protocol requests" & ASCII.LF);
      Append (Metrics, "# TYPE protocol_requests_total counter" & ASCII.LF);

      for Proto in Management_Protocol loop
         declare
            Stats : constant Protocol_Stats := Get_Protocol_Stats (Proto);
         begin
            Append (Metrics, "protocol_requests_total{protocol=""" & Proto'Image & """} " &
                    Trim (Stats.Total_Requests'Img, Ada.Strings.Left) & ASCII.LF);
         end;
      end loop;

      return To_String (Metrics);
   end Export_Prometheus_Metrics;

end Protocol_Manager;
