{******************************************************************}
{                                                                  }
{       Borland Delphi Runtime Library                             }
{       Public Definitions of HID USAGES                           }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (c) 1996-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: hidtoken.h, released March 1999.           }
{ The original Pascal code is: HidToken.pas, released 31 Jan 2000. }
{ The initial developer of the Pascal code is Robert Marquardt     }
{ (robert_marquardt att gmx dott de)                               }
{                                                                  }
{ Portions created by Robert Marquardt are                         }
{ Copyright (c) 1999, 2000 Robert Marquardt.                       }
{                                                                  }
{ Contributor(s): Marcel van Brakel (brakelm att bart dott nl)     }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit HidToken;

{$WEAKPACKAGEUNIT}

interface

const
  HIDP_ITEM_LONG        = $FE;
  HIDP_ITEM_LENGTH_DATA = $03;

  // Main Items
  // Only main items with one byte data (bSize = 1) are supported.
  HIDP_MAIN_INPUT_1         = $81;
  HIDP_MAIN_INPUT_2         = $82;
  HIDP_MAIN_OUTPUT_1        = $91;
  HIDP_MAIN_OUTPUT_2        = $92;
  HIDP_MAIN_FEATURE_1       = $B1;
  HIDP_MAIN_FEATURE_2       = $B2;
  HIDP_MAIN_COLLECTION      = $A1;
  HIDP_MAIN_ENDCOLLECTION   = $C0;
  HIDP_MAIN_COLLECTION_LINK = $00;
  HIDP_MAIN_COLLECTION_APP  = $01;

  // Global Items
  HIDP_GLOBAL_USAGE_PAGE_1   = $05;  // UsagePage of 1 byte
  HIDP_GLOBAL_USAGE_PAGE_2   = $06;  // UsagePage of 2 bytes
  HIDP_GLOBAL_USAGE_PAGE_4   = $07;  // UsagePage of 4 bytes
  HIDP_GLOBAL_LOG_MIN_1      = $15;  // minimum value of size 1 byte.
  HIDP_GLOBAL_LOG_MIN_2      = $16;  // minimum value of size 2 bytes.
  HIDP_GLOBAL_LOG_MIN_4      = $17;  // minimum value of size 4 bytes.
  HIDP_GLOBAL_LOG_MAX_1      = $25;  // maximum of size 1 byte.
  HIDP_GLOBAL_LOG_MAX_2      = $26;  // maximum of size 2 bytes.
  HIDP_GLOBAL_LOG_MAX_4      = $27;  // maximum of size 4 bytes.

  HIDP_GLOBAL_PHY_MIN_1      = $35;  // minimum value of size 1 byte.
  HIDP_GLOBAL_PHY_MIN_2      = $36;  // minimum value of size 2 bytes.
  HIDP_GLOBAL_PHY_MIN_4      = $37;  // minimum value of size 4 bytes.
  HIDP_GLOBAL_PHY_MAX_1      = $45;  // maximum of size 1 byte.
  HIDP_GLOBAL_PHY_MAX_2      = $46;  // maximum of size 2 bytes.
  HIDP_GLOBAL_PHY_MAX_4      = $47;  // maximum of size 4 bytes.

  HIDP_GLOBAL_UNIT_EXP_1     = $55;  // Exponent of size 1 byte.
  HIDP_GLOBAL_UNIT_EXP_2     = $56;  // Exponent of size 2 bytes.
  HIDP_GLOBAL_UNIT_EXP_4     = $57;  // Exponent of size 4 bytes.
  HIDP_GLOBAL_UNIT_1         = $65;  // UNIT of size 1 byte.
  HIDP_GLOBAL_UNIT_2         = $66;  // UNIT of size 2 bytes.
  HIDP_GLOBAL_UNIT_4         = $67;  // UNIT of size 4 bytes.

  HIDP_GLOBAL_REPORT_SIZE    = $75;  // Report size in bits
  HIDP_GLOBAL_REPORT_ID      = $85;  // ID only size 1 byte supported
  HIDP_GLOBAL_REPORT_COUNT_1 = $95;  // Number of data fields 1 byte
  HIDP_GLOBAL_REPORT_COUNT_2 = $96;  // Number of data fields 2 bytes
  HIDP_GLOBAL_PUSH           = $A4;  // The dreaded PUSH command
  HIDP_GLOBAL_POP            = $B4;  // And the dreaded POP command

  // Local Items
  HIDP_LOCAL_USAGE_1         = $09;
  HIDP_LOCAL_USAGE_2         = $0A;
  HIDP_LOCAL_USAGE_4         = $0B;
  HIDP_LOCAL_USAGE_MIN_1     = $19;
  HIDP_LOCAL_USAGE_MIN_2     = $1A;
  HIDP_LOCAL_USAGE_MIN_4     = $1B;
  HIDP_LOCAL_USAGE_MAX_1     = $29;
  HIDP_LOCAL_USAGE_MAX_2     = $2A;
  HIDP_LOCAL_USAGE_MAX_4     = $2B;
  HIDP_LOCAL_DESIG_INDEX     = $39;  // Designators of byte size supported
  HIDP_LOCAL_DESIG_MIN       = $49;
  HIDP_LOCAL_DESIG_MAX       = $59;
  HIDP_LOCAL_STRING_INDEX    = $79;  // String indices of size byte supported
  HIDP_LOCAL_STRING_MIN      = $89;
  HIDP_LOCAL_STRING_MAX      = $99;
  HIDP_LOCAL_DELIMITER       = $A9;

function HidPIsMain        (x: Integer): Boolean;
function HidPIsMainItem    (x: Integer): Boolean;
function HidPIsGlobalItem  (x: Integer): Boolean;
function HidPIsLocalItem   (x: Integer): Boolean;
function HidPIsReservedItem(x: Integer): Boolean;

implementation

function HidPIsMain(x: Integer): Boolean;
begin
  Result := ((x and $0C) = 0);
end;

function HidPIsMainItem(x: Integer): Boolean;
begin
  Result := ((x and $0C) = 0);
end;

function HidPIsGlobalItem(x: Integer): Boolean;
begin
  Result := ((x and $0C) = $04);
end;

function HidPIsLocalItem(x: Integer): Boolean;
begin
  Result := ((x and $0C) = $08);
end;

function HidPIsReservedItem(x: Integer): Boolean;
begin
  Result := ((x and $0C) = $0C);
end;

end.
