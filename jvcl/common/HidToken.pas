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
{ (robert_marquardt@gmx.de)                                        }
{                                                                  }
{ Portions created by Robert Marquardt are                         }
{ Copyright (c) 1999, 2000 Robert Marquardt.                       }
{                                                                  }
{ Contributor(s): Marcel van Brakel (brakelm@bart.nl)              }
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

(*$HPPEMIT ''*)
(*$HPPEMIT '#include "hidtoken.h"'*)
(*$HPPEMIT ''*)

const
  HIDP_ITEM_LONG        = $FE;
  {$EXTERNALSYM HIDP_ITEM_LONG}
  HIDP_ITEM_LENGTH_DATA = $03;
  {$EXTERNALSYM HIDP_ITEM_LENGTH_DATA}

  // Main Items
  // Only main items with one byte data (bSize = 1) are supported.
  HIDP_MAIN_INPUT_1         = $81;
  {$EXTERNALSYM HIDP_MAIN_INPUT_1}
  HIDP_MAIN_INPUT_2         = $82;
  {$EXTERNALSYM HIDP_MAIN_INPUT_2}
  HIDP_MAIN_OUTPUT_1        = $91;
  {$EXTERNALSYM HIDP_MAIN_OUTPUT_1}
  HIDP_MAIN_OUTPUT_2        = $92;
  {$EXTERNALSYM HIDP_MAIN_OUTPUT_2}
  HIDP_MAIN_FEATURE_1       = $B1;
  {$EXTERNALSYM HIDP_MAIN_FEATURE_1}
  HIDP_MAIN_FEATURE_2       = $B2;
  {$EXTERNALSYM HIDP_MAIN_FEATURE_2}
  HIDP_MAIN_COLLECTION      = $A1;
  {$EXTERNALSYM HIDP_MAIN_COLLECTION}
  HIDP_MAIN_ENDCOLLECTION   = $C0;
  {$EXTERNALSYM HIDP_MAIN_ENDCOLLECTION}
  HIDP_MAIN_COLLECTION_LINK = $00;
  {$EXTERNALSYM HIDP_MAIN_COLLECTION_LINK}
  HIDP_MAIN_COLLECTION_APP  = $01;
  {$EXTERNALSYM HIDP_MAIN_COLLECTION_APP}

  // Global Items
  HIDP_GLOBAL_USAGE_PAGE_1   = $05;  // UsagePage of 1 byte
  {$EXTERNALSYM HIDP_GLOBAL_USAGE_PAGE_1}
  HIDP_GLOBAL_USAGE_PAGE_2   = $06;  // UsagePage of 2 bytes
  {$EXTERNALSYM HIDP_GLOBAL_USAGE_PAGE_2}
  HIDP_GLOBAL_USAGE_PAGE_4   = $07;  // UsagePage of 4 bytes
  {$EXTERNALSYM HIDP_GLOBAL_USAGE_PAGE_4}
  HIDP_GLOBAL_LOG_MIN_1      = $15;  // minimum value of size 1 byte.
  {$EXTERNALSYM HIDP_GLOBAL_LOG_MIN_1}
  HIDP_GLOBAL_LOG_MIN_2      = $16;  // minimum value of size 2 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_LOG_MIN_2}
  HIDP_GLOBAL_LOG_MIN_4      = $17;  // minimum value of size 4 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_LOG_MIN_4}
  HIDP_GLOBAL_LOG_MAX_1      = $25;  // maximum of size 1 byte.
  {$EXTERNALSYM HIDP_GLOBAL_LOG_MAX_1}
  HIDP_GLOBAL_LOG_MAX_2      = $26;  // maximum of size 2 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_LOG_MAX_2}
  HIDP_GLOBAL_LOG_MAX_4      = $27;  // maximum of size 4 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_LOG_MAX_4}

  HIDP_GLOBAL_PHY_MIN_1      = $35;  // minimum value of size 1 byte.
  {$EXTERNALSYM HIDP_GLOBAL_PHY_MIN_1}
  HIDP_GLOBAL_PHY_MIN_2      = $36;  // minimum value of size 2 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_PHY_MIN_2}
  HIDP_GLOBAL_PHY_MIN_4      = $37;  // minimum value of size 4 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_PHY_MIN_4}
  HIDP_GLOBAL_PHY_MAX_1      = $45;  // maximum of size 1 byte.
  {$EXTERNALSYM HIDP_GLOBAL_PHY_MAX_1}
  HIDP_GLOBAL_PHY_MAX_2      = $46;  // maximum of size 2 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_PHY_MAX_2}
  HIDP_GLOBAL_PHY_MAX_4      = $47;  // maximum of size 4 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_PHY_MAX_4}

  HIDP_GLOBAL_UNIT_EXP_1     = $55;  // Exponent of size 1 byte.
  {$EXTERNALSYM HIDP_GLOBAL_UNIT_EXP_1}
  HIDP_GLOBAL_UNIT_EXP_2     = $56;  // Exponent of size 2 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_UNIT_EXP_2}
  HIDP_GLOBAL_UNIT_EXP_4     = $57;  // Exponent of size 4 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_UNIT_EXP_4}
  HIDP_GLOBAL_UNIT_1         = $65;  // UNIT of size 1 byte.
  {$EXTERNALSYM HIDP_GLOBAL_UNIT_1}
  HIDP_GLOBAL_UNIT_2         = $66;  // UNIT of size 2 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_UNIT_2}
  HIDP_GLOBAL_UNIT_4         = $67;  // UNIT of size 4 bytes.
  {$EXTERNALSYM HIDP_GLOBAL_UNIT_4}

  HIDP_GLOBAL_REPORT_SIZE    = $75;  // Report size in bits
  {$EXTERNALSYM HIDP_GLOBAL_REPORT_SIZE}
  HIDP_GLOBAL_REPORT_ID      = $85;  // ID only size 1 byte supported
  {$EXTERNALSYM HIDP_GLOBAL_REPORT_ID}
  HIDP_GLOBAL_REPORT_COUNT_1 = $95;  // Number of data fields 1 byte
  {$EXTERNALSYM HIDP_GLOBAL_REPORT_COUNT_1}
  HIDP_GLOBAL_REPORT_COUNT_2 = $96;  // Number of data fields 2 bytes
  {$EXTERNALSYM HIDP_GLOBAL_REPORT_COUNT_2}
  HIDP_GLOBAL_PUSH           = $A4;  // The dreaded PUSH command
  {$EXTERNALSYM HIDP_GLOBAL_PUSH}
  HIDP_GLOBAL_POP            = $B4;  // And the dreaded POP command
  {$EXTERNALSYM HIDP_GLOBAL_POP}

  // Local Items
  HIDP_LOCAL_USAGE_1         = $09;
  {$EXTERNALSYM HIDP_LOCAL_USAGE_1}
  HIDP_LOCAL_USAGE_2         = $0A;
  {$EXTERNALSYM HIDP_LOCAL_USAGE_2}
  HIDP_LOCAL_USAGE_4         = $0B;
  {$EXTERNALSYM HIDP_LOCAL_USAGE_4}
  HIDP_LOCAL_USAGE_MIN_1     = $19;
  {$EXTERNALSYM HIDP_LOCAL_USAGE_MIN_1}
  HIDP_LOCAL_USAGE_MIN_2     = $1A;
  {$EXTERNALSYM HIDP_LOCAL_USAGE_MIN_2}
  HIDP_LOCAL_USAGE_MIN_4     = $1B;
  {$EXTERNALSYM HIDP_LOCAL_USAGE_MIN_4}
  HIDP_LOCAL_USAGE_MAX_1     = $29;
  {$EXTERNALSYM HIDP_LOCAL_USAGE_MAX_1}
  HIDP_LOCAL_USAGE_MAX_2     = $2A;
  {$EXTERNALSYM HIDP_LOCAL_USAGE_MAX_2}
  HIDP_LOCAL_USAGE_MAX_4     = $2B;
  {$EXTERNALSYM HIDP_LOCAL_USAGE_MAX_4}
  HIDP_LOCAL_DESIG_INDEX     = $39;  // Designators of byte size supported
  {$EXTERNALSYM HIDP_LOCAL_DESIG_INDEX}
  HIDP_LOCAL_DESIG_MIN       = $49;
  {$EXTERNALSYM HIDP_LOCAL_DESIG_MIN}
  HIDP_LOCAL_DESIG_MAX       = $59;
  {$EXTERNALSYM HIDP_LOCAL_DESIG_MAX}
  HIDP_LOCAL_STRING_INDEX    = $79;  // String indices of size byte supported
  {$EXTERNALSYM HIDP_LOCAL_STRING_INDEX}
  HIDP_LOCAL_STRING_MIN      = $89;
  {$EXTERNALSYM HIDP_LOCAL_STRING_MIN}
  HIDP_LOCAL_STRING_MAX      = $99;
  {$EXTERNALSYM HIDP_LOCAL_STRING_MAX}
  HIDP_LOCAL_DELIMITER       = $A9;
  {$EXTERNALSYM HIDP_LOCAL_DELIMITER}

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
