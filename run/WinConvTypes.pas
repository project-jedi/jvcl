{******************************************************************}
{                                                                  }
{       Borland Delphi Runtime Library                             }
{       Config Manager API interface unit                          }
{                                                                  }
{ The original Pascal code is: WinConvTypes.pas,                   }
{ released 5 Nov 2004.                                             }
{ The initial developer of the Pascal code is Robert Marquardt     }
{ (robert_marquardt att gmx dott de)                               }
{                                                                  }
{ Portions created by Robert Marquardt are                         }
{ Copyright (C) 2004 Robert Marquardt.                             }
{                                                                  }
{ Contributor(s):                                                  }
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
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit WinConvTypes;

{$I windowsversion.inc}
{$I jvcl.inc}

interface

{$WEAKPACKAGEUNIT ON}

uses
  Windows;

type
  PHICON = ^HICON;

  {$IFDEF UNICODE}
  PCTSTR  = PWideChar;
  PTSTR   = PWideChar;
  TCHAR   = WideChar;
  {$ELSE}
  PCTSTR  = PAnsiChar;
  PTSTR   = PAnsiChar;
  TCHAR   = Char;
  {$ENDIF UNICODE}
  {$EXTERNALSYM PCTSTR}
  {$EXTERNALSYM PTSTR}
  {$EXTERNALSYM TCHAR}
  PPWSTR = ^PWideChar;
  PPASTR = ^PAnsiChar;
  PPSTR  = ^PTSTR;

{$IFNDEF RTL185_UP}
type
  // WARNING: Those types are defined as DWORD for simplicity under old versions
  // of Delphi but they really should be defined as pointers to their base types
  ULONG_PTR = DWORD;
  {$EXTERNALSYM ULONG_PTR}
  DWORD_PTR = DWORD;
  {$EXTERNALSYM DWORD_PTR}
  UINT_PTR  = DWORD;
  {$EXTERNALSYM UINT_PTR}
{$ENDIF ~RTL185_UP}

{$IFNDEF RTL220_UP}
type
  ULONG32 = ULONG;
  {$EXTERNALSYM ULONG32}
  ULONG64 = Int64;   // (rom) no unsigned Int64 available in Delphi
  {$EXTERNALSYM ULONG64}
{$ENDIF ~RTL220_UP}

implementation

end.
