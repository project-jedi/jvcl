{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  File:       dxerr9.h                                                      *}
{*                                                                            *}
{*  Content:    DirectX Error Library Include File                            *}
{*                                                                            *}
{*  DirectX 9.0 Delphi adaptation by Alexey Barkovoy                          *}
{*  E-Mail: clootie@reactor.ru                                                *}
{*                                                                            *}
{*  Modified: 26-Jan-2003                                                     *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*     http://clootie.narod.ru/delphi                                         *}
{*                                                                            *}
{******************************************************************************)
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

unit dxerr9;

interface

{$HPPEMIT '#include "dxerr9.h"'}

uses
  Windows;

(*==========================================================================;
 *
 *
 *  File:   dxerr9.h
 *  Content:    DirectX Error Library Include File
 *
 ****************************************************************************)

const
  //////////// DLL export definitions ///////////////////////////////////////
  dxerr9dll = 'dxerr9ab.dll';
  {$EXTERNALSYM dxerr9dll}

//
//  DXGetErrorString9
//
//  Desc:  Converts a DirectX 9 or earlier HRESULT to a string
//
//  Args:  HRESULT hr   Can be any error code from
//                      D3D9 D3DX9 D3D8 D3DX8 DDRAW DPLAY8 DMUSIC DSOUND DINPUT DSHOW
//
//  Return: Converted string
//

function DXGetErrorString9A(hr: HRESULT): PAnsiChar; stdcall; external dxerr9dll;
{$EXTERNALSYM DXGetErrorString9A}
function DXGetErrorString9W(hr: HRESULT): PWideChar; stdcall; external dxerr9dll;
{$EXTERNALSYM DXGetErrorString9W}

function DXGetErrorString9(hr: HRESULT): PChar;  stdcall; external dxerr9dll
  name 'DXGetErrorString9A';
{$EXTERNALSYM DXGetErrorString9}


//
//  DXGetErrorDescription9
//
//  Desc:  Returns a string description of a DirectX 9 or earlier HRESULT
//
//  Args:  HRESULT hr   Can be any error code from
//                      D3D9 D3DX9 D3D8 D3DX8 DDRAW DPLAY8 DMUSIC DSOUND DINPUT DSHOW
//
//  Return: String description
//
function DXGetErrorDescription9A(hr: HRESULT): PAnsiChar; stdcall; external dxerr9dll;
{$EXTERNALSYM DXGetErrorDescription9A}
function DXGetErrorDescription9W(hr: HRESULT): PWideChar; stdcall; external dxerr9dll;
{$EXTERNALSYM DXGetErrorDescription9W}

function DXGetErrorDescription9(hr: HRESULT): PChar;  stdcall; external dxerr9dll
  name 'DXGetErrorDescription9A';
{$EXTERNALSYM DXGetErrorDescription9}


//
//  DXTrace
//
//  Desc:  Outputs a formatted error message to the debug stream
//
//  Args:  CHAR* strFile   The current file, typically passed in using the
//                         __FILE__ macro.
//         DWORD dwLine    The current line number, typically passed in using the
//                         __LINE__ macro.
//         HRESULT hr      An HRESULT that will be traced to the debug stream.
//         CHAR* strMsg    A string that will be traced to the debug stream (may be NULL)
//         BOOL bPopMsgBox If TRUE, then a message box will popup also containing the passed info.
//
//  Return: The hr that was passed in.
//

function DXTraceA(strFile: PAnsiChar; dwLine: DWORD; hr: HRESULT; strMsg: PAnsiChar; bPopMsgBox: BOOL = FALSE): HRESULT; stdcall; external dxerr9dll;
{$EXTERNALSYM DXTraceA}
function DXTraceW(strFile: PWideChar; dwLine: DWORD; hr: HRESULT; strMsg: PWideChar; bPopMsgBox: BOOL = FALSE): HRESULT; stdcall; external dxerr9dll;
{$EXTERNALSYM DXTraceW}

function DXTrace(strFile: PChar; dwLine: DWORD; hr: HRESULT; strMsg: PChar; bPopMsgBox: BOOL = FALSE): HRESULT; stdcall; external dxerr9dll
  name 'DXTraceA';
{$EXTERNALSYM DXTrace}

//
// Helper macros
//
(*
#if defined(DEBUG) | defined(_DEBUG)
#define DXTRACE_MSG(str)              DXTrace( __FILE__, (DWORD)__LINE__, 0, str, FALSE )
#define DXTRACE_ERR(str,hr)           DXTrace( __FILE__, (DWORD)__LINE__, hr, str, FALSE )
#define DXTRACE_ERR_MSGBOX(str,hr)    DXTrace( __FILE__, (DWORD)__LINE__, hr, str, TRUE )
#else
#define DXTRACE_MSG(str)              (0L)
#define DXTRACE_ERR(str,hr)           (hr)
#define DXTRACE_ERR_MSGBOX(str,hr)    (hr)
#endif
*)

implementation

end.

