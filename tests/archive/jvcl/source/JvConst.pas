{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvConst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}


unit JvConst;

interface


uses Controls;

const
  RX_VERSION = $0002004B;  { 2.75 }

const
{ Command message for JvSpeedbar editor }
  CM_SPEEDBARCHANGED = CM_BASE + 80;
{ Command message for TJvxSpeedButton }
  CM_RXBUTTONPRESSED = CM_BASE + 81;
{ Command messages for TJvWindowHook }
  CM_RECREATEWINDOW  = CM_BASE + 82;
  CM_DESTROYHOOK     = CM_BASE + 83;
{ Notify message for TJvxTrayIcon }
  CM_TRAYICON        = CM_BASE + 84;

const
  crHand     = TCursor(14000);
  crDragHand = TCursor(14001);

const
{ TBitmap.GetTransparentColor from GRAPHICS.PAS use this value }
  PaletteMask = $02000000;

{$IFDEF VER90}
const
  SDelphiKey = 'Software\Borland\Delphi\2.0';
{$ENDIF}

{$IFDEF VER93}
const
  SDelphiKey = 'Software\Borland\C++Builder\1.0';
{$ENDIF}

{$IFDEF VER100}
const
  SDelphiKey = 'Software\Borland\Delphi\3.0';
{$ENDIF}

{$IFDEF VER110}
const
  SDelphiKey = 'Software\Borland\C++Builder\3.0';
{$ENDIF}

{$IFDEF VER120}
const
  SDelphiKey = 'Software\Borland\Delphi\4.0';
{$ENDIF}

{$IFDEF VER125}
const
  SDelphiKey = 'Software\Borland\C++Builder\4.0';
{$ENDIF}

{$IFDEF VER130}
const
  SDelphiKey = 'Software\Borland\Delphi\5.0';
{$ENDIF}

{$IFDEF VER140}
const
  SDelphiKey = 'Software\Borland\Delphi\6.0';
{$ENDIF}

{$IFDEF VER150}
const
  SDelphiKey = 'Software\Borland\Delphi\7.0';
{$ENDIF}

implementation

uses {$IFDEF WIN32} Windows, {$ELSE} WinProcs, {$ENDIF} Forms;

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.R16}
{$ENDIF}

initialization
  Screen.Cursors[crHand] := LoadCursor(hInstance, 'JV_HANDCUR');
  Screen.Cursors[crDragHand] := LoadCursor(hInstance, 'JV_DRAGCUR');
end.
