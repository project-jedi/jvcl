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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvConsts;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Forms, Controls, Graphics, Windows;

{$IFDEF VisualCLX}
const
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($F0CAA6);
  clCream = TColor($F0FBFF);
  clMedGray = TColor($A4A0A0);
{$ENDIF VisualCLX}

{$IFDEF VCL}
  { (asn) added to simplify VCL code sharing = less IFDEF's }
const
  { clx destinguishes between TFormBorderStyle & TBorderStyle }
  fbsDialog = bsDialog;
  fbsSingle = bsSingle;
  fbsNone = bsNone;
  fbsSizeable = bsSizeable;
  fbsToolWindow = bsToolWindow;
  fbsSizeToolWin = bsSizeToolWin;
{$ENDIF VCL}

  { JvEditor }
  JvEditorCompletionChars = #8 + '0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';

  { various units }
  DigitSymbols = ['0'..'9'];
  SignSymbols = ['+', '-'];
  IdentifierUppercaseLetters = ['A'..'Z'];
  IdentifierLowercaseLetters = ['a'..'z'];
  HexadecimalUppercaseLetters = ['A'..'F'];
  HexadecimalLowercaseLetters = ['a'..'f'];
  IdentifierLetters = IdentifierUppercaseLetters + IdentifierLowercaseLetters;
  IdentifierFirstSymbols = ['_'] + IdentifierLetters;
  IdentifierSymbols = IdentifierFirstSymbols + DigitSymbols;
  HexadecimalSymbols = DigitSymbols + HexadecimalUppercaseLetters + HexadecimalLowercaseLetters;

  { Menu Designer }
  { (rom) disabled unused
  SMDMenuDesigner       = 'Menu &Designer';
  SMDInsertItem         = '&Insert';
  SMDDeleteItem         = '&Delete';
  SMDCreateSubmenuItem  = 'Create &SubMenu';
  }

  { RALib 1.23 }
  // (rom) now in JvJCLUtils.pas

  { RALib 1.55 }
  {$IFDEF MSWINDOWS}
  {$IFDEF DELPHI2}
  SDelphiKey = 'Software\Borland\Delphi\2.0';
  {$ENDIF DELPHI2}
  {$IFDEF BCB1}
  SDelphiKey = 'Software\Borland\C++Builder\1.0';
  {$ENDIF BCB1}
  {$IFDEF DELPHI3}
  SDelphiKey = 'Software\Borland\Delphi\3.0';
  {$ENDIF DELPHI3}
  {$IFDEF BCB3}
  SDelphiKey = 'Software\Borland\C++Builder\3.0';
  {$ENDIF BCB3}
  {$IFDEF DELPHI4}
  SDelphiKey = 'Software\Borland\Delphi\4.0';
  {$ENDIF DELPHI4}
  {$IFDEF BCB4}
  SDelphiKey = 'Software\Borland\C++Builder\4.0';
  {$ENDIF BCB4}
  {$IFDEF DELPHI5}
  SDelphiKey = 'Software\Borland\Delphi\5.0';
  {$ENDIF DELPHI5}
  {$IFDEF BCB5}
  SDelphiKey = 'Software\Borland\C++Builder\5.0';
  {$ENDIF BCB5}
  {$IFDEF DELPHI6}
  SDelphiKey = 'Software\Borland\Delphi\6.0';
  {$ENDIF DELPHI6}
  {$IFDEF BCB6}
  SDelphiKey = 'Software\Borland\C++Builder\6.0';
  {$ENDIF BCB6}
  {$IFDEF DELPHI7}
  SDelphiKey = 'Software\Borland\Delphi\7.0';
  {$ENDIF DELPHI7}
  {$IFDEF BCB7} // will it ever be released? No.
  SDelphiKey = 'Software\Borland\C++Builder\7.0';
  {$ENDIF BCB7}
  {$IFDEF DELPHI8}
  SDelphiKey = 'Software\Borland\BDS\2.0';
  {$ENDIF DELPHI8}
  {$IFDEF DELPHI9}
  SDelphiKey = 'Software\Borland\BDS\3.0';
  {$ENDIF DELPHI9}
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  SDelphiKey = '.borland/.jvclx1';
  {$ENDIF UNIX}
  { JvDataProvider constants }
  { Consumer attributes }
  DPA_RenderDisabledAsGrayed = 1;
  DPA_RendersSingleItem = 2;
  DPA_ConsumerDisplaysList = 3;

  CM_JVBASE = CM_BASE + 80;
  { Command message for JvSpeedbar editor }
  CM_SPEEDBARCHANGED = CM_JVBASE + 0;
  { Command message for TJvSpeedButton }
  CM_JVBUTTONPRESSED = CM_JVBASE + 1;
  // (rom) disabled unused
  { Command messages for TJvWindowHook }
  //CM_RECREATEWINDOW  = CM_JVBASE + 2;
  //CM_DESTROYHOOK     = CM_JVBASE + 3;
  { Notify message for TJvxTrayIcon }
  //CM_TRAYICON        = CM_JVBASE + 4;
  CM_FORCESIZE = CM_JVBASE + 5;  // used in JvButton

  { Values for WParam for CM_SPEEDBARCHANGED message }
  SBR_CHANGED = 0; { change buttons properties  }
  SBR_DESTROYED = 1; { destroy SpeedBar           }
  SBR_BTNSELECT = 2; { select button in SpeedBar  }
  SBR_BTNSIZECHANGED = 3; { button size changed        }

  { TBitmap.GetTransparentColor from GRAPHICS.PAS use this value }
  PaletteMask = $02000000;

  // (outchy) now used
  {$IFDEF COMPILER7_UP}
  // (outchy) it was defined as $000000FF
  DEFAULT_SYSCOLOR_MASK = clSystemColor;  // $FF000000
  {$ELSE}
  DEFAULT_SYSCOLOR_MASK = $80000000;
  {$ENDIF COMPILER7_UP}

  {$IFNDEF COMPILER6_UP}
  { Standard Windows colors that are not defined in Delphi 5}
  COLOR_MENUHILIGHT = 29;
  {$EXTERNALSYM COLOR_MENUHILIGHT}
  COLOR_MENUBAR = 30;
  {$EXTERNALSYM COLOR_MENUBAR}

  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($F0CAA6);
  clCream = TColor($F0FBFF);
  clMedGray = TColor($A4A0A0);
  // (outchy) = TColor(COLOR_XXXXXXXXXXX or $80000000);
  clGradientActiveCaption = TColor(COLOR_GRADIENTACTIVECAPTION or DEFAULT_SYSCOLOR_MASK);
  clGradientInactiveCaption = TColor(COLOR_GRADIENTINACTIVECAPTION or DEFAULT_SYSCOLOR_MASK);
  clHotLight = TColor(COLOR_HOTLIGHT or DEFAULT_SYSCOLOR_MASK);
  clMenuHighlight = TColor(COLOR_MENUHILIGHT or DEFAULT_SYSCOLOR_MASK);
  clMenuBar = TColor(COLOR_MENUBAR or DEFAULT_SYSCOLOR_MASK);
  {$ENDIF COMPILER6_UP}

  {$IFNDEF COMPILER6_UP}
  {$IFDEF MSWINDOWS}
  sLineBreak = #13#10;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  sLineBreak = #10;
  {$ENDIF UNIX}
  {$ENDIF COMPILER6_UP}
  sLineBreakLen = Length(sLineBreak);

  CrLf = #13#10;
  Cr = #13;
  Lf = #10;
  Backspace = #8;
  Tab = #9;
  Esc = #27;
  Del = #127;
  CtrlC = ^C;
  CtrlH = ^H;
  CtrlI = ^I;
  CtrlJ = ^J;
  CtrlM = ^M;
  CtrlV = ^V;
  CtrlX = ^X;
  {$IFDEF MSWINDOWS}
  RegPathDelim = '\';
  PathDelim = '\';
  DriveDelim = ':';
  PathSep = ';';
  AllFilePattern = '*.*';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  RegPathDelim = '_';
  PathDelim = '/';
  AllFilePattern = '*';
  {$ENDIF UNIX}

  {const Separators is used in GetWordOnPos, JvUtils.ReplaceStrings and SubWord}
  Separators: TSysCharSet = [#00, ' ', '-', #13, #10, '.', ',', '/', '\', '#', '"', '''',
  ':', '+', '%', '*', '(', ')', ';', '=', '{', '}', '[', ']', '{', '}', '<', '>'];

  DigitChars = ['0'..'9'];
  // (rom) disabled unused
  //Brackets = ['(', ')', '[', ']', '{', '}'];
  //StdWordDelims = [#0..' ', ',', '.', ';', '/', '\', ':', '''', '"', '`'] + Brackets;

var
  crJVCLFirst: TCursor = 100;
  crMultiDragLink: TCursor = 100;
  crDragAlt: TCursor = 101;
  crMultiDragAlt: TCursor = 102;
  crMultiDragLinkAlt: TCursor = 103;
  crHand: TCursor = 104;
  crDragHand: TCursor = 105;
  // this should be incremented to always contain the last default JVCL cursor index
  crJVCLLast: TCursor = 105;

const
  ROP_DSPDxax = $00E20746;

const
  FOURCC_ACON = 'ACON';
  FOURCC_IART = 'IART';
  FOURCC_INAM = 'INAM';
  FOURCC_INFO = 'INFO';
  FOURCC_LIST = 'LIST';
  FOURCC_RIFF = 'RIFF';
  FOURCC_anih = 'anih';
  FOURCC_fram = 'fram';
  FOURCC_icon = 'icon';
  FOURCC_rate = 'rate';
  FOURCC_seq = 'seq ';

  AF_ICON = $00000001;
  AF_SEQUENCE = $00000002;

{ JvUrlGrabbers }
const
  JediAgent = 'JEDI-VCL';
  DefaultOutputFileName = 'output.txt';

const
  SC_DRAGMOVE = $F012;

const
  KeyboardShiftStates = [ssShift, ssAlt, ssCtrl];
  MouseShiftStates = [ssLeft, ssRight, ssMiddle, ssDouble];

implementation

end.

