{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvResExp.inc, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
 {$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

{$I JEDI.INC}

{ String constants }

{$IFDEF Delphi3_Up}
resourcestring
{$ELSE}
const
{$ENDIF}
  sExpertName = 'Project Resource Expert';
{$IFDEF Delphi4_Up}
  sMenuItemCaption = 'Resources';
{$ELSE}
  sMenuItemCaption = 'Pro&ject Resources';
{$ENDIF}
  sCannotRename = 'A resource named "%s" already exists';
  sInvalidName = '"%s" is not a valid resource name';
  sInvalidType = '"%s" is not a valid resource type';
  sNewBitmap = 'New Bitmap';
  sNewResource = 'New Binary Resource';
  sResType = '&Resource Type:';
  sAllFilesFilter = 'All Files (*.*)|*.*';
  sCursorFilesFilter = 'Windows Cursors (*.cur)|*.cur';
  sAniCursorFilesFilter = 'Animated Cursors (*.ani)|*.ani';
  sIconFilesFilter = 'Icon Files (*.ico)|*.ico';


{ Conditional defines that affect compilation }

{.$DEFINE DCS}
{ Client/Server or Enterprise version. Deactivate this define if you
  have Delphi Desktop, Standard or Professional Edition }

{.$DEFINE RX_MIDAS}
{ RX MIDAS components } 

{.$DEFINE RX_ADO}
{ RX ADO components } 


{$DEFINE USE_Jv_GIF}
{ This define enables GIF image support. Deactivate this define
  if you are using another GIF image support library. See README.TXT
  for the legal aspects }

{$IFDEF WIN32}
{.$DEFINE USE_PROGRESSBAR}
{ Activate this define if you do not want to use TGauge Borland sample
  component in TDBProgress component and FileUtil routines. In this case
  TProgressBar component will be used }
{$ENDIF}

{$DEFINE USE_FOUR_DIGIT_YEAR}
{ This define controls whether FourDigitYear variable is used to control
  date format in TDateEdit, TDBDateEdit components. When this define is not
  active the ShortDateFormat variable is used instead }

{.$DEFINE USE_OLD_DBLISTS}
{ Activate this define to install old BDE lists components for backward
  compatibility with oldest RX versions }

{$DEFINE DEFAULT_POPUP_CALENDAR}
{ This define controls whether a popup calendar is used as default
  instead of a modal dialog in TDateEdit, TDBDateEdit components }


