{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCL.INC, released on 2002-07-04.

Last Modified: 2002-07-20

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

{$I JEDI.INC}
// Enable this define if you want to register property and component editors in JVCL that
// are registered for JVCL as well as for non-JVCL components
{.$DEFINE JVCL_REGISTER_GLOBAL_DESIGNEDITORS}

{$IFDEF BCB}
  {$ObjExportAll ON}
{$ENDIF}

{ Activate this define if you are using Delphi 5 Standard or one of the D6/D7 Personal Editions}
{.$DEFINE DelphiPersonalEdition}

{ Activate this define if you have Mike Lischke's Theme Manager
  (http:// www.lischke-online.de) installed and available and
  you are using D6/BCB6 or below }
{.$DEFINE JVCLThemesEnabled}
{ D7 has theme support built in...}
{$IFDEF COMPILER7_UP}
{$DEFINE JVCLThemesEnabled}
{$ENDIF}

{.$DEFINE Jv_MIDAS}
{ Jv MIDAS components }

{$DEFINE USE_Jv_GIF}
{ This define enables GIF image support. Deactivate this define
  if you are using another GIF image support library. See README.TXT
  for the legal aspects }

{$IFDEF WIN32}
{$DEFINE USE_PROGRESSBAR}
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

{$IFDEF D5Standard}
{$DEFINE DelphiPersonalEdition}
{$ENDIF}

{$IFDEF D6PersonalEdition}
{$DEFINE DelphiPersonalEdition}
{$ENDIF}

{$IFDEF D7PersonalEdition}
{$DEFINE DelphiPersonalEdition}
{$ENDIF}


