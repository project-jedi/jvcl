{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCL.INC, released on 2002-07-04.

Last Modified: 2003-10-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

{$I JEDI.INC}

{ Enable this define if you want to register property and component editors included in
 JVCL for non-JVCL components as well }
{.$DEFINE JVCL_REGISTER_GLOBAL_DESIGNEDITORS}

{ Some new libraries and packages (currently JvUIB, JvWizard and JvTimeFrameWork) can be
  installed either in "stand-alone" mode or in "JVCL" mode. These packages use the
  USEJVCL define to determine whether they are installed in JVCL mode or not.
  By default, this define is enabled.
  NOTE: if you undefine USEJVCL, you can/should remove JvCoreXXX.dcp from the
  requires node in the affected package(s) }
{$DEFINE USEJVCL}

{ Enable this define if you want to use the dxgettext (http://dxgettext.sourceforge.net) 
  translation tool. }
{.$DEFINE USE_DXGETTEXT}


{$IFDEF BCB}
  {$ObjExportAll ON}
{$ENDIF}

{ Activate this define if you are using Delphi 5 Standard or one of the D6/D7 Personal Editions}
{.$DEFINE DelphiPersonalEdition}

{ Activate this define if you have Mike Lischke's Theme Manager
  (http:// www.lischke-online.de) installed and available and
  you are using D6/BCB6 or below. The ThemeManager package must
  be compiled as "never-build package". }
{$IFDEF VCL}
{.$DEFINE JVCLThemesEnabled}
{$ENDIF VCL}

{ D7 has theme support built in...}
{$IFDEF COMPILER7_UP}
 {$IFDEF VCL}
 {$DEFINE JVCLThemesEnabled}
 {$ENDIF VCL}
{$ELSE}
 {$IFDEF JVCLThemesEnabled}
  {$DEFINE JVCLThemesEnabledD56}
 {$ENDIF}
{$ENDIF}

{ Activates MIDAS components (DBRemoteLogin)}
{.$DEFINE Jv_MIDAS}

{ This define enables GIF image support. Deactivate this define
  if you are using another GIF image support library. }
{$DEFINE USE_Jv_GIF}

{ Activate this define if you do not want to use TGauge Borland sample
  component in TDBProgress component and FileUtil routines. In this case
  TProgressBar component will be used }
{$DEFINE USE_PROGRESSBAR}

{ This define controls whether FourDigitYear variable is used to control
  date format in TDateEdit, TDBDateEdit components. When this define is not
  active the ShortDateFormat variable is used instead }
{$DEFINE USE_FOUR_DIGIT_YEAR}

{ Activate this define to install old BDE lists components for backward
  compatibility with oldest RxLib versions }
{.$DEFINE USE_OLD_DBLISTS}

{ This define controls whether a popup calendar is used as default
  instead of a modal dialog in TDateEdit, TDBDateEdit components }
{$DEFINE DEFAULT_POPUP_CALENDAR}

// Automatic defines for entry level Delphi's:
{$IFDEF D5Standard}
 {$DEFINE DelphiPersonalEdition}
{$ENDIF}

{$IFDEF D6PersonalEdition}
 {$DEFINE DelphiPersonalEdition}
{$ENDIF}

{$IFDEF D7PersonalEdition}
 {$DEFINE DelphiPersonalEdition}
{$ENDIF}

// Reduces warnings on D7:
{$IFDEF COMPILER7_UP}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{ Used by JvTimeFrameWork, see JvTFDays.pas for more info on time blocks }
{$DEFINE TIMEBLOCKS}

{ This define controls whether JvHLEditor supports a highlighter for NQC. }
{$DEFINE HL_NOT_QUITE_C}

