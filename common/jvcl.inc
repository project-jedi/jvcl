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

{$I jedi.inc}

{$IFDEF QUnit}
 {$UNDEF VCL}
 {$DEFINE VisualCLX}
 {$UNDEF JVCLThemesEnabled}
{$ENDIF}

{ Enable this define if you want to register property and component editors included in
JVCL for non-JVCL components as well. }
{.$DEFINE JVCL_REGISTER_GLOBAL_DESIGNEDITORS}

{ Some new libraries and packages (currently JvUIB, JvWizard and JvTimeFrameWork) can be
installed either in "stand-alone" mode or in "JVCL" mode. These packages use the
USEJVCL define to determine whether they are installed in JVCL mode or not.
By default, this define is enabled.
NOTE: if you undefine USEJVCL, you can/should remove JvCoreXXX.dcp from the
requires node in the affected package(s). }
{$DEFINE USEJVCL}

{ Enable this define if you have the dxgettext (http://dxgettext.sourceforge.net)
translation tool installed and want to use it with the JVCL. }
{.$DEFINE USE_DXGETTEXT}

{ Enable this define if you use/have QuickReport }
{$DEFINE JVCL_UseQuickReport}

{ Enable this define if you have QReport 4 installed. This will require qr4rund7 instead
of qrpt. NB! JVCL_UseQuickReport must also be defined}
{.$DEFINE QREPORT4}

{$IFDEF COMPILER8_UP}
{$UNDEF JVCL_UseQuickReport}
{$ENDIF}

{$IFNDEF JVCL_UseQuickReport}
{$UNDEF QREPORT4}
{$ENDIF}

{$IFDEF BCB}
 {$ObjExportAll ON}
{$ENDIF}

{ Activate this define if you are using Delphi 5 Standard or one of the D6/D7 Personal Editions. }
{.$DEFINE DelphiPersonalEdition}

{ Activate this define if you have Mike Lischke's Theme Manager
(http://www.lischke-online.de) installed and available and
you are using D6/BCB6 or below. The ThemeManager package must
be compiled as "never-build package". }
{.$DEFINE JVCLThemesEnabled}

// D7 has theme support built in...
{$IFDEF COMPILER7_UP}
 {$DEFINE JVCLThemesEnabled}
{$ELSE}
 {$IFDEF JVCLThemesEnabled}
  {$DEFINE JVCLThemesEnabledD56}
 {$ENDIF}
{$ENDIF}

{$IFNDEF VCL}
 {$UNDEF JVCLThemesEnabled}
 {$UNDEF JVCLThemesEnabledD56}
{$ENDIF}

{ Activates MIDAS components (DBRemoteLogin).
NOTE: if you activate this, you must also manually add JvDlgsXXX.dcp to
the requires node of JvDBXXX.bpk !!! }
{.$DEFINE Jv_MIDAS}

{ This define enables GIF image support. Deactivate this define
if you are using another GIF image support library. }
{$DEFINE USE_JV_GIF}

{ Activate this define if you do not want to use TGauge Borland sample
component in TDBProgress component and FileUtil routines. In this case
TProgressBar component will be used. }
{$DEFINE USE_PROGRESSBAR}

{ This define controls whether FourDigitYear variable is used to control
date format in TDateEdit, TDBDateEdit components. When this define is not
active the ShortDateFormat variable is used instead. }
{$DEFINE USE_FOUR_DIGIT_YEAR}

{ This define controls whether a popup calendar is used as default
instead of a modal dialog in TDateEdit, TDBDateEdit components. }
{$DEFINE DEFAULT_POPUP_CALENDAR}

{ This define controls whether JvInterpreter handles
OLE automation calls (for VCL only). }
{$DEFINE JvInterpreter_OLEAUTO}

// only allowed for VCL
{$IFNDEF VCL}
 {$UNDEF JvInterpreter_OLEAUTO}
{$ENDIF}

{ Used by JvTimeFrameWork, see JvTFDays.pas for more info on time blocks. }
{$DEFINE Jv_TIMEBLOCKS}

{ This activates the unit versioning system where each JVCL unit gets a record that defines
which revision, date and filename the unit has. }
{.$DEFINE UNITVERSIONING}

{$IFDEF NO_UNITVERSIONING}
 {$UNDEF UNITVERSIONING}
{$ENDIF}


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