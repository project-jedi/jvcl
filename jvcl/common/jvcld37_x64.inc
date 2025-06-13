{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCL.INC, released on 2004-12-22.

Last Modified: 2004-12-22

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{ This file contains the default JVCL configuration.                           }
{------------------------------------------------------------------------------}

{%hidden%}
{ Enable this define if you are using the Delphi 6, 7 or 2005 Personal Edition. }
{.$DEFINE DelphiPersonalEdition}

{%hidden%}
{ The installer enables this define if you compile JVCL for Delphi and C++ Builder. It disables
  the class constructor/destructor support that is not supported by C++Builder. }
{$DEFINE JVCL_GENERATE_CPP_PACKAGE_FILES}

{%hidden%}
{ The installer enables this define if you compile JVCL for a Delphi edition that has
  the BDE. It is enabled by default for any 32 bit version prior to XE7 where the pro and 
  upper editions no longer have the BDE files installed by default. }
{.$DEFINE USE_BDE}

{ Enable this define if you have the dxgettext (http://dxgettext.sourceforge.net)
translation tool installed and want to use it with the JVCL. }
{.$DEFINE USE_DXGETTEXT}

{ Enable this define if you use/have QuickReport }
{.$DEFINE JVCL_UseQuickReport}

{ Enable this define if you have QReport 4 installed. This will require qr4rund7 instead
of qrpt. NB! JVCL_UseQuickReport must also be defined}
{.$DEFINE QREPORT4}

{ Enable this define if you have installed the Internet Components with the Delphi installer }
{.$DEFINE INTERNET_COMPONENTS}

{ Activate this define if you have Mike Lischke's Theme Manager
(http://www.lischke-online.de) installed and available and
you are using D6/BCB6 or below. The ThemeManager package must
be compiled as "never-build package". }
{.$DEFINE JVCLThemesEnabled}

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

{ Used by JvTimeFrameWork, see JvTFDays.pas for more info on time blocks. }
{$DEFINE Jv_TIMEBLOCKS}

{ This activates the unit versioning system where each JVCL unit gets a record that defines
which revision, date and filename the unit has. }
{.$DEFINE UNITVERSIONING}

// *********************************************************************
// Start Definition of Third Party Components
// *********************************************************************

{ Activates SM-Export Wrapper Components (in DBActions) 
For further informations have a look at http://www.scalabium.com 
ATTENTION : 
BEFORE YOU ACTIVATE THIS OPTION YOU MUST CHANGE THE
SMEXPORT PACKAGE FROM AUTOMATIC COMPILE NO MANUAL COMPILE }
{.$DEFINE USE_3RDPARTY_SMEXPORT}

{ Activates SM-Import Wrapper Components (in DBActions)
For further informations have a look at http://www.scalabium.com
ATTENTION :
BEFORE YOU ACTIVATE THIS OPTION YOU MUST CHANGE THE
SMIMPORT PACKAGE FROM AUTOMATIC COMPILE NO MANUAL COMPILE }
{.$DEFINE USE_3RDPARTY_SMIMPORT}

{ Activates Support for the DevExpress cxEditor-Controls
For further informations have a look at http://www.devexpress.com 
You have to choose between shared and non shared packages}
{.$DEFINE USE_3RDPARTY_DEVEXPRESS_CXEDITOR_SHARED}
{.$DEFINE USE_3RDPARTY_DEVEXPRESS_CXEDITOR_NON_SHARED}

{ Activates Support for the DevExpress cxGrid-Controls
For further informations have a look at http://www.devexpress.com 
You have to choose between shared and non shared packages}
{.$DEFINE USE_3RDPARTY_DEVEXPRESS_CXGRID_SHARED}
{.$DEFINE USE_3RDPARTY_DEVEXPRESS_CXGRID_NON_SHARED}

{ Activates Support for the DevExpress cxVerticalGrid-Controls
For further informations have a look at http://www.devexpress.com 
You have to choose between shared and non shared packages}
{.$DEFINE USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID_SHARED}
{.$DEFINE USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID_NON_SHARED}

{ Activates Support for the DevExpress cxPivotGrid-Controls
For further informations have a look at http://www.devexpress.com 
You have to choose between shared and non shared packages}
{.$DEFINE USE_3RDPARTY_DEVEXPRESS_CXPIVOTGRID_SHARED}
{.$DEFINE USE_3RDPARTY_DEVEXPRESS_CXPIVOTGRID_NON_SHARED}

{ Activates Support for the DevExpress cxTreeList-Controls
For further informations have a look at http://www.devexpress.com 
You have to choose between shared and non shared packages}
{.$DEFINE USE_3RDPARTY_DEVEXPRESS_CXTREELIST_SHARED}
{.$DEFINE USE_3RDPARTY_DEVEXPRESS_CXTREELIST_NON_SHARED}


{ Activates the Internet Direct (Indy)-Components
For further informations have a look at http://www.indyproject.org }
{.$DEFINE USE_3RDPARTY_INDY}

{ Activates the Internet Direct (Indy)-Components version 10.
You MUST also activate $DEFINE USE_3RDPARTY_INDY for the compilation to work
For further informations have a look at http://www.indyproject.org }
{.$DEFINE USE_3RDPARTY_INDY10}

{ Activates Support for the ICS-Components (Internet component suite
For further informations have a look at http://www.overbyte.be/
ATTENTION :
BEFORE YOU ACTIVATE THIS OPTION YOU MUST CHANGE THE
ICS* PACKAGE FROM AUTOMATIC COMPILE TO MANUAL COMPILE }
{.$DEFINE USE_3RDPARTY_ICS}

{ Activates Support for Direct Oracle Access Components
For further informations have a look at http://www.allroundautomations.com }
{.$DEFINE USE_3RDPARTY_DOA}

{ Activates Support for the DevArt VCL Oracle Data Access Components
For further informations have a look at http://www.devart.com }
{.$DEFINE USE_3RDPARTY_DEVART_ODAC}

{ Activates Support for the DevArt VCL Universal Data Access Components
For further informations have a look at http://www.devart.com }
{.$DEFINE USE_3RDPARTY_DEVART_UNIDAC}
