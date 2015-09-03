{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCL.INC, released on 2002-07-04.

Last Modified: 2008-11-11

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

{$IFDEF JVCL_NO_DEBUGINFO} // set by the Installer
 {$D-}
{$ENDIF JVCL_NO_DEBUGINFO}

{$I jedi\jedi.inc}

{$IFNDEF JEDI_INC}
ALERT_jedi_inc_incompatible
// secure against old versions of jedi.inc.
{$ENDIF !JEDI_INC}

// Reduces warnings on D7:
{$IFDEF COMPILER7_UP}
 {$WARN UNSAFE_TYPE OFF}
 {$WARN UNSAFE_CODE OFF}
 {$WARN UNSAFE_CAST OFF}
{$ENDIF COMPILER7_UP}

{$IFDEF BCB}
 {$ObjExportAll ON}
{$ENDIF BCB}


{$UNDEF JVCL_CONFIGURED}

{$IFDEF DEFAULT_JVCL_INC}
{-----------------------------------------------------------------------------}
{ Default configuration                                                       }
{-----------------------------------------------------------------------------}
 {$I jvclbase.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ELSE}
{-----------------------------------------------------------------------------}
{ Delphi                                                                      }
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI6}
 {$I jvcld6.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPIH6}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI7}
 {$I jvcld7.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPIH7}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI9}
 {$I jvcld9.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPIH9}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI10}
 {$I jvcld10.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPIH10}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI11}
 {$I jvcld11.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI11}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI12}
 {$I jvcld12.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI12}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI14}
 {$I jvcld14.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI14}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI15}
 {$I jvcld15.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI15}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI16}
 {$IFDEF CPUX86}
 {$I jvcld16.inc}
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld16_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI16}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI17}
 {$IFDEF CPUX86}
 {$I jvcld17.inc}
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld17_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI17}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI18}
 {$IFDEF CPUX86}
 {$I jvcld18.inc}
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld18_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI18}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI19}
 {$IFDEF CPUX86}
 {$I jvcld19.inc}
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld19_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI19}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI20}
 {$IFDEF CPUX86}
 {$I jvcld20.inc}
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld20_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI20}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI21}
 {$IFDEF CPUX86}
 {$I jvcld21.inc}
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld21_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI21}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI22}
 {$IFDEF CPUX86}
 {$I jvcld22.inc}
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld22_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI22}
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI23}
 {$IFDEF CPUX86}
 {$I jvcld23.inc}
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld23_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPHI23}
{-----------------------------------------------------------------------------}
{ C++Builder                                                                  }
{-----------------------------------------------------------------------------}
{$IFDEF BCB6}
 {$I jvclc6.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB6}
{-----------------------------------------------------------------------------}
{$IFDEF BCB10}
 {$I jvcld10.inc}   // same include file for BDS based C++
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB10}
{-----------------------------------------------------------------------------}
{$IFDEF BCB11}
 {$I jvcld11.inc}   // same include file for BDS based C++
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB11}
{-----------------------------------------------------------------------------}
{$IFDEF BCB12}
 {$I jvcld12.inc}   // same include file for BDS based C++
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB12}
{-----------------------------------------------------------------------------}
{$IFDEF BCB14}
 {$I jvcld14.inc}   // same include file for BDS based C++
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB14}
{-----------------------------------------------------------------------------}
{$IFDEF BCB15}
 {$I jvcld15.inc}   // same include file for BDS based C++
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB15}
{-----------------------------------------------------------------------------}
{$IFDEF BCB16}
 {$IFDEF CPUX86}
 {$I jvcld16win32.inc} // same include file for BDS based C++
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 //there is no 64-bit C++Builder
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB16}
{-----------------------------------------------------------------------------}
{$IFDEF BCB17}
 {$IFDEF CPUX86}
 {$I jvcld17win32.inc} // same include file for BDS based C++
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 //there is no 64-bit C++Builder yet
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB17}
{-----------------------------------------------------------------------------}
{$IFDEF BCB18}
 {$IFDEF CPUX86}
 {$I jvcld18win32.inc} // same include file for BDS based C++
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld18_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB18}
{-----------------------------------------------------------------------------}
{$IFDEF BCB19}
 {$IFDEF CPUX86}
 {$I jvcld19win32.inc} // same include file for BDS based C++
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld19_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB19}
{-----------------------------------------------------------------------------}
{$IFDEF BCB20}
 {$IFDEF CPUX86}
 {$I jvcld20win32.inc} // same include file for BDS based C++
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld20_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB20}
{-----------------------------------------------------------------------------}
{$IFDEF BCB21}
 {$IFDEF CPUX86}
 {$I jvcld21win32.inc} // same include file for BDS based C++
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld21_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB21}
{-----------------------------------------------------------------------------}
{$IFDEF BCB22}
 {$IFDEF CPUX86}
 {$I jvcld22win32.inc} // same include file for BDS based C++
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld22_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB22}
{-----------------------------------------------------------------------------}
{$IFDEF BCB23}
 {$IFDEF CPUX86}
 {$I jvcld23win32.inc} // same include file for BDS based C++
 {$ENDIF CPUX86}
 {$IFDEF CPUX64}
 {$I jvcld23_x64.inc}
 {$ENDIF CPUX64}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB23}
{-----------------------------------------------------------------------------}
{$ENDIF DEFAULT_JVCL_INC}
{-----------------------------------------------------------------------------}


{$IFNDEF JVCL_CONFIGURED}
 {$IFDEF SUPPORTS_COMPILETIME_MESSAGES}
  {$MESSAGE FATAL 'Your Delphi/BCB version is not supported by this JVCL version!'}
 {$ELSE}
  'Your Delphi/BCB version is not supported by this JVCL version!'
 {$ENDIF SUPPORTS_COMPILETIME_MESSAGES}
{$ENDIF !JVCL_CONFIGURED}


// check configuration dependencies

{$IFNDEF JVCL_UseQuickReport}
 {$UNDEF QREPORT4}
{$ENDIF !JVCL_UseQuickReport}

{$IFDEF JVCL_GENERATE_CPP_PACKAGE_FILES}
  // dcc32 -IL does not support class constructor/destructor
  {$UNDEF SUPPORTS_CLASS_CTORDTORS}
{$ENDIF JVCL_GENERATE_CPP_PACKAGE_FILES}

{$IFDEF NO_UNITVERSIONING} // used by the Installer
 {$UNDEF UNITVERSIONING}
{$ENDIF NO_UNITVERSIONING}

// D7 has theme support built in...
{$IFDEF COMPILER7_UP}
 {$DEFINE JVCLThemesEnabled}
{$ELSE}
 {$IFDEF JVCLThemesEnabled}
  {$DEFINE JVCLThemesEnabledD6}
 {$ENDIF JVCLThemesEnabled}
{$ENDIF COMPILER7_UP}

// XE2+ custom styles
{$IFDEF COMPILER16_UP}
  {$DEFINE JVCLStylesEnabled}
{$ENDIF COMPILER16_UP}

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR_SHARED}
  {$DEFINE USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
{$ENDIF}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR_NON_SHARED}
  {$DEFINE USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
{$ENDIF}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID_SHARED}
  {$DEFINE USE_3RDPARTY_DEVEXPRESS_CXGRID}
{$ENDIF}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID_NON_SHARED}
  {$DEFINE USE_3RDPARTY_DEVEXPRESS_CXGRID}
{$ENDIF}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID_SHARED}
  {$DEFINE USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
{$ENDIF}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID_NON_SHARED}
  {$DEFINE USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
{$ENDIF}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXPIVOTGRID_SHARED}
  {$DEFINE USE_3RDPARTY_DEVEXPRESS_CXPIVOTGRID}
{$ENDIF}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXPIVOTGRID_NON_SHARED}
  {$DEFINE USE_3RDPARTY_DEVEXPRESS_CXPIVOTGRID}
{$ENDIF}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXTREELIST_SHARED}
  {$DEFINE USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
{$ENDIF}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXTREELIST_NON_SHARED}
  {$DEFINE USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
{$ENDIF}

{$IFDEF USE_3RDPARTY_DEVART_ODAC}
  {$DEFINE USE_3RDPARTY_DEVART_DAC}
{$ENDIF}
{$IFDEF USE_3RDPARTY_DEVART_UNIDAC}
  {$DEFINE USE_3RDPARTY_DEVART_DAC}
{$ENDIF}
