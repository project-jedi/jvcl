{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCL.INC, released on 2002-07-04.

Last Modified: 2004-12-22

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$IFDEF JVCL_NO_DEBUGINFO} // set by the Installer
 {$D-}
{$ELSE}
  // default
 {$D+}
{$ENDIF JVCL_NO_DEBUGINFO}

{$IFDEF JVCLINSTALLER}
{$I ..\install\JVCLInstall\jvcl.inc}
{$ELSE}

{$A+,B-,C+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

{$I jedi.inc}

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

{$IFDEF QUnit}
 {$UNDEF VCL}
 {$DEFINE VisualCLX}
{$ENDIF}

// jedi.inc does not define VCL nor VisualCLX if the application is a console
// But the JVCL code requires that VCL or VisualCLX be defined (but not both,
// of course). So we define those symbols ourselves as it is not a matter
// for all JEDI projects. 
{$IFDEF CONSOLE}
  {$IFNDEF VCL}
    {$IFNDEF VisualCLX}
      {$IFDEF LINUX}
        {$DEFINE VisualCLX}
      {$ENDIF LINUX}
      {$IFNDEF VisualCLX}
        {$DEFINE VCL}
      {$ENDIF !VisualCLX}
    {$ENDIF !VisualCLX}
  {$ENDIF !VCL}
{$ENDIF CONSOLE}

{$IFNDEF DEFAULT_JVCL_INC}
{-----------------------------------------------------------------------------}
{ Delphi                                                                      }
{-----------------------------------------------------------------------------}
{$IFDEF DELPHI5}
 {$I jvcld5.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DELPIH5}
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
{ C++Builder                                                                  }
{-----------------------------------------------------------------------------}
{$IFDEF BCB5}
 {$I jvclc5.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF BCB5}
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
{ Default configuration                                                       }
{-----------------------------------------------------------------------------}
{$ELSE}
 {$I jvclbase.inc}
 {$DEFINE JVCL_CONFIGURED}
{$ENDIF DEFAULT_JVCL_INC}
{-----------------------------------------------------------------------------}


// check configuration

{$IFNDEF JVCL_CONFIGURED}
 {$IFDEF SUPPORTS_COMPILETIME_MESSAGES}
  {$MESSAGE FATAL 'Your Delphi/BCB version is not supported by this JVCL version!'}
 {$ELSE}
  'Your Delphi/BCB version is not supported by this JVCL version!'
 {$ENDIF SUPPORTS_COMPILETIME_MESSAGES}
{$ENDIF !JVCL_CONFIGURED}


{$IFNDEF JVCL_UseQuickReport}
 {$UNDEF QREPORT4}
{$ENDIF !JVCL_UseQuickReport}

{$IFDEF NO_UNITVERSIONING} // used by the Installer
 {$UNDEF UNITVERSIONING}
{$ENDIF NO_UNITVERSIONING}

// D7 has theme support built in...
{$IFDEF COMPILER7_UP}
 {$DEFINE JVCLThemesEnabled}
{$ELSE}
 {$IFDEF JVCLThemesEnabled}
  {$DEFINE JVCLThemesEnabledD56}
 {$ENDIF JVCLThemesEnabled}
{$ENDIF COMPILER7_UP}

// only allowed for VCL
{$IFNDEF VCL}
 {$UNDEF JvInterpreter_OLEAUTO}
 {$UNDEF JVCLThemesEnabled}
 {$UNDEF JVCLThemesEnabledD56}
{$ENDIF !VCL}

{$IFDEF CLR}
 {$WARN UNSAFE_TYPE ON}
 {$WARN UNSAFE_CODE ON}
 {$WARN UNSAFE_CAST ON}
 {$WARN UNIT_PLATFORM OFF}

 {$UNDEF JvInterpreter_OLEAUTO}
 {$UNDEF JVCLThemesEnabledD56}
 {$UNDEF UNITVERSIONING}
 {$UNDEF USE_DXGETTEXT}
 {$DEFINE USEJVCL}
 {$DEFINE MSWINDOWS}
{$ENDIF CLR}

{$ENDIF INSTALLER}
