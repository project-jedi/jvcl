{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCL.INC, released on 2004-04-14.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{***************************************************************************}
{* THIS IS A SPECIAL VERSION OF THE jvcl.inc THAT IS ONLY USED BY THE JVCL *}
{* INSTALLER.                                                              *}
{***************************************************************************}

{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

{$I jedi.inc}

{$DEFINE NO_JCL}
{$DEFINE JVCLThemesEnabled}

// Reduces warnings on D7:
{$IFDEF COMPILER7_UP}
 {$WARN UNSAFE_TYPE OFF}
 {$WARN UNSAFE_CODE OFF}
 {$WARN UNSAFE_CAST OFF}
{$ENDIF}
