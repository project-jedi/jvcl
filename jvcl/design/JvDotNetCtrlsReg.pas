{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDotNetCtrlsReg.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDotNetCtrlsReg;

{$I jvcl.inc}

interface

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvDotNetCtrlsReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvDotNetCtrlsReg.dcr}
{$ENDIF LINUX}

procedure Register;

implementation

uses
  Classes,
  {$IFDEF USEJVCL}
  JvDsgnConsts,
  {$ENDIF USEJVCL}
  {$IFNDEF DelphiPersonalEdition}
  JvDBDotNetControls,
  {$ENDIF !DelphiPersonalEdition}
  JvDotNetControls;

{$IFNDEF USEJVCL}
resourcestring
  RsPaletteDotNet = 'Jv DotNet';
  {$IFNDEF DelphiPersonalEdition}
  RsPaletteDotNetDB = 'Jv DotNet DB';
  {$ENDIF !DelphiPersonalEdition}
{$ENDIF USEJVCL}

procedure Register;
begin
  RegisterComponents(RsPaletteDotNet, [TJvDotNetCheckListBox,
    TJvDotNetEdit, TJvDotNetHotKey, TJvDotNetListBox,
    TJvDotNetListView, TJvDotNetMaskEdit, TJvDotNetMemo,
    TJvDotNetRichEdit, TJvDotNetScrollBox, TJvDotNetTreeView, TJvDotNetButton]);
  {$IFDEF USEJVCL}
  RegisterComponents(RsPaletteDotNet, [TJvDotNetFilenameEdit, TJvDotNetDirectoryEdit]);
  {$ENDIF USEJVCL}

  {$IFNDEF DelphiPersonalEdition}
  {$IFDEF USEJVCL}
  RegisterComponents(RsPaletteDotNetDB, [TJvDotNetDBFindEdit]);
  {$ENDIF USEJVCL}
  RegisterComponents(RsPaletteDotNetDB, [TJvDotNetDBEdit, TJvDotNetDBListBox,
    TJvDotNetDBLookupListBox, TJvDotNetDBMemo, TJvDotNetDBRichEdit]);
  {$ENDIF !DelphiPersonalEdition}
end;

end.
