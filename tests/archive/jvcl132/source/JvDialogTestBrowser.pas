{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDialogTestBrowser.PAS, released on 2000-07-25.

The Initial Developer of the Original Code is Pasha Sivtsov [psivtsov@mail.ru]
Portions created by Pasha Sivtsov are Copyright (C) 2000 Pasha Sivtsov.
All Rights Reserved.

Last Modified: 2002-02-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

(*=============================================================================
  Purpose : Disign-time Common Dialogs Browser
=============================================================================*)


unit JvDialogTestBrowser;

interface

uses
{$IFDEF DELPHI5} DsgnIntf {$ENDIF} {$IFDEF DELPHI6_UP} DesignEditors, DesignIntf {$ENDIF};

type
  TJvBrowseDialogEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer): string; override;
    function GetVerbCount : Integer; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

uses
  Dialogs;

type
  TJvHackCommonDialog = class(TCommonDialog)
  public
    function Execute: Boolean; override; abstract;
  end;

// Click + DblClick
//-----------------------------------------------------------------------------
procedure TJvBrowseDialogEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    Edit;
end;

// Popup Menu Item Caption
//-----------------------------------------------------------------------------
function TJvBrowseDialogEditor.GetVerb(Index: Integer): AnsiString;
begin
  Result := 'View dialog...';
end;

//-----------------------------------------------------------------------------
function TJvBrowseDialogEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//-----------------------------------------------------------------------------
procedure TJvBrowseDialogEditor.Edit;
begin
  TJvHackCommonDialog(Component).Execute;
end;

//-----------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponentEditor(TCommonDialog, TJvBrowseDialogEditor);
end;

end.
