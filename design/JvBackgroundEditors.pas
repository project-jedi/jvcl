{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBackgroundsReg.PAS, released on 2004-04-26.

The Initial Developer of the Original Code is Robert Rossmair [Robert dott Rossmair att t-online dott de]
Portions created by Robert Rossmair are Copyright (C) 2003 Robert Rossmair.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$
{$I jedi.inc}

unit JvBackgroundEditors;

interface

uses
  Classes,
  JvBackgrounds,
  JvBackgroundEditForm,
{$IFDEF COMPILER6_UP}
  DesignEditors,
  DesignIntf;
{$ELSE}
  DsgnIntf;
{$ENDIF}


{ TJvClientsProperty }

type
  TJvClientsProperty = class(TPropertyEditor)
    Clients: TJvBackgroundClients;
    Editor: TJvBackgroundClientsEditor;
    procedure EditorAddControl(const S: string);
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

implementation
uses
  SysUtils,
  Consts,
  Controls,
  PicEdit,
  TypInfo,
  Dialogs;

function TJvClientsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TJvClientsProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TJvClientsProperty.EditorAddControl(const S: string);
var
  Control: TWinControl;
begin
  Control := TWinControl(Designer.GetComponent(S));
  if Clients.IndexOf(Control) = -1
    then Editor.SrcList.Items.Add(S)
    else Editor.DstList.Items.Add(S);
end;

procedure TJvClientsProperty.Edit;
var
  I: Integer;
  Proc: TGetStrProc;
begin
  Editor := TJvBackgroundClientsEditor.Create(nil);
  try
    Clients := TJvBackgroundClients(GetOrdValue);
    {$IFDEF COMPILER6_UP}
    EditorAddControl(Designer.Root.Name);
    {$ELSE}
    EditorAddControl(Designer.GetRoot.Name);
    {$ENDIF}
    Proc := EditorAddControl;
    Designer.GetComponentNames(GetTypeData(TWinControl.ClassInfo), Proc);
    Editor.SetButtons;
    if Editor.ShowModal = mrOK then
    begin
      Clients.Clear;
      with Editor.DstList do
        for I := 0 to Items.Count-1 do
          Clients.Add(TWinControl(Designer.GetComponent(Items[I])));
      Designer.Modified;
    end;
  finally
    Clients := nil;
    Editor.Free;
    Editor := nil;
  end;
end;


end.
