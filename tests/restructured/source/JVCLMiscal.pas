{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCLMiscal.PAS, released Jun 10, 2000.

The Initial Developer of the Original Code is Petr Vones (petr.v@mujmail.cz)
Portions created by Petr Vones are Copyright (C) 2000 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: Jun 21, 2000
Current Version: 0.50

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
----------------------------------------------------------------------------- }

{$I JVCL.INC}
{$I JVCL.INC}
{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

unit JVCLMiscal;


interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, Dlgs, 
    Dialogs, ExptIntf, ToolIntf, ExtDlgs, StdCtrls,
    {$IFDEF COMPILER5} DsgnIntf, {$ENDIF} {$IFDEF COMPILER6_UP} DesignEditors, DesignIntf, {$ENDIF}
    JvMail, JvPerfMon95;

type
  TJvNosortEnumProperty = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

type
  TJvOpenDialogEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TCommonDialogHack = class(TCommonDialog);

type
  TJvMailEditor = class(TComponentEditor)
  private
    procedure Address;
    procedure SendMail;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

type
  TJvPerfStatProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;


type
  TJvExeNameProperty = class(TStringProperty)
  private
    procedure OnDialogShow(Sender: TObject);
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TJvDirectoryProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;



implementation
uses
  FileCtrl;
  
{ TJvNosortEnumProperty }

function TJvNosortEnumProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paSortList];
end;


{ TJvOpenDialogEditor }

procedure TJvOpenDialogEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = 0) and (Component is TCommonDialog) then TCommonDialogHack(Component).Execute;
end;

function TJvOpenDialogEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result := 'Dialog Test';
end;

function TJvOpenDialogEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


{ TJvMailEditor }

procedure TJvMailEditor.Address;
begin
  with Component as TJvMail do
  try
    Address(Owner.Name + '.' + Name);
  finally
    FreeSimpleMapi;
  end;
end;

procedure TJvMailEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: SendMail;
    1: Address;
  end;
end;

function TJvMailEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Send';
    1: Result := 'Address';
  end;
end;

function TJvMailEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TJvMailEditor.SendMail;
begin
  with Component as TJvMail do
  try
    SendMail;
  finally
    FreeSimpleMapi;
  end;
end;


{ TJvPerfStatProperty }

function TJvPerfStatProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TJvPerfStatProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    JvGetPerfStatItems(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;


{ TJvExeNameProperty }

procedure TJvExeNameProperty.Edit;
begin
  with TOpenDialog.Create(nil) do
  try
    FileName := GetValue;
    Filter := 'Executable files (*.exe)|*.exe|All files (*.*)|*.*';
    Title := 'Select executable ...';
    Options := Options - [ofHideReadOnly];
    OnShow := OnDialogShow;
    if Execute then
    begin
      if ofReadOnly in Options then
        SetValue(ExtractFileName(FileName))
      else
        SetValue(FileName);
    end;
  finally
    Free;
  end;
end;

function TJvExeNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure TJvExeNameProperty.OnDialogShow(Sender: TObject);
begin
  SetDlgItemText(GetParent(TOpenDialog(Sender).Handle), chx1, '&Strip file path');
end;

{ TJvDirectoryProperty }

procedure TJvDirectoryProperty.Edit;
var
  FolderName: string;
begin
  if SelectDirectory((GetComponent(0) as TComponent).Name + '.' + GetName, '',
    FolderName) then SetValue(FolderName);
end;

function TJvDirectoryProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;



end.
