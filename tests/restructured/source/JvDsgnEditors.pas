{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDsgnEditors.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):


Last Modified: 2002-09-03
Added editors for JvFooter and JvGroupHeader

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I JVCL.INC}
{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

{ Various property editors }

unit JvDsgnEditors;

interface

uses
  Windows, Forms,  {$IFDEF COMPILER6_UP} DesignIntf,  DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
  SysUtils, Classes, Dialogs, Controls;

type
  THintProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TFilenameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TPathProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TStringsProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TDirectoryPropertyEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


  TDateTimeExProperty = class(TDateTimeProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TDateExProperty = class(TDateProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TTimeExProperty = class(TTimeProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TJvGroupHeaderEditor = class(TComponentEditor)
  public
    function GetVerbCount: integer; override;
    function GetVerb(Index: integer): string; override;
    procedure ExecuteVerb(Index: integer); override;
    procedure Edit; override;
  end;

  TJvFooterEditor = class(TComponentEditor)
  public
    function GetVerbCount: integer; override;
    function GetVerb(Index: integer): string; override;
    procedure ExecuteVerb(Index: integer); override;
    procedure Edit; override;
  end;



implementation
uses
  Graphics, JvTypes, JvGroupHeader, JvFooter,
  FileCtrl, JvStrLEdit, TypInfo, JvDateTimeDlg;

procedure TFilenameProperty.Edit;
begin
  with TOpenDialog.Create(nil) do
  try
    FileName := GetStrValue;
    if Execute then
       SetStrValue(FileName);
  finally
    Free;
  end;
end;

function TFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TFilenameProperty.GetValue: string;
begin
  Result := inherited GetValue;
  if Result = '' then
    Result := '(Filename)';
end;

{ TPathProperty }

procedure TPathProperty.Edit;
var S:string;
begin
  S := GetStrValue;
   if SelectDirectory(S,[sdAllowCreate, sdPerformCreate, sdPrompt],0) then
   SetStrValue(S);
end;

function TPathProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TPathProperty.GetValue: string;
begin
  Result := inherited GetValue;
  if Result = '' then
    Result := '(Filepath)';
end;

function THintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := {inherited GetAttributes +} [paDialog];
end;

procedure THintProperty.Edit;
var
  Temp: string;
  Comp: TPersistent;
//  I, Cnt: Integer;
begin
  with TJvStrEditDlg.Create(Application) do
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + GetName
    else Caption := GetName;
    Temp := GetStrValue;
    Memo.Lines.Text := Temp;
    UpdateStatus(nil);
    if ShowModal = mrOk then begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1);
      SetStrValue(Temp);
    end;
  finally
    Free;
  end;
end;

{ TStringsProperty }

procedure TStringsProperty.Edit;
var
  Temp: string;
  Comp: TPersistent;
//  I, Cnt: Integer;
begin
  with TJvStrEditDlg.Create(Application) do
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + GetName
    else Caption := GetName;
    Temp := GetStrValue;
    Memo.Lines.Text := Temp;
    UpdateStatus(nil);
    if ShowModal = mrOk then begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1);
      SetStrValue(Temp);
    end;
  finally
    Free;
  end;
end;

function TStringsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog,paRevertable];
end;

{ TDirectoryPropertyEditor }

procedure TDirectoryPropertyEditor.Edit;
var S: string;
begin
  S := GetStrValue;
  if SelectDirectory(S, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
  begin
    SetStrValue(S);
    Modified;
  end;
end;

function TDirectoryPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;


{ TDateTimeProperty }

procedure TDateTimeExProperty.Edit;
var D: TDateTime;
begin
  D := GetFloatValue;
  if D = 0.0 then
    D := Now;
  if TfrmSelectDateTimeDlg.SelectDateTime(D, dstDateTime) then
  begin
    SetFloatValue(D);
    Designer.Modified;
  end;
end;

function TDateTimeExProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TDateExProperty }

procedure TDateExProperty.Edit;
var D: TDateTime;
begin
  D := GetFloatValue;
  if D = 0.0 then
    D := Now;
  if TfrmSelectDateTimeDlg.SelectDateTime(D, dstDate) then
  begin
    SetFloatValue(D);
    Designer.Modified;
  end;
end;

function TDateExProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TTimeExProperty }

procedure TTimeExProperty.Edit;
var D: TDateTime;
begin
  D := GetFloatValue;
  if D = 0.0 then
    D := Now;
  if TfrmSelectDateTimeDlg.SelectDateTime(D, dstTime) then
  begin
    SetFloatValue(D);
    Designer.Modified;
  end;
end;

function TTimeExProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TJvGroupHeaderEditor }

function TJvGroupHeaderEditor.GetVerbCount: integer;
begin
  Result := 2;
end;

function TJvGroupHeaderEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Standard/Flat';
    1: Result := 'Web';
  end;
end;

procedure TJvGroupHeaderEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0:
      with TJvGroupHeader(Component) do
      begin
        BevelOptions.Style := bsLowered;
        Font.Style := [];
      end;
    1:
      with TJvGroupHeader(Component) do
      begin
        BevelOptions.Style := bsShape;
        BevelOptions.Brush.Color := $00A97A1B;
        BevelOptions.Pen.Color := $00E1AD40;
        BevelOptions.Height := 3;
        Font.Style := [fsBold];
      end;
  end;
end;

procedure TJvGroupHeaderEditor.Edit;
begin
  // We don't need to add band on double click
end;

{ TJvFooterEditor }

function TJvFooterEditor.GetVerbCount: integer;
begin
  Result := 5;
end;

function TJvFooterEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Add button';
    1: Result := '-';
    2: Result := 'MS Office 2000';
    3: Result := 'MS Enterprise Manager Wizard';
    4: Result := 'Dialog Mode';
  end;
end;

procedure TJvFooterEditor.ExecuteVerb(Index: integer);
var
  FButton: TJvFooterBtn;

begin
  case Index of
    0: Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50);
    1: ;
    2:
      begin
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'Help';
        FButton.Alignment := taLeftJustify;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'OK';
        FButton.Default := True;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'Cancel';
        FButton.Cancel := True;
      end;
    3:
      begin
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'Previous';
        FButton.SpaceInterval := 0;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'Next';
        FButton.Default := True;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'Close';
      end;
    4:
      begin
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'OK';
        FButton.SpaceInterval := 0;
        FButton.Alignment := taCenter;
      end;
  end;
end;

procedure TJvFooterEditor.Edit;
begin
  // We don't need to add band on double click
end;

end.
