{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSIMDUtils.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSIMDModifyForm;

interface

{$I jedi.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, JclSysInfo, JvSIMDUtils, ToolsApi;

type
  TJvSIMDModifyFrm = class(TForm)
    ComboBoxDisplay: TComboBox;
    ComboBoxFormat: TComboBox;
    LabelDisplay: TLabel;
    LabelFormat: TLabel;
    LabelBlank: TLabel;
    PanelModify: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBoxDisplayChange(Sender: TObject);
    procedure ComboBoxFormatChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    FDisplay: TJvXMMContentType;
    FFormat: TJvSIMDFormat;
    FXMMRegister: TJvXMMRegister;
    FServices: IOTAServices;
    FLabelList: TList;
    FHistory: TStringList;
  public
    function Execute(ADisplay: TJvXMMContentType; AFormat: TJvSIMDFormat;
                     var ARegister: TJvXMMRegister):Boolean;
    procedure UpdateDisplay;
    procedure UpdateFormat;
    procedure LoadHistory;
    procedure SaveHistory;
    procedure MergeHistory;
    function ModifyRegister: Boolean;
    property XMMRegister: TJvXMMRegister read FXMMRegister;
    property Display: TJvXMMContentType read FDisplay;
    property Format: TJvSIMDFormat read FFormat;
    property History: TStringList read FHistory;
    property Services: IOTAServices read FServices;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Registry;

const
  NbEdits: array [TJvXMMContentType] of Byte =
    ( 16, 8, 4, 2, 4, 2 );
  Texts: array [TJvXMMContentType] of string =
    ( 'Byte', 'Word', 'DWord', 'QWord', 'Single', 'Double' );
  HistoryRegKey = '\History Lists\hlSIMDModify';

{ TJvSIMDModifyFrm }

function TJvSIMDModifyFrm.Execute(ADisplay: TJvXMMContentType;
  AFormat: TJvSIMDFormat; var ARegister: TJvXMMRegister): Boolean;
begin
  FXMMRegister := ARegister;
  FFormat := AFormat;
  FDisplay := ADisplay;
  FHistory := TStringList.Create;
  FHistory.Duplicates := dupIgnore;
  try
    Assert(Supports(BorlandIDEServices,IOTAServices,FServices),
      'Unable to get Borland IDE Services');
    LoadHistory;

    ComboBoxDisplay.ItemIndex := Integer(Display);
    ComboBoxFormat.Enabled := Display in [xt16Bytes..xt2QWords];
    ComboBoxFormat.ItemIndex := Integer(Format);
    UpdateDisplay;

    Result := ShowModal = mrOk;

    if Result then
      ARegister := XMMRegister;
      
    MergeHistory;
    SaveHistory;
  finally
    FHistory.Free;
  end;
end;

procedure TJvSIMDModifyFrm.UpdateDisplay;
var
  Index: Integer;
  AComboBox: TComboBox;
  ALabel: TLabel;
  X, Y: Integer;
begin
  MergeHistory;
  while PanelModify.ControlCount>0 do
    PanelModify.Controls[0].Free;
  FLabelList.Clear;

  ComboBoxDisplay.ItemIndex := Integer(Display);
  ComboBoxFormat.Enabled := Display in [xt16Bytes..xt2QWords];
  ComboBoxFormat.ItemIndex := Integer(Format);

  X := 0;
  Y := 12;
  for Index := 0 to NbEdits[Display]-1 do
  begin
    AComboBox := TComboBox.Create(Self);
    AComboBox.Parent := PanelModify;
    AComboBox.SetBounds(X+130,Y,90,AComboBox.Height);
    AComboBox.Tag := Index;
    AComboBox.Text := '';
    AComboBox.Items.Assign(History);
    ALabel := TLabel.Create(Self);
    ALabel.Parent := PanelModify;
    ALabel.SetBounds(X+5,Y+2,60,ALabel.Height);
    ALabel.Tag := Index;
    FLabelList.Add(Pointer(ALabel));
    if (Index=7) then
    begin
      Y := 12;
      X := 230;
    end else Inc(Y,32);
  end;
  UpdateFormat;
end;

procedure TJvSIMDModifyFrm.UpdateFormat;
var
  Index: Integer;
  Value: TJvSIMDValue;
begin
  Value.Display := Display;
  for Index := 0 to FLabelList.Count-1 do
  begin
    with TLabel(FLabelList.Items[Index]) do
      case Display of
        xt16Bytes  : Value.ValueByte := XMMRegister.Bytes[Tag];
        xt8Words   : Value.ValueWord := XMMRegister.Words[Tag];
        xt4DWords  : Value.ValueDWord := XMMRegister.DWords[Tag];
        xt2QWords  : Value.ValueQWord := XMMRegister.QWords[Tag];
        xt4Singles : Value.ValueSingle := XMMRegister.Singles[Tag];
        xt2Doubles : Value.ValueDouble := XMMRegister.Doubles[Tag];
      end;
    TLabel(FLabelList.Items[Index]).Caption := SysUtils.Format('%s%d = %s',[Texts[Display],Index,FormatValue(Value,Format)]);
  end;
end;

procedure TJvSIMDModifyFrm.FormCreate(Sender: TObject);
begin
  FLabelList := TList.Create;
end;

procedure TJvSIMDModifyFrm.FormDestroy(Sender: TObject);
begin
  FLabelList.Free;
end;

procedure TJvSIMDModifyFrm.ComboBoxDisplayChange(Sender: TObject);
begin
  FDisplay := TJvXMMContentType((Sender as TComboBox).ItemIndex);
  UpdateDisplay;
end;

procedure TJvSIMDModifyFrm.ComboBoxFormatChange(Sender: TObject);
begin
  FFormat := TJvSIMDFormat((Sender as TComboBox).ItemIndex);
  UpdateFormat;
end;

procedure TJvSIMDModifyFrm.LoadHistory;
var
  Registry: TRegistry;
  Index, Count: Integer;
begin
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey(Services.GetBaseRegistryKey + HistoryRegKey, False) then
    begin
      Count := Registry.ReadInteger('Count');
      History.Clear;
      for Index := 0 to Count-1 do
        History.Add(Registry.ReadString(SysUtils.Format('Item%d',[Index])));
    end;
    Registry.CloseKey;
  finally
    Registry.Free;
  end;
end;

procedure TJvSIMDModifyFrm.SaveHistory;
var
  Registry: TRegistry;
  Index: Integer;
begin
  Registry := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey(Services.GetBaseRegistryKey + HistoryRegKey, True) then
    begin
      Registry.WriteInteger('Count',History.Count);
      for Index := 0 to History.Count-1 do
        Registry.WriteString(SysUtils.Format('Item%d',[Index]),History.Strings[Index]);
    end;
    Registry.CloseKey;
  finally
    Registry.Free;
  end;
end;

procedure TJvSIMDModifyFrm.MergeHistory;
var
  i, j: Integer;
begin
  History.Duplicates := dupIgnore;
  for i := 0 to PanelModify.ControlCount-1 do
    if PanelModify.Controls[i] is TComboBox then
      with TComboBox(PanelModify.Controls[i]) do
  begin
    for j := 0 to Items.Count-1 do
      if (Items.Strings[j]<>'') and (History.IndexOf(Items.Strings[j])=-1) then
        History.Add(Items.Strings[j]);
    if (Text<>'') and (History.IndexOf(Text)=-1) then
      History.Add(Text);
  end;
  while (History.Count>30) do
    History.Delete(0);
end;

function TJvSIMDModifyFrm.ModifyRegister: Boolean;
var
  Index: Integer;
  Value: TJvSIMDValue;
begin
  Result := False;
  Value.Display := Display;
  for Index := 0 to PanelModify.ControlCount-1 do
    if PanelModify.Controls[Index] is TComboBox then
      with TComboBox(PanelModify.Controls[Index]) do
        if Text<>'' then
  begin
    case Display of
      xt16Bytes  : Value.ValueByte := XMMRegister.Bytes[Tag];
      xt8Words   : Value.ValueWord := XMMRegister.Words[Tag];
      xt4DWords  : Value.ValueDWord := XMMRegister.DWords[Tag];
      xt2QWords  : Value.ValueQWord := XMMRegister.QWords[Tag];
      xt4Singles : Value.ValueSingle := XMMRegister.Singles[Tag];
      xt2Doubles : Value.ValueDouble := XMMRegister.Doubles[Tag];
    end;
    if ParseValue(Text,Value,Format) then
      case Display of
        xt16Bytes  : FXMMRegister.Bytes[Tag] := Value.ValueByte;
        xt8Words   : FXMMRegister.Words[Tag] := Value.ValueWord;
        xt4DWords  : FXMMRegister.DWords[Tag] := Value.ValueDWord;
        xt2QWords  : FXMMRegister.QWords[Tag] := Value.ValueQWord;
        xt4Singles : FXMMRegister.Singles[Tag] := Value.ValueSingle;
        xt2Doubles : FXMMRegister.Doubles[Tag] := Value.ValueDouble;
      end
    else
    begin
      FocusControl(TComboBox(PanelModify.Controls[Index]));
      SelStart := 0;
      SelLength := Length(Text);
      Exit;
    end;
  end;
  Result := True;
end;

procedure TJvSIMDModifyFrm.ButtonOKClick(Sender: TObject);
begin
  if (ModifyRegister) then
    ModalResult := mrOk;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
