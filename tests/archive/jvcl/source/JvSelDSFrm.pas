{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSelDSFrm.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}


unit JvSelDSFrm;

interface

{$IFDEF DCS}

uses Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, DB, StdCtrls,
  RTLConsts, DesignIntf, DesignEditors, VCLEditors, JvDsgn;

type

{ TJvSelectDataSetForm }

  TJvSelectDataSetForm = class(TForm)
    GroupBox: TGroupBox;
    DataSetList: TListBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure DataSetListDblClick(Sender: TObject);
    procedure DataSetListKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FDesigner: IDesigner;
    FExclude: string;
    procedure FillDataSetList(ExcludeDataSet: TDataSet);
    procedure AddDataSet(const S: string);
  public
    { Public declarations }
  end;

{ TJvMemDataSetEditor }

  TJvMemDataSetEditor = class(TComponentEditor)
  private
    function UniqueName(Field: TField): string;
    procedure BorrowStructure;
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; virtual; abstract;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

function SelectDataSet(ADesigner: IDesigner; const ACaption: string; ExcludeDataSet: TDataSet): TDataSet;

{$ENDIF DCS}

implementation

{$IFDEF DCS}

uses DbConsts, TypInfo, JvVCLUtils, JvStrUtils, JvLConst,
  {$IFDEF COMPILER3_UP}{$IFDEF COMPILER5_UP} DsnDbCst, {$ELSE} BdeConst, {$ENDIF}{$ENDIF}
  DSDesign;

{$R *.DFM}

function SelectDataSet(ADesigner: IDesigner; const ACaption: string;
  ExcludeDataSet: TDataSet): TDataSet;
begin
  Result := nil;
  with TJvSelectDataSetForm.Create(Application) do
  try
    if ACaption <> '' then Caption := ACaption;
    FDesigner := ADesigner;
    FillDataSetList(ExcludeDataSet);
    if ShowModal = mrOk then
      if DataSetList.ItemIndex >= 0 then begin
        with DataSetList do  
{$IFDEF WIN32}
          Result := FDesigner.GetComponent(Items[ItemIndex]) as TDataSet;
{$ELSE}
          Result := FDesigner.Form.FindComponent(Items[ItemIndex]) as TDataSet;
{$ENDIF}
      end;
  finally
    Free;
  end;
end;

{ TJvMemDataSetEditor }

procedure TJvMemDataSetEditor.BorrowStructure;
var
  DataSet: TDataSet;
  I: Integer;
  Caption: string;
begin
  Caption := Component.Name;
  if (Component.Owner <> nil) and (Component.Owner.Name <> '') then
    Caption := Format({$IFDEF CBUILDER} '%s->%s' {$ELSE} '%s.%s' {$ENDIF},
      [Component.Owner.Name, Caption]);
  DataSet := SelectDataSet(Designer, Caption, TDataSet(Component));
  if DataSet <> nil then begin
    StartWait;
    try
      if not CopyStructure(DataSet, Component as TDataSet) then Exit;
      with TDataSet(Component) do begin
        for I := 0 to FieldCount - 1 do
          if Fields[I].Name = '' then 
            Fields[I].Name := UniqueName(Fields[I]);
      end;
    finally
      StopWait;
    end;
    Designer.Modified;
  end;
end;

function TJvMemDataSetEditor.UniqueName(Field: TField): string;
const
  AlphaNumeric = ['A'..'Z', 'a'..'z', '_'] + ['0'..'9'];
var
  Temp: string;
  Comp: TComponent;
  I: Integer;
begin
  Result := '';
  if (Field <> nil) then begin
    Temp := Field.FieldName;
    for I := Length(Temp) downto 1 do
      if not (Temp[I] in AlphaNumeric) then System.Delete(Temp, I, 1);
    if (Temp = '') or not IsValidIdent(Temp) then begin
      Temp := Field.ClassName;
      if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
        System.Delete(Temp, 1, 1);
    end;
  end
  else Exit;
  Temp := Component.Name + Temp;
{$IFDEF WIN32}
  Comp := Designer.GetComponent(Temp);
  if (Comp = nil) or (Comp = Field) then Result := Temp
  else Result := Designer.UniqueName(Temp);
{$ELSE}
  I := 0;
  repeat
    Result := Temp;
    if I > 0 then Result := Result + IntToStr(I);
    Comp := Designer.Form.FindComponent(Result);
    Inc(I);
  until (Comp = nil) or (Comp = Field);
{$ENDIF}
end;

procedure TJvMemDataSetEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
{$IFDEF COMPILER5_UP}
    0: ShowFieldsEditor(Designer, TDataSet(Component), TDSDesigner);
{$ELSE}
    0: ShowDatasetDesigner(Designer, TDataSet(Component));
{$ENDIF}
    1: BorrowStructure;
  end;
end;

function TJvMemDataSetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := ResStr(SDatasetDesigner);
    1: Result := LoadStr(srBorrowStructure);
  end;
end;

function TJvMemDataSetEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TJvSelectDataSetForm }

procedure TJvSelectDataSetForm.AddDataSet(const S: string);
begin
  if (S <> '') and (S <> FExclude) then DataSetList.Items.Add(S);
end;

procedure TJvSelectDataSetForm.FillDataSetList(ExcludeDataSet: TDataSet);
{$IFNDEF WIN32}
var
  I: Integer;
  Component: TComponent;
{$ENDIF}
begin
  DataSetList.Items.BeginUpdate;
  try
    DataSetList.Clear;
    FExclude := '';
    if ExcludeDataSet <> nil then FExclude := ExcludeDataSet.Name;
{$IFDEF WIN32}
    FDesigner.GetComponentNames(GetTypeData(TypeInfo(TDataSet)), AddDataSet);
{$ELSE}
    for I := 0 to FDesigner.Form.ComponentCount - 1 do begin
      Component := FDesigner.Form.Components[I];
      if (Component is TDataSet) and (Component <> ExcludeDataSet) then
        AddDataSet(Component.Name);
    end;
{$ENDIF}
    with DataSetList do begin
      if Items.Count > 0 then ItemIndex := 0;
      Enabled := Items.Count > 0;
      OkBtn.Enabled := (ItemIndex >= 0);
    end;
  finally
    DataSetList.Items.EndUpdate;
  end;
end;

procedure TJvSelectDataSetForm.DataSetListDblClick(Sender: TObject);
begin
  if DataSetList.ItemIndex >= 0 then ModalResult := mrOk;
end;

procedure TJvSelectDataSetForm.DataSetListKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = #13) and (DataSetList.ItemIndex >= 0) then
    ModalResult := mrOk;
end;

{$ENDIF DCS}

end.
