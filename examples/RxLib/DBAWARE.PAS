{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit DbAware;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, DB, DBTables, Grids, DBGrids, JvPicClip, JvBDEQuery,
  JvFormPlacement, JvComponent, JvBDEFilter, JvBDEQBE, StdCtrls, ExtCtrls,
  Buttons, DBCtrls, JvBDEIndex, JvDBLookup, Mask, JvLabel, JvDBControls,
  ComCtrls, TabNotBk, JvExControls, JvDBGrid, JvExStdCtrls, JvExDBGrids;

type
  TDBAwareForm = class(TForm)
    TabbedNotebook1: TTabbedNotebook;
    DataSource1: TDataSource;
    Table2: TTable;
    Table2SpeciesNo: TFloatField;
    Table2Category: TStringField;
    Table2Common_Name: TStringField;
    Table2Lengthcm: TFloatField;
    Table2Notes: TMemoField;
    Table2Graphic: TGraphicField;
    GroupBox1: TGroupBox;
    rxDBGrid1: TJvDBGrid ;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    GroupBox2: TGroupBox;
    DBStatusLabel1: TJvDBStatusLabel ;
    DBNavigator1: TDBNavigator;
    Label2: TLabel;
    DataSource2: TDataSource;
    QBEQuery1: TJvQBEQuery ;
    GroupBox3: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TMaskEdit;
    Label6: TLabel;
    Table1: TTable;
    DataSource3: TDataSource;
    GroupBox4: TGroupBox;
    rxDBGrid2: TJvDBGrid ;
    DBIndexCombo1: TJvDBIndexCombo ;
    Label7: TLabel;
    Table3: TTable;
    DataSource4: TDataSource;
    DBFilter1: TJvDBFilter ;
    Panel2: TPanel;
    Panel3: TPanel;
    ScrollBox: TScrollBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    EditCompany: TDBEdit;
    EditCity: TDBEdit;
    EditState: TDBEdit;
    EditZip: TDBEdit;
    EditCountry: TDBEdit;
    Label14: TLabel;
    EditPhone: TDBEdit;
    GroupBox6: TGroupBox;
    DBNavigator: TDBNavigator;
    GroupBox7: TGroupBox;
    Label15: TLabel;
    GroupBox5: TGroupBox;
    EnterQuery: TSpeedButton;
    ExecQuery: TSpeedButton;
    CancelQuery: TSpeedButton;
    RadioGroup1: TRadioGroup;
    EditCustNo: TDBEdit;
    rxDBLookupCombo1: TJvDBLookupCombo ;
    FormStorage1: TJvFormStorage ;
    Table1OrderNo: TFloatField;
    Table1ItemNo: TFloatField;
    Table1PartNo: TFloatField;
    Table1Qty: TIntegerField;
    Table1Discount: TFloatField;
    GroupBox8: TGroupBox;
    Label16: TLabel;
    Panel4: TPanel;
    Label17: TLabel;
    Label19: TLabel;
    rxDBLookupCombo2: TJvDBLookupCombo ;
    ComboBox2: TComboBox;
    DBGrid1: TJvDBGrid ;
    Table4: TTable;
    DataSource6: TDataSource;
    rxQuery1: TJvQuery ;
    DataSource5: TDataSource;
    DBStatusLabel2: TJvDBStatusLabel ;
    JvPicclip: TJvPicClip ;
    Label1: TLabel;
    CheckBox2: TCheckBox;
    rxQuery1OrderNo: TFloatField;
    rxQuery1SaleDate: TDateTimeField;
    rxQuery1Company: TStringField;
    rxQuery1CustNo: TFloatField;
    rxQuery1ShipDate: TDateTimeField;
    rxQuery1ShipToContact: TStringField;
    rxQuery1ShipToAddr1: TStringField;
    rxQuery1ShipToAddr2: TStringField;
    rxQuery1ShipToCity: TStringField;
    rxQuery1ShipToState: TStringField;
    rxQuery1ShipToZip: TStringField;
    rxQuery1ShipToCountry: TStringField;
    rxQuery1ShipToPhone: TStringField;
    rxQuery1ShipVIA: TStringField;
    rxQuery1PO: TStringField;
    rxQuery1Terms: TStringField;
    rxQuery1PaymentMethod: TStringField;
    rxQuery1ItemsTotal: TCurrencyField;
    rxQuery1TaxRate: TFloatField;
    rxQuery1Freight: TCurrencyField;
    rxQuery1AmountPaid: TCurrencyField;
    rxQuery1EmpNo: TIntegerField;
    rxQuery1LastName: TStringField;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure rxDBLookupCombo1Change(Sender: TObject);
    procedure EnterQueryClick(Sender: TObject);
    procedure ExecQueryClick(Sender: TObject);
    procedure CancelQueryClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure DBFilter1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure rxDBLookupCombo2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rxDBGrid1GetCellParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; Highlight: Boolean);
    procedure rxDBLookupCombo1GetImage(Sender: TObject; IsEmpty: Boolean; 
      var Graphic: TGraphic; var TextMargin: Integer);
    procedure DBGrid1CheckButton(Sender: TObject; ACol: Longint;
      Field: TField; var Enabled: Boolean);
    procedure DBGrid1TitleBtnClick(Sender: TObject; ACol: Longint;
      Field: TField);
    procedure CheckBox2Click(Sender: TObject);
    procedure DBGrid1GetBtnParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
      IsDown: Boolean);
    procedure rxDBGrid1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    procedure SetMacro(const MacroName, Value: string);
  public
    { Public declarations }
  end;

implementation

uses JvDBUtils, Main;

{$R *.DFM}

procedure TDBAwareForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TDBAwareForm.CheckBox1Click(Sender: TObject);
begin
  rxDBGrid1.ShowGlyphs := CheckBox1.Checked;
end;

procedure TDBAwareForm.rxDBLookupCombo1Change(Sender: TObject);
begin
  Edit1.Text := rxDBLookupCombo1.Value;
end;

procedure TDBAwareForm.EnterQueryClick(Sender: TObject);
begin
  DBFilter1.SetCapture;
end;

procedure TDBAwareForm.ExecQueryClick(Sender: TObject);
begin
  with DBFilter1 do begin
    ReadCaptureControls;
    ReleaseCapture;
    Activate;
  end;
end;

procedure TDBAwareForm.CancelQueryClick(Sender: TObject);
begin
  with DBFilter1 do begin
    ReleaseCapture;
    Deactivate;
  end;
end;

procedure TDBAwareForm.RadioGroup1Click(Sender: TObject);
begin
  DBFilter1.LogicCond := TFilterLogicCond(RadioGroup1.ItemIndex);
end;

procedure TDBAwareForm.DBFilter1Change(Sender: TObject);
begin
  ExecQuery.Enabled := DBFilter1.Captured;
  CancelQuery.Enabled := DBFilter1.Active or DBFilter1.Captured;
  EnterQuery.Enabled := True;
end;

procedure TDBAwareForm.SetMacro(const MacroName, Value: string);
begin
  with rxQuery1 do begin
    MacroByName(MacroName).AsString := Value;
    DisableControls;
    try
      Close;
      Open;
    finally
      EnableControls;
    end;
  end;
end;

procedure TDBAwareForm.ComboBox2Change(Sender: TObject);
var
  Order: string;
begin
  case ComboBox2.ItemIndex of
    0: Order := 'SaleDate';
    1: Order := 'ShipDate';
    2: Order := 'Company';
    3: Order := 'LastName';
  end;
  SetMacro('ORDER', Order);
end;

procedure TDBAwareForm.rxDBLookupCombo2Change(Sender: TObject);
var
  Value: string;
begin
  Value := TrueExpr;
  if rxDBLookupCombo2.Value <> '' then begin
    Value := 'ORDERS."CustNo"=' + rxDBLookupCombo2.Value;
  end;
  SetMacro('CUSTOMER', Value);
end;

procedure TDBAwareForm.FormCreate(Sender: TObject);
begin
  ComboBox2.ItemIndex := 1;
{$IFDEF WIN32}
  DBGrid1.Columns.State := csCustomized;
{$ENDIF}
end;

procedure TDBAwareForm.rxDBGrid1GetCellParams(Sender: TObject;
  Field: TField; AFont: TFont; var Background: TColor; Highlight: Boolean);
var
  Len: Integer;
begin
  if Field.FieldName = 'Category' then
    AFont.Style := AFont.Style + [fsBold]
  else if Field.FieldName = 'Length (cm)' then
    Background := clYellow;
  Len := (Sender as TJvDBGrid ).DataSource.DataSet.FieldByName('Length (cm)').AsInteger;
  if (Len <= 30) and (Len > 0) then
    Background := clLime  { shortest }
  else if Len >= 150 then
    AFont.Color := clRed;  { longest }
  if Highlight then begin
    AFont.Color := clHighlightText;
    Background := clHighlight;
  end;
end;

procedure TDBAwareForm.rxDBGrid1KeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = ' ') then rxDBGrid1.ToggleRowSelection;
end;

procedure TDBAwareForm.rxDBLookupCombo1GetImage(Sender: TObject;
  IsEmpty: Boolean; var Graphic: TGraphic; var TextMargin: Integer);
var
  PhoneExt: string;
begin
  TextMargin := JvPicclip.Width + 2;
  if not IsEmpty then begin
    PhoneExt := QBEQuery1.FieldByName('PhoneExt').AsString;
    Graphic := JvPicclip.GraphicCell[3];
    if Length(PhoneExt) = 0 then
      Graphic := JvPicclip.GraphicCell[4];
  end
  else Graphic := JvPicclip.GraphicCell[5];
end;

procedure TDBAwareForm.DBGrid1CheckButton(Sender: TObject; ACol: Longint;
  Field: TField; var Enabled: Boolean);
begin
  Enabled := (Field <> nil) and not (Field is TBlobField);
end;

procedure TDBAwareForm.DBGrid1TitleBtnClick(Sender: TObject; ACol: Longint;
  Field: TField);
begin
  if (Field <> nil) then begin
    SetMacro('ORDER', Field.FieldName);
    ComboBox2.ItemIndex := -1;
  end;
end;

procedure TDBAwareForm.CheckBox2Click(Sender: TObject);
begin
  rxDBGrid1.MultiSelect := CheckBox2.Checked;
end;

procedure TDBAwareForm.DBGrid1GetBtnParams(Sender: TObject; Field: TField;
  AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
  IsDown: Boolean);
begin
  if (Field <> nil) and rxQuery1.Active and (CompareText(Field.FieldName,
    rxQuery1.MacroByName('ORDER').AsString) = 0) then
    SortMarker := smDown;
end;

end.
