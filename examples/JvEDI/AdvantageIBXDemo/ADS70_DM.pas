{**************************************************************************************************}
{                                                                                                  }
{ Ray's Jedi Projects                                                                              }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit is part of EDI SDK demos.                                                              }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: Before October, 1, 2003                                                            }
{ Last modified: April 2, 2004                                                                     }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3@hotmail.com                                                           }
{   For latest EDI specific updates see http://sourceforge.net/projects/edisdk                     }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}
unit ADS70_DM;

interface

uses
  SysUtils, Classes, Forms, DB, adsdata, adsfunc, adstable, adscnnct,
  adsdictionary, JvComponent, JvEDIDBBuffering;

type
  TADS70_Data = class(TDataModule)
    adsD: TAdsDictionary;
    adsC: TAdsConnection;
    LProfile: TAdsQuery;
    SProfile: TAdsQuery;
    EProfile: TAdsQuery;
    dsLProfile: TDataSource;
    dsSProfile: TDataSource;
    dsEProfile: TDataSource;
    JvEDIDBBuffer: TJvEDIDBBuffer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure JvEDIDBBufferAfterOpenDataSets(Sender: TObject);
    procedure JvEDIDBBufferCheckForFieldChanges(FieldDefs: TJvEDIFieldDefs;
      TableName: String);
    procedure JvEDIDBBufferCreateTable(FieldDefs: TJvEDIFieldDefs;
      TableName: String);
    procedure JvEDIDBBufferTableExists(TableName: String;
      var TableExists: Boolean);
    procedure JvEDIDBBufferAlterTable(FieldDefs: TJvEDIFieldDefs;
      TableName: String);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure RefreshProfileData;
  end;

var
  ADS70_Data: TADS70_Data;

implementation

{$R *.dfm}

uses
  UnitMain, ace, JclEDI;

procedure TADS70_Data.DataModuleCreate(Sender: TObject);
begin
  adsC.ConnectPath := ExtractFilePath(Application.ExeName) + '\DB_ADS70\EDISDKDB.Add';
  adsC.Connect;
  adsD.ConnectPath := ExtractFilePath(Application.ExeName) + '\DB_ADS70\EDISDKDB.Add';
  adsD.Connect;
end;

procedure TADS70_Data.DataModuleDestroy(Sender: TObject);
begin
  if EProfile.Active then
    EProfile.Close;
  if SProfile.Active then
    SProfile.Close;
  if LProfile.Active then
    LProfile.Close;
  if adsD.IsConnected then
    adsD.Disconnect;
  if adsC.IsConnected then
    adsC.Disconnect;
end;

procedure TADS70_Data.RefreshProfileData;
begin
  if EProfile.Active then
    EProfile.Close;
  EProfile.Open;
  if SProfile.Active then
    SProfile.Close;
  SProfile.Open;
  if LProfile.Active then
    LProfile.Close;
  LProfile.Open;
end;

procedure TADS70_Data.JvEDIDBBufferAfterOpenDataSets(Sender: TObject);
begin
  FormMain.pb.Min := 0;
  FormMain.pb.Position := 0;
  FormMain.pb.Max := SProfile.RecordCount + LProfile.RecordCount;
end;

procedure TADS70_Data.JvEDIDBBufferCheckForFieldChanges(FieldDefs: TJvEDIFieldDefs; TableName: String);
var
  adsTable: TAdsTable;
  I: Integer;
  adsField: TField;
begin
  adsTable := TAdsTable.Create(nil);
  adsTable.DatabaseName := adsC.Name;
  adsTable.TableName := TableName;
  adsTable.Open;
  //
  for I := 0 to FieldDefs.Count - 1 do
  begin
    adsField := adsTable.FindField(FieldDefs[I].FieldName);
    if adsField = nil then //Check if field exists
    begin
      FieldDefs[I].UpdateStatus := usInserted;
    end
    else //Check for modifications
    begin
      if FieldDefs[I].DataType <> adsField.DataType then
      begin
        FormMain.Memo1.Lines.Add('Type Change: ' + TableName + ' - ' + FieldDefs[I].FieldName);
      end
      else
      begin
        if (FieldDefs[I].DataType = ftString) and
          (FieldDefs[I].MaximumLength > adsField.Size) then
        begin
          FieldDefs[I].MaximumLength := FieldDefs[I].MaximumLength;
          FieldDefs[I].UpdateStatus := usModified;
        end; //if
      end; //if
    end; //if
  end; //for I
  //
  if adsTable.Active then
    adsTable.Close;
  adsTable.Free;
end;

procedure TADS70_Data.JvEDIDBBufferCreateTable(FieldDefs: TJvEDIFieldDefs; TableName: String);
var
  TablePath: string;
  adsTable: TAdsTable;
  TableFields: string;
  I: Integer;
begin
  TableFields := '';
  for I := 0 to FieldDefs.Count - 1 do
  begin
    case FieldDefs[I].DataType of
      ftInteger: TableFields := TableFields + FieldDefs[I].FieldName + ',Integer' + ';';
      ftFloat: TableFields := TableFields + FieldDefs[I].FieldName + ',Double,8' + ';';
      ftString: TableFields := TableFields + FieldDefs[I].FieldName + ',Character,' +
        IntToStr(FieldDefs[I].MaximumLength) + ';';
      ftDate: TableFields := TableFields + FieldDefs[I].FieldName + ',Date' + ';';
      ftTime: TableFields := TableFields + FieldDefs[I].FieldName + ',Time' + ';';
      ftBlob: {};
    end; //case
  end;
  FormMain.Memo1.Lines.Add('Add Table: ' + TableName + ' - ' + TableFields);
  //
  TablePath := adsC.GetConnectionPath + '\' + TableName + '.adt';
  adsTable := TAdsTable.Create(nil);
  adsTable.DatabaseName := adsC.Name;
  adsTable.AdsCreateTable(TablePath, ttAdsADT, ANSI, ADS_DEFAULT, TableFields);
  adsTable.Free;
end;

procedure TADS70_Data.JvEDIDBBufferTableExists(TableName: String; var TableExists: Boolean);
var
  I: Integer;
  TableList: TStringList;
begin
  TableList := TStringList.Create;
  TableList.Sorted := True;
  adsD.GetTableNames(TableList);
  TableExists := TableList.Find(TableName, I);
  TableList.Free;
  FormMain.pb.StepBy(1);
  Application.ProcessMessages;
end;

procedure TADS70_Data.JvEDIDBBufferAlterTable(FieldDefs: TJvEDIFieldDefs; TableName: String);
var
  adsTable: TAdsTable;
  AddFields, DeleteFields, ChangeFields: string;
  I: Integer;
begin
  adsTable := TAdsTable.Create(nil);
  adsTable.DatabaseName := adsC.Name;
  adsTable.TableName := TableName;
  adsTable.Open;

  AddFields := '';
  DeleteFields := '';
  ChangeFields := '';
  for I := 0 to FieldDefs.Count - 1 do
  begin
    case FieldDefs[I].UpdateStatus of
      usInserted:
      begin
        FormMain.Memo1.Lines.Add('Add Field: ' + TableName + ' - ' + FieldDefs[I].FieldName);
        case FieldDefs[I].DataType of
          ftInteger: AddFields := AddFields + FieldDefs[I].FieldName + ',Integer' + ';';
          ftFloat: AddFields := AddFields + FieldDefs[I].FieldName + ',Double,8' + ';';
          ftString: AddFields := AddFields + FieldDefs[I].FieldName + ',Character,' +
            IntToStr(FieldDefs[I].MaximumLength) + ';';
          ftDate: AddFields := AddFields + FieldDefs[I].FieldName + ',Date' + ';';
          ftTime: AddFields := AddFields + FieldDefs[I].FieldName + ',Time' + ';';
          ftBlob: {};
        end; //case
      end;
      usModified:
      begin
        FormMain.Memo1.Lines.Add('Chg Field: ' + TableName + ' - ' + FieldDefs[I].FieldName);
        case FieldDefs[I].DataType of
          ftInteger: ChangeFields := ChangeFields + FieldDefs[I].FieldName + ',Integer' + ';';
          ftFloat: ChangeFields := ChangeFields + FieldDefs[I].FieldName + ',Double,8' + ';';
          ftString: ChangeFields := ChangeFields + FieldDefs[I].FieldName + ',Character,' +
            IntToStr(FieldDefs[I].MaximumLength) + ';';
          ftDate: ChangeFields := ChangeFields + FieldDefs[I].FieldName + ',Date' + ';';
          ftTime: ChangeFields := ChangeFields + FieldDefs[I].FieldName + ',Time' + ';';
          ftBlob: {};
        end; //case
      end;
    end; //case
  end;

  if adsTable.Active then
    adsTable.Close;
  if (AddFields <> '') or (DeleteFields <> '') or (ChangeFields <> '') then
  begin
    adsTable.Restructure(AddFields, DeleteFields, ChangeFields);
  end;
  adsTable.Free;
end;

end.
