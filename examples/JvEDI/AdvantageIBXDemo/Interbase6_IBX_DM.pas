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
unit Interbase6_IBX_DM;

interface

uses
  SysUtils, Classes, DB, IBCustomDataSet, IBDatabase, JvComponent,
  JvEDIDBBuffering, IBTable, IBQuery;

type
  TInterbase6_IBX_Data = class(TDataModule)
    JvEDIDBBuffer: TJvEDIDBBuffer;
    ibdDB: TIBDatabase;
    ibdTrans: TIBTransaction;
    EProfile: TIBDataSet;
    SProfile: TIBDataSet;
    LProfile: TIBDataSet;
    dsEProfile: TDataSource;
    dsSProfile: TDataSource;
    dsLProfile: TDataSource;
    IBTable: TIBTable;
    IBQuery: TIBQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure JvEDIDBBufferAfterOpenDataSets(Sender: TObject);
    procedure JvEDIDBBufferBeforeOpenDataSets(Sender: TObject);
    procedure JvEDIDBBufferCreateTable(FieldDefs: TJvEDIFieldDefs;
      TableName: String);
    procedure JvEDIDBBufferTableExists(TableName: String;
      var TableExists: Boolean);
    procedure JvEDIDBBufferAfterCloseDataSets(Sender: TObject);
    procedure JvEDIDBBufferBeforeApplyElementFilter(DataSet: TDataSet;
      TableName: String; var ApplyFilter: Boolean);
    procedure JvEDIDBBufferCheckForFieldChanges(FieldDefs: TJvEDIFieldDefs;
      TableName: String);
    procedure JvEDIDBBufferAlterTable(FieldDefs: TJvEDIFieldDefs;
      TableName: String);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure RefreshProfileData;    
  end;

var
  Interbase6_IBX_Data: TInterbase6_IBX_Data;

implementation

uses
  Forms, UnitMain, JclStrings;

{$R *.dfm}

procedure TInterbase6_IBX_Data.DataModuleCreate(Sender: TObject);
begin
  ibdDB.DatabaseName := ExtractFilePath(Application.ExeName) + 'DB_Interbase6\EDISDKDB.gdb';
  ibdDB.Open;
end;

procedure TInterbase6_IBX_Data.DataModuleDestroy(Sender: TObject);
begin
  if EProfile.Active then
    EProfile.Close;
  if SProfile.Active then
    SProfile.Close;
  if LProfile.Active then
    LProfile.Close;
  if ibdDB.Connected then
    ibdDB.Close;
end;

procedure TInterbase6_IBX_Data.JvEDIDBBufferAfterOpenDataSets(Sender: TObject);
begin
  FormMain.pb.Min := 0;
  FormMain.pb.Position := 0;
  SProfile.Last;
  LProfile.Last;
  FormMain.pb.Max := SProfile.RecordCount + LProfile.RecordCount;
  SProfile.First;
  LProfile.First;
end;

procedure TInterbase6_IBX_Data.JvEDIDBBufferBeforeOpenDataSets(Sender: TObject);
begin
  if not ibdTrans.InTransaction then
    ibdTrans.StartTransaction;

  if EProfile.Active then
    EProfile.Close;
  EProfile.SelectSQL.Clear;
  EProfile.SelectSQL.Add(' select * from EDI_ELEMENT_PROFILE');
  EProfile.SelectSQL.Add(' where SEGMENTID = :TableName');
  EProfile.Prepare;
end;

procedure TInterbase6_IBX_Data.JvEDIDBBufferCreateTable(FieldDefs: TJvEDIFieldDefs;
  TableName: String);
var
  TableSQL: string;
  KeyField, Constraint: string;
  I: Integer;
begin
  KeyField := '';
  Constraint := '';
  TableSQL := 'CREATE TABLE ' + AnsiUpperCase(TableName) + ' (' + AnsiCrLf;
  for I := 0 to FieldDefs.Count - 1 do
  begin
    case FieldDefs[I].DataType of
      ftInteger: TableSQL := TableSQL + '  ' + FieldDefs[I].FieldName + ' INTEGER';
      ftFloat:   TableSQL := TableSQL + '  ' + FieldDefs[I].FieldName + ' FLOAT';
      ftString:  TableSQL := TableSQL + '  ' + FieldDefs[I].FieldName + ' VARCHAR(' +
        IntToStr(FieldDefs[I].MaximumLength) + ') CHARACTER SET ASCII COLLATE ASCII';
      ftDate:    TableSQL := TableSQL + '  ' + FieldDefs[I].FieldName + ' DATE';
      ftTime:    TableSQL := TableSQL + '  ' + FieldDefs[I].FieldName + ' TIME';
      ftBlob:    {};
    end; //case

    if FieldDefs[I].FieldType = FieldType_PKey then
      TableSQL := TableSQL + ' NOT NULL';

    if (FieldDefs.Count > 1) or (KeyField <> '') then
      TableSQL := TableSQL + ',' + AnsiCrLf;

    if FieldDefs[I].FieldType = FieldType_PKey then
    begin
      KeyField := FieldDefs[I].FieldName;
      Constraint := '  CONSTRAINT PK_' + AnsiUpperCase(TableName) +
        ' PRIMARY KEY (' + FieldDefs[I].FieldName + ')' + AnsiCrLf;
    end;

  end; //for I
  TableSQL := TableSQL + Constraint + ');' + AnsiCrLf;
  TableSQL := TableSQL + 'CREATE GENERATOR ' + KeyField + '_GEN;' + AnsiCrLf;
  TableSQL := TableSQL + '   SET GENERATOR ' + KeyField + '_GEN TO 0;';

  FormMain.Memo1.Lines.Add(TableSQL + AnsiCrLf);
end;

procedure TInterbase6_IBX_Data.JvEDIDBBufferTableExists(TableName: String;
  var TableExists: Boolean);
var
  I: Integer;
  TableList: TStringList;
begin
  TableList := TStringList.Create;
  TableList.Sorted := True;
  ibdDB.GetTableNames(TableList);
  TableExists := TableList.Find(TableName, I);
  TableList.Free;
  FormMain.pb.StepBy(1);
  Application.ProcessMessages;
end;

procedure TInterbase6_IBX_Data.RefreshProfileData;
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

procedure TInterbase6_IBX_Data.JvEDIDBBufferAfterCloseDataSets(Sender: TObject);
begin
  ibdTrans.Commit;

  if EProfile.Active then
    EProfile.Close;
  if EProfile.Prepared then
    EProfile.UnPrepare;
  EProfile.SelectSQL.Clear;
  EProfile.SelectSQL.Add(' select * from EDI_ELEMENT_PROFILE');

  ibdTrans.Active := True;
end;

procedure TInterbase6_IBX_Data.JvEDIDBBufferBeforeApplyElementFilter(
  DataSet: TDataSet; TableName: String; var ApplyFilter: Boolean);
begin
  if DataSet.Active then
    DataSet.Close;
  TIBDataSet(DataSet).ParamByName('TableName').AsString := TableName;
  DataSet.Open;
end;

procedure TInterbase6_IBX_Data.JvEDIDBBufferCheckForFieldChanges(
  FieldDefs: TJvEDIFieldDefs; TableName: String);
var
  I: Integer;
  Field: TField;
begin
  if IBQuery.Active then
    IBQuery.Close;
  IBQuery.SQL.Clear;
  IBQuery.SQL.Add('select * from ' + TableName);
  IBQuery.Open;
  //
  for I := 0 to FieldDefs.Count - 1 do
  begin
    Field := IBQuery.FindField(FieldDefs[I].FieldName);
    if Field = nil then //Check if field exists
    begin
      FieldDefs[I].UpdateStatus := usInserted;
    end
    else //Check for modifications
    begin
      if FieldDefs[I].DataType <> Field.DataType then
      begin
        FormMain.Memo1.Lines.Add('Type Change: ' + TableName + ' - ' + FieldDefs[I].FieldName);
      end
      else
      begin
        if (FieldDefs[I].DataType = ftString) and
          (FieldDefs[I].MaximumLength > Field.Size) then
        begin
          FieldDefs[I].MaximumLength := FieldDefs[I].MaximumLength;
          FieldDefs[I].UpdateStatus := usModified;
        end; //if
      end; //if
    end; //if
  end; //for I
  //
  if IBQuery.Active then
    IBQuery.Close;
end;

procedure TInterbase6_IBX_Data.JvEDIDBBufferAlterTable(
  FieldDefs: TJvEDIFieldDefs; TableName: String);
var
  AlterSQL: string;
  I: Integer;
begin
  AlterSQL := '';
  for I := 0 to FieldDefs.Count - 1 do
  begin
    case FieldDefs[I].UpdateStatus of
      usInserted:
      begin
        AlterSQL := 'ALTER TABLE ' + TableName + ' ADD ' + FieldDefs[I].FieldName;
        case FieldDefs[I].DataType of
          ftInteger: AlterSQL := AlterSQL + ' INTEGER;';
          ftFloat: AlterSQL := AlterSQL + ' FLOAT;';
          ftString: AlterSQL := AlterSQL + ' VARCHAR(' + IntToStr(FieldDefs[I].MaximumLength) + ') CHARACTER SET ASCII COLLATE ASCII;';
          ftDate: AlterSQL := AlterSQL + ' DATE;';
          ftTime: AlterSQL := AlterSQL + ' TIME;';
          ftBlob: {};
        end; //case
        FormMain.Memo1.Lines.Add(AlterSQL + AnsiCrLf);
      end;
      usModified:
      begin
        AlterSQL := 'ALTER TABLE ' + TableName + ' ALTER ' + FieldDefs[I].FieldName + ' TYPE ';
        case FieldDefs[I].DataType of
          ftInteger: AlterSQL := AlterSQL + ' INTEGER;';
          ftFloat: AlterSQL := AlterSQL + ' FLOAT;';
          ftString: AlterSQL := AlterSQL + ' VARCHAR(' + IntToStr(FieldDefs[I].MaximumLength) +
            ') CHARACTER SET ASCII;';
          ftDate: AlterSQL := AlterSQL + ' DATE;';
          ftTime: AlterSQL := AlterSQL + ' TIME;';
          ftBlob: {};
        end; //case
        FormMain.Memo1.Lines.Add(AlterSQL + AnsiCrLf);
      end;
    end; //case
  end;
end;

end.
