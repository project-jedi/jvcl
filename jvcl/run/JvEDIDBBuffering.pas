{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEDIDBBuffering.PAS, released on 2004-04-05.

The Initial Developer of the Original Code is Raymond Alexander .
Portions created by Joe Doe are Copyright (C) 2004 Raymond Alexander.

All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvEDIDBBuffering;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Contnrs, DB,
  {$IFDEF VCL}
  Windows, Messages,
  {$ENDIF VCL}
  JclEDI, JclEDI_ANSIX12, JclEDISEF,
  JvComponent;

const
  Field_SegmentId = 'SegmentId';
  Field_ElementId = 'ElementId';
  Field_ElementCount = 'ElementCount';
  Field_ElementType = 'ElementType';
  Field_MaximumLength = 'MaximumLength';
  Field_OwnerLoopId = 'OwnerLoopId';
  Field_ParentLoopId = 'ParentLoopId';

  FieldType_PKey = 'PKey';
  FieldType_FKey = 'FKey';
  TransactionSetKeyName = 'TS';

type
  TJvAfterProfiledTransactionSetEvent = procedure(TransactionSet: TEDIObject) of object;
  TJvAfterProfiledSegmentEvent = procedure(Segment: TEDIObject) of object;

  // Base Class EDI Specification Profiler (TDataSet Compatible)
  TJvEDIDBProfiler = class(TJvComponent)
  private
    FElementProfiles: TDataSet;
    FSegmentProfiles: TDataSet;
    FLoopProfiles: TDataSet;
    FOnAfterProfiledTransactionSet: TJvAfterProfiledTransactionSetEvent;
    FOnAfterProfiledSegment: TJvAfterProfiledSegmentEvent;
  protected
    procedure DoAfterProfiledTransactionSet(TransactionSet: TEDIObject); virtual;
    procedure DoAfterProfiledSegment(Segment: TEDIObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildProfile; virtual; abstract;
    procedure ClearProfile; virtual;
    procedure AddElement(const SegmentId, ElementId, ElementType: string;
      MaximumLength: Integer); virtual;
    procedure UpdateElement(const SegmentId, ElementId, ElementType: string;
      MaximumLength, Count: Integer); virtual;
    procedure AddSegment(const SegmentId, OwnerLoopId, ParentLoopId: string); virtual;
    procedure AddLoop(const OwnerLoopId, ParentLoopId: string); virtual;
    function ElementExist(const SegmentId, ElementId: string): Boolean; virtual;
    function SegmentExist(const SegmentId, OwnerLoopId, ParentLoopId: string): Boolean; virtual;
    function LoopExist(const OwnerLoopId, ParentLoopId: string): Boolean; virtual;
  published
    property ElementProfiles: TDataSet read FElementProfiles write FElementProfiles;
    property SegmentProfiles: TDataSet read FSegmentProfiles write FSegmentProfiles;
    property LoopProfiles: TDataSet read FLoopProfiles write FLoopProfiles;
    property OnAfterProfiledTransactionSet: TJvAfterProfiledTransactionSetEvent
      read FOnAfterProfiledTransactionSet write FOnAfterProfiledTransactionSet;
    property OnAfterProfiledSegment: TJvAfterProfiledSegmentEvent read FOnAfterProfiledSegment
      write FOnAfterProfiledSegment;
  end;

  // EDI Specification Profiler (JclEDI_ANSIX12.pas)
  TJvEDIDBSpecProfiler = class(TJvEDIDBProfiler)
  public
    procedure BuildProfile(EDIFileSpec: TEDIFileSpec); reintroduce;
  end;

  // Standard Exchange Format (SEF) EDI Specification Profiler (JclEDISEF.pas)
  TJvEDIDBSEFProfiler = class(TJvEDIDBProfiler)
  public
    procedure BuildProfile(EDISEFFile: TEDISEFFile); reintroduce;
  end;

  TJvEDIFieldDef = class(TCollectionItem)
  private
    FFieldName: string;
    FFieldType: string;
    FDataType: TFieldType;
    FMaximumLength: Integer;
    FUpdateStatus: TUpdateStatus;
  public
    constructor Create(Collection: TCollection); override;
  published
    property FieldName: string read FFieldName write FFieldName;
    property FieldType: string read FFieldType write FFieldType;
    property DataType: TFieldType read FDataType write FDataType;
    property MaximumLength: Integer read FMaximumLength write FMaximumLength;
    property UpdateStatus: TUpdateStatus read FUpdateStatus write FUpdateStatus;
  end;

  TJvEDIFieldDefs = class(TCollection)
  private
    function GetItem(Index: Integer): TJvEDIFieldDef;
    procedure SetItem(Index: Integer; Value: TJvEDIFieldDef);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TJvEDIFieldDef;
    property Items[Index: Integer]: TJvEDIFieldDef read GetItem write SetItem; default;
  end;

  TJvTableExistsEvent = procedure(TableName: string; var TableExists: Boolean) of object;
  TJvTableProfileEvent = procedure(FieldDefs: TJvEDIFieldDefs; TableName: string) of object;
  TJvCreateTableEvent = TJvTableProfileEvent;
  TJvCheckForFieldChangesEvent = TJvTableProfileEvent;
  TJvAlterTableEvent = TJvTableProfileEvent;
  TJvResolveFieldDefTypeEvent = procedure(FieldDef: TJvEDIFieldDef) of object;
  TJvBeforeApplyElementFilterEvent = procedure(DataSet: TDataSet; TableName: string;
    var ApplyFilter: Boolean) of object;

  TJvEDIDBBuffer = class(TJvComponent)
  private
    FElementProfiles: TDataSet;
    FSegmentProfiles: TDataSet;
    FLoopProfiles: TDataSet;
    FLoopKeyPrefix: string;
    FSegmentKeyPrefix: string;
    FKeySuffix: string;
    FElementNonKeyPrefix: string;
    FOnBeforeOpenDataSets: TNotifyEvent;
    FOnAfterOpenDataSets: TNotifyEvent;
    FOnBeforeCloseDataSets: TNotifyEvent;
    FOnAfterCloseDataSets: TNotifyEvent;
    FOnTableExists: TJvTableExistsEvent;
    FOnCreateTable: TJvCreateTableEvent;
    FOnCheckForFieldChanges: TJvCheckForFieldChangesEvent;
    FOnAlterTable: TJvAlterTableEvent;
    FOnResolveFieldDefDataType: TJvResolveFieldDefTypeEvent;
    FOnBeforeApplyElementFilter: TJvBeforeApplyElementFilterEvent;
    procedure CreateFieldDefs(FieldDefs: TJvEDIFieldDefs;
      const TableName, OwnerLoopId, ParentLoopId: string; DefaultUpdateStatus: TUpdateStatus);
    procedure CreateLoopFieldDefs(FieldDefs: TJvEDIFieldDefs; const TableName, ParentLoopId: string;
      DefaultUpdateStatus: TUpdateStatus);
  protected
    procedure DoBeforeOpenDataSets; virtual;
    procedure DoAfterOpenDataSets; virtual;
    procedure DoBeforeCloseDataSets; virtual;
    procedure DoAfterCloseDataSets; virtual;
    procedure DoTableExists(const TableName: string; var TableExists: Boolean); virtual;
    procedure DoCreateTable(FieldDefs: TJvEDIFieldDefs; const TableName: string); virtual;
    procedure DoCheckForFieldChanges(FieldDefs: TJvEDIFieldDefs; const TableName: string); virtual;
    procedure DoAlterTable(FieldDefs: TJvEDIFieldDefs; const TableName: string); virtual;
    procedure DoResolveFieldDefDataType(FieldDef: TJvEDIFieldDef); virtual;
    procedure DoBeforeApplyElementFilter(DataSet: TDataSet; const Table: string;
      var ApplyFilter: Boolean); virtual;
    //
    procedure OpenProfileDataSets; virtual;
    procedure CloseProfileDataSets; virtual;
    function TableExists(const TableName: string): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SyncProfilesWithBuffer; virtual;
  published
    property ElementProfiles: TDataSet read FElementProfiles write FElementProfiles;
    property SegmentProfiles: TDataSet read FSegmentProfiles write FSegmentProfiles;
    property LoopProfiles: TDataSet read FLoopProfiles write FLoopProfiles;
    //
    property KeySuffix: string read FKeySuffix write FKeySuffix;
    property LoopKeyPrefix: string read FLoopKeyPrefix write FLoopKeyPrefix;
    property SegmentKeyPrefix: string read FSegmentKeyPrefix write FSegmentKeyPrefix;
    property ElementNonKeyPrefix: string read FElementNonKeyPrefix write FElementNonKeyPrefix;
    //
    property OnBeforeOpenDataSets: TNotifyEvent read FOnBeforeOpenDataSets
      write FOnBeforeOpenDataSets;
    property OnAfterOpenDataSets: TNotifyEvent read FOnAfterOpenDataSets
      write FOnAfterOpenDataSets;
    property OnBeforeCloseDataSets: TNotifyEvent read FOnBeforeCloseDataSets
      write FOnBeforeCloseDataSets;
    property OnAfterCloseDataSets: TNotifyEvent read FOnAfterCloseDataSets
      write FOnAfterCloseDataSets;
    property OnTableExists: TJvTableExistsEvent read FOnTableExists write FOnTableExists;
    property OnCreateTable: TJvCreateTableEvent read FOnCreateTable write FOnCreateTable;
    property OnCheckForFieldChanges: TJvCheckForFieldChangesEvent read FOnCheckForFieldChanges
      write FOnCheckForFieldChanges;
    property OnAlterTable: TJvAlterTableEvent read FOnAlterTable write FOnAlterTable;
    property OnResolveFieldDefType: TJvResolveFieldDefTypeEvent read FOnResolveFieldDefDataType
      write FOnResolveFieldDefDataType;
    property OnBeforeApplyElementFilter: TJvBeforeApplyElementFilterEvent
      read FOnBeforeApplyElementFilter write FOnBeforeApplyElementFilter;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  JvResources, JvTypes;

const
  Default_LoopKeyPrefix = 'Loop_';
  Default_KeySuffix = '_Id';
  Default_SegmentKeyPrefix = '';
  Default_ElementNonKeyPrefix = 'E';

//=== { TJvEDIDBProfiler } ===================================================

constructor TJvEDIDBProfiler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FElementProfiles := nil;
  FSegmentProfiles := nil;
  FLoopProfiles := nil;
end;

destructor TJvEDIDBProfiler.Destroy;
begin
  FElementProfiles := nil;
  FSegmentProfiles := nil;
  FLoopProfiles := nil;
  inherited Destroy;
end;

procedure TJvEDIDBProfiler.AddElement(const SegmentId, ElementId, ElementType: string;
  MaximumLength: Integer);
begin
  with FElementProfiles do
  begin
    Insert;
    FieldByName(Field_SegmentId).AsString := SegmentId;
    FieldByName(Field_ElementId).AsString := ElementId;
    FieldByName(Field_ElementCount).AsInteger := 1;
    FieldByName(Field_ElementType).AsString := ElementType;
    FieldByName(Field_MaximumLength).AsInteger := MaximumLength;
    Post;
  end;
end;

procedure TJvEDIDBProfiler.AddLoop(const OwnerLoopId, ParentLoopId: string);
begin
  with FLoopProfiles do
  begin
    Insert;
    FieldByName(Field_OwnerLoopId).AsString := OwnerLoopId;
    FieldByName(Field_ParentLoopId).AsString := ParentLoopId;
    Post;
  end;
end;

procedure TJvEDIDBProfiler.AddSegment(const SegmentId, OwnerLoopId, ParentLoopId: string);
begin
  with FSegmentProfiles do
  begin
    Insert;
    FieldByName(Field_SegmentId).AsString := SegmentId;
    FieldByName(Field_OwnerLoopId).AsString := OwnerLoopId;
    FieldByName(Field_ParentLoopId).AsString := ParentLoopId;
    Post;
  end;
end;

procedure TJvEDIDBProfiler.ClearProfile;
begin
  FElementProfiles.First;
  while not FElementProfiles.Eof do
    FElementProfiles.Delete;
  FSegmentProfiles.First;
  while not FSegmentProfiles.Eof do
    FSegmentProfiles.Delete;
  FLoopProfiles.First;
  while not FLoopProfiles.Eof do
    FLoopProfiles.Delete;
end;

procedure TJvEDIDBProfiler.DoAfterProfiledSegment(Segment: TEDIObject);
begin
  if Assigned(FOnAfterProfiledSegment) then
    FOnAfterProfiledSegment(Segment);
end;

procedure TJvEDIDBProfiler.DoAfterProfiledTransactionSet(TransactionSet: TEDIObject);
begin
  if Assigned(FOnAfterProfiledTransactionSet) then
    FOnAfterProfiledTransactionSet(TransactionSet);
end;

function TJvEDIDBProfiler.ElementExist(const SegmentId, ElementId: string): Boolean;
begin
  FElementProfiles.First;
  Result := FElementProfiles.Locate(Field_SegmentId + ';' + Field_ElementId,
    VarArrayOf([SegmentId, ElementId]), [loCaseInsensitive]);
end;

function TJvEDIDBProfiler.LoopExist(const OwnerLoopId, ParentLoopId: string): Boolean;
begin
  FLoopProfiles.First;
  Result := FLoopProfiles.Locate(Field_OwnerLoopId + ';' + Field_ParentLoopId,
    VarArrayOf([OwnerLoopId, ParentLoopId]), [loCaseInsensitive]);
end;

function TJvEDIDBProfiler.SegmentExist(const SegmentId, OwnerLoopId, ParentLoopId: string): Boolean;
begin
  FSegmentProfiles.First;
  Result := FSegmentProfiles.Locate(Field_SegmentId + ';' + Field_OwnerLoopId + ';' +
    Field_ParentLoopId, VarArrayOf([SegmentId, OwnerLoopId, ParentLoopId]), [loCaseInsensitive]);
end;

procedure TJvEDIDBProfiler.UpdateElement(const SegmentId, ElementId, ElementType: string;
  MaximumLength, Count: Integer);
begin
  with FElementProfiles do
  begin
    Edit;
    if Count > FieldByName(Field_ElementCount).AsInteger then
      FieldByName(Field_ElementCount).AsInteger := Count;
    FieldByName(Field_ElementType).AsString := ElementType;
    if MaximumLength > FieldByName(Field_MaximumLength).AsInteger then
      FieldByName(Field_MaximumLength).AsInteger := MaximumLength;
    Post;
  end;
end;

//=== { TJvEDIDBSpecProfiler } ===============================================

procedure TJvEDIDBSpecProfiler.BuildProfile(EDIFileSpec: TEDIFileSpec);
var
  I, F, T, S, E: Integer;
  TransactionSet: TEDITransactionSetSpec;
  Segment: TEDISegmentSpec;
  Element: TEDIElementSpec;
  RecordExists: Boolean;
  ElementList: TStrings;
begin
  if (FElementProfiles = nil) or (FSegmentProfiles = nil) or (FLoopProfiles = nil) then
    raise EJVCLException.CreateRes(@RsENoProfileDatasets);
  FElementProfiles.Filtered := False;
  FSegmentProfiles.Filtered := False;
  FLoopProfiles.Filtered := False;
  ElementList := TStringList.Create;
  for I := 0 to EDIFileSpec.InterchangeControlCount - 1 do
  begin
    for F := 0 to EDIFileSpec[I].FunctionalGroupCount - 1 do
      for T := 0 to EDIFileSpec[I][F].TransactionSetCount - 1 do
      begin
        TransactionSet := TEDITransactionSetSpec(EDIFileSpec[I][F][T]);
        for S := 0 to TransactionSet.SegmentCount - 1 do
        begin
          ElementList.Clear;
          Segment := TEDISegmentSpec(TransactionSet[S]);
          RecordExists := LoopExist(Segment.OwnerLoopId, Segment.ParentLoopId);
          if not RecordExists then
            AddLoop(Segment.OwnerLoopId, Segment.ParentLoopId);
          RecordExists := SegmentExist(Segment.SegmentId, Segment.OwnerLoopId,
            Segment.ParentLoopId);
          if not RecordExists then
            AddSegment(Segment.SegmentId, Segment.OwnerLoopId, Segment.ParentLoopId);
          for E := 0 to Segment.ElementCount - 1 do
          begin
            Element := TEDIElementSpec(Segment.Element[E]);
            if ElementList.Values[Element.Id] = '' then
              ElementList.Values[Element.Id] := '0';
            ElementList.Values[Element.Id] :=
              IntToStr(StrToInt(ElementList.Values[Element.Id]) + 1);
            RecordExists := ElementExist(Segment.SegmentId, Element.Id);
            if not RecordExists then
              AddElement(Segment.SegmentId, Element.Id, Element.ElementType, Element.MaximumLength)
            else
              UpdateElement(Segment.SegmentId, Element.Id, Element.ElementType,
                Element.MaximumLength, StrToInt(ElementList.Values[Element.Id]));
          end;
          DoAfterProfiledSegment(Segment);
        end;
        DoAfterProfiledTransactionSet(TransactionSet);
      end;
    end;
  ElementList.Free;
end;

//=== { TJvEDIDBSEFProfiler } ================================================

procedure TJvEDIDBSEFProfiler.BuildProfile(EDISEFFile: TEDISEFFile);
var
  E, I, J: Integer;
  RecordExists: Boolean;
  ElementStrList: TStrings;
  Id: string;
  SEFSet: TEDISEFSet;
  SEFSegment: TEDISEFSegment;
  SEFElement: TEDISEFElement;
  SegmentList: TObjectList;
  ElementList: TObjectList;
begin
  if (FElementProfiles = nil) or (FSegmentProfiles = nil) or (FLoopProfiles = nil) then
    raise EJVCLException.CreateRes(@RsENoProfileDatasets);
  FElementProfiles.Filtered := False;
  FSegmentProfiles.Filtered := False;
  FLoopProfiles.Filtered := False;
  for I := 0 to EDISEFFile.SETS.Count - 1 do
  begin
    SEFSet := TEDISEFSet(EDISEFFile.SETS[I]);
    SegmentList := SEFSet.GetSegmentObjectList;
    try
      for J := 0 to SegmentList.Count - 1 do
      begin
        SEFSegment := TEDISEFSegment(SegmentList[J]);
        RecordExists := LoopExist(SEFSegment.OwnerLoopId, SEFSegment.ParentLoopId);
        if not RecordExists then
          AddLoop(SEFSegment.OwnerLoopId, SEFSegment.ParentLoopId);
        RecordExists := SegmentExist(SEFSegment.SegmentId, SEFSegment.OwnerLoopId,
          SEFSegment.ParentLoopId);
        if not RecordExists then
          AddSegment(SEFSegment.SegmentId, SEFSegment.OwnerLoopId, SEFSegment.ParentLoopId);
        ElementList := SEFSegment.GetElementObjectList;
        ElementStrList := TStringList.Create;
        try
          ElementStrList.Clear;
          for E := 0 to ElementList.Count - 1 do
          begin
            if ElementList[E] is TEDISEFElement then
            begin
              SEFElement := TEDISEFElement(ElementList[E]);
              Id := SEFSegment.Id + SEFElement.Id;
              if ElementStrList.Values[Id] = '' then
                ElementStrList.Values[Id] := '0';
              ElementStrList.Values[Id] :=
                IntToStr(StrToInt(ElementStrList.Values[Id]) + 1);
              RecordExists := ElementExist(SEFSegment.Id, SEFElement.Id);
              if not RecordExists then
                AddElement(SEFSegment.Id, SEFElement.Id, SEFElement.ElementType,
                  SEFElement.MaximumLength)
              else
                UpdateElement(SEFSegment.Id, SEFElement.Id, SEFElement.ElementType,
                  SEFElement.MaximumLength, StrToInt(ElementStrList.Values[Id]));
            end;
          end;
        finally
          ElementStrList.Free;
          ElementList.Free;
        end;
        DoAfterProfiledSegment(SEFSegment);
      end;
    finally
      SegmentList.Free;
    end;
    DoAfterProfiledTransactionSet(SEFSet);
  end;
end;

//=== { TJclEDIFieldDef } ====================================================

constructor TJvEDIFieldDef.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FUpdateStatus := usUnmodified;
end;

//=== { TJvEDIFieldDefs } ====================================================

function TJvEDIFieldDefs.Add: TJvEDIFieldDef;
begin
  Result := TJvEDIFieldDef(inherited Add);
end;

function TJvEDIFieldDefs.GetItem(Index: Integer): TJvEDIFieldDef;
begin
  Result := TJvEDIFieldDef(inherited GetItem(Index));
end;

procedure TJvEDIFieldDefs.SetItem(Index: Integer; Value: TJvEDIFieldDef);
begin
  inherited SetItem(Index, Value);
end;

procedure TJvEDIFieldDefs.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
end;

//=== { TJvEDIDBBuffer } =====================================================

constructor TJvEDIDBBuffer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLoopKeyPrefix := Default_LoopKeyPrefix;
  FKeySuffix := Default_KeySuffix;
  FSegmentKeyPrefix := Default_SegmentKeyPrefix;
  FElementNonKeyPrefix := Default_ElementNonKeyPrefix;
end;

procedure TJvEDIDBBuffer.CloseProfileDataSets;
begin
  DoBeforeCloseDataSets;
  if FLoopProfiles.Active then
    FLoopProfiles.Close;
  if FLoopProfiles.Active then
    FElementProfiles.Close;
  if FLoopProfiles.Active then
    FSegmentProfiles.Close;
  DoAfterCloseDataSets;
end;

procedure TJvEDIDBBuffer.DoAfterOpenDataSets;
begin
  if Assigned(FOnAfterOpenDataSets) then
    FOnAfterOpenDataSets(Self);
end;

procedure TJvEDIDBBuffer.DoAlterTable(FieldDefs: TJvEDIFieldDefs; const TableName: string);
begin
  if Assigned(FOnAlterTable) then
    FOnAlterTable(FieldDefs, TableName);
end;

procedure TJvEDIDBBuffer.DoBeforeCloseDataSets;
begin
  if Assigned(FOnBeforeCloseDataSets) then
    FOnBeforeCloseDataSets(Self);
end;

procedure TJvEDIDBBuffer.DoAfterCloseDataSets;
begin
  if Assigned(FOnAfterCloseDataSets) then
    FOnAfterCloseDataSets(Self);
end;

procedure TJvEDIDBBuffer.DoBeforeOpenDataSets;
begin
  if Assigned(FOnBeforeOpenDataSets) then
    FOnBeforeOpenDataSets(Self);
end;

procedure TJvEDIDBBuffer.DoCheckForFieldChanges(FieldDefs: TJvEDIFieldDefs; const TableName: string);
begin
  if Assigned(FOnCheckForFieldChanges) then
    FOnCheckForFieldChanges(FieldDefs, TableName);
end;

procedure TJvEDIDBBuffer.DoCreateTable(FieldDefs: TJvEDIFieldDefs; const TableName: string);
begin
  if Assigned(FOnCreateTable) then
    FOnCreateTable(FieldDefs, TableName);
end;

procedure TJvEDIDBBuffer.DoTableExists(const TableName: string; var TableExists: Boolean);
begin
  if Assigned(FOnTableExists) then
    FOnTableExists(TableName, TableExists);
end;

procedure TJvEDIDBBuffer.CreateFieldDefs(FieldDefs: TJvEDIFieldDefs;
  const TableName, OwnerLoopId, ParentLoopId: string; DefaultUpdateStatus: TUpdateStatus);
var
  FieldDef: TJvEDIFieldDef;
  ApplyFilter: Boolean;
  I: Integer;
begin
  FieldDefs.Clear;
  //Primary Key
  FieldDef := FieldDefs.Add;
  FieldDef.FieldName := FSegmentKeyPrefix + TableName + FKeySuffix; // Primary Key
  FieldDef.FieldType := FieldType_PKey;
  FieldDef.DataType := ftInteger;
  FieldDef.MaximumLength := 1;
  FieldDef.UpdateStatus := DefaultUpdateStatus;
  //Foreign Key
  FieldDef := FieldDefs.Add;
  if (OwnerLoopId = NA_LoopId) or (OwnerLoopId = '') then
    FieldDef.FieldName := TransactionSetKeyName + FKeySuffix // Transaction Set Foreign Key
  else
    FieldDef.FieldName := FLoopKeyPrefix + OwnerLoopId + FKeySuffix; // Loop Foreign Key
  FieldDef.FieldType := FieldType_FKey;
  FieldDef.DataType := ftInteger;
  FieldDef.MaximumLength := 1;
  FieldDef.UpdateStatus := DefaultUpdateStatus;
  //Fields
  ApplyFilter := True;
  DoBeforeApplyElementFilter(FElementProfiles, TableName, ApplyFilter);
  if ApplyFilter then
  begin
    FElementProfiles.Filtered := False;
    FElementProfiles.Filter := Field_SegmentId + ' = ' + QuotedStr(TableName);
    FElementProfiles.Filtered := True;
  end;
  FElementProfiles.First;
  while not FElementProfiles.Eof do
  begin
    for I := 1 to FElementProfiles.FieldByName(Field_ElementCount).AsInteger do
    begin
      FieldDef := FieldDefs.Add;
      FieldDef.FieldName := FElementNonKeyPrefix +
        FElementProfiles.FieldByName(Field_ElementId).AsString + '_' + IntToStr(I);
      FieldDef.FieldType := FElementProfiles.FieldByName(Field_ElementType).AsString;
      if FieldDef.FieldType = '' then
        FieldDef.DataType := ftString
      else
      if FieldDef.FieldType[1] = EDIDataType_Numeric then
        FieldDef.DataType := ftInteger
      else
      if FieldDef.FieldType = EDIDataType_Decimal then
        FieldDef.DataType := ftFloat
      else
      if FieldDef.FieldType = EDIDataType_Identifier then
        FieldDef.DataType := ftString
      else
      if FieldDef.FieldType = EDIDataType_String then
        FieldDef.DataType := ftString
      else
      if FieldDef.FieldType = EDIDataType_Date then
        FieldDef.DataType := ftDate
      else
      if FieldDef.FieldType = EDIDataType_Time then
        FieldDef.DataType := ftTime
      else
      if FieldDef.FieldType = EDIDataType_Binary then
        FieldDef.DataType := ftBlob
      else
        FieldDef.DataType := ftString;
      FieldDef.MaximumLength := FElementProfiles.FieldByName(Field_MaximumLength).AsInteger;
      FieldDef.UpdateStatus := DefaultUpdateStatus;
      DoResolveFieldDefDataType(FieldDef);
    end;
    FElementProfiles.Next;
  end;
end;

procedure TJvEDIDBBuffer.OpenProfileDataSets;
begin
  DoBeforeOpenDataSets;
  FSegmentProfiles.Open;
  FElementProfiles.Open;
  FLoopProfiles.Open;
  DoAfterOpenDataSets;
end;

procedure TJvEDIDBBuffer.SyncProfilesWithBuffer;
var
  TableName, OwnerLoopId, ParentLoopId: string;
  FieldDefs: TJvEDIFieldDefs;
begin
  FieldDefs := TJvEDIFieldDefs.Create(TJvEDIFieldDef);
  OpenProfileDataSets;
  while not FLoopProfiles.Eof do
  begin
    OwnerLoopId := FLoopProfiles.FieldByName(Field_OwnerLoopId).AsString;
    TableName := FLoopKeyPrefix + OwnerLoopId;
    ParentLoopId := FLoopProfiles.FieldByName(Field_ParentLoopId).AsString;
    if (OwnerLoopId <> NA_LoopId) and (not TableExists(TableName)) then
    begin
      CreateLoopFieldDefs(FieldDefs, TableName, ParentLoopId, usInserted);
      DoCreateTable(FieldDefs, TableName);
    end
    else
    if OwnerLoopId <> NA_LoopId then
    begin
      CreateLoopFieldDefs(FieldDefs, TableName, ParentLoopId, usUnmodified);
      DoCheckForFieldChanges(FieldDefs, TableName);
      DoAlterTable(FieldDefs, TableName);
    end;
    FLoopProfiles.Next;
  end;
  while not FSegmentProfiles.Eof do
  begin
    TableName := FSegmentProfiles.FieldByName(Field_SegmentId).AsString;
    OwnerLoopId := FSegmentProfiles.FieldByName(Field_OwnerLoopId).AsString;
    ParentLoopId := FSegmentProfiles.FieldByName(Field_ParentLoopId).AsString;
    if not TableExists(TableName) then
    begin
      CreateFieldDefs(FieldDefs, TableName, OwnerLoopId, ParentLoopId, usInserted);
      DoCreateTable(FieldDefs, TableName);
    end
    else
    begin
      CreateFieldDefs(FieldDefs, TableName, OwnerLoopId, ParentLoopId, usUnmodified);
      DoCheckForFieldChanges(FieldDefs, TableName);
      DoAlterTable(FieldDefs, TableName);
    end;
    FSegmentProfiles.Next;
  end;
  CloseProfileDataSets;
  FieldDefs.Free;
end;

function TJvEDIDBBuffer.TableExists(const TableName: string): Boolean;
begin
  Result := False;
  DoTableExists(TableName, Result);
end;

procedure TJvEDIDBBuffer.DoResolveFieldDefDataType(FieldDef: TJvEDIFieldDef);
begin
  if Assigned(FOnResolveFieldDefDataType) then
    FOnResolveFieldDefDataType(FieldDef);
end;

procedure TJvEDIDBBuffer.CreateLoopFieldDefs(FieldDefs: TJvEDIFieldDefs;
  const TableName, ParentLoopId: string; DefaultUpdateStatus: TUpdateStatus);
var
  FieldDef: TJvEDIFieldDef;
begin
  FieldDefs.Clear;
  if TableName = NA_LoopId then
    Exit;
  //Primary Key
  FieldDef := FieldDefs.Add;
  FieldDef.FieldName := TableName + FKeySuffix; // Primary Key
  FieldDef.FieldType := FieldType_PKey;
  FieldDef.DataType := ftInteger;
  FieldDef.MaximumLength := 1;
  FieldDef.UpdateStatus := DefaultUpdateStatus;
  //Foriegn Key
  FieldDef := FieldDefs.Add;
  if (ParentLoopId = NA_LoopId) or (ParentLoopId = '') then
    FieldDef.FieldName := TransactionSetKeyName + FKeySuffix // Transaction Set Foreign Key
  else
    FieldDef.FieldName := FLoopKeyPrefix + ParentLoopId + FKeySuffix; // Foreign Key
  FieldDef.FieldType := FieldType_FKey;
  FieldDef.DataType := ftInteger;
  FieldDef.MaximumLength := 1;
  FieldDef.UpdateStatus := DefaultUpdateStatus;
end;

procedure TJvEDIDBBuffer.DoBeforeApplyElementFilter(DataSet: TDataSet; const Table: string;
  var ApplyFilter: Boolean);
begin
  if Assigned(FOnBeforeApplyElementFilter) then
    FOnBeforeApplyElementFilter(DataSet, Table, ApplyFilter);
end;

end.

