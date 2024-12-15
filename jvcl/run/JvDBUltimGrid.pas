{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBUltimGrid.PAS, released on 2004-07-22.

The Initial Developers of the Original Code are: Frédéric Leneuf-Magaud
Copyright (c) 2004 Frédéric Leneuf-Magaud
All Rights Reserved.

Contributors:
  Niels v/d Spek

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

-----------------------------------------------------------------------------
HOW TO SORT FIELDS:
-----------------------------------------------------------------------------

---= Delphi example =---

// Don't forget to set SortWith and assign OnIndexNotFound/OnUserSort if needed

var
  MyFields: TSortFields;

SetLength(MyFields, 2);
MyFields[0].Name := 'Country';
MyFields[0].Order := JvGridSort_ASC;
MyFields[1].Name := 'Sales';
MyFields[1].Order := JvGridSort_DESC;
MyUltimGrid.Sort(MyFields);

if not MyUltimGrid.SortOK then ...

---= BCB example =---

// Don't forget to set SortWith and assign OnIndexNotFound/OnUserSort if needed

TSortFields MyFields;

MyFields.set_length(2);
MyFields[0].Name = "Country";
MyFields[0].Order = JvGridSort_ASC;
MyFields[1].Name = "Sales";
MyFields[1].Order = JvGridSort_DESC;
MyUltimGrid->Sort(MyFields);

if (!MyUltimGrid->SortOK) ...

---= MANUAL SORTING =---

if TitleButtons is true then...

First click = the selected field is sorted in ascending order
Second click = the selected field is sorted in descending order
Shift+Click / Ctrl+Click = multi-column sorting

-----------------------------------------------------------------------------
HOW TO SEARCH A VALUE:
-----------------------------------------------------------------------------

---= Delphi example =---

var
  // Declare these vars as global vars if you want to use SearchNext
  ResultCol: Integer;
  ResultField: TField;

with MyUltimGrid do
begin
  SearchFields.Clear;
  SearchFields.Add('Category');
  SearchFields.Add('Common_Name');
  SearchFields.Add('Species Name');
  SearchFields.Add('Notes');
  if not Search('fish', ResultCol, ResultField, False, False, True) then ...
end;

// then:
if not MyUltimGrid.SearchNext(ResultCol, ResultField, False, False, True) then ...

---= BCB example =---

// Declare these vars as global vars if you want to use SearchNext
int ResultCol;
TField *ResultField;

MyUltimGrid->SearchFields->Clear();
MyUltimGrid->SearchFields->Add("Category");
MyUltimGrid->SearchFields->Add("Common_Name");
MyUltimGrid->SearchFields->Add("Species Name");
MyUltimGrid->SearchFields->Add("Notes");
if (!MyUltimGrid->Search("fish", ResultCol, ResultField, false, false, true)) ...

// then:
if (!MyUltimGrid->SearchNext(ResultCol, ResultField, false, false, true)) ...

-----------------------------------------------------------------------------
Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBUltimGrid;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Variants, Classes, SysUtils, Graphics, Controls, DB,
  JvDBGrid; {JvTypes contains Exception base class}

const
  JvGridSort_ASC = True;
  JvGridSort_UP = True;
  JvGridSort_DESC = False;
  JvGridSort_DOWN = False;

type
  TSortField = record
    Name: string;
    Order: Boolean;
  end;
  TSortFields = array of TSortField;

  TJvDBUltimGrid = class;
  TIndexNotFoundEvent = procedure(Sender: TJvDBUltimGrid; FieldsToSort: TSortFields;
    IndexFieldNames: string; DescFields: string; var Retry: Boolean) of object;
  TUserSortEvent = procedure(Sender: TJvDBUltimGrid; var FieldsToSort: TSortFields;
    SortString: string; var SortOK: Boolean) of object;
  TRestoreGridPosEvent = procedure(Sender: TJvDBUltimGrid; SavedBookmark: TBookmark;
    SavedRowPos: Integer) of object;
  TCheckIfValidSortFieldEvent = function(Sender: TJvDBUltimGrid;
    FieldToSort: TField): Boolean of object;
  TGetSortFieldNameEvent = procedure(Sender: TJvDBUltimGrid; var FieldName: string) of object;

  TSortWith = (swIndex, swFields, swClient, swUserFunc, swWhere);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDBUltimGrid = class(TJvDBGrid)
  private
    FSortedFields: TSortFields;
    FSortWith: TSortWith;
    FSortOK: Boolean;
    FRestoreOnSort: Boolean;
    FSortExcludedFields: string;
    FMasterFields: string;
    FIndexCounter: Cardinal;

    FMultiColSort: Boolean;
    FOnIndexNotFound: TIndexNotFoundEvent;
    FOnUserSort: TUserSortEvent;
    FOnCheckIfValidSortField: TCheckIfValidSortFieldEvent;
    FSavedBookmark: {$IFDEF RTL200_UP}TBookmark{$ELSE}TBookmarkStr{$ENDIF RTL200_UP};
    FSavedRowPos: Integer;
    FOnRestoreGridPosition: TRestoreGridPosEvent;
    FValueToSearch: Variant;
    FSearchFields: TStringList;
    FOnGetSortFieldName: TGetSortFieldNameEvent;
    FOnAfterSort: TNotifyEvent;
    procedure SetMultiColSort(const Value: Boolean);
    function PrivateSearch(var ResultCol: Integer; var ResultField: TField;
      const CaseSensitive, WholeFieldOnly, Next: Boolean): Boolean;
    procedure SetSortExcludedFields(const Value: string);
    procedure SetMasterFields(const Value: string);
  protected
    function SortMarkerAssigned(const AFieldName: string): Boolean; override;
    procedure DoTitleClick(ACol: Longint; AField: TField); override;
    procedure GetSortFieldName(var FieldName: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Sort(FieldsToSort: TSortFields);
    property SortedFields: TSortFields read FSortedFields;
    property SortOK: Boolean read FSortOK;
    procedure SaveGridPosition;
    procedure RestoreGridPosition(Mode: TResyncMode = [rmExact, rmCenter]);
    property SearchFields: TStringList read FSearchFields write FSearchFields;
    function Search(const ValueToSearch: Variant; var ResultCol: Integer;
      var ResultField: TField; const CaseSensitive, WholeFieldOnly, Focus: Boolean): Boolean;
    function SearchNext(var ResultCol: Integer; var ResultField: TField;
      const CaseSensitive, WholeFieldOnly, Focus: Boolean): Boolean;
    // Reset any sorting is done so far. Then user can sort dataset using a sorting dataset property
    procedure ClearSortedFields;
  published
    property SortedField stored False; // Property of JvDBGrid not used in JvDBUltimGrid
    property SortMarker stored False; // Property of JvDBGrid hidden in JvDBUltimGrid

    { SortWith:
      swIndex    : for BDE tables (assignment of OnIndexNotFound is recommended)
      swFields   : for ADO tables
      swClient   : for ClientDataSets
      swUserFunc : for other data providers (assignment of OnUserSort is mandatory) }
    property SortWith: TSortWith read FSortWith write FSortWith default swIndex;

    { MultiColSort: is the sorting allowed on several columns or only one ? }
    property MultiColSort: Boolean read FMultiColSort write SetMultiColSort default True;

    { Current record position is restored after sorting }
    property RestoreOnSort: Boolean read FRestoreOnSort write FRestoreOnSort default True;
    { User can exclude columns from sorting. 'Fieldname1;Fieldname2;Fieldname3' }
    property SortExcludedFields: string read FSortExcludedFields write SetSortExcludedFields;

    { Fix problem on detail dataset when insert record after you have click title to sort }
    { Needed for detail datasets in a master/detail relationship }
    property MasterFields: string read FMasterFields write SetMasterFields;

    { OnIndexNotFound: fired when SortWith = swIndex and the sorting index is not found }
    property OnIndexNotFound: TIndexNotFoundEvent read FOnIndexNotFound write FOnIndexNotFound;

    { OnUserSort: fired when SortWith = swUserFunc }
    property OnUserSort: TUserSortEvent read FOnUserSort write FOnUserSort;

    { OnCheckIfValidSortField allows to define your own checking routine for sorting fields }
    property OnCheckIfValidSortField: TCheckIfValidSortFieldEvent
      read FOnCheckIfValidSortField write FOnCheckIfValidSortField;

    { OnRestoreGridPosition: fired when RestoreGridPosition is called }
    property OnRestoreGridPosition: TRestoreGridPosEvent
      read FOnRestoreGridPosition write FOnRestoreGridPosition;

    { OnGetSortFieldName: allows to override the sort marker field }
    property OnGetSortFieldName: TGetSortFieldNameEvent read FOnGetSortFieldName Write FOnGetSortFieldName;
    
    { OnAfterSort: fired after the table was sorted. }
    property OnAfterSort: TNotifyEvent read FOnAfterSort write FOnAfterSort;
  end;

  EJvDBUltimGrid = class(Exception);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  TypInfo, Forms, DBGrids,
  JclStrings, JclSysUtils,
  JvResources, JvJCLUtils, JvDBUtils;

constructor TJvDBUltimGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortedFields := nil;
  FSortWith := swIndex;
  FSortOK := True;
  FMultiColSort := True;
  FOnIndexNotFound := nil;
  FOnUserSort := nil;
  FRestoreOnSort := True;
  FSavedBookmark := {$IFDEF RTL200_UP}nil{$ELSE}''{$ENDIF RTL200_UP};
  FSavedRowPos := 0;
  FOnRestoreGridPosition := nil;
  FValueToSearch := Null;
  FSearchFields := TStringList.Create;
end;

destructor TJvDBUltimGrid.Destroy;
begin
  FSearchFields.Free;
  inherited Destroy;
end;

function TJvDBUltimGrid.SortMarkerAssigned(const AFieldName: string): Boolean;
var
  SF: Integer;
  SortFieldName: string;
begin
  Result := False;
  if Assigned(FSortedFields) then
  begin
    SortFieldName := AFieldName;
    { Let the user override the sort marker field }
    GetSortFieldName(SortFieldName);

    for SF := 0 to Length(FSortedFields) - 1 do
      if AnsiSameText(SortFieldName, FSortedFields[SF].Name) then
      begin
        if FSortedFields[SF].Order = JvGridSort_UP then
          inherited ChangeSortMarker(smUp)
        else
          inherited ChangeSortMarker(smDown);
        Result := True;
        Break;
      end;
  end;
end;

procedure TJvDBUltimGrid.Sort(FieldsToSort: TSortFields);
const
  cIndexDefs = 'IndexDefs';
  cIndexName = 'IndexName';
  cIndexFieldNames = 'IndexFieldNames';
var
  DSet: TDataSet;

  procedure UpdateProp(const PropName: string; const Value: string; SortField: TField);
  var
    Error: Boolean;
  begin
    // Workaround for ADO 10 KB key limitation
    Error := (SortWith = swFields) and // swFields: for ADO tables
             (SortField.DataType in [ftString, ftWideString]) and
             (SortField.DataSize > 9361);

    if not Error then 
    begin
      SetStrProp(DSet, PropName, Value);
      FSortedFields := FieldsToSort;
      FSortOK := True;
    end 
    else
    begin 
      FSortOK := False;
    end;
  end;

var
  SortString, DescString: string;
  MaxFTS: Integer;

  procedure SearchIndex(SortField: TField);
  var
    IndexDefs: TIndexDefs;
    I, J: Integer;
  begin
    IndexDefs := TIndexDefs(GetOrdProp(DSet, cIndexDefs));
    IndexDefs.Update;
    for I := 0 to IndexDefs.Count - 1 do
      if AnsiSameText(SortString, IndexDefs.Items[I].Fields) then
      begin
        // The search succeeds if:
        // - no descending order is requested
        //   and the index found has no desc fields nor the flag ixDescending set to true
        // - descending order is requested
        //   and the index found has exactly the same desc fields
        // - descending order is requested
        //   and the index found has no desc fields but its flag ixDescending is true
        if DescString = '' then
        begin
          if (IndexDefs.Items[I].DescFields = '') and
            not (ixDescending in IndexDefs.Items[I].Options) then
          begin
            UpdateProp(cIndexName, IndexDefs.Items[I].Name, SortField);
            Break;
          end;
        end
        else
        if AnsiSameText(DescString, IndexDefs.Items[I].DescFields) then
        begin
          UpdateProp(cIndexName, IndexDefs.Items[I].Name, SortField);
          Break;
        end
        else
        if (IndexDefs.Items[I].DescFields = '') and
          (ixDescending in IndexDefs.Items[I].Options) then
        begin
          for J := 0 to MaxFTS do
            FieldsToSort[J].Order := JvGridSort_DESC;
          UpdateProp(cIndexName, IndexDefs.Items[I].Name, SortField);
          Break;
        end;
      end;
  end;

  procedure SetClientIndex(SortField: TField);
  const
    cIndexPrefix = '_Idx_';
  var
    IndexDefs: TIndexDefs;
    I: Integer;
    NewIndexName: string;
  begin
    IndexDefs := TIndexDefs(GetOrdProp(DSet, cIndexDefs));
    IndexDefs.Update;
    { Search for an existing index... }
    for I := 0 to IndexDefs.Count - 1 do begin
      if (Pos(cIndexPrefix, IndexDefs.Items[I].Name) = 1) and // Search among the indexes this procedure creates by itselt.
         (AnsiSameText(SortString, IndexDefs.Items[I].Fields)) and
         (AnsiSameText(DescString, IndexDefs.Items[I].DescFields))
      then begin
        NewIndexName := IndexDefs.Items[I].Name;
        UpdateProp(cIndexName, NewIndexName, SortField);
        Break;
      end
    end;
    { ... or else create a new one }
    Inc(FIndexCounter);
    NewIndexName := cIndexPrefix + IntToStr(FIndexCounter{IndexDefs.Count});
    with IndexDefs.AddIndexDef do begin
      Name := NewIndexName;
      Fields := SortString;
      CaseInsFields := SortString;
      DescFields := DescString;
    end;
    UpdateProp(cIndexName, NewIndexName, SortField);
  end;

var
  FTS: Integer;
  SortField: TField;
  Retry: Boolean;
  FieldIsValid: Boolean;
begin
  // Test for dataset class compatibility with respect to sort method
  case SortWith of
    swIndex:
      if not InheritsFromByName(DataSource.DataSet.ClassType, 'TTable') then
        raise EJvDBUltimGrid.Create('Only TTable or derived classes or allowed for SortWith = swIndex');
    swFields:
      if not InheritsFromByName(DataSource.DataSet.ClassType, 'TCustomADODataSet') then
        raise EJvDBUltimGrid.Create('Only TCustomADODataSet or derived classes or allowed for SortWith = swFields');
    swClient:
      if not InheritsFromByName(DataSource.DataSet.ClassType, 'TCustomClientDataSet') then
        raise EJvDBUltimGrid.Create('Only TCustomClientDataSet or derived classes or allowed for SortWith = swClient');
    swUserFunc: ;
    swWhere: ;
  end;

  FSortOK := False;
  if Assigned(DataLink) and DataLink.Active and Assigned(FieldsToSort) and
    (DataSource.DataSet <> nil) and not (DataSource.DataSet.Eof and DataSource.DataSet.Bof) then
  begin
    // Dataset must be in browse mode
    DSet := DataSource.DataSet;
    DSet.CheckBrowseMode;

    // Checking of OnUserSort assignment
    if Assigned(OnUserSort) then
      SortWith := swUserFunc;
    if (SortWith = swUserFunc) and not Assigned(OnUserSort) then
      raise EJVCLDbGridException.CreateRes(@RsEJvDBGridUserSortNotAssigned);

    // Checking of index properties
    if (SortWith in [swIndex, swClient]) and
      not (IsPublishedProp(DSet, cIndexDefs) and IsPublishedProp(DSet, cIndexName)) then
      raise EJVCLDbGridException.CreateRes(@RsEJvDBGridIndexPropertyMissing)
    else
    if (SortWith = swFields) and
      not IsPublishedProp(DSet, cIndexFieldNames) then
      raise EJVCLDbGridException.CreateRes(@RsEJvDBGridIndexPropertyMissing);

    if FRestoreOnSort then
      SaveGridPosition;

    // Sorting
    Screen.Cursor := crHourGlass;
    DSet.DisableControls;
    try
      SortString := '';

      {*** FMasterFields needed for:
        - ADO datasets in a master/detail relationship for the detail dataset.
        - TClientDataSets with Provider or else TClientDataSets loses recordset.
        ( Not tested for BDE tables )
      ***}
      if (SortWith in [swFields, swClient]) and (FMasterFields <> '') then
        SortString := FMasterFields;

      DescString := '';
      MaxFTS := Length(FieldsToSort) - 1;
      for FTS := 0 to MaxFTS do
      begin
        FieldsToSort[FTS].Name := Trim(FieldsToSort[FTS].Name);
        SortField := nil; // to avoid warning
        if SortWith <> swWhere then
        begin
          SortField := DSet.FieldByName(FieldsToSort[FTS].Name);
          if Assigned(OnCheckIfValidSortField) then
            FieldIsValid := OnCheckIfValidSortField(Self, SortField)
          else
            FieldIsValid := not (SortField is TBlobField) and not (SortField is TBytesField)
              and ((SortField.FieldKind = fkData) or (SortField.FieldKind = fkInternalCalc));
          if not FieldIsValid then
          begin
            // No sorting of binary or special fields
            if BeepOnError then
            begin
              SysUtils.Beep;
              Continue;
            end
            else
              raise EJVCLDbGridException.CreateRes(@RsEJvDBGridBadFieldKind);
          end;
        end;

        if SortWith = swIndex then
        begin
          // Sort with index
          if SortString <> '' then
            SortString := SortString + ';';
          SortString := SortString + FieldsToSort[FTS].Name;
          if FieldsToSort[FTS].Order = JvGridSort_DESC then
          begin
            if DescString <> '' then
              DescString := DescString + ';';
            DescString := DescString + FieldsToSort[FTS].Name;
          end;
          if FTS = MaxFTS then
          begin
            SearchIndex(SortField);
            if not SortOK then
            begin
              if Assigned(OnIndexNotFound) then
              begin
                Retry := False;
                OnIndexNotFound(Self, FieldsToSort, SortString, DescString, Retry);
                if Retry then
                begin
                  SearchIndex(SortField);
                  if not SortOK then
                    raise EJVCLDbGridException.CreateRes(@RsEJvDBGridIndexMissing);
                end;
              end
              else
                raise EJVCLDbGridException.CreateRes(@RsEJvDBGridIndexMissing);
            end;
          end;
        end
        else
        if SortWith = swClient then
        begin
          // Sort with IndexName
          if SortString <> '' then
            SortString := SortString + ';';
          SortString := SortString + FieldsToSort[FTS].Name;
          if FieldsToSort[FTS].Order = JvGridSort_DESC then
          begin
            if DescString <> '' then
              DescString := DescString + ';';
            DescString := DescString + FieldsToSort[FTS].Name;
          end;
          if FTS = MaxFTS then
          begin
            SetClientIndex(SortField);
            if not SortOK then
            begin
              { }
            end;
          end;
        end

        else
        if SortWith in [swFields, swUserFunc, swWhere] then
        begin
          // Sort with fields (temporary index), user function or where clausel
          if SortString <> '' then
            SortString := SortString + ',';
          if SortWith = swWhere then
            SortString := SortString + FieldsToSort[FTS].Name
          else
            SortString := SortString + '[' + FieldsToSort[FTS].Name + ']';

          if FieldsToSort[FTS].Order = JvGridSort_ASC then
            SortString := SortString + ' ASC'
          else
            SortString := SortString + ' DESC';
          if FTS = MaxFTS then
          begin
            if SortWith = swUserFunc then
            begin
              OnUserSort(Self, FieldsToSort, SortString, FSortOK);
              if SortOK then
                FSortedFields := FieldsToSort;
            end
            else
              UpdateProp(cIndexFieldNames, SortString, SortField);
          end;
        end;
      end;
      if FSortOK and Assigned(FOnAfterSort) then
        FOnAfterSort(Self);
    finally
      DSet.EnableControls;
      Screen.Cursor := crDefault;
    end;

    if FRestoreOnSort then
      RestoreGridPosition([rmExact]); // Position current record on top.
//      RestoreGridPosition; // Center current record.
  end;
end;

procedure TJvDBUltimGrid.SetMultiColSort(const Value: Boolean);
begin
  if FMultiColSort <> Value then
  begin
    FMultiColSort := Value;
    if Assigned(FSortedFields) and not FMultiColSort then
    begin
      SetLength(FSortedFields, 1);
      Sort(FSortedFields);
    end;
  end;
end;

procedure TJvDBUltimGrid.SetSortExcludedFields(const Value: string);
begin
  if FSortExcludedFields <> Value then
    FSortExcludedFields := StringReplace(Trim(Value), ';', ',', [rfReplaceAll]);
end;

procedure TJvDBUltimGrid.SetMasterFields(const Value: string);
begin
  if FMasterFields <> Value then
    FMasterFields := StringReplace(Trim(Value), ';', ',', [rfReplaceAll]);
end;

procedure TJvDBUltimGrid.ClearSortedFields;
var
  SF: Integer;
begin
  if Assigned(FSortedFields) then
  begin
    for SF := 0 to Length(FSortedFields) - 1 do
      ChangeSortMarker(smNone);
    SetLength(FSortedFields, 0);
    Invalidate;
  end;
end;

procedure TJvDBUltimGrid.DoTitleClick(ACol: Longint; AField: TField);
var
  Keys: TKeyboardState;
  Found, ShiftOrCtrlKeyPressed: Boolean;
  SortArraySize: Integer;
  FieldsToSort: TSortFields;
  I: Integer;
  SortFieldName: string;

  function IsExcluded(F: TField): Boolean;
  var
    I: Integer;
  begin
    Result:=False;
    if FSortExcludedFields <> '' then
      for I:=1 to WordCount(FSortExcludedFields, [',']) do
        if AnsiSameText(F.FieldName, Trim(ExtractWord(I, FSortExcludedFields, [',']))) then 
        begin
          Result := True;
          Exit;
        end;
  end;

begin
  FSortOK := False;
  try
    if AutoSort and Assigned(AField) and not IsExcluded(AField) then
    begin
      Found := False;
      SortArraySize := 1;

      SortFieldName := AField.FieldName;
      { Let the user override the sort marker field }
      GetSortFieldName(SortFieldName);

      if Assigned(FSortedFields) then
      begin
        ShiftOrCtrlKeyPressed := MultiColSort and GetKeyboardState(Keys);
        if ShiftOrCtrlKeyPressed then
          ShiftOrCtrlKeyPressed :=
            (((Keys[VK_SHIFT] and $80) <> 0) or ((Keys[VK_CONTROL] and $80) <> 0));
        SetLength(FieldsToSort, Length(FSortedFields));
        for I := 0 to Length(FSortedFields) - 1 do
        begin
          FieldsToSort[I].Name := FSortedFields[I].Name;
          if AnsiSameText(SortFieldName, FSortedFields[I].Name) then
          begin
            Found := True;
            if not ShiftOrCtrlKeyPressed then
            begin
              SetLength(FieldsToSort, 1);
              FieldsToSort[0].Name := SortFieldName;
              FieldsToSort[0].Order := not FSortedFields[I].Order;
              Break;
            end
            else
              FieldsToSort[I].Order := not FSortedFields[I].Order;
          end
          else
            FieldsToSort[I].Order := FSortedFields[I].Order;
        end;
        if (not Found) and ShiftOrCtrlKeyPressed then
          SortArraySize := Length(FSortedFields) + 1;
      end;
      if not Found then
      begin
        SetLength(FieldsToSort, SortArraySize);
        FieldsToSort[SortArraySize - 1].Name := SortFieldName;
        FieldsToSort[SortArraySize - 1].Order := JvGridSort_ASC;
      end;
      Sort(FieldsToSort);
    end;
  finally
    if Assigned(OnTitleBtnClick) then
      OnTitleBtnClick(Self, ACol, AField);
  end;
end;

procedure TJvDBUltimGrid.GetSortFieldName(var FieldName: string);
begin
  if Assigned(FOnGetSortFieldName) then
    FOnGetSortFieldName(Self, FieldName);
end;

procedure TJvDBUltimGrid.SaveGridPosition;
begin
  FSavedBookmark := DataLink.DataSet.Bookmark;
  FSavedRowPos := DataLink.ActiveRecord;
end;

procedure TJvDBUltimGrid.RestoreGridPosition(Mode: TResyncMode = [rmExact, rmCenter]);
begin
  if Assigned(FOnRestoreGridPosition) then
  begin
    if (FSavedBookmark <> {$IFDEF RTL200_UP}nil{$ELSE}''{$ENDIF RTL200_UP}) and DataLink.DataSet.BookmarkValid({$IFNDEF RTL200_UP}Pointer{$ENDIF ~RTL200_UP}(FSavedBookmark)) then
      GotoBookmarkEx(DataLink.DataSet, {$IFNDEF RTL200_UP}Pointer{$ENDIF ~RTL200_UP}(FSavedBookmark), [rmExact], False);

    DataLink.ActiveRecord := FSavedRowPos;
    FOnRestoreGridPosition(Self, {$IFNDEF RTL200_UP}Pointer{$ENDIF ~RTL200_UP}(FSavedBookmark), FSavedRowPos);
  end
  else
  if (FSavedBookmark <> {$IFDEF RTL200_UP}nil{$ELSE}''{$ENDIF RTL200_UP}) and DataLink.DataSet.BookmarkValid({$IFNDEF RTL200_UP}Pointer{$ENDIF ~RTL200_UP}(FSavedBookmark)) then
    GotoBookmarkEx(DataLink.DataSet, {$IFNDEF RTL200_UP}Pointer{$ENDIF ~RTL200_UP}(FSavedBookmark), Mode, False);
end;

function TJvDBUltimGrid.PrivateSearch(var ResultCol: Integer; var ResultField: TField;
  const CaseSensitive, WholeFieldOnly, Next: Boolean): Boolean;
var
  DSet: TDataSet;
  Start, ColNo, I: Integer;
  Found: Boolean;
  FieldText: string;
  ValueToSearchStr: string;
begin
  Result := False;
  if Assigned(DataLink) and DataLink.Active then
  begin
    Screen.Cursor := crHourGlass;
    DSet := DataSource.DataSet;
    DSet.DisableControls;
    try
      // Start location
      SaveGridPosition;
      if Next then
      begin
        Start := Col;
        if not (dgIndicator in Options) then
          Inc(Start);
      end
      else
      begin
        Start := 0;
        DSet.First;
      end;

      ValueToSearchStr := VarToStr(FValueToSearch);
      // The search begins...
      while not DSet.Eof do
      begin
        for ColNo := Start to Columns.Count - 1 do
          for I := 0 to SearchFields.Count - 1 do
          begin
            if AnsiSameText(SearchFields[I], Columns[ColNo].FieldName) then
              with Columns[ColNo].Field do
              begin
                if Assigned(OnGetText) then
                  FieldText := DisplayText
                else
                  FieldText := AsString;
                if FieldText <> '' then
                begin
                  // Search inside the field content
                  if CaseSensitive then
                  begin
                    if WholeFieldOnly then
                      Found := AnsiSameStr(ValueToSearchStr, FieldText)
                    else
                      Found := StrSearch(ValueToSearchStr, FieldText) > 0;
                  end
                  else
                  begin
                    if WholeFieldOnly then
                      Found := AnsiSameText(ValueToSearchStr, FieldText)
                    else
                      Found := StrFind(ValueToSearchStr, FieldText) > 0;
                  end;

                  // Text found ! -> exit
                  if Found then
                  begin
                    ResultCol := ColNo;
                    if dgIndicator in Options then
                      Inc(ResultCol);
                    ResultField := Columns[ColNo].Field;
                    Result := True;
                    Exit;
                  end;
                end;
              end;
          end;
        Start := 0;
        DSet.Next;
      end;
    finally
      DSet.EnableControls;
      Screen.Cursor := crDefault;
    end;
  end;
end;

function TJvDBUltimGrid.Search(const ValueToSearch: Variant; var ResultCol: Integer;
  var ResultField: TField; const CaseSensitive, WholeFieldOnly, Focus: Boolean): Boolean;
begin
  Result := False;
  if (SearchFields.Count > 0) and (ValueToSearch <> Null) and (ValueToSearch <> '') then
  begin
    FValueToSearch := ValueToSearch;
    Result := PrivateSearch(ResultCol, ResultField, CaseSensitive, WholeFieldOnly, False);
    if Result then
    begin
      Self.Col := ResultCol;
      if Focus and Self.Visible and Self.CanFocus then
        Self.SetFocus;
    end
    else
      RestoreGridPosition;
  end;
end;

function TJvDBUltimGrid.SearchNext(var ResultCol: Integer; var ResultField: TField;
  const CaseSensitive, WholeFieldOnly, Focus: Boolean): Boolean;
begin
  Result := False;
  if (SearchFields.Count > 0) and (FValueToSearch <> Null) and (FValueToSearch <> '') then
  begin
    Result := PrivateSearch(ResultCol, ResultField, CaseSensitive, WholeFieldOnly, True);
    if Result then
    begin
      Self.Col := ResultCol;
      if Focus and Self.Visible and Self.CanFocus then
        Self.SetFocus;
    end
    else
      RestoreGridPosition;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.