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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

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
  if not Search('fish', ResultCol, ResultField, False, True) then ...
end;

// Then:
if not MyUltimGrid.SearchNext(ResultCol, ResultField, False, True) then ...

---= BCB example =---

// Declare these vars as global vars if you want to use SearchNext
int ResultCol;
TField *ResultField;

MyUltimGrid->SearchFields->Clear();
MyUltimGrid->SearchFields->Add("Category");
MyUltimGrid->SearchFields->Add("Common_Name");
MyUltimGrid->SearchFields->Add("Species Name");
MyUltimGrid->SearchFields->Add("Notes");
if (!MyUltimGrid->Search("fish", ResultCol, ResultField, false, true)) ...

// Then:
if (!MyUltimGrid->SearchNext(ResultCol, ResultField, false, true)) ...

-----------------------------------------------------------------------------
Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBUltimGrid;

{$I jvcl.inc}

interface

uses
  {$IFDEF VCL}
  Windows,
  {$ENDIF VCL}
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  Classes, Graphics, Controls, DB, JvDBGrid,
  JvTypes; {JvTypes contains Exception base class}

const
  JvGridSort_ASC = True;
  JvGridSort_UP = True;
  JvGridSort_DESC = False;
  JvGridSort_DOWN = False;

type
  TSortField = Record
    Name: String;
    Order: Boolean;
  End;
  TSortFields = Array Of TSortField;

  TJvDBUltimGrid = class;
  TIndexNotFoundEvent = procedure(Sender: TJvDBUltimGrid; FieldsToSort: TSortFields;
    IndexFieldNames: string; DescFields: string; var Retry: Boolean) of object;
  TUserSortEvent = procedure(Sender: TJvDBUltimGrid; var FieldsToSort: TSortFields;
    SortString: string; var SortOK: Boolean) of object;
  TRestoreGridPosEvent = procedure(Sender: TJvDBUltimGrid; SavedBookmark: TBookmark;
    SavedRowPos: Integer) of object;

  TSortWith = (swIndex, swFields, swUserFunc);

  TJvDBUltimGrid = class(TJvDBGrid)
  private
    FSortedFields: TSortFields;
    FSortWith: TSortWith;
    FSortOK: Boolean;
    FOnIndexNotFound: TIndexNotFoundEvent;
    FOnUserSort: TUserSortEvent;
    FSavedBookmark: TBookmarkStr;
    FSavedRowPos: Integer;
    FOnRestoreGridPosition: TRestoreGridPosEvent;
    FValueToSearch: Variant;
    FSearchFields: TStringList;
    function PrivateSearch(var ResultCol: Integer; var ResultField: TField;
      const CaseSensitive, Next: Boolean): Boolean;
  protected
    function SortMarkerAssigned(const AFieldName: string): Boolean; override;
    procedure DoTitleClick(ACol: Longint; AField: TField); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Sort(FieldsToSort: TSortFields);
    property SortedFields: TSortFields read FSortedFields;
    property SortOK: Boolean read FSortOK;
    procedure SaveGridPosition;
    procedure RestoreGridPosition;
    property SearchFields: TStringList read FSearchFields write FSearchFields;
    function Search(const ValueToSearch: Variant; var ResultCol: Integer;
      var ResultField: TField; const CaseSensitive, Focus: Boolean): Boolean;
    function SearchNext(var ResultCol: Integer; var ResultField: TField;
      const CaseSensitive, Focus: Boolean): Boolean;
  published
    property SortedField stored False; // Property of JvDBGrid not used in JvDBUltimGrid
    property SortMarker stored False;  // Property of JvDBGrid hidden in JvDBUltimGrid

    { SortWith:
      swIndex    : for BDE tables (assignment of OnIndexNotFound is recommended)
      swFields   : for ADO tables
      swUserFunc : for other data providers (assignment of OnUserSort is mandatory) }
    property SortWith: TSortWith read FSortWith write FSortWith default swIndex;
    { OnIndexNotFound: fired when SortWith = swIndex and the sorting index is not found }
    property OnIndexNotFound: TIndexNotFoundEvent
      read FOnIndexNotFound write FOnIndexNotFound;
    { OnUserSort: fired when SortWith = swUserFunc }
    property OnUserSort: TUserSortEvent read FOnUserSort write FOnUserSort;
    { OnRestoreGridPosition: fired when RestoreGridPosition is called }
    property OnRestoreGridPosition: TRestoreGridPosEvent
      read FOnRestoreGridPosition write FOnRestoreGridPosition;
  end;

implementation

uses
  TypInfo, Forms, SysUtils,
  JvResources, DBGrids, JclStrings;

//=== { TJvDBUltimGrid } ==========================================================

constructor TJvDBUltimGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortedFields := nil;
  FSortWith := swIndex;
  FSortOK := True;
  FOnIndexNotFound := nil;
  FOnUserSort := nil;
  FSavedBookmark := '';
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
begin
  Result := False;
  if Assigned(FSortedFields) then
  begin
  for SF := 0 to Length(FSortedFields) - 1 do
    if AnsiSameText(AFieldName, FSortedFields[SF].Name) then
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

  procedure UpdateProp(const PropName: string; const Value: string);
  begin
    SetStrProp(DSet, PropName, Value);
    FSortedFields := FieldsToSort;
    FSortOK := True;
  end;

var
  SortString,
  DescString: string;
  MaxFTS: Integer;

  procedure SearchIndex;
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
          if (IndexDefs.Items[I].DescFields = '')
          and not (ixDescending in IndexDefs.Items[I].Options) then
          begin
            UpdateProp(cIndexName, IndexDefs.Items[I].Name);
            Break;
          end;
        end
        else
        if AnsiSameText(DescString, IndexDefs.Items[I].DescFields) then
        begin
          UpdateProp(cIndexName, IndexDefs.Items[I].Name);
          Break;
        end
        else
        if (IndexDefs.Items[I].DescFields = '')
        and (ixDescending in IndexDefs.Items[I].Options) then
        begin
          for J := 0 to MaxFTS do
            FieldsToSort[J].Order := JvGridSort_DESC;
          UpdateProp(cIndexName, IndexDefs.Items[I].Name);
          Break;
        end;
      end;
  end;

var
  FTS: Integer;
  SortField: TField;
  Retry: Boolean;
begin
  FSortOK := False;
  if Assigned(DataLink) and DataLink.Active and Assigned(FieldsToSort) then
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
    if (SortWith = swIndex) and
    not (IsPublishedProp(DSet, cIndexDefs) and IsPublishedProp(DSet, cIndexName)) then
      raise EJVCLDbGridException.CreateRes(@RsEJvDBGridIndexPropertyMissing)
    else
    if (SortWith = swFields) and
    not IsPublishedProp(DSet, cIndexFieldNames) then
      raise EJVCLDbGridException.CreateRes(@RsEJvDBGridIndexPropertyMissing);

    // Sorting
    Screen.Cursor := crHourGlass;
    DSet.DisableControls;
    try
      SortString := '';
      DescString := '';
      MaxFTS := Length(FieldsToSort) - 1;
      for FTS := 0 to MaxFTS do
      begin
        FieldsToSort[FTS].Name := Trim(FieldsToSort[FTS].Name);
        SortField := DSet.FieldByName(FieldsToSort[FTS].Name);
        if (SortField is TBlobField) or (SortField is TBytesField) or
          ((SortField.FieldKind <> fkData) and (SortField.FieldKind <> fkInternalCalc)) then
          // No sorting of binary or special fields
          raise EJVCLDbGridException.CreateRes(@RsEJvDBGridBadFieldKind)
        else
        if SortWith = swIndex then
        begin
          // Sort with index
          if SortString <> '' Then
            SortString := SortString + ';';
          SortString := SortString + FieldsToSort[FTS].Name;
          if FieldsToSort[FTS].Order = JvGridSort_DESC then
          begin
            if DescString <> '' Then
              DescString := DescString + ';';
            DescString := DescString + FieldsToSort[FTS].Name;
          end;
          if FTS = MaxFTS Then
          begin
            SearchIndex;
            if not SortOK then
            begin
              if Assigned(OnIndexNotFound) then
              begin
                Retry := False;
                OnIndexNotFound(Self, FieldsToSort, SortString, DescString, Retry);
                if Retry then
                begin
                  SearchIndex;
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
        if (SortWith = swFields) or (SortWith = swUserFunc) then
        begin
          // Sort with fields (temporary index) or user function
          if SortString <> '' Then
            SortString := SortString + ',';
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
              UpdateProp(cIndexFieldNames, SortString);
          end;  
        end;
      end;
    finally
      DSet.EnableControls;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TJvDBUltimGrid.DoTitleClick(ACol: Longint; AField: TField);
var
  Keys: TKeyboardState;
  Found,
  ShiftOrCtrlKeyPressed: Boolean;
  SortArraySize: Integer;
  FieldsToSort: TSortFields;
  I: Integer;
begin
  FSortOK := False;
  try
    if Assigned(AField) then
    begin
      Found := False;
      SortArraySize := 1;
      if Assigned(FSortedFields) then
      begin
        ShiftOrCtrlKeyPressed := GetKeyBoardState(Keys);
        if ShiftOrCtrlKeyPressed then
          ShiftOrCtrlKeyPressed := (((Keys[VK_SHIFT] and $80) <> 0)
                                 or ((Keys[VK_CONTROL] and $80) <> 0));
        SetLength(FieldsToSort, Length(FSortedFields));
        for I := 0 to Length(FSortedFields) - 1 do
        begin
          FieldsToSort[I].Name := FSortedFields[I].Name;
          if AnsiSameText(AField.FieldName, FSortedFields[I].Name) Then
          begin
            Found := True;
            if not ShiftOrCtrlKeyPressed then
            begin
              SetLength(FieldsToSort, 1);
              FieldsToSort[0].Name := AField.FieldName;
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
        FieldsToSort[SortArraySize - 1].Name := AField.FieldName;
        FieldsToSort[SortArraySize - 1].Order := JvGridSort_ASC;
      end;
      Sort(FieldsToSort);
    end;
  finally
    if Assigned(OnTitleBtnClick) then
      OnTitleBtnClick(Self, ACol, AField);
  end;
end;

procedure TJvDBUltimGrid.SaveGridPosition;
begin
  FSavedBookmark := DataLink.DataSet.Bookmark;
  FSavedRowPos := DataLink.ActiveRecord;
end;

procedure TJvDBUltimGrid.RestoreGridPosition;
begin
  if Assigned(OnRestoreGridPosition) then
  begin
    // This example for ADO datasets positions the dataset cursor exactly
    // where it was before it moves (put this code into your event):
    //
    // Delphi code:
    // if (MyADODataSet.BookmarkValid(SavedBookmark)) then
    //   MyADODataSet.Recordset.Bookmark := POleVariant(SavedBookmark)^;
    // try MyADODataSet.Resync([rmExact]); except end;
    //
    // BCB code:
    // if (MyADODataSet->BookmarkValid(SavedBookmark))
    //   MyADODataSet->Recordset->Bookmark = *(POleVariant)SavedBookmark;
    // try {MyADODataSet->Resync(TResyncMode() << rmExact);} catch (...) {}
    //
    DataLink.ActiveRecord := FSavedRowPos;
    OnRestoreGridPosition(Self, Pointer(FSavedBookmark), FSavedRowPos);
  end
  else
  if DataLink.DataSet.BookmarkValid(Pointer(FSavedBookmark)) then
    DataLink.DataSet.GotoBookmark(Pointer(FSavedBookmark));
end;

function TJvDBUltimGrid.PrivateSearch(var ResultCol: Integer; var ResultField: TField;
                                      const CaseSensitive, Next: Boolean): Boolean;
var
  DSet: TDataSet;
  Start,
  ColNo, I: Integer;
  Found: Boolean;
  FieldText: string;
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
                    Found := (StrSearch(string(FValueToSearch), FieldText) > 0)
                  else
                    Found := (StrFind(string(FValueToSearch), FieldText) > 0);

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
  var ResultField: TField; const CaseSensitive, Focus: Boolean): Boolean;
begin
  Result := False;
  if (SearchFields.Count > 0) and (ValueToSearch <> Null) and (ValueToSearch <> '') then
  begin
    FValueToSearch := ValueToSearch;
    Result := PrivateSearch(ResultCol, ResultField, CaseSensitive, False);
    if Result and Focus then
    begin
      Self.Col := ResultCol;
      if Self.Visible then
        Self.SetFocus;
    end
    else
      RestoreGridPosition;
  end;
end;

function TJvDBUltimGrid.SearchNext(var ResultCol: Integer; var ResultField: TField;
                                   const CaseSensitive, Focus: Boolean): Boolean;
begin
  Result := False;
  if (SearchFields.Count > 0) and (FValueToSearch <> Null) and (FValueToSearch <> '') then
  begin
    Result := PrivateSearch(ResultCol, ResultField, CaseSensitive, True);
    if Result and Focus then
    begin
      Self.Col := ResultCol;
      if Self.Visible then
        Self.SetFocus;
    end
    else
      RestoreGridPosition;
  end;
end;

end.