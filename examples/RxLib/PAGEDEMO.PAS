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

unit PageDemo;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, DBCtrls, DB, DBTables, JvPageManager,
  JvComponent, JvFormPlacement, JvBDEQuery, JvDBCombobox, JvToolEdit,
  JvDBControls, Mask, JvLabel, Grids, DBGrids, ExtCtrls, JvExControls,
  JvDBGrid, JvExStdCtrls, JvExMask, JvExDBGrids;

type
  TClientAssistanceMode = (camNew, camEdit, camDelete, camView);
  TClientAssistantPage = (pageToDo, pageSearchConfirm, pageSearchParams,
    pageClientsBrowse, pageClientEdit);

  TClientAssistant = class(TForm)
    BtnPanel: TPanel;
    Bevel1: TBevel;
    Notebook: TNotebook;
    BackBtn: TButton;
    NextBtn: TButton;
    CloseBtn: TButton;
    SaveBtn: TButton;
    NewBtn: TRadioButton;
    EditBtn: TRadioButton;
    DeleteBtn: TRadioButton;
    ClientsDS: TDataSource;
    Image1: TImage;
    Label1: TLabel;
    Label16: TLabel;
    ViewBtn: TRadioButton;
    Image2: TImage;
    Label17: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    Label27: TLabel;
    SrchName: TEdit;
    SrchFirstName: TEdit;
    SrchCity: TEdit;
    SrchState: TEdit;
    SrchOccupation: TEdit;
    ExactBtn: TCheckBox;
    DoSearchBtn: TRadioButton;
    SkipSearchBtn: TRadioButton;
    Image3: TImage;
    NewSearchHint: TLabel;
    Label24: TLabel;
    Image4: TImage;
    Label25: TLabel;
    Grid: TJvDBGrid ;
    SearchQuery: TJvQuery ;
    SearchQueryDS: TDataSource;
    FormStorage: TJvFormStorage ;
    PageManager: TJvPageManager ;
    ToDo: TJvPageProxy ;
    SearchConfirm: TJvPageProxy ;
    SearchParams: TJvPageProxy ;
    ClientsBrowse: TJvPageProxy ;
    ClientEdit: TJvPageProxy ;
    Clients: TTable;
    SearchQueryLAST_NAME: TStringField;
    SearchQueryFIRST_NAME: TStringField;
    SearchQueryACCT_NBR: TFloatField;
    SearchQueryADDRESS_1: TStringField;
    SearchQueryCITY: TStringField;
    SearchQuerySTATE: TStringField;
    SearchQueryZIP: TStringField;
    SearchQueryTELEPHONE: TStringField;
    SearchQueryDATE_OPEN: TDateField;
    SearchQuerySS_NUMBER: TFloatField;
    SearchQueryBIRTH_DATE: TDateField;
    SearchQueryRISK_LEVEL: TStringField;
    SearchQueryOCCUPATION: TStringField;
    SearchQueryOBJECTIVES: TStringField;
    SearchQueryINTERESTS: TStringField;
    Image5: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    EditACCT_NBR: TDBEdit;
    EditFIRST_NAME: TDBEdit;
    EditADDRESS_: TDBEdit;
    EditCITY: TDBEdit;
    EditSTATE: TDBEdit;
    EditZIP: TDBEdit;
    EditTELEPHONE: TDBEdit;
    EditOCCUPATION: TDBEdit;
    EditLAST_NAME: TDBEdit;
    Label9: TLabel;
    Label10: TLabel;
    EditSS_NUMBER: TDBEdit;
    Label13: TLabel;
    Label15: TLabel;
    EditOBJECTIVES: TDBEdit;
    EditBIRTH_DATE: TJvDBDateEdit ;
    Label5: TLabel;
    EditINTERESTS: TDBEdit;
    EditIMAGE: TDBImage;
    EditDATE_OPEN: TJvDBDateEdit ;
    RxLabel1: TJvLabel ;
    ClientsLAST_NAME: TStringField;
    ClientsFIRST_NAME: TStringField;
    ClientsACCT_NBR: TFloatField;
    ClientsADDRESS_1: TStringField;
    ClientsCITY: TStringField;
    ClientsSTATE: TStringField;
    ClientsZIP: TStringField;
    ClientsTELEPHONE: TStringField;
    ClientsDATE_OPEN: TDateField;
    ClientsSS_NUMBER: TFloatField;
    ClientsPICTURE: TStringField;
    ClientsBIRTH_DATE: TDateField;
    ClientsRISK_LEVEL: TStringField;
    ClientsOCCUPATION: TStringField;
    ClientsOBJECTIVES: TStringField;
    ClientsINTERESTS: TStringField;
    ClientsIMAGE: TBlobField;
    ComboRISK_LEVEL: TJvDBComboBox;
    procedure SaveBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PageManagerGetNextPage(CurrentPage: Integer;
      var NewPage: Integer);
    procedure PageManagerGetPriorPage(CurrentPage: Integer;
      var NewPage: Integer);
    procedure ToDoLeave(Next: Boolean);
    procedure SearchParamsLeave(Next: Boolean);
    procedure ClientsBrowseLeave(Next: Boolean);
    procedure ClientEditEnter(Next: Boolean);
    procedure ClientEditLeave(Next: Boolean);
    procedure ClientsBrowseShow(Next: Boolean);
    procedure CloseBtnClick(Sender: TObject);
    procedure ClientDataChange(Sender: TField);
    procedure ClientEditShow(Next: Boolean);
    procedure EditChange(Sender: TObject);
  private
    { Private declarations }
    procedure DoSearch;
  public
    { Public declarations }
    Mode: TClientAssistanceMode;
  end;

implementation

uses JvDBUtils, JvBdeUtils, Main;

{$R *.DFM}

{$B-}

procedure TClientAssistant.FormCreate(Sender: TObject);
begin
  Clients.Open;
end;

procedure TClientAssistant.SaveBtnClick(Sender: TObject);
begin
  Clients.Post;
  PageManager.SetPage(Integer(pageToDo), True);
end;

procedure TClientAssistant.ToDoLeave(Next: Boolean);
begin
  if NewBtn.Checked then Mode := camNew
  else if EditBtn.Checked then Mode := camEdit
  else if DeleteBtn.Checked then Mode := camDelete
  else if ViewBtn.Checked then Mode := camView;
end;

procedure TClientAssistant.SearchParamsLeave(Next: Boolean);
begin
  if Next then DoSearch;
end;

procedure TClientAssistant.ClientsBrowseLeave(Next: Boolean);
begin
  if Next then begin
    if IsDataSetEmpty(SearchQuery) or not
      DataSetFindValue(Clients, SearchQuery.FieldByName('ACCT_NBR').AsString,
        'ACCT_NBR') then DatabaseError('No data found');
    if (Mode = camDelete) and ConfirmDelete then begin
      Clients.Delete;
      RefreshQuery(SearchQuery);
    end;
  end
  else SearchQuery.Close;
end;

procedure TClientAssistant.ClientEditEnter(Next: Boolean);
begin
  if Mode = camNew then Clients.Append
  else if Mode = camEdit then Clients.Edit;
  SaveBtn.Visible := (Clients.State in [dsInsert, dsEdit]);
  SaveBtn.Enabled := False;
end;

procedure TClientAssistant.ClientEditLeave(Next: Boolean);
begin
  if not Next then ConfirmDataSetCancel(Clients);
  SaveBtn.Visible := False;
end;

procedure TClientAssistant.DoSearch;
begin
  with SearchQuery do begin
    Close;
    MacroByName('LAST_NAME').AsString := FormatSQLCondition('LAST_NAME',
      '', SrchName.Text, ftString, ExactBtn.Checked);
    MacroByName('FIRST_NAME').AsString := FormatSQLCondition('FIRST_NAME',
      '', SrchFirstName.Text, ftString, ExactBtn.Checked);
    MacroByName('CITY').AsString := FormatSQLCondition('CITY',
      '', SrchCity.Text, ftString, ExactBtn.Checked);
    MacroByName('OCCUPATION').AsString := FormatSQLCondition('OCCUPATION',
      '', SrchOccupation.Text, ftString, ExactBtn.Checked);
    MacroByName('STATE').AsString := FormatSQLCondition('STATE',
      '', SrchState.Text, ftString, ExactBtn.Checked);
    Open;
  end;
end;

procedure TClientAssistant.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Clients.Cancel;
end;

procedure TClientAssistant.PageManagerGetNextPage(CurrentPage: Integer;
  var NewPage: Integer);
begin
  case TClientAssistantPage(CurrentPage) of
    pageTodo:
      begin
        case Mode of
          camNew:
            NewPage := Integer(pageSearchConfirm);
          camEdit, camDelete, camView:
            NewPage := Integer(pageSearchParams);
        end;
      end;
    pageSearchConfirm:
      begin
        if DoSearchBtn.Checked then
          NewPage := Integer(pageSearchParams)
        else if SkipSearchBtn.Checked then
          NewPage := Integer(pageClientEdit);
      end;
    pageSearchParams: NewPage := Succ(CurrentPage);
    pageClientsBrowse:
      begin
        NewPage := Succ(CurrentPage);
        if Mode = camDelete then NewPage := CurrentPage
        else if Mode in [camEdit, camView] then begin
          if IsDataSetEmpty(SearchQuery) then NewPage := -1;
        end;
      end;
    pageClientEdit: NewPage := -1;
  end;
end;

procedure TClientAssistant.PageManagerGetPriorPage(CurrentPage: Integer;
  var NewPage: Integer);
begin
  case TClientAssistantPage(CurrentPage) of
    pageTodo: NewPage := -1;
    pageSearchConfirm: NewPage := Pred(CurrentPage);
    pageSearchParams:
      begin
        case Mode of
          camNew:
            NewPage := Integer(pageSearchConfirm);
          camEdit, camDelete, camView:
            NewPage := Integer(pageToDo);
        end;
      end;
    pageClientsBrowse: NewPage := Pred(CurrentPage);
    pageClientEdit:
      begin
        if not DoSearchBtn.Checked and (Mode = camNew) then
          NewPage := Integer(pageSearchConfirm)
        else NewPage := Integer(pageClientsBrowse);
      end;
  end;
end;

procedure TClientAssistant.ClientsBrowseShow(Next: Boolean);
begin
  ActiveControl := Grid;
end;

procedure TClientAssistant.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TClientAssistant.ClientDataChange(Sender: TField);
begin
  SaveBtn.Enabled := True;
end;

procedure TClientAssistant.ClientEditShow(Next: Boolean);
begin
  ActiveControl := EditACCT_NBR;
end;

procedure TClientAssistant.EditChange(Sender: TObject);
begin
  SaveBtn.Enabled := True;
end;

end.
