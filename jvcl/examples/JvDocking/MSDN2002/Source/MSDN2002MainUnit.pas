{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}


unit MSDN2002MainUnit;
{$I jvcl.inc}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, ImgList, JvDockControlForm, JvDockVIDStyle,
  JvDockVSNetStyle, StdCtrls, ComCtrls, ToolWin, ExtCtrls, shdocvw
  
  {$IFDEF USEJVCL}
  , JvComponent, JvAppStorage, JvAppIniStorage
  {$ENDIF};

type
  { Helper class; set property Form, and then use the properties or methods
    of the adapter class to manipulate the form }
  TDockFormAdapter = class
  private
    FForm: TCustomForm;
    FDockClient: TJvDockClient;
    function GetCanAutoHide: Boolean;
    function GetDockClient: TJvDockClient;
    function GetIsAutoHidden: Boolean;
    function GetIsDockable: Boolean;
    function GetIsFloating: Boolean;
    function GetIsTabbedDocument: Boolean;
    procedure SetForm(AForm: TCustomForm);
    procedure SetIsAutoHidden(const Value: Boolean);
    procedure SetIsDockable(const Value: Boolean);
    procedure SetIsFloating(const Value: Boolean);
    procedure SetIsTabbedDocument(const Value: Boolean);
  public
    constructor Create(AForm: TCustomForm);

    procedure Float;
    procedure Hide;
    procedure Show;
    procedure AutoHide;
    procedure UnAutoHide;

    property Form: TCustomForm read FForm write SetForm;
    property DockClient: TJvDockClient read GetDockClient;

    { In Visual Studio there are 5 states a window can be in

      * Floating
      * Dockable
      * Tabbed Document (in the mdi space) // not implemented
      * AutoHide
      * Hide
    }

    property CanAutoHide: Boolean read GetCanAutoHide;
    property IsAutoHidden: Boolean read GetIsAutoHidden write SetIsAutoHidden;
    property IsTabbedDocument: Boolean read GetIsTabbedDocument write SetIsTabbedDocument;
    property IsDockable: Boolean read GetIsDockable write SetIsDockable;
    property IsFloating: Boolean read GetIsFloating write SetIsFloating;
  end;

  TMSDN2002 = class(TForm)
    ActionList1: TActionList;
    File_File_Action: TAction;
    MainMenu1: TMainMenu;
    File_Print_Action: TAction;
    File_Exit_Action: TAction;
    Edit_Edit_Action: TAction;
    Edit_Undo_Action: TAction;
    Edit_Redo_Action: TAction;
    Edit_Cut_Action: TAction;
    Edit_Copy_Action: TAction;
    Edit_Paste_Action: TAction;
    Edit_Delete_Action: TAction;
    Edit_Select_All_Action: TAction;
    Edit_Find_in_this_Topic_Action: TAction;
    View_Web_Browser_Action: TAction;
    View_Navigation_Action: TAction;
    View_Toolbars_Action: TAction;
    View_Full_Screen_Action: TAction;
    View_Text_Size_Action: TAction;
    View_View_Source_Action: TAction;
    View_Web_Browser_Show_Web_Browser_Action: TAction;
    View_Web_Browser_Web_Navigate_Back_Action: TAction;
    View_Web_Browser_Web_Navigate_Forward_Action: TAction;
    View_Web_Browser_Home_Action: TAction;
    View_Web_Browser_Search_Action: TAction;
    View_Toolbars_Full_Screen_Action: TAction;
    View_Toolbars_Standard_Action: TAction;
    View_Toolbars_Customize_Action: TAction;
    View_Text_Size_Largest_Action: TAction;
    View_Text_Size_Larger_Action: TAction;
    View_Text_Size_Medium_Action: TAction;
    View_Text_Size_Smaller_Action: TAction;
    View_Text_Size_Smallest_Action: TAction;
    Tools_Tools_Action: TAction;
    Tools_Customize_Action: TAction;
    Tools_Options_Action: TAction;
    Window_Window_Action: TAction;
    Window_New_Window_Action: TAction;
    Window_Dockable_Action: TAction;
    Window_Split_Action: TAction;
    Window_Hide_Action: TAction;
    Window_Floating_Action: TAction;
    Window_Auto_Hide_Action: TAction;
    Window_Close_All_Document_Action: TAction;
    Window_Windows_Action: TAction;
    Help_Help_Action: TAction;
    Help_Contents_Action: TAction;
    Help_Index_Action: TAction;
    Help_Search_Action: TAction;
    Help_Favorites_Action: TAction;
    Help_Index_results_Action: TAction;
    Help_Search_results_Action: TAction;
    Help_Edit_Filters_Action: TAction;
    Help_Previous_topic_Action: TAction;
    Help_Next_topic_Action: TAction;
    Help_Sync_Contents_Action: TAction;
    Help_Technical_Support_Action: TAction;
    Help_Help_on_Help_Action: TAction;
    Help_About_Action: TAction;
    ImageList1: TImageList;
    File1: TMenuItem;
    Print1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    N3: TMenuItem;
    FindinthisTopic1: TMenuItem;
    ViewSource1: TMenuItem;
    View_View_Action: TAction;
    WebBrowser1: TMenuItem;
    ShowWebBrowser1: TMenuItem;
    WebNavigateBack1: TMenuItem;
    WebNavigateForward1: TMenuItem;
    Home1: TMenuItem;
    Search1: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    Navigation1: TMenuItem;
    Contents1: TMenuItem;
    Index1: TMenuItem;
    Search2: TMenuItem;
    Favorites1: TMenuItem;
    Toolbars1: TMenuItem;
    N6: TMenuItem;
    FullScreen1: TMenuItem;
    Standard1: TMenuItem;
    N7: TMenuItem;
    Customize1: TMenuItem;
    FullScreen2: TMenuItem;
    N8: TMenuItem;
    ViewTextSizeAction1: TMenuItem;
    N9: TMenuItem;
    ViewSource2: TMenuItem;
    Largest1: TMenuItem;
    Larger1: TMenuItem;
    Medium1: TMenuItem;
    Smaller1: TMenuItem;
    Smallest1: TMenuItem;
    Tools1: TMenuItem;
    Customize2: TMenuItem;
    Options1: TMenuItem;
    Window1: TMenuItem;
    Split1: TMenuItem;
    NewWindow1: TMenuItem;
    Dockable1: TMenuItem;
    N10: TMenuItem;
    Hide1: TMenuItem;
    Floating1: TMenuItem;
    N11: TMenuItem;
    AutoHide1: TMenuItem;
    CloseAllDocument1: TMenuItem;
    N12: TMenuItem;
    Windows1: TMenuItem;
    Help1: TMenuItem;
    Contents2: TMenuItem;
    Index2: TMenuItem;
    Search3: TMenuItem;
    Favorites2: TMenuItem;
    Indexresults1: TMenuItem;
    Searchresults1: TMenuItem;
    EditFilters1: TMenuItem;
    Previoustopic1: TMenuItem;
    Nexttopic1: TMenuItem;
    SyncContents1: TMenuItem;
    TechnicalSupport1: TMenuItem;
    HelponHelp1: TMenuItem;
    About1: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    Exit1: TMenuItem;
    lbDockServer1: TJvDockServer;
    ControlBar1: TControlBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    View_Web_Browser_Stop_Browser_Action: TAction;
    View_Web_Browser_Refresh_Browser_Action: TAction;
    Help_Add_to_Favorites_Action: TAction;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    URLComboBox: TComboBox;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    StatusBar1: TStatusBar;
    PopupMenu1: TPopupMenu;
    Dockable_Item: TMenuItem;
    Hide_Item: TMenuItem;
    Float_Item: TMenuItem;
    AutoHide_Item: TMenuItem;
    JvDockVSNetStyle1: TJvDockVSNetStyle;
    procedure File_Print_ActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Help_Contents_ActionExecute(Sender: TObject);
    procedure Help_Index_ActionExecute(Sender: TObject);
    procedure Help_Search_ActionExecute(Sender: TObject);
    procedure Help_Favorites_ActionExecute(Sender: TObject);
    procedure Help_Index_results_ActionExecute(Sender: TObject);
    procedure Help_Search_results_ActionExecute(Sender: TObject);
    procedure File_File_ActionExecute(Sender: TObject);
    procedure File_Exit_ActionExecute(Sender: TObject);
    procedure Hide_ItemClick(Sender: TObject);
    procedure Float_ItemClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Dockable_ItemClick(Sender: TObject);
    procedure AutoHide_ItemClick(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF USEJVCL}
    JvAppStorage:TJvAppIniFileStorage;
    {$ENDIF}
    FAdapter: TDockFormAdapter;
    procedure CreateXPMenu;          //
    procedure CreateToolForm;        // create everything
    procedure DefaultDockLayout;
    procedure LoadDockInfo;
    procedure LoadToolFormLayout;    // load previous layout
    procedure SaveToolFormLayout;    // save current layout
    procedure CreateVSNETPageControl;
  public
    { Public declarations }
  end;

var
  MSDN2002: TMSDN2002;

implementation

uses
  XPMenu, ContentsFormUnit, FavoritesFormUnit, IndexFormUnit,
  IndexResultFormUnit, SearchFormUnit, SearchResultFormUnit,
  JvDockGlobals;
const
  cStorageFilename = 'DockLayout.ini';
type
  TAccessWinControl = class(TWinControl);

{$R *.DFM}

procedure TMSDN2002.CreateXPMenu;
begin
  with TXPMenu.Create(Self) do
    Active := True;
end;

procedure TMSDN2002.AutoHide_ItemClick(Sender: TObject);
begin
  if PopupMenu1.PopupComponent is TForm then
    with FAdapter do
    begin
      Form := TForm(PopupMenu1.PopupComponent);
      IsAutoHidden := not IsAutoHidden;
    end;
end;

procedure TMSDN2002.CreateToolForm;
begin
  ContentsForm := TContentsForm.Create(Application);
  ImageList1.GetIcon(20, ContentsForm.Icon);
  IndexForm := TIndexForm.Create(Application);
  ImageList1.GetIcon(21, IndexForm.Icon);
  SearchForm := TSearchForm.Create(Application);
  ImageList1.GetIcon(22, SearchForm.Icon);
  FavoritesForm := TFavoritesForm.Create(Application);
  ImageList1.GetIcon(23, FavoritesForm.Icon);
  IndexResultForm := TIndexResultForm.Create(Application);
  ImageList1.GetIcon(25, IndexResultForm.Icon);
  SearchResultForm := TSearchResultForm.Create(Application);
  ImageList1.GetIcon(26, SearchResultForm.Icon);
end;

procedure TMSDN2002.LoadDockInfo;
begin
  CreateXPMenu;
  CreateToolForm;
  if not FileExists(ExtractFilePath(Application.ExeName) + cStorageFilename) then
    DefaultDockLayout
  else
    LoadToolFormLayout;
end;

procedure TMSDN2002.DefaultDockLayout;

  { Dock AForm on APanel, and set it to 'auto hide' }
  procedure VSNetDockForm(AForm: TForm; APanel: TJvDockVSNETPanel);
  begin
    //  AForm.Width := 180;
    AForm.Top := 10000;
    //  AForm.Visible := true;
    AForm.ManualDock(APanel,nil,APanel.Align);
    APanel.ShowDockPanel(True, AForm);
    APanel.DoHideControl(AForm);
  end;
begin
  lbDockServer1.RightDockPanel.Width := 180;
  lbDockServer1.LeftDockPanel.Width := 180;
  lbDockServer1.BottomDockPanel.Height := 100;

  VSNetDockForm(ContentsForm, lbDockServer1.RightDockPanel as TJvDockVSNETPanel);
  VSNetDockForm(FavoritesForm, lbDockServer1.RightDockPanel as TJvDockVSNETPanel);
  VSNetDockForm(IndexForm, lbDockServer1.LeftDockPanel as TJvDockVSNETPanel);
  VSNetDockForm(IndexResultForm, lbDockServer1.LeftDockPanel as TJvDockVSNETPanel);
  VSNetDockForm(SearchForm, lbDockServer1.BottomDockPanel as TJvDockVSNETPanel);
  VSNetDockForm(SearchResultForm, lbDockServer1.BottomDockPanel as TJvDockVSNETPanel);
end;

procedure TMSDN2002.LoadToolFormLayout;
begin
  {$IFDEF USEJVCL}
  JvAppStorage.Filename := ExtractFilePath(Application.ExeName) + cStorageFilename;
  JvAppStorage.Reload; // !!!
  LoadDockTreeFromAppStorage(JvAppStorage);
  {$ELSE}
  LoadDockTreeFromFile(ExtractFilePath(Application.ExeName) + cStorageFilename);
  {$ENDIF}
end;

procedure TMSDN2002.SaveToolFormLayout;
begin
  {$IFDEF USEJVCL}
  JvAppStorage.Filename := ExtractFilePath(Application.ExeName) + cStorageFilename;
  SaveDockTreeToAppStorage(JvAppStorage);
  JvAppStorage.Flush;  // !!!
  {$ELSE}
  SaveDockTreeToFile(ExtractFilePath(Application.ExeName) + cStorageFilename);
  {$ENDIF}
end;

procedure TMSDN2002.File_Print_ActionExecute(Sender: TObject);
begin
  { Show some info about action }
  ShowMessage(Format('You clicked ''%s''', [TAction(Sender).Caption]));
end;

procedure TMSDN2002.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SaveToolFormLayout;
  FAdapter.Free;
end;

procedure TMSDN2002.FormCreate(Sender: TObject);
begin
  FAdapter := TDockFormAdapter.Create(nil);
  {$IFDEF USEJVCL}
  JvAppStorage := TJvAppIniFileStorage.Create(self);
  {$ENDIF}
  CreateVSNETPageControl;
  LoadDockInfo;
end;

procedure TMSDN2002.Help_Contents_ActionExecute(Sender: TObject);
begin
  ShowDockForm(ContentsForm);
end;

procedure TMSDN2002.Help_Index_ActionExecute(Sender: TObject);
begin
  ShowDockForm(IndexForm);
end;

procedure TMSDN2002.Help_Search_ActionExecute(Sender: TObject);
begin
  ShowDockForm(SearchForm);
end;

procedure TMSDN2002.Help_Favorites_ActionExecute(Sender: TObject);
begin
  ShowDockForm(FavoritesForm);
end;

procedure TMSDN2002.Help_Index_results_ActionExecute(
  Sender: TObject);
begin
  ShowDockForm(IndexResultForm);
end;

procedure TMSDN2002.Help_Search_results_ActionExecute(
  Sender: TObject);
begin
  ShowDockForm(SearchResultForm);
end;

procedure TMSDN2002.CreateVSNETPageControl;
var Page: TJvDockVSNETTabPageControl;
  Sheet: TJvDockVSNETTabSheet;
  WebBrowser: TWebBrowser;
  URL: OleVariant;
  PageFont: TFont;
begin
  Page := TJvDockVSNETTabPageControl.Create(Self);
  Page.Parent := Self;
  Page.Align := alClient;
  Page.TabPosition := tpTop;
  Page.ActiveSheetColor := clBtnface;
  Page.InactiveFont.Color := clGray;
  Page.InactiveSheetColor := VSNETPageInactiveSheetColor;
  Page.ParentFont := True;
  SetDockSite(Page, False);
  Page.SendToBack;
  Sheet := TJvDockVSNETTabSheet.Create(Page);
  Sheet.PageControl := Page;
  Sheet.Caption := 'MSDN Library Start';
  PageFont := Page.ActiveFont;
  PageFont.Name := 'Tahoma';
  PageFont.Style := [fsBold];
  Page.ActiveFont := PageFont;

  WebBrowser := TWebBrowser.Create(Sheet);
  TWinControl(WebBrowser).Parent := Sheet;
  WebBrowser.Align := alClient;
  URL := ExtractFilePath(Application.EXEName) + 'msdnstart\msdnstart.htm';
  WebBrowser.Navigate2(URL);
end;

procedure TMSDN2002.File_File_ActionExecute(Sender: TObject);
begin
  // do nothing
end;

procedure TMSDN2002.File_Exit_ActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMSDN2002.PopupMenu1Popup(Sender: TObject);
begin
  if PopupMenu1.PopupComponent is TForm then
    with FAdapter do
    begin
      Form := TForm(PopupMenu1.PopupComponent);
      AutoHide_Item.Checked := IsAutoHidden;
      AutoHide_Item.Visible := CanAutoHide;
      Dockable_Item.Checked := IsDockable;
      Dockable_Item.Enabled := not IsAutoHidden;
      Float_Item.Checked := IsFloating;
      Float_Item.Enabled := not IsAutoHidden;
      Hide_Item.Enabled := not IsAutoHidden;
    end;
end;

procedure TMSDN2002.Dockable_ItemClick(Sender: TObject);
begin
  if PopupMenu1.PopupComponent is TForm then
    with FAdapter do
    begin
      Form := TForm(PopupMenu1.PopupComponent);
      IsDockable := not IsDockable;
    end;
end;

procedure TMSDN2002.Hide_ItemClick(Sender: TObject);
begin
  if PopupMenu1.PopupComponent is TForm then
    with FAdapter do
    begin
      Form := TForm(PopupMenu1.PopupComponent);
      Hide;
    end;
end;

procedure TMSDN2002.Float_ItemClick(Sender: TObject);
begin
  if PopupMenu1.PopupComponent is TForm then
    with FAdapter do
    begin
      Form := TForm(PopupMenu1.PopupComponent);
      IsFloating := not IsFloating;
    end;
end;

//=== { TDockFormAdapter } ===================================================

constructor TDockFormAdapter.Create(AForm: TCustomForm);
begin
  inherited Create;
  Form := AForm;
end;

procedure TDockFormAdapter.AutoHide;
begin
  if Form.HostDockSite is TJvDockVSNETPanel then
    TJvDockVSNETPanel(Form.HostDockSite).DoHideControl(Form);
end;

procedure TDockFormAdapter.Float;
begin
  SetIsFloating(True);
end;

function TDockFormAdapter.GetCanAutoHide: Boolean;
begin
  Result := IsAutoHidden or (DockClient.DockState <> JvDockState_Floating);
end;

function TDockFormAdapter.GetDockClient: TJvDockClient;
begin
  if FDockClient = nil then
    FDockClient := FindDockClient(Form);
  Result := FDockClient;
end;

function TDockFormAdapter.GetIsAutoHidden: Boolean;
var
  HostDockSite: TWinControl;
begin
  HostDockSite := Form.HostDockSite;
  while (HostDockSite <> nil) and
    (HostDockSite.Parent <> nil) and
    (HostDockSite.Parent.HostDockSite <> nil) do
    HostDockSite := HostDockSite.Parent.HostDockSite;
  Result := HostDockSite is TJvDockVSPopupPanel;
end;

function TDockFormAdapter.GetIsDockable: Boolean;
begin
  Result := not IsAutoHidden and not IsTabbedDocument and DockClient.EnableDock;
end;

function TDockFormAdapter.GetIsFloating: Boolean;
begin
  Result := not IsAutoHidden and not IsTabbedDocument and not DockClient.EnableDock;
end;

procedure TDockFormAdapter.Hide;
begin
  HideDockForm(Form);
  // or call DockClient.HideParentForm;
end;

procedure TDockFormAdapter.SetForm(AForm: TCustomForm);
begin
  if AForm <> FForm then
  begin
    FDockClient := nil;
    FForm := AForm;
  end;
end;

procedure TDockFormAdapter.SetIsAutoHidden(const Value: Boolean);
begin
  if Value then
    AutoHide
  else
    UnAutoHide;
end;

procedure TDockFormAdapter.SetIsDockable(const Value: Boolean);
begin
  if not IsAutoHidden then
  begin
    if Value then
    begin
      if IsTabbedDocument then
        IsTabbedDocument := False;

      DockClient.EnableDock := True;
    end
    else
    begin
      if not IsFloating then
        IsTabbedDocument := True;
    end;
  end;
end;

procedure TDockFormAdapter.SetIsFloating(const Value: Boolean);
begin
  if not IsAutoHidden then
  begin
    if Value then
    begin
      if IsTabbedDocument then
        IsTabbedDocument := False;

      DockClient.RestoreChild;
      DockClient.EnableDock := False;
    end
    else
    begin
      if not IsDockable then
        IsTabbedDocument := True;
    end;
  end;
end;

procedure TDockFormAdapter.Show;
begin
  ShowDockForm(Form);
  // or call DockClient.ShowParentForm;
end;

procedure TDockFormAdapter.UnAutoHide;
begin
  UnAutoHideDockForm(Form);
end;

function TDockFormAdapter.GetIsTabbedDocument: Boolean;
begin
  { Not implemented }
  Result := False;
end;

procedure TDockFormAdapter.SetIsTabbedDocument(const Value: Boolean);
begin
  { Not implemented }
end;

end.

