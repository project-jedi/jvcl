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
  private
    { Private declarations }
    {$IFDEF USEJVCL}
    JvAppStorage:TJvAppIniFileStorage;
    {$ENDIF}
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
begin

  lbDockServer1.RightDockPanel.Width := 180;
  lbDockServer1.LeftDockPanel.Width := 180;
  lbDockServer1.BottomDockPanel.Height := 100;

//  ContentsForm.Width := 180;
  ContentsForm.Top := 10000;
//  ContentsForm.Visible := true;
  ContentsForm.ManualDock(lbDockServer1.RightDockPanel,nil, alNone);
  lbDockServer1.RightDockPanel.ShowDockPanel(true,ContentsForm);
  TJvDockVSNETPanel(lbDockServer1.RightDockPanel).DoHideControl(ContentsForm);

//  FavoritesForm.Width := 180;
  FavoritesForm.Top := 10000;
//  FavoritesForm.Visible := true;
  FavoritesForm.ManualDock(FavoritesForm,nil,alNone);
  lbDockServer1.RightDockPanel.ShowDockPanel(true,FavoritesForm);
  TJvDockVSNETPanel(lbDockServer1.RightDockPanel).DoHideControl(FavoritesForm);

//  IndexForm.Width := 180;
  IndexForm.Top := 10000;
//  IndexForm.Visible := true;
  IndexForm.ManualDock(IndexForm,lbDockServer1.LeftDockPanel,alLeft);
  lbDockServer1.LeftDockPanel.ShowDockPanel(true,IndexForm);
  TJvDockVSNETPanel(lbDockServer1.LeftDockPanel).DoAutoHideControl(IndexForm);

//  IndexResultForm.Width := 180;
  IndexResultForm.Top := 10000;
//  IndexResultForm.Visible := true;
  IndexResultForm.ManualDock(IndexResultForm,lbDockServer1.LeftDockPanel,alLeft);
  lbDockServer1.LeftDockPanel.ShowDockPanel(true,IndexResultForm);
  TJvDockVSNETPanel(lbDockServer1.LeftDockPanel).DoAutoHideControl(IndexResultForm);

//  SearchForm.Height := 100;
  SearchForm.Top := 10000;
//  SearchForm.Visible := true;
  SearchForm.ManualDock(SearchForm,lbDockServer1.BottomDockPanel,alBottom);
  lbDockServer1.BottomDockPanel.ShowDockPanel(true,SearchForm);
  TJvDockVSNETPanel(lbDockServer1.BottomDockPanel).DoAutoHideControl(SearchForm);

//  SearchResultForm.Height := 100;
  SearchResultForm.Top := 10000;
//  SearchResultForm.Visible := true;
  SearchResultForm.ManualDock(SearchResultForm,lbDockServer1.BottomDockPanel,alBottom);
  lbDockServer1.BottomDockPanel.ShowDockPanel(true,SearchResultForm);
  TJvDockVSNETPanel(lbDockServer1.BottomDockPanel).DoAutoHideControl(SearchResultForm);
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
end;

procedure TMSDN2002.FormCreate(Sender: TObject);
begin
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
var DockClient: TJvDockClient;
begin
  if PopupMenu1.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu1.PopupComponent));
    if DockClient <> nil then
    begin
      Dockable_Item.Checked := DockClient.EnableDock;
      if DockClient.DockState = JvDockState_Floating then
      begin
        Float_Item.Caption := 'Docking';
        AutoHide_Item.Visible := False;
      end else
      begin
        Float_Item.Caption := 'Floating';
      end;
    end;
  end;
end;

procedure TMSDN2002.Dockable_ItemClick(Sender: TObject);
var DockClient: TJvDockClient;
begin
  if PopupMenu1.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu1.PopupComponent));
    DockClient.EnableDock := not DockClient.EnableDock;
    DockClient.RestoreChild;
  end;
end;

procedure TMSDN2002.Hide_ItemClick(Sender: TObject);
var DockClient: TJvDockClient;
begin
  if PopupMenu1.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu1.PopupComponent));
    if DockClient <> nil then
      DockClient.HideParentForm;
  end;
end;

procedure TMSDN2002.Float_ItemClick(Sender: TObject);
var DockClient: TJvDockClient;
begin
  if PopupMenu1.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu1.PopupComponent));
    if DockClient <> nil then
      DockClient.RestoreChild;
  end;
end;

end.
